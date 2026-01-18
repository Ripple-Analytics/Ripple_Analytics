import SwiftUI
import BackgroundTasks
import CoreMotion
import CoreLocation
import HealthKit

@main
struct MentalModelsApp: App {
    @StateObject private var appState = AppState()
    @StateObject private var learningEngine = ContinuousLearningEngine()
    @StateObject private var sensorManager = SensorDataManager()
    
    init() {
        registerBackgroundTasks()
    }
    
    var body: some Scene {
        WindowGroup {
            ContentView()
                .environmentObject(appState)
                .environmentObject(learningEngine)
                .environmentObject(sensorManager)
                .onAppear {
                    learningEngine.startContinuousLearning()
                    sensorManager.startCollecting()
                }
        }
    }
    
    private func registerBackgroundTasks() {
        BGTaskScheduler.shared.register(
            forTaskWithIdentifier: "com.ripple.mentalmodels.learning",
            using: nil
        ) { task in
            handleLearningTask(task: task as! BGProcessingTask)
        }
        
        BGTaskScheduler.shared.register(
            forTaskWithIdentifier: "com.ripple.mentalmodels.sync",
            using: nil
        ) { task in
            handleSyncTask(task: task as! BGAppRefreshTask)
        }
    }
    
    private func handleLearningTask(task: BGProcessingTask) {
        scheduleNextLearningTask()
        
        let queue = OperationQueue()
        queue.maxConcurrentOperationCount = 1
        
        let operation = LearningOperation(engine: learningEngine)
        
        task.expirationHandler = {
            queue.cancelAllOperations()
        }
        
        operation.completionBlock = {
            task.setTaskCompleted(success: !operation.isCancelled)
        }
        
        queue.addOperation(operation)
    }
    
    private func handleSyncTask(task: BGAppRefreshTask) {
        scheduleNextSyncTask()
        
        Task {
            do {
                try await learningEngine.syncWithServer()
                task.setTaskCompleted(success: true)
            } catch {
                task.setTaskCompleted(success: false)
            }
        }
    }
    
    private func scheduleNextLearningTask() {
        let request = BGProcessingTaskRequest(identifier: "com.ripple.mentalmodels.learning")
        request.requiresNetworkConnectivity = false
        request.requiresExternalPower = false
        request.earliestBeginDate = Date(timeIntervalSinceNow: 60)
        
        do {
            try BGTaskScheduler.shared.submit(request)
        } catch {
            print("Could not schedule learning task: \(error)")
        }
    }
    
    private func scheduleNextSyncTask() {
        let request = BGAppRefreshTaskRequest(identifier: "com.ripple.mentalmodels.sync")
        request.earliestBeginDate = Date(timeIntervalSinceNow: 300)
        
        do {
            try BGTaskScheduler.shared.submit(request)
        } catch {
            print("Could not schedule sync task: \(error)")
        }
    }
}

class LearningOperation: Operation {
    let engine: ContinuousLearningEngine
    
    init(engine: ContinuousLearningEngine) {
        self.engine = engine
    }
    
    override func main() {
        guard !isCancelled else { return }
        
        let semaphore = DispatchSemaphore(value: 0)
        
        Task {
            await engine.processBackgroundLearning()
            semaphore.signal()
        }
        
        semaphore.wait()
    }
}

class AppState: ObservableObject {
    @Published var isConnected = false
    @Published var serverURL: String = "http://localhost:8001"
    @Published var totalModelsLearned: Int = 0
    @Published var totalInsightsGenerated: Int = 0
    @Published var learningRate: Double = 0.0
    @Published var lastSyncTime: Date?
    
    func updateStats(models: Int, insights: Int, rate: Double) {
        DispatchQueue.main.async {
            self.totalModelsLearned = models
            self.totalInsightsGenerated = insights
            self.learningRate = rate
        }
    }
}
