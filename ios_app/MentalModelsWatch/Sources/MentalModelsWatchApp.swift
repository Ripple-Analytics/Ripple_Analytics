import SwiftUI
import WatchKit
import HealthKit
import WatchConnectivity

@main
struct MentalModelsWatchApp: App {
    @StateObject private var watchState = WatchAppState()
    @StateObject private var learningEngine = WatchLearningEngine()
    @StateObject private var healthManager = WatchHealthManager()
    
    var body: some Scene {
        WindowGroup {
            ContentView()
                .environmentObject(watchState)
                .environmentObject(learningEngine)
                .environmentObject(healthManager)
                .onAppear {
                    learningEngine.startContinuousLearning()
                    healthManager.startMonitoring()
                }
        }
    }
}

class WatchAppState: ObservableObject {
    @Published var isConnectedToPhone = false
    @Published var lastSyncTime: Date?
    @Published var insightsCount = 0
    @Published var learningActive = true
}

class WatchLearningEngine: ObservableObject {
    @Published var isLearning = false
    @Published var dataPointsCollected = 0
    @Published var patternsDetected = 0
    
    private var timer: Timer?
    
    func startContinuousLearning() {
        isLearning = true
        
        timer = Timer.scheduledTimer(withTimeInterval: 5.0, repeats: true) { [weak self] _ in
            self?.processLearningCycle()
        }
    }
    
    func stopContinuousLearning() {
        timer?.invalidate()
        timer = nil
        isLearning = false
    }
    
    private func processLearningCycle() {
        dataPointsCollected += 1
        
        if dataPointsCollected % 10 == 0 {
            patternsDetected += 1
        }
    }
}

class WatchHealthManager: NSObject, ObservableObject {
    @Published var heartRate: Double = 0
    @Published var steps: Int = 0
    @Published var activeCalories: Double = 0
    @Published var isMonitoring = false
    
    private let healthStore = HKHealthStore()
    private var heartRateQuery: HKQuery?
    
    func startMonitoring() {
        guard HKHealthStore.isHealthDataAvailable() else { return }
        
        let typesToRead: Set<HKObjectType> = [
            HKObjectType.quantityType(forIdentifier: .heartRate)!,
            HKObjectType.quantityType(forIdentifier: .stepCount)!,
            HKObjectType.quantityType(forIdentifier: .activeEnergyBurned)!
        ]
        
        healthStore.requestAuthorization(toShare: nil, read: typesToRead) { [weak self] success, error in
            if success {
                DispatchQueue.main.async {
                    self?.isMonitoring = true
                }
                self?.startHeartRateStreaming()
            }
        }
    }
    
    private func startHeartRateStreaming() {
        guard let heartRateType = HKObjectType.quantityType(forIdentifier: .heartRate) else { return }
        
        let query = HKAnchoredObjectQuery(
            type: heartRateType,
            predicate: nil,
            anchor: nil,
            limit: HKObjectQueryNoLimit
        ) { [weak self] query, samples, deletedObjects, anchor, error in
            self?.processHeartRateSamples(samples)
        }
        
        query.updateHandler = { [weak self] query, samples, deletedObjects, anchor, error in
            self?.processHeartRateSamples(samples)
        }
        
        healthStore.execute(query)
        heartRateQuery = query
    }
    
    private func processHeartRateSamples(_ samples: [HKSample]?) {
        guard let samples = samples as? [HKQuantitySample], let sample = samples.last else { return }
        
        let heartRate = sample.quantity.doubleValue(for: HKUnit(from: "count/min"))
        
        DispatchQueue.main.async {
            self.heartRate = heartRate
        }
    }
}

struct ContentView: View {
    @EnvironmentObject var watchState: WatchAppState
    @EnvironmentObject var learningEngine: WatchLearningEngine
    @EnvironmentObject var healthManager: WatchHealthManager
    
    var body: some View {
        TabView {
            DashboardView()
            
            HealthView()
            
            ModelsView()
            
            SettingsView()
        }
        .tabViewStyle(.verticalPage)
    }
}

struct DashboardView: View {
    @EnvironmentObject var learningEngine: WatchLearningEngine
    
    var body: some View {
        ScrollView {
            VStack(spacing: 12) {
                HStack {
                    Image(systemName: "brain.head.profile")
                        .font(.title2)
                        .foregroundColor(.blue)
                    
                    Text("Mental Models")
                        .font(.headline)
                }
                
                Divider()
                
                HStack {
                    VStack(alignment: .leading) {
                        Text("\(learningEngine.dataPointsCollected)")
                            .font(.title2)
                            .fontWeight(.bold)
                        Text("Data Points")
                            .font(.caption2)
                            .foregroundColor(.secondary)
                    }
                    
                    Spacer()
                    
                    VStack(alignment: .trailing) {
                        Text("\(learningEngine.patternsDetected)")
                            .font(.title2)
                            .fontWeight(.bold)
                        Text("Patterns")
                            .font(.caption2)
                            .foregroundColor(.secondary)
                    }
                }
                
                LearningStatusView(isActive: learningEngine.isLearning)
            }
            .padding()
        }
    }
}

struct LearningStatusView: View {
    let isActive: Bool
    
    var body: some View {
        HStack {
            Circle()
                .fill(isActive ? Color.green : Color.gray)
                .frame(width: 8, height: 8)
            
            Text(isActive ? "Learning Active" : "Paused")
                .font(.caption)
                .foregroundColor(isActive ? .green : .secondary)
        }
        .padding(.vertical, 8)
        .padding(.horizontal, 12)
        .background(Color(.darkGray).opacity(0.3))
        .clipShape(Capsule())
    }
}

struct HealthView: View {
    @EnvironmentObject var healthManager: WatchHealthManager
    
    var body: some View {
        ScrollView {
            VStack(spacing: 16) {
                Text("Health Data")
                    .font(.headline)
                
                HealthMetricView(
                    icon: "heart.fill",
                    value: String(format: "%.0f", healthManager.heartRate),
                    unit: "BPM",
                    color: .red
                )
                
                HealthMetricView(
                    icon: "figure.walk",
                    value: "\(healthManager.steps)",
                    unit: "Steps",
                    color: .green
                )
                
                HealthMetricView(
                    icon: "flame.fill",
                    value: String(format: "%.0f", healthManager.activeCalories),
                    unit: "Cal",
                    color: .orange
                )
            }
            .padding()
        }
    }
}

struct HealthMetricView: View {
    let icon: String
    let value: String
    let unit: String
    let color: Color
    
    var body: some View {
        HStack {
            Image(systemName: icon)
                .font(.title3)
                .foregroundColor(color)
            
            VStack(alignment: .leading) {
                Text(value)
                    .font(.title3)
                    .fontWeight(.bold)
                Text(unit)
                    .font(.caption2)
                    .foregroundColor(.secondary)
            }
            
            Spacer()
        }
        .padding(12)
        .background(Color(.darkGray).opacity(0.3))
        .clipShape(RoundedRectangle(cornerRadius: 12))
    }
}

struct ModelsView: View {
    let quickModels = [
        ("Circle of Competence", "brain"),
        ("Inversion", "arrow.uturn.backward"),
        ("First Principles", "cube"),
        ("Margin of Safety", "shield")
    ]
    
    var body: some View {
        ScrollView {
            VStack(spacing: 8) {
                Text("Quick Models")
                    .font(.headline)
                
                ForEach(quickModels, id: \.0) { model in
                    HStack {
                        Image(systemName: model.1)
                            .font(.caption)
                            .foregroundColor(.blue)
                        
                        Text(model.0)
                            .font(.caption)
                        
                        Spacer()
                        
                        Image(systemName: "chevron.right")
                            .font(.caption2)
                            .foregroundColor(.secondary)
                    }
                    .padding(10)
                    .background(Color(.darkGray).opacity(0.3))
                    .clipShape(RoundedRectangle(cornerRadius: 8))
                }
            }
            .padding()
        }
    }
}

struct SettingsView: View {
    @EnvironmentObject var watchState: WatchAppState
    @EnvironmentObject var learningEngine: WatchLearningEngine
    
    var body: some View {
        ScrollView {
            VStack(spacing: 12) {
                Text("Settings")
                    .font(.headline)
                
                Toggle(isOn: $watchState.learningActive) {
                    Label("Learning", systemImage: "brain")
                        .font(.caption)
                }
                .onChange(of: watchState.learningActive) { newValue in
                    if newValue {
                        learningEngine.startContinuousLearning()
                    } else {
                        learningEngine.stopContinuousLearning()
                    }
                }
                
                HStack {
                    Image(systemName: watchState.isConnectedToPhone ? "iphone" : "iphone.slash")
                        .foregroundColor(watchState.isConnectedToPhone ? .green : .red)
                    
                    Text(watchState.isConnectedToPhone ? "Connected" : "Disconnected")
                        .font(.caption)
                    
                    Spacer()
                }
                .padding(10)
                .background(Color(.darkGray).opacity(0.3))
                .clipShape(RoundedRectangle(cornerRadius: 8))
            }
            .padding()
        }
    }
}

#Preview {
    ContentView()
        .environmentObject(WatchAppState())
        .environmentObject(WatchLearningEngine())
        .environmentObject(WatchHealthManager())
}
