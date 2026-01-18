import SwiftUI
import WatchKit
import HealthKit
import WatchConnectivity

// MARK: - Watch Design System (M&S + Costco: Monochrome + Red)
struct WatchDesign {
    // Monochrome palette
    static let black = Color.black
    static let white = Color.white
    static let gray900 = Color(white: 0.1)
    static let gray700 = Color(white: 0.2)
    static let gray600 = Color(white: 0.3)
    static let gray500 = Color(white: 0.4)
    static let gray400 = Color(white: 0.5)
    static let gray200 = Color(white: 0.8)
    
    // Strategic red accent - ONLY color allowed
    static let accent = Color(red: 0.8, green: 0.1, blue: 0.1)
    
    // Typography for Watch
    static let titleFont = Font.system(size: 16, weight: .semibold)
    static let bodyFont = Font.system(size: 14, weight: .regular)
    static let captionFont = Font.system(size: 12, weight: .regular)
    static let metricFont = Font.system(size: 28, weight: .bold, design: .monospaced)
    static let smallMetricFont = Font.system(size: 20, weight: .bold, design: .monospaced)
    static let microFont = Font.system(size: 10, weight: .medium)
}

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
                    Text("MENTAL MODELS")
                        .font(WatchDesign.microFont)
                        .foregroundColor(WatchDesign.gray500)
                    
                    Spacer()
                    
                    Circle()
                        .fill(learningEngine.isLearning ? WatchDesign.accent : WatchDesign.gray500)
                        .frame(width: 6, height: 6)
                }
                
                Rectangle()
                    .fill(WatchDesign.gray200)
                    .frame(height: 1)
                
                HStack {
                    VStack(alignment: .leading, spacing: 2) {
                        Text("\(learningEngine.dataPointsCollected)")
                            .font(WatchDesign.smallMetricFont)
                            .foregroundColor(WatchDesign.gray900)
                        Text("DATA PTS")
                            .font(WatchDesign.microFont)
                            .foregroundColor(WatchDesign.gray500)
                    }
                    
                    Spacer()
                    
                    VStack(alignment: .trailing, spacing: 2) {
                        Text("\(learningEngine.patternsDetected)")
                            .font(WatchDesign.smallMetricFont)
                            .foregroundColor(WatchDesign.accent)
                        Text("PATTERNS")
                            .font(WatchDesign.microFont)
                            .foregroundColor(WatchDesign.gray500)
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
                .fill(isActive ? WatchDesign.accent : WatchDesign.gray500)
                .frame(width: 6, height: 6)
            
            Text(isActive ? "LEARNING" : "PAUSED")
                .font(WatchDesign.microFont)
                .foregroundColor(isActive ? WatchDesign.accent : WatchDesign.gray500)
        }
        .padding(.vertical, 6)
        .padding(.horizontal, 10)
        .background(WatchDesign.gray200.opacity(0.3))
        .clipShape(Capsule())
    }
}

struct HealthView: View {
    @EnvironmentObject var healthManager: WatchHealthManager
    
    var body: some View {
        ScrollView {
            VStack(spacing: 12) {
                Text("HEALTH DATA")
                    .font(WatchDesign.microFont)
                    .foregroundColor(WatchDesign.gray500)
                    .frame(maxWidth: .infinity, alignment: .leading)
                
                HealthMetricView(
                    icon: "heart.fill",
                    value: String(format: "%.0f", healthManager.heartRate),
                    unit: "BPM",
                    isHighlighted: true
                )
                
                HealthMetricView(
                    icon: "figure.walk",
                    value: "\(healthManager.steps)",
                    unit: "STEPS",
                    isHighlighted: false
                )
                
                HealthMetricView(
                    icon: "flame.fill",
                    value: String(format: "%.0f", healthManager.activeCalories),
                    unit: "CAL",
                    isHighlighted: false
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
    let isHighlighted: Bool
    
    var body: some View {
        HStack {
            Image(systemName: icon)
                .font(.system(size: 14))
                .foregroundColor(isHighlighted ? WatchDesign.accent : WatchDesign.gray600)
            
            VStack(alignment: .leading, spacing: 2) {
                Text(value)
                    .font(WatchDesign.smallMetricFont)
                    .foregroundColor(isHighlighted ? WatchDesign.accent : WatchDesign.gray900)
                Text(unit)
                    .font(WatchDesign.microFont)
                    .foregroundColor(WatchDesign.gray500)
            }
            
            Spacer()
        }
        .padding(10)
        .background(WatchDesign.gray200.opacity(0.3))
        .clipShape(RoundedRectangle(cornerRadius: 8))
    }
}

struct ModelsView: View {
    let quickModels = [
        ("Circle of Competence", "list.bullet", 5),
        ("Inversion", "arrow.uturn.backward", 5),
        ("First Principles", "cube", 5),
        ("Margin of Safety", "shield", 5)
    ]
    
    var body: some View {
        ScrollView {
            VStack(spacing: 8) {
                HStack {
                    Text("129 MODELS")
                        .font(WatchDesign.microFont)
                        .foregroundColor(WatchDesign.gray500)
                    Spacer()
                }
                
                ForEach(quickModels, id: \.0) { model in
                    HStack {
                        VStack(alignment: .leading, spacing: 2) {
                            Text(model.0)
                                .font(WatchDesign.captionFont)
                                .foregroundColor(WatchDesign.gray900)
                            
                            Text("\(model.2) failures")
                                .font(WatchDesign.microFont)
                                .foregroundColor(WatchDesign.accent)
                        }
                        
                        Spacer()
                        
                        Image(systemName: "chevron.right")
                            .font(.system(size: 10))
                            .foregroundColor(WatchDesign.gray400)
                    }
                    .padding(10)
                    .background(WatchDesign.gray200.opacity(0.3))
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
                HStack {
                    Text("SETTINGS")
                        .font(WatchDesign.microFont)
                        .foregroundColor(WatchDesign.gray500)
                    Spacer()
                }
                
                // Learning Toggle
                HStack {
                    Circle()
                        .fill(watchState.learningActive ? WatchDesign.accent : WatchDesign.gray500)
                        .frame(width: 6, height: 6)
                    
                    Text("LEARNING")
                        .font(WatchDesign.captionFont)
                        .foregroundColor(WatchDesign.gray900)
                    
                    Spacer()
                    
                    Toggle("", isOn: $watchState.learningActive)
                        .labelsHidden()
                        .tint(WatchDesign.accent)
                }
                .padding(10)
                .background(WatchDesign.gray200.opacity(0.3))
                .clipShape(RoundedRectangle(cornerRadius: 8))
                .onChange(of: watchState.learningActive) { newValue in
                    if newValue {
                        learningEngine.startContinuousLearning()
                    } else {
                        learningEngine.stopContinuousLearning()
                    }
                }
                
                // Connection Status
                HStack {
                    Image(systemName: watchState.isConnectedToPhone ? "iphone" : "iphone.slash")
                        .font(.system(size: 14))
                        .foregroundColor(watchState.isConnectedToPhone ? WatchDesign.gray600 : WatchDesign.accent)
                    
                    Text(watchState.isConnectedToPhone ? "CONNECTED" : "DISCONNECTED")
                        .font(WatchDesign.captionFont)
                        .foregroundColor(watchState.isConnectedToPhone ? WatchDesign.gray600 : WatchDesign.accent)
                    
                    Spacer()
                }
                .padding(10)
                .background(WatchDesign.gray200.opacity(0.3))
                .clipShape(RoundedRectangle(cornerRadius: 8))
                
                // Version
                HStack {
                    Text("VERSION")
                        .font(WatchDesign.microFont)
                        .foregroundColor(WatchDesign.gray500)
                    
                    Spacer()
                    
                    Text("1.0.0")
                        .font(WatchDesign.captionFont)
                        .foregroundColor(WatchDesign.gray600)
                }
                .padding(10)
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
