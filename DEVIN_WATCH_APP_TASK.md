# DEVIN TASK: Apple Watch App (WatchOS)

## PRIORITY: HIGH
## BRANCH: `feature/apple-watch-app`

---

## Overview

Build a native Apple Watch application using **SwiftUI** that provides wrist-based access to the Mental Models System. This app will:

1. **Display key stats** at a glance
2. **Quick capture** thoughts via voice (Siri)
3. **Complications** for watch faces
4. **Haptic alerts** for Lollapalooza events
5. **Decision reminders** with haptic nudges
6. **Offline sync** with iPhone companion app

---

## Tech Stack

- **Language**: Swift 5.9+
- **UI Framework**: SwiftUI for WatchOS
- **Architecture**: MVVM
- **Connectivity**: WatchConnectivity framework
- **Complications**: ClockKit / WidgetKit
- **Notifications**: UserNotifications + WKExtendedRuntimeSession

---

## File Structure

```
MentalModelsWatch/
├── App/
│   └── MentalModelsWatchApp.swift
├── Models/
│   ├── WatchStats.swift
│   └── QuickCapture.swift
├── Views/
│   ├── ContentView.swift
│   ├── StatsView.swift
│   ├── CaptureView.swift
│   ├── ModelsListView.swift
│   └── SettingsView.swift
├── ViewModels/
│   ├── WatchViewModel.swift
│   └── CaptureViewModel.swift
├── Services/
│   ├── WatchConnectivityService.swift
│   └── HapticService.swift
├── Complications/
│   ├── ComplicationController.swift
│   └── ComplicationViews.swift
└── Resources/
    └── Assets.xcassets
```

---

## Core Requirements

### 1. Main Watch App

```swift
// App/MentalModelsWatchApp.swift
import SwiftUI
import WatchKit

@main
struct MentalModelsWatchApp: App {
    @StateObject private var connectivityService = WatchConnectivityService()
    
    var body: some Scene {
        WindowGroup {
            ContentView()
                .environmentObject(connectivityService)
        }
    }
}

// Views/ContentView.swift
struct ContentView: View {
    @EnvironmentObject var connectivity: WatchConnectivityService
    
    var body: some View {
        TabView {
            StatsView()
            CaptureView()
            ModelsListView()
            SettingsView()
        }
        .tabViewStyle(.page)
    }
}
```

### 2. Stats View (Glanceable)

```swift
// Views/StatsView.swift
import SwiftUI

struct StatsView: View {
    @EnvironmentObject var connectivity: WatchConnectivityService
    @State private var showingDataSource = false
    @State private var selectedStat: StatType?
    
    var body: some View {
        ScrollView {
            VStack(spacing: 12) {
                // Header
                HStack {
                    Image(systemName: "brain")
                        .foregroundColor(.blue)
                    Text("Mental Models")
                        .font(.headline)
                }
                
                // Stats Grid - All Clickable
                LazyVGrid(columns: [GridItem(.flexible()), GridItem(.flexible())], spacing: 8) {
                    StatButton(
                        title: "Models",
                        value: connectivity.stats?.modelCount,
                        icon: "brain",
                        color: .blue
                    ) {
                        selectedStat = .models
                        showingDataSource = true
                    }
                    
                    StatButton(
                        title: "Failures",
                        value: connectivity.stats?.failureModeCount,
                        icon: "exclamationmark.triangle",
                        color: .orange
                    ) {
                        selectedStat = .failureModes
                        showingDataSource = true
                    }
                    
                    StatButton(
                        title: "Today",
                        value: connectivity.stats?.analysesToday,
                        icon: "doc.text",
                        color: .green
                    ) {
                        selectedStat = .analysesToday
                        showingDataSource = true
                    }
                    
                    StatButton(
                        title: "Lollapalooza",
                        value: connectivity.stats?.lollapaloozaCount,
                        icon: "sparkles",
                        color: .purple
                    ) {
                        selectedStat = .lollapalooza
                        showingDataSource = true
                    }
                }
                
                // Lollapalooza Alert
                if let lollapalooza = connectivity.stats?.lollapaloozaCount, lollapalooza > 0 {
                    LollapaloozaAlert(count: lollapalooza)
                }
                
                // Last Sync
                if let lastSync = connectivity.lastSyncTime {
                    Text("Synced \(lastSync, style: .relative) ago")
                        .font(.caption2)
                        .foregroundColor(.secondary)
                }
            }
            .padding()
        }
        .sheet(isPresented: $showingDataSource) {
            if let stat = selectedStat {
                DataSourceView(statType: stat, stats: connectivity.stats)
            }
        }
    }
}

struct StatButton: View {
    let title: String
    let value: Int?
    let icon: String
    let color: Color
    let action: () -> Void
    
    var body: some View {
        Button(action: action) {
            VStack(spacing: 4) {
                Image(systemName: icon)
                    .font(.title3)
                    .foregroundColor(color)
                
                Text(value.map { "\($0)" } ?? "NA")
                    .font(.system(.title2, design: .rounded, weight: .bold))
                
                Text(title)
                    .font(.caption2)
                    .foregroundColor(.secondary)
            }
            .frame(maxWidth: .infinity)
            .padding(.vertical, 8)
            .background(Color(.darkGray).opacity(0.3))
            .cornerRadius(8)
        }
        .buttonStyle(.plain)
    }
}

struct LollapaloozaAlert: View {
    let count: Int
    @State private var isAnimating = false
    
    var body: some View {
        HStack {
            Image(systemName: "sparkles")
                .foregroundColor(.purple)
                .scaleEffect(isAnimating ? 1.2 : 1.0)
            
            Text("\(count) Lollapalooza!")
                .font(.caption)
                .fontWeight(.semibold)
        }
        .padding(.horizontal, 12)
        .padding(.vertical, 6)
        .background(Color.purple.opacity(0.2))
        .cornerRadius(20)
        .onAppear {
            withAnimation(.easeInOut(duration: 0.5).repeatForever()) {
                isAnimating = true
            }
        }
    }
}

struct DataSourceView: View {
    let statType: StatType
    let stats: WatchStats?
    
    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 8) {
                Text("Data Source")
                    .font(.headline)
                
                Text("Table: \(statType.tableName)")
                    .font(.caption)
                
                Text("Query: \(statType.query)")
                    .font(.caption2)
                    .foregroundColor(.secondary)
                
                if let fetchedAt = stats?.fetchedAt {
                    Text("Fetched: \(fetchedAt, style: .relative) ago")
                        .font(.caption2)
                        .foregroundColor(.secondary)
                }
                
                Divider()
                
                Text("Value: \(statType.value(from: stats) ?? "NA")")
                    .font(.title3)
                    .fontWeight(.bold)
            }
            .padding()
        }
    }
}

enum StatType {
    case models, failureModes, analysesToday, lollapalooza
    
    var tableName: String {
        switch self {
        case .models: return "mental_models"
        case .failureModes: return "failure_modes"
        case .analysesToday: return "analyses"
        case .lollapalooza: return "analyses (filtered)"
        }
    }
    
    var query: String {
        switch self {
        case .models: return "SELECT COUNT(*) FROM mental_models"
        case .failureModes: return "SELECT COUNT(*) FROM failure_modes"
        case .analysesToday: return "SELECT COUNT(*) FROM analyses WHERE DATE(created_at) = CURDATE()"
        case .lollapalooza: return "SELECT COUNT(*) FROM analyses WHERE lollapalooza_detected = true"
        }
    }
    
    func value(from stats: WatchStats?) -> String? {
        guard let stats = stats else { return nil }
        switch self {
        case .models: return "\(stats.modelCount)"
        case .failureModes: return "\(stats.failureModeCount)"
        case .analysesToday: return "\(stats.analysesToday)"
        case .lollapalooza: return "\(stats.lollapaloozaCount)"
        }
    }
}
```

### 3. Voice Capture

```swift
// Views/CaptureView.swift
import SwiftUI
import Speech

struct CaptureView: View {
    @StateObject private var viewModel = CaptureViewModel()
    
    var body: some View {
        VStack(spacing: 16) {
            Text("Quick Capture")
                .font(.headline)
            
            // Voice Button
            Button(action: { viewModel.toggleRecording() }) {
                ZStack {
                    Circle()
                        .fill(viewModel.isRecording ? Color.red : Color.blue)
                        .frame(width: 60, height: 60)
                    
                    Image(systemName: viewModel.isRecording ? "stop.fill" : "mic.fill")
                        .font(.title2)
                        .foregroundColor(.white)
                }
            }
            .buttonStyle(.plain)
            
            // Status
            if viewModel.isRecording {
                Text("Listening...")
                    .font(.caption)
                    .foregroundColor(.red)
            } else if viewModel.isProcessing {
                ProgressView()
                Text("Analyzing...")
                    .font(.caption)
            } else if let result = viewModel.lastResult {
                VStack(spacing: 4) {
                    Text("\(result.detectedCount) models")
                        .font(.caption)
                    if result.lollapalooza {
                        Text("🎯 Lollapalooza!")
                            .font(.caption)
                            .foregroundColor(.purple)
                    }
                }
            }
            
            // Transcribed Text Preview
            if !viewModel.transcribedText.isEmpty {
                Text(viewModel.transcribedText)
                    .font(.caption2)
                    .lineLimit(3)
                    .foregroundColor(.secondary)
            }
        }
        .padding()
    }
}

// ViewModels/CaptureViewModel.swift
import Foundation
import Speech
import WatchKit

class CaptureViewModel: NSObject, ObservableObject {
    @Published var isRecording = false
    @Published var isProcessing = false
    @Published var transcribedText = ""
    @Published var lastResult: CaptureResult?
    
    private var audioEngine: AVAudioEngine?
    private var recognitionRequest: SFSpeechAudioBufferRecognitionRequest?
    private var recognitionTask: SFSpeechRecognitionTask?
    private let speechRecognizer = SFSpeechRecognizer(locale: Locale(identifier: "en-US"))
    
    func toggleRecording() {
        if isRecording {
            stopRecording()
        } else {
            startRecording()
        }
    }
    
    private func startRecording() {
        // Request authorization
        SFSpeechRecognizer.requestAuthorization { [weak self] status in
            guard status == .authorized else { return }
            
            DispatchQueue.main.async {
                self?.isRecording = true
                self?.transcribedText = ""
                // Haptic feedback
                WKInterfaceDevice.current().play(.start)
            }
            
            self?.startSpeechRecognition()
        }
    }
    
    private func stopRecording() {
        audioEngine?.stop()
        recognitionRequest?.endAudio()
        isRecording = false
        
        // Haptic feedback
        WKInterfaceDevice.current().play(.stop)
        
        // Process the captured text
        if !transcribedText.isEmpty {
            analyzeText()
        }
    }
    
    private func startSpeechRecognition() {
        // Implementation of speech recognition
        // Uses SFSpeechRecognizer to transcribe audio
    }
    
    private func analyzeText() {
        isProcessing = true
        
        // Send to iPhone for analysis via WatchConnectivity
        WatchConnectivityService.shared.sendForAnalysis(transcribedText) { [weak self] result in
            DispatchQueue.main.async {
                self?.isProcessing = false
                self?.lastResult = result
                
                // Haptic for Lollapalooza
                if result.lollapalooza {
                    WKInterfaceDevice.current().play(.notification)
                } else {
                    WKInterfaceDevice.current().play(.success)
                }
            }
        }
    }
}

struct CaptureResult {
    let detectedCount: Int
    let lollapalooza: Bool
    let topModels: [String]
}
```

### 4. Watch Connectivity

```swift
// Services/WatchConnectivityService.swift
import Foundation
import WatchConnectivity

class WatchConnectivityService: NSObject, ObservableObject {
    static let shared = WatchConnectivityService()
    
    @Published var stats: WatchStats?
    @Published var lastSyncTime: Date?
    @Published var isReachable = false
    
    private var session: WCSession?
    
    override init() {
        super.init()
        
        if WCSession.isSupported() {
            session = WCSession.default
            session?.delegate = self
            session?.activate()
        }
    }
    
    func requestSync() {
        guard let session = session, session.isReachable else {
            // Use cached data
            return
        }
        
        session.sendMessage(["action": "sync"], replyHandler: { [weak self] response in
            if let statsData = response["stats"] as? Data,
               let stats = try? JSONDecoder().decode(WatchStats.self, from: statsData) {
                DispatchQueue.main.async {
                    self?.stats = stats
                    self?.lastSyncTime = Date()
                }
            }
        }, errorHandler: { error in
            print("Sync error: \(error)")
        })
    }
    
    func sendForAnalysis(_ text: String, completion: @escaping (CaptureResult) -> Void) {
        guard let session = session, session.isReachable else {
            // Queue for later
            return
        }
        
        session.sendMessage(["action": "analyze", "text": text], replyHandler: { response in
            let result = CaptureResult(
                detectedCount: response["detectedCount"] as? Int ?? 0,
                lollapalooza: response["lollapalooza"] as? Bool ?? false,
                topModels: response["topModels"] as? [String] ?? []
            )
            completion(result)
        }, errorHandler: { error in
            print("Analysis error: \(error)")
        })
    }
}

extension WatchConnectivityService: WCSessionDelegate {
    func session(_ session: WCSession, activationDidCompleteWith activationState: WCSessionActivationState, error: Error?) {
        DispatchQueue.main.async {
            self.isReachable = session.isReachable
        }
        
        if activationState == .activated {
            requestSync()
        }
    }
    
    func sessionReachabilityDidChange(_ session: WCSession) {
        DispatchQueue.main.async {
            self.isReachable = session.isReachable
        }
        
        if session.isReachable {
            requestSync()
        }
    }
    
    func session(_ session: WCSession, didReceiveMessage message: [String : Any]) {
        // Handle push updates from iPhone
        if let action = message["action"] as? String {
            switch action {
            case "lollapalooza":
                // Lollapalooza detected - haptic alert!
                DispatchQueue.main.async {
                    WKInterfaceDevice.current().play(.notification)
                }
            case "statsUpdate":
                if let statsData = message["stats"] as? Data,
                   let stats = try? JSONDecoder().decode(WatchStats.self, from: statsData) {
                    DispatchQueue.main.async {
                        self.stats = stats
                        self.lastSyncTime = Date()
                    }
                }
            default:
                break
            }
        }
    }
}

struct WatchStats: Codable {
    let modelCount: Int
    let failureModeCount: Int
    let analysesToday: Int
    let lollapaloozaCount: Int
    let fetchedAt: Date
    
    // Data source for traceability
    let dataSource: String
}
```

### 5. Complications

```swift
// Complications/ComplicationViews.swift
import SwiftUI
import WidgetKit

struct MentalModelsComplication: Widget {
    let kind: String = "MentalModelsComplication"
    
    var body: some WidgetConfiguration {
        StaticConfiguration(kind: kind, provider: ComplicationProvider()) { entry in
            ComplicationView(entry: entry)
        }
        .configurationDisplayName("Mental Models")
        .description("Quick stats at a glance")
        .supportedFamilies([
            .accessoryCircular,
            .accessoryRectangular,
            .accessoryInline,
            .accessoryCorner
        ])
    }
}

struct ComplicationProvider: TimelineProvider {
    func placeholder(in context: Context) -> ComplicationEntry {
        ComplicationEntry(date: Date(), stats: nil)
    }
    
    func getSnapshot(in context: Context, completion: @escaping (ComplicationEntry) -> Void) {
        let entry = ComplicationEntry(date: Date(), stats: WatchConnectivityService.shared.stats)
        completion(entry)
    }
    
    func getTimeline(in context: Context, completion: @escaping (Timeline<ComplicationEntry>) -> Void) {
        let entry = ComplicationEntry(date: Date(), stats: WatchConnectivityService.shared.stats)
        let nextUpdate = Calendar.current.date(byAdding: .minute, value: 15, to: Date())!
        let timeline = Timeline(entries: [entry], policy: .after(nextUpdate))
        completion(timeline)
    }
}

struct ComplicationEntry: TimelineEntry {
    let date: Date
    let stats: WatchStats?
}

struct ComplicationView: View {
    var entry: ComplicationEntry
    @Environment(\.widgetFamily) var family
    
    var body: some View {
        switch family {
        case .accessoryCircular:
            CircularComplication(stats: entry.stats)
        case .accessoryRectangular:
            RectangularComplication(stats: entry.stats)
        case .accessoryInline:
            InlineComplication(stats: entry.stats)
        case .accessoryCorner:
            CornerComplication(stats: entry.stats)
        @unknown default:
            CircularComplication(stats: entry.stats)
        }
    }
}

struct CircularComplication: View {
    let stats: WatchStats?
    
    var body: some View {
        ZStack {
            AccessoryWidgetBackground()
            
            VStack(spacing: 0) {
                Image(systemName: "brain")
                    .font(.caption)
                
                Text(stats.map { "\($0.modelCount)" } ?? "NA")
                    .font(.system(.title3, design: .rounded, weight: .bold))
            }
        }
    }
}

struct RectangularComplication: View {
    let stats: WatchStats?
    
    var body: some View {
        HStack {
            VStack(alignment: .leading) {
                Text("Mental Models")
                    .font(.caption2)
                    .foregroundColor(.secondary)
                
                HStack(spacing: 12) {
                    Label(stats.map { "\($0.modelCount)" } ?? "NA", systemImage: "brain")
                    Label(stats.map { "\($0.lollapaloozaCount)" } ?? "0", systemImage: "sparkles")
                }
                .font(.caption)
            }
            
            Spacer()
        }
    }
}

struct InlineComplication: View {
    let stats: WatchStats?
    
    var body: some View {
        if let stats = stats, stats.lollapaloozaCount > 0 {
            Text("🎯 \(stats.lollapaloozaCount) Lollapalooza")
        } else {
            Text("🧠 \(stats?.modelCount ?? 0) Models")
        }
    }
}

struct CornerComplication: View {
    let stats: WatchStats?
    
    var body: some View {
        ZStack {
            Text(stats.map { "\($0.modelCount)" } ?? "NA")
                .font(.system(.title, design: .rounded, weight: .bold))
        }
        .widgetLabel {
            Text("Models")
        }
    }
}
```

### 6. Haptic Service

```swift
// Services/HapticService.swift
import WatchKit

class HapticService {
    static let shared = HapticService()
    
    private let device = WKInterfaceDevice.current()
    
    /// Play haptic for Lollapalooza detection
    func playLollapalooza() {
        // Triple haptic for emphasis
        device.play(.notification)
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.3) {
            self.device.play(.notification)
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.6) {
            self.device.play(.notification)
        }
    }
    
    /// Play haptic for successful capture
    func playSuccess() {
        device.play(.success)
    }
    
    /// Play haptic for starting recording
    func playStart() {
        device.play(.start)
    }
    
    /// Play haptic for stopping recording
    func playStop() {
        device.play(.stop)
    }
    
    /// Play haptic for failure mode warning
    func playWarning() {
        device.play(.failure)
    }
    
    /// Play subtle haptic for data refresh
    func playRefresh() {
        device.play(.click)
    }
}
```

---

## Design Requirements

### Visual Design
- **Glanceable** - All key info visible in 2 seconds
- **High contrast** - Readable in bright sunlight
- **Large touch targets** - Minimum 44pt
- **SF Symbols** throughout
- **Consistent with iOS app** styling

### Interactions
- **Digital Crown** for scrolling
- **Force Touch** (if supported) for quick actions
- **Haptic feedback** for all important events
- **Voice input** as primary capture method

### Complications
- **Circular**: Model count with brain icon
- **Rectangular**: Model count + Lollapalooza count
- **Inline**: Lollapalooza alert or model count
- **Corner**: Model count with label

---

## Acceptance Criteria

- [ ] App launches on WatchOS 10+
- [ ] Stats view shows real data from iPhone
- [ ] All numbers are clickable to show data source
- [ ] Voice capture works and sends to iPhone for analysis
- [ ] Haptic feedback for Lollapalooza detection
- [ ] Complications show real data
- [ ] Syncs with iPhone via WatchConnectivity
- [ ] Works offline with cached data
- [ ] No hardcoded fake data anywhere
- [ ] "NA" displayed when data unavailable

---

## Integration with iOS App

The Watch app requires the iOS companion app to be installed. Add this to the iOS app:

```swift
// iOS App - WatchConnectivity Handler
import WatchConnectivity

class WatchSessionManager: NSObject, WCSessionDelegate {
    static let shared = WatchSessionManager()
    
    func startSession() {
        if WCSession.isSupported() {
            let session = WCSession.default
            session.delegate = self
            session.activate()
        }
    }
    
    func session(_ session: WCSession, didReceiveMessage message: [String : Any], replyHandler: @escaping ([String : Any]) -> Void) {
        guard let action = message["action"] as? String else { return }
        
        switch action {
        case "sync":
            Task {
                let stats = try? await APIService.shared.fetchStats()
                let statsData = try? JSONEncoder().encode(stats)
                replyHandler(["stats": statsData as Any])
            }
            
        case "analyze":
            if let text = message["text"] as? String {
                Task {
                    let analysis = try? await APIService.shared.analyzeText(text)
                    replyHandler([
                        "detectedCount": analysis?.detectedModels.count ?? 0,
                        "lollapalooza": analysis?.lollapaloozaDetected ?? false,
                        "topModels": analysis?.detectedModels.prefix(3).map { $0.modelId.uuidString } ?? []
                    ])
                }
            }
            
        default:
            replyHandler([:])
        }
    }
    
    // Push Lollapalooza alerts to Watch
    func notifyWatchOfLollapalooza() {
        guard WCSession.default.isReachable else { return }
        WCSession.default.sendMessage(["action": "lollapalooza"], replyHandler: nil)
    }
}
```

---

## Timeline

- **Day 1**: Core Watch app structure, Stats view
- **Day 2**: Voice capture, WatchConnectivity
- **Day 3**: Complications (all families)
- **Day 4**: Haptic service, polish
- **Day 5**: Testing, iOS integration, submission prep

---

## Notes

- Watch app is companion to iOS app (requires iPhone)
- Prioritize glanceability - users look at watch for 2-3 seconds
- Haptics are crucial for non-visual feedback
- Battery efficiency matters - minimize background work
- Every number must trace to real data source
- "NA" when data unavailable, never fake numbers
