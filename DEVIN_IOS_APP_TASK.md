# DEVIN TASK: iOS App (SwiftUI)

## PRIORITY: HIGH
## BRANCH: `feature/ios-app`

---

## Overview

Build a native iOS application using **SwiftUI** that provides mobile access to the Mental Models System. This app will:

1. **Display mental models** organized by Munger's hierarchy
2. **Quick capture** thoughts/observations for analysis
3. **View analysis results** from desktop/web
4. **Decision journal** for tracking decisions
5. **Widgets** for home screen quick access
6. **Siri Shortcuts** for voice capture
7. **Offline mode** with local caching

---

## Tech Stack

- **Language**: Swift 5.9+
- **UI Framework**: SwiftUI
- **Architecture**: MVVM with Combine
- **Networking**: URLSession + async/await
- **Local Storage**: SwiftData (iOS 17+) or Core Data
- **Widgets**: WidgetKit
- **Siri**: App Intents framework
- **Charts**: Swift Charts

---

## File Structure

```
MentalModels/
├── App/
│   ├── MentalModelsApp.swift
│   └── AppDelegate.swift
├── Models/
│   ├── MentalModel.swift
│   ├── Category.swift
│   ├── FailureMode.swift
│   ├── Analysis.swift
│   ├── Decision.swift
│   └── Observation.swift
├── Views/
│   ├── Dashboard/
│   │   ├── DashboardView.swift
│   │   └── StatsCard.swift
│   ├── Models/
│   │   ├── ModelsListView.swift
│   │   ├── CategoryView.swift
│   │   └── ModelDetailView.swift
│   ├── Analysis/
│   │   ├── AnalysisListView.swift
│   │   └── AnalysisDetailView.swift
│   ├── Capture/
│   │   ├── QuickCaptureView.swift
│   │   └── VoiceCaptureView.swift
│   ├── Journal/
│   │   ├── DecisionJournalView.swift
│   │   └── DecisionEntryView.swift
│   └── Settings/
│       └── SettingsView.swift
├── ViewModels/
│   ├── DashboardViewModel.swift
│   ├── ModelsViewModel.swift
│   ├── AnalysisViewModel.swift
│   └── JournalViewModel.swift
├── Services/
│   ├── APIService.swift
│   ├── AnalysisService.swift
│   ├── SyncService.swift
│   └── CacheService.swift
├── Widgets/
│   ├── MentalModelsWidget.swift
│   ├── QuickCaptureWidget.swift
│   └── StatsWidget.swift
├── Intents/
│   ├── CaptureIntent.swift
│   └── AnalyzeIntent.swift
└── Resources/
    ├── Assets.xcassets
    └── Localizable.strings
```

---

## Core Requirements

### 1. Data Models

```swift
// Models/MentalModel.swift
import Foundation
import SwiftData

@Model
final class MentalModel: Identifiable {
    @Attribute(.unique) var id: UUID
    var name: String
    var slug: String
    var description: String
    var categoryId: UUID
    var keywords: [String]
    var examples: [String]
    var relatedModels: [UUID]
    var createdAt: Date
    var updatedAt: Date
    
    // Munger's hierarchy level
    var hierarchyLevel: HierarchyLevel
    
    enum HierarchyLevel: String, Codable {
        case hardSciences = "hard_sciences"
        case lifeSciences = "life_sciences"
        case appliedKnowledge = "applied_knowledge"
        case flightSchool = "flight_school"
        case flightSimulators = "flight_simulators"
        case metaSkills = "meta_skills"
    }
}

@Model
final class Category: Identifiable {
    @Attribute(.unique) var id: UUID
    var name: String
    var slug: String
    var description: String
    var hierarchyLevel: MentalModel.HierarchyLevel
    var sortOrder: Int
    var iconName: String
    
    @Relationship(deleteRule: .cascade)
    var models: [MentalModel]
}

@Model
final class FailureMode: Identifiable {
    @Attribute(.unique) var id: UUID
    var name: String
    var description: String
    var severity: Severity
    var modelId: UUID
    
    enum Severity: String, Codable {
        case critical, high, medium, low
    }
}

@Model
final class Analysis: Identifiable {
    @Attribute(.unique) var id: UUID
    var sourceText: String
    var sourceType: SourceType
    var detectedModels: [DetectedModel]
    var lollapaloozaDetected: Bool
    var failureModes: [UUID]
    var createdAt: Date
    var syncedAt: Date?
    
    enum SourceType: String, Codable {
        case quickCapture, voiceCapture, imported, desktop
    }
    
    struct DetectedModel: Codable {
        var modelId: UUID
        var score: Double
        var confidence: Confidence
        var evidence: [String]
        
        enum Confidence: String, Codable {
            case veryHigh, high, moderate, low
        }
    }
}

@Model
final class Decision: Identifiable {
    @Attribute(.unique) var id: UUID
    var title: String
    var context: String
    var modelsApplied: [UUID]
    var expectedOutcome: String
    var actualOutcome: String?
    var lessonsLearned: String?
    var rating: Int? // 1-5
    var createdAt: Date
    var reviewedAt: Date?
}
```

### 2. Dashboard View

```swift
// Views/Dashboard/DashboardView.swift
import SwiftUI
import Charts

struct DashboardView: View {
    @StateObject private var viewModel = DashboardViewModel()
    
    var body: some View {
        NavigationStack {
            ScrollView {
                VStack(spacing: 20) {
                    // Stats Row - All clickable to show source
                    HStack(spacing: 16) {
                        ClickableStatCard(
                            title: "Mental Models",
                            value: viewModel.modelCount,
                            icon: "brain",
                            color: .blue
                        ) {
                            viewModel.showDataSource(for: .modelCount)
                        }
                        
                        ClickableStatCard(
                            title: "Failure Modes",
                            value: viewModel.failureModeCount,
                            icon: "exclamationmark.triangle",
                            color: .orange
                        ) {
                            viewModel.showDataSource(for: .failureModeCount)
                        }
                    }
                    
                    HStack(spacing: 16) {
                        ClickableStatCard(
                            title: "Analyses Today",
                            value: viewModel.analysesToday,
                            icon: "doc.text.magnifyingglass",
                            color: .green
                        ) {
                            viewModel.showDataSource(for: .analysesToday)
                        }
                        
                        ClickableStatCard(
                            title: "Lollapalooza",
                            value: viewModel.lollapaloozaCount,
                            icon: "sparkles",
                            color: .purple
                        ) {
                            viewModel.showDataSource(for: .lollapaloozaCount)
                        }
                    }
                    
                    // Munger's Hierarchy Section
                    MungerHierarchySection(viewModel: viewModel)
                    
                    // Recent Activity
                    RecentActivitySection(analyses: viewModel.recentAnalyses)
                    
                    // Quick Actions
                    QuickActionsSection()
                }
                .padding()
            }
            .navigationTitle("Mental Models")
            .toolbar {
                ToolbarItem(placement: .primaryAction) {
                    Button(action: { viewModel.sync() }) {
                        Image(systemName: "arrow.triangle.2.circlepath")
                    }
                }
            }
            .sheet(isPresented: $viewModel.showingDataSource) {
                DataSourceSheet(source: viewModel.currentDataSource)
            }
        }
    }
}

struct ClickableStatCard: View {
    let title: String
    let value: Int?
    let icon: String
    let color: Color
    let action: () -> Void
    
    var body: some View {
        Button(action: action) {
            VStack(alignment: .leading, spacing: 8) {
                HStack {
                    Image(systemName: icon)
                        .foregroundColor(color)
                    Spacer()
                    Image(systemName: "info.circle")
                        .font(.caption)
                        .foregroundColor(.secondary)
                }
                
                Text(value.map { "\($0)" } ?? "NA")
                    .font(.system(size: 28, weight: .bold, design: .rounded))
                    .foregroundColor(.primary)
                
                Text(title)
                    .font(.caption)
                    .foregroundColor(.secondary)
            }
            .padding()
            .background(Color(.systemBackground))
            .cornerRadius(12)
            .shadow(color: .black.opacity(0.05), radius: 5, x: 0, y: 2)
        }
        .buttonStyle(.plain)
    }
}

struct MungerHierarchySection: View {
    @ObservedObject var viewModel: DashboardViewModel
    
    var body: some View {
        VStack(alignment: .leading, spacing: 12) {
            Text("Munger's Hierarchy")
                .font(.headline)
            
            ForEach(MentalModel.HierarchyLevel.allCases, id: \.self) { level in
                HierarchyRow(
                    level: level,
                    categories: viewModel.categories(for: level),
                    modelCount: viewModel.modelCount(for: level)
                )
            }
        }
        .padding()
        .background(Color(.systemBackground))
        .cornerRadius(12)
    }
}
```

### 3. Quick Capture

```swift
// Views/Capture/QuickCaptureView.swift
import SwiftUI

struct QuickCaptureView: View {
    @StateObject private var viewModel = QuickCaptureViewModel()
    @Environment(\.dismiss) private var dismiss
    
    var body: some View {
        NavigationStack {
            VStack(spacing: 20) {
                // Text Input
                TextEditor(text: $viewModel.captureText)
                    .frame(minHeight: 150)
                    .padding(8)
                    .background(Color(.systemGray6))
                    .cornerRadius(12)
                
                // Voice Input Button
                Button(action: { viewModel.startVoiceCapture() }) {
                    Label("Voice Capture", systemImage: "mic.fill")
                        .frame(maxWidth: .infinity)
                        .padding()
                        .background(Color.blue)
                        .foregroundColor(.white)
                        .cornerRadius(12)
                }
                
                // Analysis Progress
                if viewModel.isAnalyzing {
                    ProgressView(value: viewModel.analysisProgress) {
                        Text(viewModel.analysisStatus)
                    }
                }
                
                // Results Preview
                if let results = viewModel.analysisResults {
                    AnalysisPreview(results: results)
                }
                
                Spacer()
            }
            .padding()
            .navigationTitle("Quick Capture")
            .toolbar {
                ToolbarItem(placement: .cancellationAction) {
                    Button("Cancel") { dismiss() }
                }
                ToolbarItem(placement: .confirmationAction) {
                    Button("Analyze") {
                        viewModel.analyze()
                    }
                    .disabled(viewModel.captureText.isEmpty || viewModel.isAnalyzing)
                }
            }
        }
    }
}
```

### 4. API Service

```swift
// Services/APIService.swift
import Foundation

actor APIService {
    static let shared = APIService()
    
    private let baseURL = URL(string: "https://your-web-app.manus.space/api")!
    private var authToken: String?
    
    func setAuthToken(_ token: String) {
        self.authToken = token
    }
    
    // MARK: - Models
    
    func fetchModels() async throws -> [MentalModel] {
        let url = baseURL.appendingPathComponent("v1/models")
        let data = try await request(url: url)
        return try JSONDecoder().decode([MentalModel].self, from: data)
    }
    
    func fetchCategories() async throws -> [Category] {
        let url = baseURL.appendingPathComponent("v1/categories")
        let data = try await request(url: url)
        return try JSONDecoder().decode([Category].self, from: data)
    }
    
    func fetchFailureModes() async throws -> [FailureMode] {
        let url = baseURL.appendingPathComponent("v1/failure-modes")
        let data = try await request(url: url)
        return try JSONDecoder().decode([FailureMode].self, from: data)
    }
    
    // MARK: - Analysis
    
    func analyzeText(_ text: String) async throws -> Analysis {
        let url = baseURL.appendingPathComponent("v1/analyze")
        let body = ["text": text]
        let data = try await request(url: url, method: "POST", body: body)
        return try JSONDecoder().decode(Analysis.self, from: data)
    }
    
    func syncAnalysis(_ analysis: Analysis) async throws {
        let url = baseURL.appendingPathComponent("v1/analyses")
        _ = try await request(url: url, method: "POST", body: analysis)
    }
    
    // MARK: - Stats
    
    func fetchStats() async throws -> DashboardStats {
        let url = baseURL.appendingPathComponent("v1/stats")
        let data = try await request(url: url)
        return try JSONDecoder().decode(DashboardStats.self, from: data)
    }
    
    // MARK: - Private
    
    private func request(url: URL, method: String = "GET", body: Encodable? = nil) async throws -> Data {
        var request = URLRequest(url: url)
        request.httpMethod = method
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        
        if let token = authToken {
            request.setValue("Bearer \(token)", forHTTPHeaderField: "Authorization")
        }
        
        if let body = body {
            request.httpBody = try JSONEncoder().encode(body)
        }
        
        let (data, response) = try await URLSession.shared.data(for: request)
        
        guard let httpResponse = response as? HTTPURLResponse,
              200..<300 ~= httpResponse.statusCode else {
            throw APIError.invalidResponse
        }
        
        return data
    }
    
    enum APIError: Error {
        case invalidResponse
        case unauthorized
        case serverError(String)
    }
}

struct DashboardStats: Codable {
    let modelCount: Int
    let failureModeCount: Int
    let analysesToday: Int
    let lollapaloozaCount: Int
    let categoryCounts: [String: Int]
    
    // Data source info for traceability
    let dataSource: DataSource
    
    struct DataSource: Codable {
        let query: String
        let table: String
        let fetchedAt: Date
    }
}
```

### 5. Widgets

```swift
// Widgets/MentalModelsWidget.swift
import WidgetKit
import SwiftUI

struct MentalModelsWidget: Widget {
    let kind: String = "MentalModelsWidget"
    
    var body: some WidgetConfiguration {
        StaticConfiguration(kind: kind, provider: Provider()) { entry in
            MentalModelsWidgetView(entry: entry)
        }
        .configurationDisplayName("Mental Models")
        .description("Quick stats and recent activity")
        .supportedFamilies([.systemSmall, .systemMedium, .systemLarge])
    }
}

struct Provider: TimelineProvider {
    func placeholder(in context: Context) -> WidgetEntry {
        WidgetEntry(date: Date(), stats: nil)
    }
    
    func getSnapshot(in context: Context, completion: @escaping (WidgetEntry) -> Void) {
        Task {
            let stats = try? await APIService.shared.fetchStats()
            completion(WidgetEntry(date: Date(), stats: stats))
        }
    }
    
    func getTimeline(in context: Context, completion: @escaping (Timeline<WidgetEntry>) -> Void) {
        Task {
            let stats = try? await APIService.shared.fetchStats()
            let entry = WidgetEntry(date: Date(), stats: stats)
            let nextUpdate = Calendar.current.date(byAdding: .minute, value: 15, to: Date())!
            let timeline = Timeline(entries: [entry], policy: .after(nextUpdate))
            completion(timeline)
        }
    }
}

struct WidgetEntry: TimelineEntry {
    let date: Date
    let stats: DashboardStats?
}

struct MentalModelsWidgetView: View {
    var entry: WidgetEntry
    @Environment(\.widgetFamily) var family
    
    var body: some View {
        switch family {
        case .systemSmall:
            SmallWidgetView(stats: entry.stats)
        case .systemMedium:
            MediumWidgetView(stats: entry.stats)
        case .systemLarge:
            LargeWidgetView(stats: entry.stats)
        @unknown default:
            SmallWidgetView(stats: entry.stats)
        }
    }
}

struct SmallWidgetView: View {
    let stats: DashboardStats?
    
    var body: some View {
        VStack(alignment: .leading) {
            HStack {
                Image(systemName: "brain")
                    .foregroundColor(.blue)
                Text("Models")
                    .font(.caption)
            }
            
            Text(stats.map { "\($0.modelCount)" } ?? "NA")
                .font(.system(size: 36, weight: .bold, design: .rounded))
            
            Spacer()
            
            if let lollapalooza = stats?.lollapaloozaCount, lollapalooza > 0 {
                HStack {
                    Image(systemName: "sparkles")
                        .foregroundColor(.purple)
                    Text("\(lollapalooza) Lollapalooza")
                        .font(.caption2)
                }
            }
        }
        .padding()
    }
}
```

### 6. Siri Shortcuts

```swift
// Intents/CaptureIntent.swift
import AppIntents

struct CaptureThoughtIntent: AppIntent {
    static var title: LocalizedStringResource = "Capture Thought"
    static var description = IntentDescription("Capture a thought for mental model analysis")
    
    @Parameter(title: "Thought")
    var thought: String
    
    static var parameterSummary: some ParameterSummary {
        Summary("Capture \(\.$thought)")
    }
    
    func perform() async throws -> some IntentResult & ProvidesDialog {
        let analysis = try await APIService.shared.analyzeText(thought)
        
        let topModels = analysis.detectedModels.prefix(3)
            .map { $0.modelId.uuidString }
            .joined(separator: ", ")
        
        return .result(dialog: "Analyzed. Top models: \(topModels)")
    }
}

struct AnalyzeClipboardIntent: AppIntent {
    static var title: LocalizedStringResource = "Analyze Clipboard"
    static var description = IntentDescription("Analyze clipboard text for mental models")
    
    func perform() async throws -> some IntentResult & ProvidesDialog {
        guard let text = UIPasteboard.general.string else {
            return .result(dialog: "No text in clipboard")
        }
        
        let analysis = try await APIService.shared.analyzeText(text)
        
        if analysis.lollapaloozaDetected {
            return .result(dialog: "Lollapalooza detected! Multiple models converging.")
        }
        
        let count = analysis.detectedModels.count
        return .result(dialog: "Found \(count) mental models in the text.")
    }
}

// App Shortcuts Provider
struct MentalModelsShortcuts: AppShortcutsProvider {
    static var appShortcuts: [AppShortcut] {
        AppShortcut(
            intent: CaptureThoughtIntent(),
            phrases: [
                "Capture thought in \(.applicationName)",
                "Analyze this with \(.applicationName)",
                "Mental model check"
            ],
            shortTitle: "Capture Thought",
            systemImageName: "brain"
        )
        
        AppShortcut(
            intent: AnalyzeClipboardIntent(),
            phrases: [
                "Analyze clipboard with \(.applicationName)",
                "Check clipboard for mental models"
            ],
            shortTitle: "Analyze Clipboard",
            systemImageName: "doc.on.clipboard"
        )
    }
}
```

---

## Design Requirements

### Visual Design
- **Clean, minimal interface** following iOS Human Interface Guidelines
- **SF Symbols** for all icons
- **Dynamic Type** support for accessibility
- **Dark mode** support
- **Haptic feedback** for important actions

### Navigation
- **Tab bar** with: Dashboard, Models, Capture, Journal, Settings
- **Hierarchical navigation** within each tab
- **Search** available throughout

### Data Display
- **Every number clickable** to show data source
- **NA** displayed when data unavailable
- **Pull to refresh** on all list views
- **Loading states** with skeletons

---

## Acceptance Criteria

- [ ] App launches on iOS 17+
- [ ] Dashboard shows real stats from API
- [ ] All numbers are clickable to show data source
- [ ] Models organized by Munger's hierarchy
- [ ] Quick capture works with text and voice
- [ ] Analysis results display correctly
- [ ] Decision journal CRUD operations work
- [ ] Widgets display real data
- [ ] Siri shortcuts work for capture and analysis
- [ ] Offline mode caches data locally
- [ ] Syncs with web app when online
- [ ] No hardcoded fake data anywhere

---

## Timeline

- **Day 1-2**: Core data models, API service, SwiftData setup
- **Day 3-4**: Dashboard, Models list, Model detail views
- **Day 5-6**: Quick capture, voice input, analysis display
- **Day 7-8**: Decision journal, settings
- **Day 9**: Widgets, Siri shortcuts
- **Day 10**: Testing, polish, App Store preparation

---

## Notes

- Use async/await throughout
- Implement proper error handling with user-friendly messages
- Cache aggressively for offline support
- Follow Munger's hierarchy in all organization
- Every number must trace back to real data source
