import SwiftUI

// MARK: - Design System (M&S + Costco: Monochrome + Red Accents)
struct DesignSystem {
    // Monochrome palette
    static let black = Color.black
    static let white = Color.white
    static let gray900 = Color(white: 0.1)
    static let gray800 = Color(white: 0.15)
    static let gray700 = Color(white: 0.2)
    static let gray600 = Color(white: 0.3)
    static let gray500 = Color(white: 0.4)
    static let gray400 = Color(white: 0.5)
    static let gray300 = Color(white: 0.6)
    static let gray200 = Color(white: 0.8)
    static let gray100 = Color(white: 0.9)
    static let gray50 = Color(white: 0.95)
    
    // Strategic red accent - ONLY color allowed
    static let accent = Color(red: 0.8, green: 0.1, blue: 0.1)
    static let accentLight = Color(red: 0.9, green: 0.2, blue: 0.2)
    
    // Typography - Clean, professional
    static let titleFont = Font.system(size: 17, weight: .semibold, design: .default)
    static let headlineFont = Font.system(size: 15, weight: .semibold, design: .default)
    static let bodyFont = Font.system(size: 13, weight: .regular, design: .default)
    static let captionFont = Font.system(size: 11, weight: .regular, design: .default)
    static let metricFont = Font.system(size: 24, weight: .bold, design: .monospaced)
    static let smallMetricFont = Font.system(size: 18, weight: .bold, design: .monospaced)
    static let microFont = Font.system(size: 9, weight: .medium, design: .default)
}

// MARK: - Haptic Feedback Manager
class HapticManager {
    static let shared = HapticManager()
    
    func impact(_ style: UIImpactFeedbackGenerator.FeedbackStyle = .medium) {
        let generator = UIImpactFeedbackGenerator(style: style)
        generator.impactOccurred()
    }
    
    func notification(_ type: UINotificationFeedbackGenerator.FeedbackType) {
        let generator = UINotificationFeedbackGenerator()
        generator.notificationOccurred(type)
    }
    
    func selection() {
        let generator = UISelectionFeedbackGenerator()
        generator.selectionChanged()
    }
}

// MARK: - Main Content View
struct ContentView: View {
    @EnvironmentObject var appState: AppState
    @EnvironmentObject var learningEngine: ContinuousLearningEngine
    @EnvironmentObject var sensorManager: SensorDataManager
    
    @State private var selectedTab = 0
    @State private var showingSettings = false
    
    var body: some View {
        ZStack {
            DesignSystem.gray50
                .ignoresSafeArea()
            
            TabView(selection: $selectedTab) {
                DashboardView()
                    .tabItem {
                        Label("Dashboard", systemImage: "square.grid.2x2")
                    }
                    .tag(0)
                
                ModelsView()
                    .tabItem {
                        Label("Models", systemImage: "list.bullet")
                    }
                    .tag(1)
                
                AnalyzeView()
                    .tabItem {
                        Label("Analyze", systemImage: "magnifyingglass")
                    }
                    .tag(2)
                
                InsightsView()
                    .tabItem {
                        Label("Insights", systemImage: "lightbulb")
                    }
                    .tag(3)
                
                LearningView()
                    .tabItem {
                        Label("Learning", systemImage: "arrow.triangle.2.circlepath")
                    }
                    .tag(4)
            }
            .tint(DesignSystem.accent)
            .onChange(of: selectedTab) { _ in
                HapticManager.shared.selection()
            }
        }
        .sheet(isPresented: $showingSettings) {
            SettingsView()
        }
    }
}

// MARK: - Dashboard View (Value Line Density)
struct DashboardView: View {
    @EnvironmentObject var appState: AppState
    @EnvironmentObject var learningEngine: ContinuousLearningEngine
    
    @State private var animateCards = false
    
    var body: some View {
        NavigationStack {
            ScrollView {
                VStack(spacing: 1) {
                    statusBar
                    metricsGrid
                    activitySection
                    quickActionsSection
                }
            }
            .background(DesignSystem.gray100)
            .navigationTitle("Mental Models")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .navigationBarTrailing) {
                    Button(action: {
                        HapticManager.shared.impact(.light)
                    }) {
                        Image(systemName: "gearshape")
                            .font(.system(size: 16, weight: .medium))
                            .foregroundColor(DesignSystem.gray600)
                    }
                }
            }
        }
        .onAppear {
            withAnimation(.easeOut(duration: 0.3)) {
                animateCards = true
            }
        }
    }
    
    private var statusBar: some View {
        HStack(spacing: 0) {
            HStack(spacing: 6) {
                Circle()
                    .fill(learningEngine.isLearning ? DesignSystem.accent : DesignSystem.gray400)
                    .frame(width: 6, height: 6)
                
                Text(learningEngine.isLearning ? "LEARNING" : "IDLE")
                    .font(DesignSystem.microFont)
                    .foregroundColor(learningEngine.isLearning ? DesignSystem.accent : DesignSystem.gray500)
            }
            
            Spacer()
            
            Text("Updated \(formatTime(Date()))")
                .font(DesignSystem.microFont)
                .foregroundColor(DesignSystem.gray500)
        }
        .padding(.horizontal, 16)
        .padding(.vertical, 8)
        .background(DesignSystem.white)
    }
    
    private var metricsGrid: some View {
        VStack(spacing: 1) {
            HStack(spacing: 1) {
                MetricCell(label: "MODELS", value: "129", change: nil, isHighlighted: false)
                MetricCell(label: "FAILURES", value: "645", change: nil, isHighlighted: false)
                MetricCell(label: "CATEGORIES", value: "34", change: nil, isHighlighted: false)
            }
            
            HStack(spacing: 1) {
                MetricCell(label: "INSIGHTS", value: "\(learningEngine.insightsGenerated)", change: "+12", isHighlighted: true)
                MetricCell(label: "DATA PTS", value: formatCompact(learningEngine.dataPointsProcessed), change: nil, isHighlighted: false)
                MetricCell(label: "RATE/SEC", value: String(format: "%.1f", learningEngine.learningRatePerSecond), change: nil, isHighlighted: false)
            }
            
            HStack(spacing: 1) {
                MetricCell(label: "UPDATES", value: "\(learningEngine.modelsUpdated)", change: nil, isHighlighted: false)
                MetricCell(label: "COVERAGE", value: "100%", change: nil, isHighlighted: false)
                MetricCell(label: "ACCURACY", value: "94.2%", change: "+0.3", isHighlighted: true)
            }
        }
        .background(DesignSystem.gray200)
    }
    
    private var activitySection: some View {
        VStack(spacing: 0) {
            HStack {
                Text("RECENT ACTIVITY")
                    .font(DesignSystem.microFont)
                    .foregroundColor(DesignSystem.gray500)
                Spacer()
                Text("VIEW ALL")
                    .font(DesignSystem.microFont)
                    .foregroundColor(DesignSystem.accent)
            }
            .padding(.horizontal, 16)
            .padding(.vertical, 10)
            .background(DesignSystem.gray100)
            
            VStack(spacing: 0) {
                ActivityItem(time: "2m", title: "Document analyzed", detail: "3 models detected", isNew: true)
                Divider().background(DesignSystem.gray200)
                ActivityItem(time: "5m", title: "Pattern discovered", detail: "Confirmation bias", isNew: true)
                Divider().background(DesignSystem.gray200)
                ActivityItem(time: "12m", title: "Lollapalooza detected", detail: "High confidence", isNew: false)
                Divider().background(DesignSystem.gray200)
                ActivityItem(time: "1h", title: "Sync completed", detail: "847 items", isNew: false)
            }
            .background(DesignSystem.white)
        }
        .padding(.top, 12)
    }
    
    private var quickActionsSection: some View {
        VStack(spacing: 0) {
            HStack {
                Text("QUICK ACTIONS")
                    .font(DesignSystem.microFont)
                    .foregroundColor(DesignSystem.gray500)
                Spacer()
            }
            .padding(.horizontal, 16)
            .padding(.vertical, 10)
            .background(DesignSystem.gray100)
            
            HStack(spacing: 0) {
                QuickAction(icon: "doc.text.magnifyingglass", label: "Analyze")
                Divider().frame(height: 40).background(DesignSystem.gray200)
                QuickAction(icon: "doc.viewfinder", label: "Scan")
                Divider().frame(height: 40).background(DesignSystem.gray200)
                QuickAction(icon: "mic", label: "Voice")
                Divider().frame(height: 40).background(DesignSystem.gray200)
                QuickAction(icon: "square.and.arrow.up", label: "Share")
            }
            .background(DesignSystem.white)
        }
        .padding(.top, 12)
        .padding(.bottom, 20)
    }
    
    private func formatTime(_ date: Date) -> String {
        let formatter = DateFormatter()
        formatter.dateFormat = "HH:mm"
        return formatter.string(from: date)
    }
    
    private func formatCompact(_ num: Int) -> String {
        if num >= 1000000 { return String(format: "%.1fM", Double(num) / 1000000) }
        else if num >= 1000 { return String(format: "%.1fK", Double(num) / 1000) }
        return "\(num)"
    }
}

// MARK: - Metric Cell (Value Line Style)
struct MetricCell: View {
    let label: String
    let value: String
    let change: String?
    let isHighlighted: Bool
    
    var body: some View {
        VStack(spacing: 2) {
            Text(label)
                .font(DesignSystem.microFont)
                .foregroundColor(DesignSystem.gray500)
            
            Text(value)
                .font(DesignSystem.smallMetricFont)
                .foregroundColor(isHighlighted ? DesignSystem.accent : DesignSystem.gray900)
            
            if let change = change {
                Text(change)
                    .font(DesignSystem.microFont)
                    .foregroundColor(DesignSystem.accent)
            } else {
                Text(" ")
                    .font(DesignSystem.microFont)
            }
        }
        .frame(maxWidth: .infinity)
        .padding(.vertical, 12)
        .background(DesignSystem.white)
    }
}

// MARK: - Activity Item
struct ActivityItem: View {
    let time: String
    let title: String
    let detail: String
    let isNew: Bool
    
    var body: some View {
        HStack(spacing: 12) {
            Text(time)
                .font(DesignSystem.captionFont)
                .foregroundColor(DesignSystem.gray500)
                .frame(width: 30, alignment: .trailing)
            
            if isNew {
                Circle().fill(DesignSystem.accent).frame(width: 4, height: 4)
            } else {
                Circle().fill(Color.clear).frame(width: 4, height: 4)
            }
            
            VStack(alignment: .leading, spacing: 1) {
                Text(title)
                    .font(DesignSystem.bodyFont)
                    .foregroundColor(DesignSystem.gray900)
                Text(detail)
                    .font(DesignSystem.captionFont)
                    .foregroundColor(DesignSystem.gray500)
            }
            
            Spacer()
            
            Image(systemName: "chevron.right")
                .font(.system(size: 10, weight: .medium))
                .foregroundColor(DesignSystem.gray400)
        }
        .padding(.horizontal, 16)
        .padding(.vertical, 10)
        .contentShape(Rectangle())
        .onTapGesture { HapticManager.shared.impact(.light) }
    }
}

// MARK: - Quick Action
struct QuickAction: View {
    let icon: String
    let label: String
    
    var body: some View {
        Button(action: { HapticManager.shared.impact(.medium) }) {
            VStack(spacing: 6) {
                Image(systemName: icon)
                    .font(.system(size: 20, weight: .regular))
                    .foregroundColor(DesignSystem.gray700)
                Text(label)
                    .font(DesignSystem.microFont)
                    .foregroundColor(DesignSystem.gray600)
            }
            .frame(maxWidth: .infinity)
            .padding(.vertical, 16)
        }
        .buttonStyle(PlainButtonStyle())
    }
}

// MARK: - Models View (High Density Table)
struct ModelsView: View {
    @State private var searchText = ""
    @State private var selectedCategory: String?
    @State private var sortOrder: SortOrder = .name
    
    enum SortOrder { case name, category, failures }
    
    let categories = ["Mathematics", "Accounting", "Physics", "Chemistry", "Engineering", "Biology", "Psychology", "Economics"]
    
    var body: some View {
        NavigationStack {
            VStack(spacing: 0) {
                categoryBar
                sortBar
                modelsList
            }
            .background(DesignSystem.gray100)
            .navigationTitle("129 Models")
            .navigationBarTitleDisplayMode(.inline)
            .searchable(text: $searchText, prompt: "Search models...")
        }
    }
    
    private var categoryBar: some View {
        ScrollView(.horizontal, showsIndicators: false) {
            HStack(spacing: 0) {
                CategoryTab(title: "ALL", isSelected: selectedCategory == nil) {
                    HapticManager.shared.selection()
                    selectedCategory = nil
                }
                ForEach(categories, id: \.self) { category in
                    CategoryTab(title: category.uppercased(), isSelected: selectedCategory == category) {
                        HapticManager.shared.selection()
                        selectedCategory = category
                    }
                }
            }
            .padding(.horizontal, 8)
        }
        .padding(.vertical, 8)
        .background(DesignSystem.white)
    }
    
    private var sortBar: some View {
        HStack(spacing: 0) {
            Text("NAME").font(DesignSystem.microFont).foregroundColor(sortOrder == .name ? DesignSystem.accent : DesignSystem.gray500).frame(maxWidth: .infinity, alignment: .leading)
            Text("CATEGORY").font(DesignSystem.microFont).foregroundColor(sortOrder == .category ? DesignSystem.accent : DesignSystem.gray500).frame(width: 80)
            Text("FAIL").font(DesignSystem.microFont).foregroundColor(sortOrder == .failures ? DesignSystem.accent : DesignSystem.gray500).frame(width: 40, alignment: .trailing)
        }
        .padding(.horizontal, 16)
        .padding(.vertical, 8)
        .background(DesignSystem.gray200)
    }
    
    private var modelsList: some View {
        ScrollView {
            LazyVStack(spacing: 0) {
                ForEach(sampleModels, id: \.name) { model in
                    ModelRow(model: model)
                    Divider().background(DesignSystem.gray200)
                }
            }
            .background(DesignSystem.white)
        }
    }
    
    private var sampleModels: [MentalModelItem] {
        [
            MentalModelItem(name: "Circle of Competence", category: "Thinking", thinker: "Buffett", failures: 5),
            MentalModelItem(name: "Inversion", category: "Thinking", thinker: "Munger", failures: 5),
            MentalModelItem(name: "First Principles", category: "Thinking", thinker: "Aristotle", failures: 5),
            MentalModelItem(name: "Confirmation Bias", category: "Psychology", thinker: "Wason", failures: 5),
            MentalModelItem(name: "Compound Interest", category: "Math", thinker: "Einstein", failures: 5),
            MentalModelItem(name: "Network Effects", category: "Economics", thinker: "Metcalfe", failures: 5),
            MentalModelItem(name: "Margin of Safety", category: "Investing", thinker: "Graham", failures: 5),
            MentalModelItem(name: "Second-Order Thinking", category: "Thinking", thinker: "Marks", failures: 5),
            MentalModelItem(name: "Incentives", category: "Psychology", thinker: "Munger", failures: 5),
            MentalModelItem(name: "Opportunity Cost", category: "Economics", thinker: "Various", failures: 5),
            MentalModelItem(name: "Occam's Razor", category: "Thinking", thinker: "Ockham", failures: 5),
            MentalModelItem(name: "Hanlon's Razor", category: "Thinking", thinker: "Hanlon", failures: 5),
            MentalModelItem(name: "Survivorship Bias", category: "Statistics", thinker: "Wald", failures: 5),
            MentalModelItem(name: "Regression to Mean", category: "Statistics", thinker: "Galton", failures: 5),
            MentalModelItem(name: "Pareto Principle", category: "Math", thinker: "Pareto", failures: 5)
        ]
    }
}

struct MentalModelItem {
    let name: String
    let category: String
    let thinker: String
    let failures: Int
}

struct CategoryTab: View {
    let title: String
    let isSelected: Bool
    let action: () -> Void
    
    var body: some View {
        Button(action: action) {
            Text(title)
                .font(DesignSystem.microFont)
                .foregroundColor(isSelected ? DesignSystem.white : DesignSystem.gray600)
                .padding(.horizontal, 12)
                .padding(.vertical, 6)
                .background(isSelected ? DesignSystem.gray900 : Color.clear)
                .cornerRadius(4)
        }
    }
}

struct ModelRow: View {
    let model: MentalModelItem
    
    var body: some View {
        HStack(spacing: 0) {
            VStack(alignment: .leading, spacing: 2) {
                Text(model.name).font(DesignSystem.bodyFont).foregroundColor(DesignSystem.gray900)
                Text(model.thinker).font(DesignSystem.captionFont).foregroundColor(DesignSystem.gray500)
            }
            .frame(maxWidth: .infinity, alignment: .leading)
            
            Text(model.category).font(DesignSystem.captionFont).foregroundColor(DesignSystem.gray600).frame(width: 80)
            Text("\(model.failures)").font(DesignSystem.captionFont).foregroundColor(DesignSystem.accent).frame(width: 40, alignment: .trailing)
        }
        .padding(.horizontal, 16)
        .padding(.vertical, 10)
        .contentShape(Rectangle())
        .onTapGesture { HapticManager.shared.impact(.light) }
    }
}

// MARK: - Analyze View
struct AnalyzeView: View {
    @State private var inputText = ""
    @State private var isAnalyzing = false
    @State private var analysisResult: AnalysisResult?
    
    var body: some View {
        NavigationStack {
            VStack(spacing: 0) {
                inputSection
                if let result = analysisResult {
                    resultsSection(result)
                } else {
                    emptyState
                }
            }
            .background(DesignSystem.gray100)
            .navigationTitle("Analyze")
            .navigationBarTitleDisplayMode(.inline)
        }
    }
    
    private var inputSection: some View {
        VStack(spacing: 0) {
            TextEditor(text: $inputText)
                .font(DesignSystem.bodyFont)
                .foregroundColor(DesignSystem.gray900)
                .frame(height: 120)
                .padding(12)
                .background(DesignSystem.white)
                .overlay(
                    Group {
                        if inputText.isEmpty {
                            Text("Enter text to analyze with 129 mental models...")
                                .font(DesignSystem.bodyFont)
                                .foregroundColor(DesignSystem.gray400)
                                .padding(16)
                        }
                    },
                    alignment: .topLeading
                )
            
            HStack {
                Text("\(inputText.count) characters").font(DesignSystem.microFont).foregroundColor(DesignSystem.gray500)
                Spacer()
                Button(action: { HapticManager.shared.impact(.medium); analyze() }) {
                    Text("ANALYZE")
                        .font(DesignSystem.microFont)
                        .fontWeight(.semibold)
                        .foregroundColor(DesignSystem.white)
                        .padding(.horizontal, 20)
                        .padding(.vertical, 8)
                        .background(inputText.isEmpty ? DesignSystem.gray400 : DesignSystem.accent)
                        .cornerRadius(4)
                }
                .disabled(inputText.isEmpty)
            }
            .padding(.horizontal, 12)
            .padding(.vertical, 8)
            .background(DesignSystem.gray100)
        }
    }
    
    private var emptyState: some View {
        VStack(spacing: 16) {
            Spacer()
            Image(systemName: "magnifyingglass").font(.system(size: 40, weight: .thin)).foregroundColor(DesignSystem.gray400)
            Text("Enter text above to analyze").font(DesignSystem.bodyFont).foregroundColor(DesignSystem.gray500)
            Text("All 129 mental models will be applied").font(DesignSystem.captionFont).foregroundColor(DesignSystem.gray400)
            Spacer()
        }
    }
    
    private func resultsSection(_ result: AnalysisResult) -> some View {
        ScrollView {
            VStack(spacing: 1) {
                HStack {
                    Text("ANALYSIS RESULTS").font(DesignSystem.microFont).foregroundColor(DesignSystem.gray500)
                    Spacer()
                    Text("\(result.modelsDetected.count) models detected").font(DesignSystem.microFont).foregroundColor(DesignSystem.accent)
                }
                .padding(.horizontal, 16).padding(.vertical, 10).background(DesignSystem.gray100)
                
                ForEach(result.modelsDetected, id: \.name) { model in
                    ResultRow(model: model)
                    Divider().background(DesignSystem.gray200)
                }
                
                if !result.biasesDetected.isEmpty {
                    HStack {
                        Text("BIASES DETECTED").font(DesignSystem.microFont).foregroundColor(DesignSystem.gray500)
                        Spacer()
                        Text("\(result.biasesDetected.count)").font(DesignSystem.microFont).foregroundColor(DesignSystem.accent)
                    }
                    .padding(.horizontal, 16).padding(.vertical, 10).background(DesignSystem.gray100)
                    
                    ForEach(result.biasesDetected, id: \.self) { bias in
                        BiasRow(bias: bias)
                        Divider().background(DesignSystem.gray200)
                    }
                }
            }
            .background(DesignSystem.white)
        }
    }
    
    private func analyze() {
        isAnalyzing = true
        HapticManager.shared.notification(.success)
        DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
            analysisResult = AnalysisResult(
                modelsDetected: [
                    ModelResult(name: "Confirmation Bias", confidence: 0.92, category: "Psychology"),
                    ModelResult(name: "Incentives", confidence: 0.87, category: "Psychology"),
                    ModelResult(name: "Second-Order Thinking", confidence: 0.78, category: "Thinking")
                ],
                biasesDetected: ["Recency Bias", "Availability Heuristic"],
                lollapaloozaScore: 0.65
            )
            isAnalyzing = false
        }
    }
}

struct AnalysisResult {
    let modelsDetected: [ModelResult]
    let biasesDetected: [String]
    let lollapaloozaScore: Double
}

struct ModelResult {
    let name: String
    let confidence: Double
    let category: String
}

struct ResultRow: View {
    let model: ModelResult
    
    var body: some View {
        HStack(spacing: 0) {
            VStack(alignment: .leading, spacing: 2) {
                Text(model.name).font(DesignSystem.bodyFont).foregroundColor(DesignSystem.gray900)
                Text(model.category).font(DesignSystem.captionFont).foregroundColor(DesignSystem.gray500)
            }
            .frame(maxWidth: .infinity, alignment: .leading)
            
            Text("\(Int(model.confidence * 100))%")
                .font(DesignSystem.smallMetricFont)
                .foregroundColor(model.confidence > 0.8 ? DesignSystem.accent : DesignSystem.gray600)
        }
        .padding(.horizontal, 16).padding(.vertical, 10)
    }
}

struct BiasRow: View {
    let bias: String
    
    var body: some View {
        HStack {
            Circle().fill(DesignSystem.accent).frame(width: 4, height: 4)
            Text(bias).font(DesignSystem.bodyFont).foregroundColor(DesignSystem.gray900)
            Spacer()
        }
        .padding(.horizontal, 16).padding(.vertical, 10).background(DesignSystem.white)
    }
}

// MARK: - Insights View
struct InsightsView: View {
    @EnvironmentObject var learningEngine: ContinuousLearningEngine
    
    var body: some View {
        NavigationStack {
            VStack(spacing: 0) {
                HStack {
                    Text("TOTAL INSIGHTS").font(DesignSystem.microFont).foregroundColor(DesignSystem.gray500)
                    Spacer()
                    Text("\(learningEngine.insightsGenerated)").font(DesignSystem.smallMetricFont).foregroundColor(DesignSystem.accent)
                }
                .padding(.horizontal, 16).padding(.vertical, 12).background(DesignSystem.white)
                
                ScrollView {
                    LazyVStack(spacing: 0) {
                        ForEach(sampleInsights, id: \.title) { insight in
                            InsightRow(insight: insight)
                            Divider().background(DesignSystem.gray200)
                        }
                    }
                    .background(DesignSystem.white)
                }
            }
            .background(DesignSystem.gray100)
            .navigationTitle("Insights")
            .navigationBarTitleDisplayMode(.inline)
        }
    }
    
    private var sampleInsights: [Insight] {
        [
            Insight(title: "Lollapalooza Effect Detected", detail: "Multiple biases reinforcing each other", time: "2m ago", isImportant: true),
            Insight(title: "Pattern: Confirmation Bias", detail: "Detected in 73% of documents this week", time: "1h ago", isImportant: false),
            Insight(title: "Model Correlation Found", detail: "Incentives and Social Proof often appear together", time: "3h ago", isImportant: false),
            Insight(title: "Learning Milestone", detail: "10,000 data points processed", time: "1d ago", isImportant: true)
        ]
    }
}

struct Insight {
    let title: String
    let detail: String
    let time: String
    let isImportant: Bool
}

struct InsightRow: View {
    let insight: Insight
    
    var body: some View {
        HStack(spacing: 12) {
            if insight.isImportant {
                Rectangle().fill(DesignSystem.accent).frame(width: 3)
            }
            
            VStack(alignment: .leading, spacing: 4) {
                HStack {
                    Text(insight.title)
                        .font(DesignSystem.bodyFont)
                        .fontWeight(insight.isImportant ? .semibold : .regular)
                        .foregroundColor(DesignSystem.gray900)
                    Spacer()
                    Text(insight.time).font(DesignSystem.captionFont).foregroundColor(DesignSystem.gray500)
                }
                Text(insight.detail).font(DesignSystem.captionFont).foregroundColor(DesignSystem.gray600)
            }
            .padding(.vertical, 12)
            .padding(.trailing, 16)
            .padding(.leading, insight.isImportant ? 8 : 16)
        }
        .contentShape(Rectangle())
        .onTapGesture { HapticManager.shared.impact(.light) }
    }
}

// MARK: - Learning View
struct LearningView: View {
    @EnvironmentObject var learningEngine: ContinuousLearningEngine
    @EnvironmentObject var sensorManager: SensorDataManager
    
    var body: some View {
        NavigationStack {
            ScrollView {
                VStack(spacing: 1) {
                    learningStatusSection
                    learningMetricsSection
                    sensorsSection
                }
            }
            .background(DesignSystem.gray100)
            .navigationTitle("Learning")
            .navigationBarTitleDisplayMode(.inline)
        }
    }
    
    private var learningStatusSection: some View {
        VStack(spacing: 0) {
            HStack {
                Text("STATUS").font(DesignSystem.microFont).foregroundColor(DesignSystem.gray500)
                Spacer()
            }
            .padding(.horizontal, 16).padding(.vertical, 10).background(DesignSystem.gray100)
            
            HStack {
                Circle().fill(learningEngine.isLearning ? DesignSystem.accent : DesignSystem.gray400).frame(width: 8, height: 8)
                Text(learningEngine.isLearning ? "CONTINUOUS LEARNING ACTIVE" : "LEARNING PAUSED")
                    .font(DesignSystem.bodyFont)
                    .foregroundColor(learningEngine.isLearning ? DesignSystem.accent : DesignSystem.gray600)
                Spacer()
                Button(action: { HapticManager.shared.impact(.medium) }) {
                    Text(learningEngine.isLearning ? "PAUSE" : "START")
                        .font(DesignSystem.microFont)
                        .fontWeight(.semibold)
                        .foregroundColor(DesignSystem.white)
                        .padding(.horizontal, 16).padding(.vertical, 6)
                        .background(learningEngine.isLearning ? DesignSystem.gray600 : DesignSystem.accent)
                        .cornerRadius(4)
                }
            }
            .padding(.horizontal, 16).padding(.vertical, 12).background(DesignSystem.white)
        }
    }
    
    private var learningMetricsSection: some View {
        VStack(spacing: 0) {
            HStack {
                Text("METRICS").font(DesignSystem.microFont).foregroundColor(DesignSystem.gray500)
                Spacer()
            }
            .padding(.horizontal, 16).padding(.vertical, 10).background(DesignSystem.gray100)
            
            VStack(spacing: 1) {
                HStack(spacing: 1) {
                    MetricCell(label: "DATA POINTS", value: formatCompact(learningEngine.dataPointsProcessed), change: nil, isHighlighted: false)
                    MetricCell(label: "INSIGHTS", value: "\(learningEngine.insightsGenerated)", change: nil, isHighlighted: true)
                    MetricCell(label: "UPDATES", value: "\(learningEngine.modelsUpdated)", change: nil, isHighlighted: false)
                }
                HStack(spacing: 1) {
                    MetricCell(label: "RATE/SEC", value: String(format: "%.1f", learningEngine.learningRatePerSecond), change: nil, isHighlighted: false)
                    MetricCell(label: "ACCURACY", value: "94.2%", change: nil, isHighlighted: false)
                    MetricCell(label: "UPTIME", value: "24h", change: nil, isHighlighted: false)
                }
            }
            .background(DesignSystem.gray200)
        }
    }
    
    private var sensorsSection: some View {
        VStack(spacing: 0) {
            HStack {
                Text("SENSORS").font(DesignSystem.microFont).foregroundColor(DesignSystem.gray500)
                Spacer()
            }
            .padding(.horizontal, 16).padding(.vertical, 10).background(DesignSystem.gray100)
            
            VStack(spacing: 0) {
                SensorRow(name: "Motion", status: sensorManager.motionEnabled ? "Active" : "Inactive", isActive: sensorManager.motionEnabled)
                Divider().background(DesignSystem.gray200)
                SensorRow(name: "Location", status: sensorManager.locationEnabled ? "Active" : "Inactive", isActive: sensorManager.locationEnabled)
                Divider().background(DesignSystem.gray200)
                SensorRow(name: "Health", status: sensorManager.healthEnabled ? "Active" : "Inactive", isActive: sensorManager.healthEnabled)
            }
            .background(DesignSystem.white)
        }
    }
    
    private func formatCompact(_ num: Int) -> String {
        if num >= 1000000 { return String(format: "%.1fM", Double(num) / 1000000) }
        else if num >= 1000 { return String(format: "%.1fK", Double(num) / 1000) }
        return "\(num)"
    }
}

struct SensorRow: View {
    let name: String
    let status: String
    let isActive: Bool
    
    var body: some View {
        HStack {
            Text(name).font(DesignSystem.bodyFont).foregroundColor(DesignSystem.gray900)
            Spacer()
            HStack(spacing: 6) {
                Circle().fill(isActive ? DesignSystem.accent : DesignSystem.gray400).frame(width: 6, height: 6)
                Text(status).font(DesignSystem.captionFont).foregroundColor(isActive ? DesignSystem.accent : DesignSystem.gray500)
            }
        }
        .padding(.horizontal, 16).padding(.vertical, 12)
    }
}

// MARK: - Settings View
struct SettingsView: View {
    @Environment(\.dismiss) var dismiss
    @EnvironmentObject var appState: AppState
    
    var body: some View {
        NavigationStack {
            List {
                Section {
                    SettingRow(title: "Server URL", value: appState.serverURL)
                    SettingRow(title: "Sync Interval", value: "5 min")
                } header: { Text("CONNECTION").font(DesignSystem.microFont) }
                
                Section {
                    SettingRow(title: "Background Learning", value: "On")
                    SettingRow(title: "Notifications", value: "On")
                    SettingRow(title: "Haptic Feedback", value: "On")
                } header: { Text("PREFERENCES").font(DesignSystem.microFont) }
                
                Section {
                    SettingRow(title: "Version", value: "1.0.0")
                    SettingRow(title: "Build", value: "2026.01.18")
                } header: { Text("ABOUT").font(DesignSystem.microFont) }
            }
            .listStyle(.insetGrouped)
            .navigationTitle("Settings")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .navigationBarTrailing) {
                    Button("Done") { HapticManager.shared.impact(.light); dismiss() }
                        .foregroundColor(DesignSystem.accent)
                }
            }
        }
    }
}

struct SettingRow: View {
    let title: String
    let value: String
    
    var body: some View {
        HStack {
            Text(title).font(DesignSystem.bodyFont).foregroundColor(DesignSystem.gray900)
            Spacer()
            Text(value).font(DesignSystem.bodyFont).foregroundColor(DesignSystem.gray500)
        }
    }
}

#Preview {
    ContentView()
        .environmentObject(AppState())
        .environmentObject(ContinuousLearningEngine())
        .environmentObject(SensorDataManager())
}
