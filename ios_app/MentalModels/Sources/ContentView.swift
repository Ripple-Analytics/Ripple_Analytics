import SwiftUI

struct ContentView: View {
    @EnvironmentObject var appState: AppState
    @EnvironmentObject var learningEngine: ContinuousLearningEngine
    @EnvironmentObject var sensorManager: SensorDataManager
    
    @State private var selectedTab = 0
    @State private var showingSettings = false
    
    var body: some View {
        ZStack {
            LinearGradient(
                colors: [Color(.systemBackground), Color(.systemGray6)],
                startPoint: .top,
                endPoint: .bottom
            )
            .ignoresSafeArea()
            
            TabView(selection: $selectedTab) {
                DashboardView()
                    .tabItem {
                        Label("Dashboard", systemImage: "square.grid.2x2.fill")
                    }
                    .tag(0)
                
                ModelsView()
                    .tabItem {
                        Label("Models", systemImage: "brain.head.profile")
                    }
                    .tag(1)
                
                AnalyzeView()
                    .tabItem {
                        Label("Analyze", systemImage: "waveform.circle.fill")
                    }
                    .tag(2)
                
                InsightsView()
                    .tabItem {
                        Label("Insights", systemImage: "lightbulb.fill")
                    }
                    .tag(3)
                
                LearningView()
                    .tabItem {
                        Label("Learning", systemImage: "arrow.triangle.2.circlepath")
                    }
                    .tag(4)
            }
            .tint(.blue)
        }
        .sheet(isPresented: $showingSettings) {
            SettingsView()
        }
    }
}

struct DashboardView: View {
    @EnvironmentObject var appState: AppState
    @EnvironmentObject var learningEngine: ContinuousLearningEngine
    
    @State private var animateCards = false
    
    var body: some View {
        NavigationStack {
            ScrollView {
                VStack(spacing: 24) {
                    heroCard
                    
                    LazyVGrid(columns: [
                        GridItem(.flexible()),
                        GridItem(.flexible())
                    ], spacing: 16) {
                        StatCard(
                            title: "Models",
                            value: "129",
                            icon: "brain.head.profile",
                            color: .blue
                        )
                        
                        StatCard(
                            title: "Insights",
                            value: "\(learningEngine.insightsGenerated)",
                            icon: "lightbulb.fill",
                            color: .yellow
                        )
                        
                        StatCard(
                            title: "Data Points",
                            value: formatNumber(learningEngine.dataPointsProcessed),
                            icon: "chart.dots.scatter",
                            color: .green
                        )
                        
                        StatCard(
                            title: "Learning Rate",
                            value: String(format: "%.1f/s", learningEngine.learningRatePerSecond),
                            icon: "speedometer",
                            color: .orange
                        )
                    }
                    .padding(.horizontal)
                    
                    recentActivitySection
                    
                    quickActionsSection
                }
                .padding(.vertical)
            }
            .navigationTitle("Mental Models")
            .toolbar {
                ToolbarItem(placement: .navigationBarTrailing) {
                    Button(action: {}) {
                        Image(systemName: "gearshape.fill")
                            .font(.system(size: 18, weight: .medium))
                    }
                }
            }
        }
        .onAppear {
            withAnimation(.spring(response: 0.6, dampingFraction: 0.8)) {
                animateCards = true
            }
        }
    }
    
    private var heroCard: some View {
        VStack(alignment: .leading, spacing: 16) {
            HStack {
                VStack(alignment: .leading, spacing: 4) {
                    Text("Learning Active")
                        .font(.subheadline)
                        .foregroundColor(.white.opacity(0.8))
                    
                    Text("Your AI is improving")
                        .font(.title2)
                        .fontWeight(.bold)
                        .foregroundColor(.white)
                }
                
                Spacer()
                
                ZStack {
                    Circle()
                        .fill(.white.opacity(0.2))
                        .frame(width: 60, height: 60)
                    
                    Image(systemName: "brain")
                        .font(.system(size: 28))
                        .foregroundColor(.white)
                        .symbolEffect(.pulse, options: .repeating)
                }
            }
            
            HStack(spacing: 24) {
                VStack(alignment: .leading) {
                    Text("\(learningEngine.modelsUpdated)")
                        .font(.title)
                        .fontWeight(.bold)
                        .foregroundColor(.white)
                    Text("Updates")
                        .font(.caption)
                        .foregroundColor(.white.opacity(0.7))
                }
                
                VStack(alignment: .leading) {
                    Text("\(learningEngine.insightsGenerated)")
                        .font(.title)
                        .fontWeight(.bold)
                        .foregroundColor(.white)
                    Text("Insights")
                        .font(.caption)
                        .foregroundColor(.white.opacity(0.7))
                }
                
                Spacer()
                
                LearningIndicator(isActive: learningEngine.isLearning)
            }
        }
        .padding(24)
        .background(
            LinearGradient(
                colors: [Color.blue, Color.purple],
                startPoint: .topLeading,
                endPoint: .bottomTrailing
            )
        )
        .clipShape(RoundedRectangle(cornerRadius: 24, style: .continuous))
        .shadow(color: .blue.opacity(0.3), radius: 20, x: 0, y: 10)
        .padding(.horizontal)
        .offset(y: animateCards ? 0 : 50)
        .opacity(animateCards ? 1 : 0)
    }
    
    private var recentActivitySection: some View {
        VStack(alignment: .leading, spacing: 16) {
            Text("Recent Activity")
                .font(.headline)
                .padding(.horizontal)
            
            VStack(spacing: 12) {
                ActivityRow(
                    icon: "doc.text.fill",
                    title: "Document analyzed",
                    subtitle: "3 mental models detected",
                    time: "2m ago",
                    color: .blue
                )
                
                ActivityRow(
                    icon: "brain.head.profile",
                    title: "Pattern discovered",
                    subtitle: "Confirmation bias correlation",
                    time: "5m ago",
                    color: .purple
                )
                
                ActivityRow(
                    icon: "chart.line.uptrend.xyaxis",
                    title: "Insight generated",
                    subtitle: "Lollapalooza effect detected",
                    time: "12m ago",
                    color: .green
                )
            }
            .padding(.horizontal)
        }
    }
    
    private var quickActionsSection: some View {
        VStack(alignment: .leading, spacing: 16) {
            Text("Quick Actions")
                .font(.headline)
                .padding(.horizontal)
            
            ScrollView(.horizontal, showsIndicators: false) {
                HStack(spacing: 12) {
                    QuickActionButton(
                        title: "Analyze Text",
                        icon: "doc.text.magnifyingglass",
                        color: .blue
                    )
                    
                    QuickActionButton(
                        title: "Scan Document",
                        icon: "doc.viewfinder",
                        color: .green
                    )
                    
                    QuickActionButton(
                        title: "Voice Input",
                        icon: "mic.fill",
                        color: .orange
                    )
                    
                    QuickActionButton(
                        title: "Share Insight",
                        icon: "square.and.arrow.up",
                        color: .purple
                    )
                }
                .padding(.horizontal)
            }
        }
    }
    
    private func formatNumber(_ num: Int) -> String {
        if num >= 1000000 {
            return String(format: "%.1fM", Double(num) / 1000000)
        } else if num >= 1000 {
            return String(format: "%.1fK", Double(num) / 1000)
        }
        return "\(num)"
    }
}

struct StatCard: View {
    let title: String
    let value: String
    let icon: String
    let color: Color
    
    @State private var isPressed = false
    
    var body: some View {
        VStack(alignment: .leading, spacing: 12) {
            HStack {
                Image(systemName: icon)
                    .font(.system(size: 20, weight: .semibold))
                    .foregroundColor(color)
                
                Spacer()
            }
            
            Text(value)
                .font(.system(size: 28, weight: .bold, design: .rounded))
                .foregroundColor(.primary)
            
            Text(title)
                .font(.subheadline)
                .foregroundColor(.secondary)
        }
        .padding(20)
        .background(Color(.systemBackground))
        .clipShape(RoundedRectangle(cornerRadius: 20, style: .continuous))
        .shadow(color: .black.opacity(0.05), radius: 10, x: 0, y: 5)
        .scaleEffect(isPressed ? 0.95 : 1)
        .animation(.spring(response: 0.3, dampingFraction: 0.6), value: isPressed)
        .onTapGesture {
            isPressed = true
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.1) {
                isPressed = false
            }
        }
    }
}

struct LearningIndicator: View {
    let isActive: Bool
    
    @State private var isAnimating = false
    
    var body: some View {
        HStack(spacing: 4) {
            ForEach(0..<3) { index in
                Circle()
                    .fill(.white)
                    .frame(width: 8, height: 8)
                    .scaleEffect(isAnimating ? 1 : 0.5)
                    .opacity(isAnimating ? 1 : 0.3)
                    .animation(
                        .easeInOut(duration: 0.6)
                        .repeatForever()
                        .delay(Double(index) * 0.2),
                        value: isAnimating
                    )
            }
        }
        .onAppear {
            if isActive {
                isAnimating = true
            }
        }
        .onChange(of: isActive) { newValue in
            isAnimating = newValue
        }
    }
}

struct ActivityRow: View {
    let icon: String
    let title: String
    let subtitle: String
    let time: String
    let color: Color
    
    var body: some View {
        HStack(spacing: 16) {
            ZStack {
                Circle()
                    .fill(color.opacity(0.1))
                    .frame(width: 44, height: 44)
                
                Image(systemName: icon)
                    .font(.system(size: 18, weight: .medium))
                    .foregroundColor(color)
            }
            
            VStack(alignment: .leading, spacing: 2) {
                Text(title)
                    .font(.subheadline)
                    .fontWeight(.medium)
                
                Text(subtitle)
                    .font(.caption)
                    .foregroundColor(.secondary)
            }
            
            Spacer()
            
            Text(time)
                .font(.caption2)
                .foregroundColor(.secondary)
        }
        .padding(16)
        .background(Color(.systemBackground))
        .clipShape(RoundedRectangle(cornerRadius: 16, style: .continuous))
        .shadow(color: .black.opacity(0.03), radius: 8, x: 0, y: 4)
    }
}

struct QuickActionButton: View {
    let title: String
    let icon: String
    let color: Color
    
    @State private var isPressed = false
    
    var body: some View {
        VStack(spacing: 12) {
            ZStack {
                Circle()
                    .fill(color.gradient)
                    .frame(width: 56, height: 56)
                
                Image(systemName: icon)
                    .font(.system(size: 24, weight: .medium))
                    .foregroundColor(.white)
            }
            
            Text(title)
                .font(.caption)
                .fontWeight(.medium)
                .foregroundColor(.primary)
                .multilineTextAlignment(.center)
        }
        .frame(width: 80)
        .scaleEffect(isPressed ? 0.9 : 1)
        .animation(.spring(response: 0.3, dampingFraction: 0.6), value: isPressed)
        .onTapGesture {
            let impactFeedback = UIImpactFeedbackGenerator(style: .medium)
            impactFeedback.impactOccurred()
            
            isPressed = true
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.1) {
                isPressed = false
            }
        }
    }
}

struct ModelsView: View {
    @State private var searchText = ""
    @State private var selectedCategory: String?
    
    let categories = [
        "Psychology", "Thinking Tools", "Economics", 
        "Competitive Advantage", "Mathematics", 
        "Physics/Systems", "Biology", "Organizational"
    ]
    
    var body: some View {
        NavigationStack {
            ScrollView {
                VStack(spacing: 20) {
                    categoryPicker
                    
                    modelsList
                }
                .padding(.vertical)
            }
            .navigationTitle("Mental Models")
            .searchable(text: $searchText, prompt: "Search 129 models...")
        }
    }
    
    private var categoryPicker: some View {
        ScrollView(.horizontal, showsIndicators: false) {
            HStack(spacing: 12) {
                CategoryChip(
                    title: "All",
                    isSelected: selectedCategory == nil,
                    color: .blue
                ) {
                    withAnimation(.spring(response: 0.3)) {
                        selectedCategory = nil
                    }
                }
                
                ForEach(categories, id: \.self) { category in
                    CategoryChip(
                        title: category,
                        isSelected: selectedCategory == category,
                        color: colorForCategory(category)
                    ) {
                        withAnimation(.spring(response: 0.3)) {
                            selectedCategory = category
                        }
                    }
                }
            }
            .padding(.horizontal)
        }
    }
    
    private var modelsList: some View {
        LazyVStack(spacing: 12) {
            ForEach(sampleModels, id: \.name) { model in
                ModelCard(model: model)
            }
        }
        .padding(.horizontal)
    }
    
    private func colorForCategory(_ category: String) -> Color {
        switch category {
        case "Psychology": return .purple
        case "Thinking Tools": return .blue
        case "Economics": return .green
        case "Competitive Advantage": return .orange
        case "Mathematics": return .red
        case "Physics/Systems": return .cyan
        case "Biology": return .mint
        case "Organizational": return .indigo
        default: return .gray
        }
    }
    
    private var sampleModels: [MentalModelItem] {
        [
            MentalModelItem(name: "Circle of Competence", category: "Thinking Tools", thinker: "Warren Buffett"),
            MentalModelItem(name: "Inversion", category: "Thinking Tools", thinker: "Charlie Munger"),
            MentalModelItem(name: "First Principles", category: "Thinking Tools", thinker: "Aristotle"),
            MentalModelItem(name: "Confirmation Bias", category: "Psychology", thinker: "Peter Wason"),
            MentalModelItem(name: "Compound Interest", category: "Mathematics", thinker: "Albert Einstein"),
            MentalModelItem(name: "Network Effects", category: "Economics", thinker: "Robert Metcalfe"),
            MentalModelItem(name: "Margin of Safety", category: "Competitive Advantage", thinker: "Benjamin Graham"),
            MentalModelItem(name: "Second-Order Thinking", category: "Thinking Tools", thinker: "Howard Marks")
        ]
    }
}

struct MentalModelItem {
    let name: String
    let category: String
    let thinker: String
}

struct CategoryChip: View {
    let title: String
    let isSelected: Bool
    let color: Color
    let action: () -> Void
    
    var body: some View {
        Button(action: action) {
            Text(title)
                .font(.subheadline)
                .fontWeight(.medium)
                .foregroundColor(isSelected ? .white : .primary)
                .padding(.horizontal, 16)
                .padding(.vertical, 10)
                .background(isSelected ? color : Color(.systemGray6))
                .clipShape(Capsule())
        }
    }
}

struct ModelCard: View {
    let model: MentalModelItem
    
    var body: some View {
        HStack(spacing: 16) {
            ZStack {
                RoundedRectangle(cornerRadius: 12, style: .continuous)
                    .fill(colorForCategory(model.category).opacity(0.1))
                    .frame(width: 48, height: 48)
                
                Image(systemName: iconForCategory(model.category))
                    .font(.system(size: 20, weight: .medium))
                    .foregroundColor(colorForCategory(model.category))
            }
            
            VStack(alignment: .leading, spacing: 4) {
                Text(model.name)
                    .font(.subheadline)
                    .fontWeight(.semibold)
                
                HStack(spacing: 8) {
                    Text(model.category)
                        .font(.caption)
                        .foregroundColor(colorForCategory(model.category))
                    
                    Text("•")
                        .foregroundColor(.secondary)
                    
                    Text(model.thinker)
                        .font(.caption)
                        .foregroundColor(.secondary)
                }
            }
            
            Spacer()
            
            Image(systemName: "chevron.right")
                .font(.system(size: 14, weight: .medium))
                .foregroundColor(.secondary)
        }
        .padding(16)
        .background(Color(.systemBackground))
        .clipShape(RoundedRectangle(cornerRadius: 16, style: .continuous))
        .shadow(color: .black.opacity(0.03), radius: 8, x: 0, y: 4)
    }
    
    private func colorForCategory(_ category: String) -> Color {
        switch category {
        case "Psychology": return .purple
        case "Thinking Tools": return .blue
        case "Economics": return .green
        case "Competitive Advantage": return .orange
        case "Mathematics": return .red
        case "Physics/Systems": return .cyan
        case "Biology": return .mint
        case "Organizational": return .indigo
        default: return .gray
        }
    }
    
    private func iconForCategory(_ category: String) -> String {
        switch category {
        case "Psychology": return "brain"
        case "Thinking Tools": return "lightbulb"
        case "Economics": return "chart.line.uptrend.xyaxis"
        case "Competitive Advantage": return "trophy"
        case "Mathematics": return "function"
        case "Physics/Systems": return "atom"
        case "Biology": return "leaf"
        case "Organizational": return "person.3"
        default: return "questionmark"
        }
    }
}

struct AnalyzeView: View {
    @State private var inputText = ""
    @State private var isAnalyzing = false
    @State private var analysisResult: AnalysisResult?
    
    var body: some View {
        NavigationStack {
            ScrollView {
                VStack(spacing: 24) {
                    inputSection
                    
                    if let result = analysisResult {
                        resultsSection(result)
                    }
                }
                .padding()
            }
            .navigationTitle("Analyze")
        }
    }
    
    private var inputSection: some View {
        VStack(alignment: .leading, spacing: 16) {
            Text("Enter text to analyze")
                .font(.headline)
            
            TextEditor(text: $inputText)
                .frame(minHeight: 150)
                .padding(12)
                .background(Color(.systemGray6))
                .clipShape(RoundedRectangle(cornerRadius: 16, style: .continuous))
            
            Button(action: analyze) {
                HStack {
                    if isAnalyzing {
                        ProgressView()
                            .tint(.white)
                    } else {
                        Image(systemName: "waveform.circle.fill")
                    }
                    Text(isAnalyzing ? "Analyzing..." : "Analyze with Mental Models")
                        .fontWeight(.semibold)
                }
                .frame(maxWidth: .infinity)
                .padding(.vertical, 16)
                .background(inputText.isEmpty ? Color.gray : Color.blue)
                .foregroundColor(.white)
                .clipShape(RoundedRectangle(cornerRadius: 14, style: .continuous))
            }
            .disabled(inputText.isEmpty || isAnalyzing)
        }
    }
    
    private func resultsSection(_ result: AnalysisResult) -> some View {
        VStack(alignment: .leading, spacing: 20) {
            Text("Analysis Results")
                .font(.headline)
            
            ForEach(result.models, id: \.name) { model in
                ResultCard(model: model)
            }
            
            if !result.biases.isEmpty {
                BiasesCard(biases: result.biases)
            }
            
            if result.lollapalooza {
                LollapaloozaCard()
            }
        }
    }
    
    private func analyze() {
        isAnalyzing = true
        
        DispatchQueue.main.asyncAfter(deadline: .now() + 2) {
            analysisResult = AnalysisResult(
                models: [
                    ModelResult(name: "Confirmation Bias", relevance: 0.85, category: "Psychology"),
                    ModelResult(name: "Incentive Effects", relevance: 0.72, category: "Psychology"),
                    ModelResult(name: "Second-Order Thinking", relevance: 0.68, category: "Thinking Tools")
                ],
                biases: ["Confirmation Bias", "Availability Heuristic"],
                lollapalooza: true
            )
            isAnalyzing = false
        }
    }
}

struct AnalysisResult {
    let models: [ModelResult]
    let biases: [String]
    let lollapalooza: Bool
}

struct ModelResult {
    let name: String
    let relevance: Double
    let category: String
}

struct ResultCard: View {
    let model: ModelResult
    
    var body: some View {
        VStack(alignment: .leading, spacing: 12) {
            HStack {
                Text(model.name)
                    .font(.subheadline)
                    .fontWeight(.semibold)
                
                Spacer()
                
                Text("\(Int(model.relevance * 100))%")
                    .font(.subheadline)
                    .fontWeight(.bold)
                    .foregroundColor(.blue)
            }
            
            GeometryReader { geometry in
                ZStack(alignment: .leading) {
                    RoundedRectangle(cornerRadius: 4)
                        .fill(Color(.systemGray5))
                        .frame(height: 8)
                    
                    RoundedRectangle(cornerRadius: 4)
                        .fill(Color.blue.gradient)
                        .frame(width: geometry.size.width * model.relevance, height: 8)
                }
            }
            .frame(height: 8)
            
            Text(model.category)
                .font(.caption)
                .foregroundColor(.secondary)
        }
        .padding(16)
        .background(Color(.systemBackground))
        .clipShape(RoundedRectangle(cornerRadius: 16, style: .continuous))
        .shadow(color: .black.opacity(0.03), radius: 8, x: 0, y: 4)
    }
}

struct BiasesCard: View {
    let biases: [String]
    
    var body: some View {
        VStack(alignment: .leading, spacing: 12) {
            HStack {
                Image(systemName: "exclamationmark.triangle.fill")
                    .foregroundColor(.orange)
                Text("Biases Detected")
                    .font(.subheadline)
                    .fontWeight(.semibold)
            }
            
            ForEach(biases, id: \.self) { bias in
                HStack {
                    Circle()
                        .fill(Color.orange)
                        .frame(width: 6, height: 6)
                    Text(bias)
                        .font(.subheadline)
                }
            }
        }
        .padding(16)
        .background(Color.orange.opacity(0.1))
        .clipShape(RoundedRectangle(cornerRadius: 16, style: .continuous))
    }
}

struct LollapaloozaCard: View {
    var body: some View {
        HStack(spacing: 16) {
            Image(systemName: "sparkles")
                .font(.system(size: 32))
                .foregroundColor(.purple)
            
            VStack(alignment: .leading, spacing: 4) {
                Text("Lollapalooza Effect!")
                    .font(.headline)
                    .foregroundColor(.purple)
                
                Text("Multiple mental models are interacting to create a powerful combined effect.")
                    .font(.caption)
                    .foregroundColor(.secondary)
            }
        }
        .padding(16)
        .background(Color.purple.opacity(0.1))
        .clipShape(RoundedRectangle(cornerRadius: 16, style: .continuous))
    }
}

struct InsightsView: View {
    var body: some View {
        NavigationStack {
            ScrollView {
                VStack(spacing: 20) {
                    InsightCard(
                        title: "Pattern Discovered",
                        description: "Your decision-making shows strong correlation with confirmation bias when under time pressure.",
                        icon: "brain.head.profile",
                        color: .purple,
                        time: "2 hours ago"
                    )
                    
                    InsightCard(
                        title: "Learning Milestone",
                        description: "You've analyzed 100 documents this week, improving pattern recognition by 15%.",
                        icon: "chart.line.uptrend.xyaxis",
                        color: .green,
                        time: "Yesterday"
                    )
                    
                    InsightCard(
                        title: "Model Recommendation",
                        description: "Based on your recent analyses, consider applying 'Inversion' more frequently.",
                        icon: "lightbulb.fill",
                        color: .yellow,
                        time: "2 days ago"
                    )
                }
                .padding()
            }
            .navigationTitle("Insights")
        }
    }
}

struct InsightCard: View {
    let title: String
    let description: String
    let icon: String
    let color: Color
    let time: String
    
    var body: some View {
        VStack(alignment: .leading, spacing: 16) {
            HStack {
                ZStack {
                    Circle()
                        .fill(color.opacity(0.1))
                        .frame(width: 44, height: 44)
                    
                    Image(systemName: icon)
                        .font(.system(size: 20, weight: .medium))
                        .foregroundColor(color)
                }
                
                VStack(alignment: .leading, spacing: 2) {
                    Text(title)
                        .font(.headline)
                    
                    Text(time)
                        .font(.caption)
                        .foregroundColor(.secondary)
                }
                
                Spacer()
            }
            
            Text(description)
                .font(.subheadline)
                .foregroundColor(.secondary)
                .lineSpacing(4)
        }
        .padding(20)
        .background(Color(.systemBackground))
        .clipShape(RoundedRectangle(cornerRadius: 20, style: .continuous))
        .shadow(color: .black.opacity(0.05), radius: 10, x: 0, y: 5)
    }
}

struct LearningView: View {
    @EnvironmentObject var learningEngine: ContinuousLearningEngine
    @EnvironmentObject var sensorManager: SensorDataManager
    
    var body: some View {
        NavigationStack {
            ScrollView {
                VStack(spacing: 24) {
                    learningStatusCard
                    
                    sensorDataCard
                    
                    learningStatsGrid
                }
                .padding()
            }
            .navigationTitle("Learning")
        }
    }
    
    private var learningStatusCard: some View {
        VStack(spacing: 20) {
            HStack {
                VStack(alignment: .leading, spacing: 4) {
                    Text(learningEngine.isLearning ? "Learning Active" : "Learning Paused")
                        .font(.headline)
                    
                    Text("Continuously improving from your data")
                        .font(.caption)
                        .foregroundColor(.secondary)
                }
                
                Spacer()
                
                Toggle("", isOn: .constant(learningEngine.isLearning))
                    .labelsHidden()
            }
            
            HStack(spacing: 32) {
                VStack {
                    Text("\(learningEngine.dataPointsProcessed)")
                        .font(.title2)
                        .fontWeight(.bold)
                    Text("Data Points")
                        .font(.caption)
                        .foregroundColor(.secondary)
                }
                
                VStack {
                    Text(String(format: "%.1f/s", learningEngine.learningRatePerSecond))
                        .font(.title2)
                        .fontWeight(.bold)
                    Text("Learn Rate")
                        .font(.caption)
                        .foregroundColor(.secondary)
                }
                
                VStack {
                    Text("\(learningEngine.insightsGenerated)")
                        .font(.title2)
                        .fontWeight(.bold)
                    Text("Insights")
                        .font(.caption)
                        .foregroundColor(.secondary)
                }
            }
        }
        .padding(20)
        .background(Color(.systemBackground))
        .clipShape(RoundedRectangle(cornerRadius: 20, style: .continuous))
        .shadow(color: .black.opacity(0.05), radius: 10, x: 0, y: 5)
    }
    
    private var sensorDataCard: some View {
        VStack(alignment: .leading, spacing: 16) {
            Text("Active Sensors")
                .font(.headline)
            
            LazyVGrid(columns: [GridItem(.flexible()), GridItem(.flexible())], spacing: 12) {
                SensorIndicator(name: "Accelerometer", isActive: true, icon: "move.3d")
                SensorIndicator(name: "Gyroscope", isActive: true, icon: "gyroscope")
                SensorIndicator(name: "Location", isActive: true, icon: "location.fill")
                SensorIndicator(name: "Health", isActive: true, icon: "heart.fill")
                SensorIndicator(name: "Pedometer", isActive: true, icon: "figure.walk")
                SensorIndicator(name: "Altimeter", isActive: true, icon: "mountain.2.fill")
            }
        }
        .padding(20)
        .background(Color(.systemBackground))
        .clipShape(RoundedRectangle(cornerRadius: 20, style: .continuous))
        .shadow(color: .black.opacity(0.05), radius: 10, x: 0, y: 5)
    }
    
    private var learningStatsGrid: some View {
        VStack(alignment: .leading, spacing: 16) {
            Text("Learning Statistics")
                .font(.headline)
            
            VStack(spacing: 12) {
                StatRow(label: "Patterns Detected", value: "47")
                StatRow(label: "Correlations Found", value: "23")
                StatRow(label: "Anomalies Identified", value: "8")
                StatRow(label: "Models Updated", value: "\(learningEngine.modelsUpdated)")
            }
        }
        .padding(20)
        .background(Color(.systemBackground))
        .clipShape(RoundedRectangle(cornerRadius: 20, style: .continuous))
        .shadow(color: .black.opacity(0.05), radius: 10, x: 0, y: 5)
    }
}

struct SensorIndicator: View {
    let name: String
    let isActive: Bool
    let icon: String
    
    var body: some View {
        HStack(spacing: 12) {
            Image(systemName: icon)
                .font(.system(size: 16, weight: .medium))
                .foregroundColor(isActive ? .green : .gray)
            
            VStack(alignment: .leading, spacing: 2) {
                Text(name)
                    .font(.caption)
                    .fontWeight(.medium)
                
                Text(isActive ? "Active" : "Inactive")
                    .font(.caption2)
                    .foregroundColor(isActive ? .green : .secondary)
            }
            
            Spacer()
        }
        .padding(12)
        .background(Color(.systemGray6))
        .clipShape(RoundedRectangle(cornerRadius: 12, style: .continuous))
    }
}

struct StatRow: View {
    let label: String
    let value: String
    
    var body: some View {
        HStack {
            Text(label)
                .font(.subheadline)
                .foregroundColor(.secondary)
            
            Spacer()
            
            Text(value)
                .font(.subheadline)
                .fontWeight(.semibold)
        }
    }
}

struct SettingsView: View {
    @Environment(\.dismiss) var dismiss
    
    var body: some View {
        NavigationStack {
            List {
                Section("Server") {
                    HStack {
                        Text("URL")
                        Spacer()
                        Text("localhost:8001")
                            .foregroundColor(.secondary)
                    }
                    
                    HStack {
                        Text("Status")
                        Spacer()
                        HStack(spacing: 6) {
                            Circle()
                                .fill(Color.green)
                                .frame(width: 8, height: 8)
                            Text("Connected")
                                .foregroundColor(.secondary)
                        }
                    }
                }
                
                Section("Learning") {
                    Toggle("Background Learning", isOn: .constant(true))
                    Toggle("Sensor Data Collection", isOn: .constant(true))
                    Toggle("Health Data", isOn: .constant(true))
                }
                
                Section("About") {
                    HStack {
                        Text("Version")
                        Spacer()
                        Text("1.0.0")
                            .foregroundColor(.secondary)
                    }
                }
            }
            .navigationTitle("Settings")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .navigationBarTrailing) {
                    Button("Done") {
                        dismiss()
                    }
                }
            }
        }
    }
}

#Preview {
    ContentView()
        .environmentObject(AppState())
        .environmentObject(ContinuousLearningEngine())
        .environmentObject(SensorDataManager())
}
