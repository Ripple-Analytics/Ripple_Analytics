// MentalModelsApp.swift
// Mental Models System - iOS App
// Clean, information-dense interface following M&S + Costco design language

import SwiftUI

@main
struct MentalModelsApp: App {
    @StateObject private var appState = AppState()
    @StateObject private var apiClient = APIClient()
    
    var body: some Scene {
        WindowGroup {
            ContentView()
                .environmentObject(appState)
                .environmentObject(apiClient)
                .preferredColorScheme(.light)
        }
    }
}

// MARK: - App State

class AppState: ObservableObject {
    @Published var selectedTab: Tab = .dashboard
    @Published var isAuthenticated = false
    @Published var user: User?
    @Published var notifications: [AppNotification] = []
    
    enum Tab: String, CaseIterable {
        case dashboard = "Dashboard"
        case models = "Models"
        case decisions = "Decisions"
        case analysis = "Analysis"
        case settings = "Settings"
        
        var icon: String {
            switch self {
            case .dashboard: return "chart.bar"
            case .models: return "brain"
            case .decisions: return "doc.text"
            case .analysis: return "waveform.path.ecg"
            case .settings: return "gear"
            }
        }
    }
}

// MARK: - Models

struct User: Codable, Identifiable {
    let id: String
    let name: String
    let email: String
    let avatarUrl: String?
}

struct MentalModel: Codable, Identifiable {
    let id: String
    let name: String
    let slug: String
    let description: String
    let category: String
    let discipline: String
    let source: String
    let usageCount: Int
    let effectivenessScore: Double
    let failureModes: [FailureMode]
}

struct FailureMode: Codable, Identifiable {
    let id: String
    let name: String
    let description: String
    let severity: String
    let mitigation: String
}

struct Decision: Codable, Identifiable {
    let id: String
    let title: String
    let context: String
    let modelsApplied: [String]
    let outcome: String?
    let outcomeRating: Int?
    let createdAt: Date
    let updatedAt: Date
}

struct AppNotification: Identifiable {
    let id: String
    let title: String
    let message: String
    let type: NotificationType
    let createdAt: Date
    var isRead: Bool
    
    enum NotificationType: String {
        case alert, warning, info, success
    }
}

// MARK: - API Client

class APIClient: ObservableObject {
    @Published var isLoading = false
    @Published var error: String?
    
    private let baseURL: String
    
    init(baseURL: String = "http://localhost:8080/api") {
        self.baseURL = baseURL
    }
    
    func fetch<T: Decodable>(_ endpoint: String) async throws -> T {
        guard let url = URL(string: "\(baseURL)\(endpoint)") else {
            throw APIError.invalidURL
        }
        
        let (data, response) = try await URLSession.shared.data(from: url)
        
        guard let httpResponse = response as? HTTPURLResponse,
              (200...299).contains(httpResponse.statusCode) else {
            throw APIError.serverError
        }
        
        let decoder = JSONDecoder()
        decoder.dateDecodingStrategy = .iso8601
        return try decoder.decode(T.self, from: data)
    }
    
    func post<T: Decodable, U: Encodable>(_ endpoint: String, body: U) async throws -> T {
        guard let url = URL(string: "\(baseURL)\(endpoint)") else {
            throw APIError.invalidURL
        }
        
        var request = URLRequest(url: url)
        request.httpMethod = "POST"
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.httpBody = try JSONEncoder().encode(body)
        
        let (data, response) = try await URLSession.shared.data(for: request)
        
        guard let httpResponse = response as? HTTPURLResponse,
              (200...299).contains(httpResponse.statusCode) else {
            throw APIError.serverError
        }
        
        let decoder = JSONDecoder()
        decoder.dateDecodingStrategy = .iso8601
        return try decoder.decode(T.self, from: data)
    }
    
    enum APIError: Error {
        case invalidURL
        case serverError
        case decodingError
    }
}

// MARK: - Content View

struct ContentView: View {
    @EnvironmentObject var appState: AppState
    
    var body: some View {
        TabView(selection: $appState.selectedTab) {
            DashboardView()
                .tabItem {
                    Label(AppState.Tab.dashboard.rawValue, systemImage: AppState.Tab.dashboard.icon)
                }
                .tag(AppState.Tab.dashboard)
            
            ModelsListView()
                .tabItem {
                    Label(AppState.Tab.models.rawValue, systemImage: AppState.Tab.models.icon)
                }
                .tag(AppState.Tab.models)
            
            DecisionsView()
                .tabItem {
                    Label(AppState.Tab.decisions.rawValue, systemImage: AppState.Tab.decisions.icon)
                }
                .tag(AppState.Tab.decisions)
            
            AnalysisView()
                .tabItem {
                    Label(AppState.Tab.analysis.rawValue, systemImage: AppState.Tab.analysis.icon)
                }
                .tag(AppState.Tab.analysis)
            
            SettingsView()
                .tabItem {
                    Label(AppState.Tab.settings.rawValue, systemImage: AppState.Tab.settings.icon)
                }
                .tag(AppState.Tab.settings)
        }
        .tint(Color.red)
    }
}

// MARK: - Dashboard View

struct DashboardView: View {
    @State private var metrics: DashboardMetrics?
    @State private var recentDecisions: [Decision] = []
    @State private var topModels: [MentalModel] = []
    
    var body: some View {
        NavigationStack {
            ScrollView {
                VStack(spacing: 16) {
                    // Metrics Grid
                    LazyVGrid(columns: [
                        GridItem(.flexible()),
                        GridItem(.flexible())
                    ], spacing: 12) {
                        MetricCard(title: "Models Used", value: "47", delta: "+3", isPositive: true)
                        MetricCard(title: "Decisions", value: "128", delta: "+12", isPositive: true)
                        MetricCard(title: "Effectiveness", value: "78%", delta: "+5%", isPositive: true)
                        MetricCard(title: "Health Score", value: "85", delta: "-2", isPositive: false)
                    }
                    .padding(.horizontal)
                    
                    // Recent Activity
                    VStack(alignment: .leading, spacing: 8) {
                        SectionHeader(title: "Recent Decisions")
                        
                        ForEach(0..<3) { i in
                            DecisionRow(
                                title: "Investment Analysis Q\(i+1)",
                                models: ["Second-Order Thinking", "Inversion"],
                                date: Date().addingTimeInterval(Double(-i * 86400))
                            )
                        }
                    }
                    .padding(.horizontal)
                    
                    // Top Models
                    VStack(alignment: .leading, spacing: 8) {
                        SectionHeader(title: "Most Effective Models")
                        
                        ForEach(0..<5) { i in
                            ModelRow(
                                name: ["Circle of Competence", "Margin of Safety", "Inversion", "Second-Order Thinking", "Opportunity Cost"][i],
                                category: ["Psychology", "Investing", "General", "General", "Economics"][i],
                                effectiveness: [92, 88, 85, 82, 79][i]
                            )
                        }
                    }
                    .padding(.horizontal)
                }
                .padding(.vertical)
            }
            .background(Color(.systemGray6))
            .navigationTitle("Dashboard")
            .navigationBarTitleDisplayMode(.large)
        }
    }
}

struct DashboardMetrics: Codable {
    let modelsUsed: Int
    let decisionsCount: Int
    let effectivenessScore: Double
    let healthScore: Int
}

// MARK: - Models List View

struct ModelsListView: View {
    @State private var searchText = ""
    @State private var selectedCategory: String?
    @State private var models: [MentalModel] = []
    
    let categories = ["All", "Psychology", "Economics", "Biology", "Physics", "Mathematics", "Systems"]
    
    var body: some View {
        NavigationStack {
            VStack(spacing: 0) {
                // Category Filter
                ScrollView(.horizontal, showsIndicators: false) {
                    HStack(spacing: 8) {
                        ForEach(categories, id: \.self) { category in
                            CategoryChip(
                                title: category,
                                isSelected: selectedCategory == category || (selectedCategory == nil && category == "All")
                            ) {
                                selectedCategory = category == "All" ? nil : category
                            }
                        }
                    }
                    .padding(.horizontal)
                    .padding(.vertical, 8)
                }
                .background(Color(.systemBackground))
                
                Divider()
                
                // Models List
                List {
                    ForEach(0..<20) { i in
                        NavigationLink {
                            ModelDetailView(modelId: "\(i)")
                        } label: {
                            ModelListItem(
                                name: sampleModelNames[i % sampleModelNames.count],
                                category: categories[(i % (categories.count - 1)) + 1],
                                usageCount: 50 - i * 2,
                                effectiveness: 95 - i
                            )
                        }
                    }
                }
                .listStyle(.plain)
            }
            .navigationTitle("Mental Models")
            .searchable(text: $searchText, prompt: "Search 129 models...")
        }
    }
    
    let sampleModelNames = [
        "Circle of Competence", "Margin of Safety", "Inversion", "Second-Order Thinking",
        "Opportunity Cost", "Sunk Cost", "Confirmation Bias", "Survivorship Bias",
        "Regression to Mean", "Pareto Principle", "Compound Interest", "Network Effects",
        "Economies of Scale", "Moats", "Mr. Market", "Intrinsic Value"
    ]
}

// MARK: - Model Detail View

struct ModelDetailView: View {
    let modelId: String
    @State private var model: MentalModel?
    
    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 20) {
                // Header
                VStack(alignment: .leading, spacing: 8) {
                    Text("Circle of Competence")
                        .font(.title)
                        .fontWeight(.bold)
                    
                    HStack(spacing: 12) {
                        Badge(text: "Psychology", color: .blue)
                        Badge(text: "Investing", color: .green)
                    }
                    
                    Text("Understanding the boundaries of your knowledge and expertise to make better decisions within your area of competence.")
                        .font(.body)
                        .foregroundColor(.secondary)
                }
                .padding()
                .background(Color(.systemBackground))
                .cornerRadius(8)
                
                // Metrics
                HStack(spacing: 16) {
                    MetricCard(title: "Usage", value: "47", delta: nil, isPositive: true)
                    MetricCard(title: "Effectiveness", value: "92%", delta: "+3%", isPositive: true)
                }
                .padding(.horizontal)
                
                // Failure Modes
                VStack(alignment: .leading, spacing: 12) {
                    Text("Failure Modes")
                        .font(.headline)
                        .padding(.horizontal)
                    
                    ForEach(0..<3) { i in
                        FailureModeCard(
                            name: ["Overconfidence", "Boundary Blindness", "Static Thinking"][i],
                            description: "Description of the failure mode and how to avoid it.",
                            severity: ["High", "Medium", "Low"][i]
                        )
                    }
                }
                
                // Related Models
                VStack(alignment: .leading, spacing: 12) {
                    Text("Related Models")
                        .font(.headline)
                        .padding(.horizontal)
                    
                    ScrollView(.horizontal, showsIndicators: false) {
                        HStack(spacing: 12) {
                            ForEach(0..<4) { i in
                                RelatedModelCard(
                                    name: ["Margin of Safety", "Inversion", "Second-Order", "Sunk Cost"][i]
                                )
                            }
                        }
                        .padding(.horizontal)
                    }
                }
            }
            .padding(.vertical)
        }
        .background(Color(.systemGray6))
        .navigationBarTitleDisplayMode(.inline)
    }
}

// MARK: - Decisions View

struct DecisionsView: View {
    @State private var decisions: [Decision] = []
    @State private var showNewDecision = false
    
    var body: some View {
        NavigationStack {
            List {
                ForEach(0..<10) { i in
                    DecisionListItem(
                        title: "Decision \(i + 1): Strategic Analysis",
                        context: "Evaluating market entry opportunity",
                        modelsUsed: 3,
                        outcome: i % 3 == 0 ? "Positive" : (i % 3 == 1 ? "Negative" : nil),
                        date: Date().addingTimeInterval(Double(-i * 86400 * 2))
                    )
                }
            }
            .listStyle(.plain)
            .navigationTitle("Decision Journal")
            .toolbar {
                ToolbarItem(placement: .primaryAction) {
                    Button {
                        showNewDecision = true
                    } label: {
                        Image(systemName: "plus")
                    }
                }
            }
            .sheet(isPresented: $showNewDecision) {
                NewDecisionView()
            }
        }
    }
}

struct NewDecisionView: View {
    @Environment(\.dismiss) var dismiss
    @State private var title = ""
    @State private var context = ""
    @State private var selectedModels: Set<String> = []
    
    var body: some View {
        NavigationStack {
            Form {
                Section("Decision") {
                    TextField("Title", text: $title)
                    TextField("Context", text: $context, axis: .vertical)
                        .lineLimit(3...6)
                }
                
                Section("Models to Apply") {
                    ForEach(["Inversion", "Second-Order Thinking", "Circle of Competence", "Margin of Safety"], id: \.self) { model in
                        Toggle(model, isOn: Binding(
                            get: { selectedModels.contains(model) },
                            set: { if $0 { selectedModels.insert(model) } else { selectedModels.remove(model) } }
                        ))
                    }
                }
            }
            .navigationTitle("New Decision")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .cancellationAction) {
                    Button("Cancel") { dismiss() }
                }
                ToolbarItem(placement: .confirmationAction) {
                    Button("Save") { dismiss() }
                        .disabled(title.isEmpty)
                }
            }
        }
    }
}

// MARK: - Analysis View

struct AnalysisView: View {
    @State private var analysisText = ""
    @State private var isAnalyzing = false
    @State private var results: AnalysisResults?
    
    var body: some View {
        NavigationStack {
            VStack(spacing: 16) {
                // Input
                VStack(alignment: .leading, spacing: 8) {
                    Text("Analyze Text")
                        .font(.headline)
                    
                    TextEditor(text: $analysisText)
                        .frame(height: 120)
                        .padding(8)
                        .background(Color(.systemGray6))
                        .cornerRadius(8)
                    
                    Button {
                        isAnalyzing = true
                        // Simulate analysis
                        DispatchQueue.main.asyncAfter(deadline: .now() + 2) {
                            isAnalyzing = false
                            results = AnalysisResults(
                                modelsDetected: ["Confirmation Bias", "Sunk Cost Fallacy"],
                                lollapaloozaScore: 0.72,
                                recommendations: ["Consider inversion", "Apply second-order thinking"]
                            )
                        }
                    } label: {
                        HStack {
                            if isAnalyzing {
                                ProgressView()
                                    .tint(.white)
                            }
                            Text(isAnalyzing ? "Analyzing..." : "Analyze")
                        }
                        .frame(maxWidth: .infinity)
                        .padding()
                        .background(Color.red)
                        .foregroundColor(.white)
                        .cornerRadius(8)
                    }
                    .disabled(analysisText.isEmpty || isAnalyzing)
                }
                .padding()
                
                // Results
                if let results = results {
                    VStack(alignment: .leading, spacing: 16) {
                        Text("Analysis Results")
                            .font(.headline)
                        
                        // Detected Models
                        VStack(alignment: .leading, spacing: 8) {
                            Text("Models Detected")
                                .font(.subheadline)
                                .foregroundColor(.secondary)
                            
                            ForEach(results.modelsDetected, id: \.self) { model in
                                HStack {
                                    Image(systemName: "brain")
                                        .foregroundColor(.red)
                                    Text(model)
                                }
                            }
                        }
                        
                        // Lollapalooza Score
                        VStack(alignment: .leading, spacing: 4) {
                            Text("Lollapalooza Score")
                                .font(.subheadline)
                                .foregroundColor(.secondary)
                            
                            HStack {
                                Text("\(Int(results.lollapaloozaScore * 100))%")
                                    .font(.title)
                                    .fontWeight(.bold)
                                
                                Spacer()
                                
                                ProgressView(value: results.lollapaloozaScore)
                                    .tint(.red)
                                    .frame(width: 100)
                            }
                        }
                        
                        // Recommendations
                        VStack(alignment: .leading, spacing: 8) {
                            Text("Recommendations")
                                .font(.subheadline)
                                .foregroundColor(.secondary)
                            
                            ForEach(results.recommendations, id: \.self) { rec in
                                HStack {
                                    Image(systemName: "lightbulb")
                                        .foregroundColor(.orange)
                                    Text(rec)
                                }
                            }
                        }
                    }
                    .padding()
                    .background(Color(.systemBackground))
                    .cornerRadius(8)
                    .padding(.horizontal)
                }
                
                Spacer()
            }
            .background(Color(.systemGray6))
            .navigationTitle("Analysis")
        }
    }
}

struct AnalysisResults {
    let modelsDetected: [String]
    let lollapaloozaScore: Double
    let recommendations: [String]
}

// MARK: - Settings View

struct SettingsView: View {
    @EnvironmentObject var appState: AppState
    
    var body: some View {
        NavigationStack {
            List {
                Section("Account") {
                    HStack {
                        Image(systemName: "person.circle.fill")
                            .font(.largeTitle)
                            .foregroundColor(.gray)
                        
                        VStack(alignment: .leading) {
                            Text("User Name")
                                .font(.headline)
                            Text("user@example.com")
                                .font(.caption)
                                .foregroundColor(.secondary)
                        }
                    }
                    .padding(.vertical, 4)
                }
                
                Section("Preferences") {
                    NavigationLink("Notifications") {
                        Text("Notification Settings")
                    }
                    NavigationLink("Data & Sync") {
                        Text("Data Settings")
                    }
                    NavigationLink("Appearance") {
                        Text("Appearance Settings")
                    }
                }
                
                Section("About") {
                    HStack {
                        Text("Version")
                        Spacer()
                        Text("1.0.0")
                            .foregroundColor(.secondary)
                    }
                    
                    NavigationLink("Privacy Policy") {
                        Text("Privacy Policy")
                    }
                    NavigationLink("Terms of Service") {
                        Text("Terms of Service")
                    }
                }
                
                Section {
                    Button("Sign Out", role: .destructive) {
                        appState.isAuthenticated = false
                    }
                }
            }
            .navigationTitle("Settings")
        }
    }
}

// MARK: - Reusable Components

struct MetricCard: View {
    let title: String
    let value: String
    let delta: String?
    let isPositive: Bool
    
    var body: some View {
        VStack(alignment: .leading, spacing: 4) {
            Text(title)
                .font(.caption)
                .foregroundColor(.secondary)
                .textCase(.uppercase)
            
            HStack(alignment: .firstTextBaseline, spacing: 4) {
                Text(value)
                    .font(.title2)
                    .fontWeight(.semibold)
                
                if let delta = delta {
                    Text(delta)
                        .font(.caption)
                        .fontWeight(.medium)
                        .foregroundColor(isPositive ? .green : .red)
                }
            }
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .padding()
        .background(Color(.systemBackground))
        .cornerRadius(8)
    }
}

struct SectionHeader: View {
    let title: String
    
    var body: some View {
        HStack {
            Text(title)
                .font(.headline)
            Spacer()
            Button("See All") {}
                .font(.caption)
                .foregroundColor(.red)
        }
    }
}

struct DecisionRow: View {
    let title: String
    let models: [String]
    let date: Date
    
    var body: some View {
        HStack {
            VStack(alignment: .leading, spacing: 4) {
                Text(title)
                    .font(.subheadline)
                    .fontWeight(.medium)
                
                Text(models.joined(separator: ", "))
                    .font(.caption)
                    .foregroundColor(.secondary)
            }
            
            Spacer()
            
            Text(date, style: .date)
                .font(.caption)
                .foregroundColor(.secondary)
        }
        .padding()
        .background(Color(.systemBackground))
        .cornerRadius(8)
    }
}

struct ModelRow: View {
    let name: String
    let category: String
    let effectiveness: Int
    
    var body: some View {
        HStack {
            VStack(alignment: .leading, spacing: 2) {
                Text(name)
                    .font(.subheadline)
                    .fontWeight(.medium)
                
                Text(category)
                    .font(.caption)
                    .foregroundColor(.secondary)
            }
            
            Spacer()
            
            Text("\(effectiveness)%")
                .font(.subheadline)
                .fontWeight(.semibold)
                .foregroundColor(.green)
        }
        .padding()
        .background(Color(.systemBackground))
        .cornerRadius(8)
    }
}

struct CategoryChip: View {
    let title: String
    let isSelected: Bool
    let action: () -> Void
    
    var body: some View {
        Button(action: action) {
            Text(title)
                .font(.caption)
                .fontWeight(.medium)
                .padding(.horizontal, 12)
                .padding(.vertical, 6)
                .background(isSelected ? Color.red : Color(.systemGray5))
                .foregroundColor(isSelected ? .white : .primary)
                .cornerRadius(16)
        }
    }
}

struct ModelListItem: View {
    let name: String
    let category: String
    let usageCount: Int
    let effectiveness: Int
    
    var body: some View {
        VStack(alignment: .leading, spacing: 4) {
            Text(name)
                .font(.body)
                .fontWeight(.medium)
            
            HStack {
                Text(category)
                    .font(.caption)
                    .foregroundColor(.secondary)
                
                Spacer()
                
                HStack(spacing: 12) {
                    Label("\(usageCount)", systemImage: "chart.bar")
                    Label("\(effectiveness)%", systemImage: "checkmark.circle")
                }
                .font(.caption)
                .foregroundColor(.secondary)
            }
        }
        .padding(.vertical, 4)
    }
}

struct Badge: View {
    let text: String
    let color: Color
    
    var body: some View {
        Text(text)
            .font(.caption)
            .fontWeight(.medium)
            .padding(.horizontal, 8)
            .padding(.vertical, 4)
            .background(color.opacity(0.1))
            .foregroundColor(color)
            .cornerRadius(4)
    }
}

struct FailureModeCard: View {
    let name: String
    let description: String
    let severity: String
    
    var severityColor: Color {
        switch severity {
        case "High": return .red
        case "Medium": return .orange
        default: return .yellow
        }
    }
    
    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            HStack {
                Text(name)
                    .font(.subheadline)
                    .fontWeight(.semibold)
                
                Spacer()
                
                Text(severity)
                    .font(.caption)
                    .fontWeight(.medium)
                    .padding(.horizontal, 8)
                    .padding(.vertical, 2)
                    .background(severityColor.opacity(0.1))
                    .foregroundColor(severityColor)
                    .cornerRadius(4)
            }
            
            Text(description)
                .font(.caption)
                .foregroundColor(.secondary)
        }
        .padding()
        .background(Color(.systemBackground))
        .cornerRadius(8)
        .padding(.horizontal)
    }
}

struct RelatedModelCard: View {
    let name: String
    
    var body: some View {
        VStack {
            Image(systemName: "brain")
                .font(.title2)
                .foregroundColor(.red)
            
            Text(name)
                .font(.caption)
                .fontWeight(.medium)
                .multilineTextAlignment(.center)
        }
        .frame(width: 80, height: 80)
        .background(Color(.systemBackground))
        .cornerRadius(8)
    }
}

struct DecisionListItem: View {
    let title: String
    let context: String
    let modelsUsed: Int
    let outcome: String?
    let date: Date
    
    var outcomeColor: Color {
        switch outcome {
        case "Positive": return .green
        case "Negative": return .red
        default: return .gray
        }
    }
    
    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            HStack {
                Text(title)
                    .font(.body)
                    .fontWeight(.medium)
                
                Spacer()
                
                if let outcome = outcome {
                    Text(outcome)
                        .font(.caption)
                        .fontWeight(.medium)
                        .padding(.horizontal, 8)
                        .padding(.vertical, 2)
                        .background(outcomeColor.opacity(0.1))
                        .foregroundColor(outcomeColor)
                        .cornerRadius(4)
                }
            }
            
            Text(context)
                .font(.caption)
                .foregroundColor(.secondary)
            
            HStack {
                Label("\(modelsUsed) models", systemImage: "brain")
                Spacer()
                Text(date, style: .date)
            }
            .font(.caption)
            .foregroundColor(.secondary)
        }
        .padding(.vertical, 4)
    }
}

#Preview {
    ContentView()
        .environmentObject(AppState())
        .environmentObject(APIClient())
}
