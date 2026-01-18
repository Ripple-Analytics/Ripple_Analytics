// Mental Models iOS App
// Design Language: M&S + Costco
// - Clean typography, high information density
// - Monochrome palette with strategic red accents
// - Steve Jobs level attention to detail

import SwiftUI

// MARK: - Design System

struct DesignSystem {
    // Colors - Monochrome with red accent
    struct Colors {
        static let background = Color(hex: "FAFAFA")
        static let surface = Color.white
        static let text = Color(hex: "1A1A1A")
        static let textSecondary = Color(hex: "666666")
        static let textTertiary = Color(hex: "999999")
        static let border = Color(hex: "E5E5E5")
        static let accent = Color(hex: "CC0000")  // Costco red
        static let accentLight = Color(hex: "FFE5E5")
        static let success = Color(hex: "2E7D32")
        static let warning = Color(hex: "F57C00")
    }
    
    // Typography - Clean, professional
    struct Typography {
        static let largeTitle = Font.system(size: 28, weight: .bold, design: .default)
        static let title = Font.system(size: 22, weight: .semibold, design: .default)
        static let headline = Font.system(size: 17, weight: .semibold, design: .default)
        static let body = Font.system(size: 15, weight: .regular, design: .default)
        static let callout = Font.system(size: 14, weight: .regular, design: .default)
        static let caption = Font.system(size: 12, weight: .regular, design: .default)
        static let mono = Font.system(size: 13, weight: .medium, design: .monospaced)
    }
    
    // Spacing
    struct Spacing {
        static let xs: CGFloat = 4
        static let sm: CGFloat = 8
        static let md: CGFloat = 16
        static let lg: CGFloat = 24
        static let xl: CGFloat = 32
    }
}

// MARK: - Color Extension

extension Color {
    init(hex: String) {
        let hex = hex.trimmingCharacters(in: CharacterSet.alphanumerics.inverted)
        var int: UInt64 = 0
        Scanner(string: hex).scanHexInt64(&int)
        let a, r, g, b: UInt64
        switch hex.count {
        case 6:
            (a, r, g, b) = (255, int >> 16, int >> 8 & 0xFF, int & 0xFF)
        default:
            (a, r, g, b) = (255, 0, 0, 0)
        }
        self.init(
            .sRGB,
            red: Double(r) / 255,
            green: Double(g) / 255,
            blue: Double(b) / 255,
            opacity: Double(a) / 255
        )
    }
}

// MARK: - Main Content View

struct ContentView: View {
    @State private var selectedTab = 0
    
    var body: some View {
        TabView(selection: $selectedTab) {
            DashboardView()
                .tabItem {
                    Image(systemName: "chart.bar.fill")
                    Text("Dashboard")
                }
                .tag(0)
            
            NewsView()
                .tabItem {
                    Image(systemName: "newspaper.fill")
                    Text("News")
                }
                .tag(1)
            
            ModelsView()
                .tabItem {
                    Image(systemName: "brain.head.profile")
                    Text("Models")
                }
                .tag(2)
            
            AnalysisView()
                .tabItem {
                    Image(systemName: "waveform.path.ecg")
                    Text("Analysis")
                }
                .tag(3)
            
            MeshView()
                .tabItem {
                    Image(systemName: "network")
                    Text("Mesh")
                }
                .tag(4)
        }
        .tint(DesignSystem.Colors.accent)
    }
}

// MARK: - Dashboard View

struct DashboardView: View {
    var body: some View {
        NavigationView {
            ScrollView {
                VStack(spacing: DesignSystem.Spacing.md) {
                    // Key Metrics Row
                    HStack(spacing: DesignSystem.Spacing.sm) {
                        MetricCard(title: "Models", value: "129", trend: nil)
                        MetricCard(title: "Decisions", value: "847", trend: "+12")
                        MetricCard(title: "Accuracy", value: "73%", trend: "+2.1%")
                    }
                    .padding(.horizontal, DesignSystem.Spacing.md)
                    
                    // Processing Status
                    ProcessingStatusCard()
                        .padding(.horizontal, DesignSystem.Spacing.md)
                    
                    // Recent Analyses
                    RecentAnalysesSection()
                    
                    // Active Alerts
                    AlertsSection()
                }
                .padding(.vertical, DesignSystem.Spacing.md)
            }
            .background(DesignSystem.Colors.background)
            .navigationTitle("Dashboard")
            .navigationBarTitleDisplayMode(.large)
        }
    }
}

// MARK: - Metric Card

struct MetricCard: View {
    let title: String
    let value: String
    let trend: String?
    
    var body: some View {
        VStack(alignment: .leading, spacing: DesignSystem.Spacing.xs) {
            Text(title)
                .font(DesignSystem.Typography.caption)
                .foregroundColor(DesignSystem.Colors.textSecondary)
            
            HStack(alignment: .firstTextBaseline, spacing: DesignSystem.Spacing.xs) {
                Text(value)
                    .font(DesignSystem.Typography.title)
                    .foregroundColor(DesignSystem.Colors.text)
                
                if let trend = trend {
                    Text(trend)
                        .font(DesignSystem.Typography.caption)
                        .foregroundColor(trend.hasPrefix("+") ? DesignSystem.Colors.success : DesignSystem.Colors.accent)
                }
            }
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .padding(DesignSystem.Spacing.md)
        .background(DesignSystem.Colors.surface)
        .cornerRadius(8)
        .overlay(
            RoundedRectangle(cornerRadius: 8)
                .stroke(DesignSystem.Colors.border, lineWidth: 1)
        )
    }
}

// MARK: - Processing Status Card

struct ProcessingStatusCard: View {
    var body: some View {
        VStack(alignment: .leading, spacing: DesignSystem.Spacing.md) {
            HStack {
                Text("Processing Status")
                    .font(DesignSystem.Typography.headline)
                    .foregroundColor(DesignSystem.Colors.text)
                
                Spacer()
                
                HStack(spacing: DesignSystem.Spacing.xs) {
                    Circle()
                        .fill(DesignSystem.Colors.success)
                        .frame(width: 8, height: 8)
                    Text("Active")
                        .font(DesignSystem.Typography.caption)
                        .foregroundColor(DesignSystem.Colors.success)
                }
            }
            
            // Progress bars
            VStack(spacing: DesignSystem.Spacing.sm) {
                ProcessingRow(label: "CPU", value: 87, color: DesignSystem.Colors.text)
                ProcessingRow(label: "GPU", value: 92, color: DesignSystem.Colors.accent)
                ProcessingRow(label: "Queue", value: 1247, isCount: true)
            }
            
            // Throughput
            HStack {
                VStack(alignment: .leading) {
                    Text("Throughput")
                        .font(DesignSystem.Typography.caption)
                        .foregroundColor(DesignSystem.Colors.textSecondary)
                    Text("3.2K items/min")
                        .font(DesignSystem.Typography.mono)
                        .foregroundColor(DesignSystem.Colors.text)
                }
                
                Spacer()
                
                VStack(alignment: .trailing) {
                    Text("Uptime")
                        .font(DesignSystem.Typography.caption)
                        .foregroundColor(DesignSystem.Colors.textSecondary)
                    Text("14d 7h 23m")
                        .font(DesignSystem.Typography.mono)
                        .foregroundColor(DesignSystem.Colors.text)
                }
            }
        }
        .padding(DesignSystem.Spacing.md)
        .background(DesignSystem.Colors.surface)
        .cornerRadius(8)
        .overlay(
            RoundedRectangle(cornerRadius: 8)
                .stroke(DesignSystem.Colors.border, lineWidth: 1)
        )
    }
}

struct ProcessingRow: View {
    let label: String
    let value: Int
    var color: Color = DesignSystem.Colors.text
    var isCount: Bool = false
    
    var body: some View {
        HStack {
            Text(label)
                .font(DesignSystem.Typography.callout)
                .foregroundColor(DesignSystem.Colors.textSecondary)
                .frame(width: 50, alignment: .leading)
            
            if isCount {
                Spacer()
                Text("\(value)")
                    .font(DesignSystem.Typography.mono)
                    .foregroundColor(DesignSystem.Colors.text)
            } else {
                GeometryReader { geometry in
                    ZStack(alignment: .leading) {
                        Rectangle()
                            .fill(DesignSystem.Colors.border)
                            .frame(height: 4)
                        
                        Rectangle()
                            .fill(color)
                            .frame(width: geometry.size.width * CGFloat(value) / 100, height: 4)
                    }
                    .cornerRadius(2)
                }
                .frame(height: 4)
                
                Text("\(value)%")
                    .font(DesignSystem.Typography.mono)
                    .foregroundColor(DesignSystem.Colors.text)
                    .frame(width: 45, alignment: .trailing)
            }
        }
    }
}

// MARK: - Recent Analyses Section

struct RecentAnalysesSection: View {
    var body: some View {
        VStack(alignment: .leading, spacing: DesignSystem.Spacing.sm) {
            HStack {
                Text("Recent Analyses")
                    .font(DesignSystem.Typography.headline)
                    .foregroundColor(DesignSystem.Colors.text)
                
                Spacer()
                
                Button("See All") {
                    // Navigate to full list
                }
                .font(DesignSystem.Typography.callout)
                .foregroundColor(DesignSystem.Colors.accent)
            }
            .padding(.horizontal, DesignSystem.Spacing.md)
            
            ScrollView(.horizontal, showsIndicators: false) {
                HStack(spacing: DesignSystem.Spacing.sm) {
                    AnalysisCard(
                        title: "Fed Rate Decision",
                        source: "Reuters",
                        models: ["Incentives", "Second-Order"],
                        risk: "Medium",
                        time: "2m ago"
                    )
                    
                    AnalysisCard(
                        title: "NVIDIA Earnings",
                        source: "Bloomberg",
                        models: ["Moats", "Network Effects"],
                        risk: "Low",
                        time: "15m ago"
                    )
                    
                    AnalysisCard(
                        title: "China Trade Policy",
                        source: "FT",
                        models: ["Game Theory", "Incentives"],
                        risk: "High",
                        time: "1h ago"
                    )
                }
                .padding(.horizontal, DesignSystem.Spacing.md)
            }
        }
    }
}

struct AnalysisCard: View {
    let title: String
    let source: String
    let models: [String]
    let risk: String
    let time: String
    
    var riskColor: Color {
        switch risk {
        case "High": return DesignSystem.Colors.accent
        case "Medium": return DesignSystem.Colors.warning
        default: return DesignSystem.Colors.success
        }
    }
    
    var body: some View {
        VStack(alignment: .leading, spacing: DesignSystem.Spacing.sm) {
            HStack {
                Text(source)
                    .font(DesignSystem.Typography.caption)
                    .foregroundColor(DesignSystem.Colors.textSecondary)
                
                Spacer()
                
                Text(time)
                    .font(DesignSystem.Typography.caption)
                    .foregroundColor(DesignSystem.Colors.textTertiary)
            }
            
            Text(title)
                .font(DesignSystem.Typography.headline)
                .foregroundColor(DesignSystem.Colors.text)
                .lineLimit(2)
            
            HStack {
                ForEach(models, id: \.self) { model in
                    Text(model)
                        .font(DesignSystem.Typography.caption)
                        .foregroundColor(DesignSystem.Colors.textSecondary)
                        .padding(.horizontal, 6)
                        .padding(.vertical, 2)
                        .background(DesignSystem.Colors.background)
                        .cornerRadius(4)
                }
            }
            
            HStack {
                Text("Risk:")
                    .font(DesignSystem.Typography.caption)
                    .foregroundColor(DesignSystem.Colors.textSecondary)
                
                Text(risk)
                    .font(DesignSystem.Typography.caption)
                    .fontWeight(.semibold)
                    .foregroundColor(riskColor)
            }
        }
        .frame(width: 200)
        .padding(DesignSystem.Spacing.md)
        .background(DesignSystem.Colors.surface)
        .cornerRadius(8)
        .overlay(
            RoundedRectangle(cornerRadius: 8)
                .stroke(DesignSystem.Colors.border, lineWidth: 1)
        )
    }
}

// MARK: - Alerts Section

struct AlertsSection: View {
    var body: some View {
        VStack(alignment: .leading, spacing: DesignSystem.Spacing.sm) {
            Text("Active Alerts")
                .font(DesignSystem.Typography.headline)
                .foregroundColor(DesignSystem.Colors.text)
                .padding(.horizontal, DesignSystem.Spacing.md)
            
            VStack(spacing: 1) {
                AlertRow(
                    type: .lollapalooza,
                    title: "Lollapalooza Detected",
                    description: "5 models converging on NVDA analysis",
                    time: "Just now"
                )
                
                AlertRow(
                    type: .risk,
                    title: "High Risk Pattern",
                    description: "Confirmation bias detected in recent decisions",
                    time: "10m ago"
                )
                
                AlertRow(
                    type: .insight,
                    title: "New Insight",
                    description: "Cross-domain pattern found: Tech + Psychology",
                    time: "1h ago"
                )
            }
            .background(DesignSystem.Colors.surface)
            .cornerRadius(8)
            .overlay(
                RoundedRectangle(cornerRadius: 8)
                    .stroke(DesignSystem.Colors.border, lineWidth: 1)
            )
            .padding(.horizontal, DesignSystem.Spacing.md)
        }
    }
}

enum AlertType {
    case lollapalooza, risk, insight
    
    var icon: String {
        switch self {
        case .lollapalooza: return "star.fill"
        case .risk: return "exclamationmark.triangle.fill"
        case .insight: return "lightbulb.fill"
        }
    }
    
    var color: Color {
        switch self {
        case .lollapalooza: return DesignSystem.Colors.accent
        case .risk: return DesignSystem.Colors.warning
        case .insight: return DesignSystem.Colors.success
        }
    }
}

struct AlertRow: View {
    let type: AlertType
    let title: String
    let description: String
    let time: String
    
    var body: some View {
        HStack(spacing: DesignSystem.Spacing.md) {
            Image(systemName: type.icon)
                .foregroundColor(type.color)
                .frame(width: 24)
            
            VStack(alignment: .leading, spacing: 2) {
                Text(title)
                    .font(DesignSystem.Typography.callout)
                    .fontWeight(.medium)
                    .foregroundColor(DesignSystem.Colors.text)
                
                Text(description)
                    .font(DesignSystem.Typography.caption)
                    .foregroundColor(DesignSystem.Colors.textSecondary)
            }
            
            Spacer()
            
            Text(time)
                .font(DesignSystem.Typography.caption)
                .foregroundColor(DesignSystem.Colors.textTertiary)
            
            Image(systemName: "chevron.right")
                .font(.system(size: 12, weight: .semibold))
                .foregroundColor(DesignSystem.Colors.textTertiary)
        }
        .padding(DesignSystem.Spacing.md)
        .background(DesignSystem.Colors.surface)
    }
}

// MARK: - News View

struct NewsView: View {
    @State private var selectedCategory = "All"
    let categories = ["All", "Financial", "Tech", "General", "Academic"]
    
    var body: some View {
        NavigationView {
            VStack(spacing: 0) {
                // Category Pills
                ScrollView(.horizontal, showsIndicators: false) {
                    HStack(spacing: DesignSystem.Spacing.sm) {
                        ForEach(categories, id: \.self) { category in
                            CategoryPill(
                                title: category,
                                isSelected: selectedCategory == category
                            ) {
                                withAnimation(.easeInOut(duration: 0.2)) {
                                    selectedCategory = category
                                }
                            }
                        }
                    }
                    .padding(.horizontal, DesignSystem.Spacing.md)
                    .padding(.vertical, DesignSystem.Spacing.sm)
                }
                .background(DesignSystem.Colors.surface)
                
                Divider()
                
                // News List
                ScrollView {
                    LazyVStack(spacing: 0) {
                        NewsRow(
                            title: "Federal Reserve Signals Potential Rate Cut in Q2",
                            source: "Reuters",
                            time: "2 min ago",
                            isAnalyzed: true,
                            relevantModels: 7
                        )
                        
                        NewsRow(
                            title: "NVIDIA Reports Record Quarterly Revenue",
                            source: "Bloomberg",
                            time: "15 min ago",
                            isAnalyzed: true,
                            relevantModels: 5
                        )
                        
                        NewsRow(
                            title: "China Announces New Trade Tariffs",
                            source: "Financial Times",
                            time: "1 hour ago",
                            isAnalyzed: false,
                            relevantModels: 0
                        )
                        
                        NewsRow(
                            title: "Apple Vision Pro Sales Below Expectations",
                            source: "WSJ",
                            time: "2 hours ago",
                            isAnalyzed: true,
                            relevantModels: 4
                        )
                    }
                }
            }
            .background(DesignSystem.Colors.background)
            .navigationTitle("News")
            .navigationBarTitleDisplayMode(.large)
            .toolbar {
                ToolbarItem(placement: .navigationBarTrailing) {
                    Button(action: {}) {
                        Image(systemName: "line.3.horizontal.decrease.circle")
                            .foregroundColor(DesignSystem.Colors.text)
                    }
                }
            }
        }
    }
}

struct CategoryPill: View {
    let title: String
    let isSelected: Bool
    let action: () -> Void
    
    var body: some View {
        Button(action: action) {
            Text(title)
                .font(DesignSystem.Typography.callout)
                .fontWeight(isSelected ? .semibold : .regular)
                .foregroundColor(isSelected ? .white : DesignSystem.Colors.text)
                .padding(.horizontal, DesignSystem.Spacing.md)
                .padding(.vertical, DesignSystem.Spacing.sm)
                .background(isSelected ? DesignSystem.Colors.text : DesignSystem.Colors.background)
                .cornerRadius(20)
        }
    }
}

struct NewsRow: View {
    let title: String
    let source: String
    let time: String
    let isAnalyzed: Bool
    let relevantModels: Int
    
    var body: some View {
        VStack(spacing: 0) {
            HStack(alignment: .top, spacing: DesignSystem.Spacing.md) {
                VStack(alignment: .leading, spacing: DesignSystem.Spacing.xs) {
                    HStack {
                        Text(source)
                            .font(DesignSystem.Typography.caption)
                            .foregroundColor(DesignSystem.Colors.textSecondary)
                        
                        Text("•")
                            .foregroundColor(DesignSystem.Colors.textTertiary)
                        
                        Text(time)
                            .font(DesignSystem.Typography.caption)
                            .foregroundColor(DesignSystem.Colors.textTertiary)
                    }
                    
                    Text(title)
                        .font(DesignSystem.Typography.body)
                        .foregroundColor(DesignSystem.Colors.text)
                        .lineLimit(2)
                    
                    if isAnalyzed {
                        HStack(spacing: DesignSystem.Spacing.xs) {
                            Image(systemName: "checkmark.circle.fill")
                                .font(.system(size: 12))
                                .foregroundColor(DesignSystem.Colors.success)
                            
                            Text("\(relevantModels) models applied")
                                .font(DesignSystem.Typography.caption)
                                .foregroundColor(DesignSystem.Colors.textSecondary)
                        }
                    }
                }
                
                Spacer()
                
                Button(action: {}) {
                    Text(isAnalyzed ? "View" : "Analyze")
                        .font(DesignSystem.Typography.caption)
                        .fontWeight(.semibold)
                        .foregroundColor(isAnalyzed ? DesignSystem.Colors.text : .white)
                        .padding(.horizontal, DesignSystem.Spacing.md)
                        .padding(.vertical, DesignSystem.Spacing.sm)
                        .background(isAnalyzed ? DesignSystem.Colors.background : DesignSystem.Colors.accent)
                        .cornerRadius(6)
                        .overlay(
                            RoundedRectangle(cornerRadius: 6)
                                .stroke(isAnalyzed ? DesignSystem.Colors.border : Color.clear, lineWidth: 1)
                        )
                }
            }
            .padding(DesignSystem.Spacing.md)
            .background(DesignSystem.Colors.surface)
            
            Divider()
        }
    }
}

// MARK: - Models View

struct ModelsView: View {
    @State private var searchText = ""
    
    var body: some View {
        NavigationView {
            ScrollView {
                VStack(alignment: .leading, spacing: DesignSystem.Spacing.md) {
                    // Munger Hierarchy Categories
                    ForEach(MungerCategory.allCases, id: \.self) { category in
                        ModelCategorySection(category: category)
                    }
                }
                .padding(.vertical, DesignSystem.Spacing.md)
            }
            .background(DesignSystem.Colors.background)
            .navigationTitle("Mental Models")
            .navigationBarTitleDisplayMode(.large)
            .searchable(text: $searchText, prompt: "Search 129 models...")
        }
    }
}

enum MungerCategory: String, CaseIterable {
    case mathematics = "Mathematics"
    case accounting = "Accounting"
    case physics = "Physics"
    case chemistry = "Chemistry"
    case engineering = "Engineering"
    case biology = "Biology/Physiology"
    case psychology = "Psychology"
    case economics = "Economics"
    
    var icon: String {
        switch self {
        case .mathematics: return "function"
        case .accounting: return "dollarsign.circle"
        case .physics: return "atom"
        case .chemistry: return "flask"
        case .engineering: return "gearshape.2"
        case .biology: return "leaf"
        case .psychology: return "brain"
        case .economics: return "chart.line.uptrend.xyaxis"
        }
    }
    
    var modelCount: Int {
        switch self {
        case .mathematics: return 12
        case .accounting: return 8
        case .physics: return 11
        case .chemistry: return 6
        case .engineering: return 9
        case .biology: return 10
        case .psychology: return 34
        case .economics: return 20
        }
    }
}

struct ModelCategorySection: View {
    let category: MungerCategory
    @State private var isExpanded = false
    
    var body: some View {
        VStack(spacing: 0) {
            Button(action: {
                withAnimation(.easeInOut(duration: 0.2)) {
                    isExpanded.toggle()
                }
            }) {
                HStack {
                    Image(systemName: category.icon)
                        .font(.system(size: 18))
                        .foregroundColor(DesignSystem.Colors.accent)
                        .frame(width: 32)
                    
                    Text(category.rawValue)
                        .font(DesignSystem.Typography.headline)
                        .foregroundColor(DesignSystem.Colors.text)
                    
                    Spacer()
                    
                    Text("\(category.modelCount)")
                        .font(DesignSystem.Typography.callout)
                        .foregroundColor(DesignSystem.Colors.textSecondary)
                    
                    Image(systemName: isExpanded ? "chevron.up" : "chevron.down")
                        .font(.system(size: 12, weight: .semibold))
                        .foregroundColor(DesignSystem.Colors.textTertiary)
                }
                .padding(DesignSystem.Spacing.md)
                .background(DesignSystem.Colors.surface)
            }
            
            if isExpanded {
                VStack(spacing: 1) {
                    // Sample models for the category
                    ModelRow(name: "Compound Interest", usageCount: 47)
                    ModelRow(name: "Probability Theory", usageCount: 32)
                    ModelRow(name: "Bayesian Updating", usageCount: 28)
                }
                .background(DesignSystem.Colors.border)
            }
        }
        .background(DesignSystem.Colors.surface)
        .cornerRadius(8)
        .overlay(
            RoundedRectangle(cornerRadius: 8)
                .stroke(DesignSystem.Colors.border, lineWidth: 1)
        )
        .padding(.horizontal, DesignSystem.Spacing.md)
    }
}

struct ModelRow: View {
    let name: String
    let usageCount: Int
    
    var body: some View {
        HStack {
            Text(name)
                .font(DesignSystem.Typography.body)
                .foregroundColor(DesignSystem.Colors.text)
            
            Spacer()
            
            Text("\(usageCount) uses")
                .font(DesignSystem.Typography.caption)
                .foregroundColor(DesignSystem.Colors.textTertiary)
            
            Image(systemName: "chevron.right")
                .font(.system(size: 12, weight: .semibold))
                .foregroundColor(DesignSystem.Colors.textTertiary)
        }
        .padding(.horizontal, DesignSystem.Spacing.md)
        .padding(.vertical, DesignSystem.Spacing.sm)
        .padding(.leading, 32)
        .background(DesignSystem.Colors.surface)
    }
}

// MARK: - Analysis View

struct AnalysisView: View {
    var body: some View {
        NavigationView {
            ScrollView {
                VStack(spacing: DesignSystem.Spacing.md) {
                    // Quick Actions
                    HStack(spacing: DesignSystem.Spacing.sm) {
                        QuickActionButton(icon: "doc.text.viewfinder", title: "Scan Document")
                        QuickActionButton(icon: "mic.fill", title: "Voice Query")
                        QuickActionButton(icon: "camera.fill", title: "Photo Analysis")
                    }
                    .padding(.horizontal, DesignSystem.Spacing.md)
                    
                    // Lollapalooza Detector
                    LollapaloozaCard()
                        .padding(.horizontal, DesignSystem.Spacing.md)
                    
                    // Recent Analyses
                    VStack(alignment: .leading, spacing: DesignSystem.Spacing.sm) {
                        Text("Analysis History")
                            .font(DesignSystem.Typography.headline)
                            .foregroundColor(DesignSystem.Colors.text)
                            .padding(.horizontal, DesignSystem.Spacing.md)
                        
                        VStack(spacing: 1) {
                            AnalysisHistoryRow(
                                title: "Q4 Earnings Analysis",
                                type: "Document",
                                modelsUsed: 12,
                                date: "Today"
                            )
                            AnalysisHistoryRow(
                                title: "Investment Thesis Review",
                                type: "Voice",
                                modelsUsed: 8,
                                date: "Yesterday"
                            )
                            AnalysisHistoryRow(
                                title: "Competitor Analysis",
                                type: "Document",
                                modelsUsed: 15,
                                date: "Jan 15"
                            )
                        }
                        .background(DesignSystem.Colors.surface)
                        .cornerRadius(8)
                        .overlay(
                            RoundedRectangle(cornerRadius: 8)
                                .stroke(DesignSystem.Colors.border, lineWidth: 1)
                        )
                        .padding(.horizontal, DesignSystem.Spacing.md)
                    }
                }
                .padding(.vertical, DesignSystem.Spacing.md)
            }
            .background(DesignSystem.Colors.background)
            .navigationTitle("Analysis")
            .navigationBarTitleDisplayMode(.large)
        }
    }
}

struct QuickActionButton: View {
    let icon: String
    let title: String
    
    var body: some View {
        VStack(spacing: DesignSystem.Spacing.sm) {
            Image(systemName: icon)
                .font(.system(size: 24))
                .foregroundColor(DesignSystem.Colors.accent)
            
            Text(title)
                .font(DesignSystem.Typography.caption)
                .foregroundColor(DesignSystem.Colors.text)
        }
        .frame(maxWidth: .infinity)
        .padding(DesignSystem.Spacing.md)
        .background(DesignSystem.Colors.surface)
        .cornerRadius(8)
        .overlay(
            RoundedRectangle(cornerRadius: 8)
                .stroke(DesignSystem.Colors.border, lineWidth: 1)
        )
    }
}

struct LollapaloozaCard: View {
    var body: some View {
        VStack(alignment: .leading, spacing: DesignSystem.Spacing.md) {
            HStack {
                Image(systemName: "star.fill")
                    .foregroundColor(DesignSystem.Colors.accent)
                
                Text("Lollapalooza Detector")
                    .font(DesignSystem.Typography.headline)
                    .foregroundColor(DesignSystem.Colors.text)
                
                Spacer()
                
                Text("Active")
                    .font(DesignSystem.Typography.caption)
                    .foregroundColor(DesignSystem.Colors.success)
                    .padding(.horizontal, 8)
                    .padding(.vertical, 4)
                    .background(DesignSystem.Colors.success.opacity(0.1))
                    .cornerRadius(4)
            }
            
            Text("Monitoring for convergence of multiple mental models on the same conclusion.")
                .font(DesignSystem.Typography.callout)
                .foregroundColor(DesignSystem.Colors.textSecondary)
            
            HStack {
                VStack(alignment: .leading) {
                    Text("Last Detection")
                        .font(DesignSystem.Typography.caption)
                        .foregroundColor(DesignSystem.Colors.textTertiary)
                    Text("2 hours ago")
                        .font(DesignSystem.Typography.mono)
                        .foregroundColor(DesignSystem.Colors.text)
                }
                
                Spacer()
                
                VStack(alignment: .trailing) {
                    Text("Total Detections")
                        .font(DesignSystem.Typography.caption)
                        .foregroundColor(DesignSystem.Colors.textTertiary)
                    Text("47")
                        .font(DesignSystem.Typography.mono)
                        .foregroundColor(DesignSystem.Colors.text)
                }
            }
        }
        .padding(DesignSystem.Spacing.md)
        .background(DesignSystem.Colors.accentLight)
        .cornerRadius(8)
        .overlay(
            RoundedRectangle(cornerRadius: 8)
                .stroke(DesignSystem.Colors.accent.opacity(0.3), lineWidth: 1)
        )
    }
}

struct AnalysisHistoryRow: View {
    let title: String
    let type: String
    let modelsUsed: Int
    let date: String
    
    var body: some View {
        HStack {
            VStack(alignment: .leading, spacing: 2) {
                Text(title)
                    .font(DesignSystem.Typography.body)
                    .foregroundColor(DesignSystem.Colors.text)
                
                HStack(spacing: DesignSystem.Spacing.sm) {
                    Text(type)
                        .font(DesignSystem.Typography.caption)
                        .foregroundColor(DesignSystem.Colors.textSecondary)
                    
                    Text("•")
                        .foregroundColor(DesignSystem.Colors.textTertiary)
                    
                    Text("\(modelsUsed) models")
                        .font(DesignSystem.Typography.caption)
                        .foregroundColor(DesignSystem.Colors.textSecondary)
                }
            }
            
            Spacer()
            
            Text(date)
                .font(DesignSystem.Typography.caption)
                .foregroundColor(DesignSystem.Colors.textTertiary)
            
            Image(systemName: "chevron.right")
                .font(.system(size: 12, weight: .semibold))
                .foregroundColor(DesignSystem.Colors.textTertiary)
        }
        .padding(DesignSystem.Spacing.md)
        .background(DesignSystem.Colors.surface)
    }
}

// MARK: - Mesh View

struct MeshView: View {
    var body: some View {
        NavigationView {
            ScrollView {
                VStack(spacing: DesignSystem.Spacing.md) {
                    // Mesh Status
                    MeshStatusCard()
                        .padding(.horizontal, DesignSystem.Spacing.md)
                    
                    // Connected Devices
                    VStack(alignment: .leading, spacing: DesignSystem.Spacing.sm) {
                        Text("Connected Devices")
                            .font(DesignSystem.Typography.headline)
                            .foregroundColor(DesignSystem.Colors.text)
                            .padding(.horizontal, DesignSystem.Spacing.md)
                        
                        VStack(spacing: 1) {
                            DeviceRow(
                                name: "MacBook Pro",
                                type: "Desktop",
                                status: .active,
                                cpu: 87,
                                gpu: 92
                            )
                            DeviceRow(
                                name: "iPhone 15 Pro",
                                type: "Mobile",
                                status: .active,
                                cpu: 23,
                                gpu: nil
                            )
                            DeviceRow(
                                name: "Apple Watch",
                                type: "Watch",
                                status: .idle,
                                cpu: 5,
                                gpu: nil
                            )
                            DeviceRow(
                                name: "Home Server",
                                type: "Server",
                                status: .active,
                                cpu: 95,
                                gpu: 88
                            )
                        }
                        .background(DesignSystem.Colors.surface)
                        .cornerRadius(8)
                        .overlay(
                            RoundedRectangle(cornerRadius: 8)
                                .stroke(DesignSystem.Colors.border, lineWidth: 1)
                        )
                        .padding(.horizontal, DesignSystem.Spacing.md)
                    }
                }
                .padding(.vertical, DesignSystem.Spacing.md)
            }
            .background(DesignSystem.Colors.background)
            .navigationTitle("Compute Mesh")
            .navigationBarTitleDisplayMode(.large)
        }
    }
}

struct MeshStatusCard: View {
    var body: some View {
        VStack(alignment: .leading, spacing: DesignSystem.Spacing.md) {
            HStack {
                Text("Mesh Status")
                    .font(DesignSystem.Typography.headline)
                    .foregroundColor(DesignSystem.Colors.text)
                
                Spacer()
                
                HStack(spacing: DesignSystem.Spacing.xs) {
                    Circle()
                        .fill(DesignSystem.Colors.success)
                        .frame(width: 8, height: 8)
                    Text("Healthy")
                        .font(DesignSystem.Typography.caption)
                        .foregroundColor(DesignSystem.Colors.success)
                }
            }
            
            HStack(spacing: DesignSystem.Spacing.lg) {
                VStack(alignment: .leading) {
                    Text("Devices")
                        .font(DesignSystem.Typography.caption)
                        .foregroundColor(DesignSystem.Colors.textSecondary)
                    Text("4")
                        .font(DesignSystem.Typography.title)
                        .foregroundColor(DesignSystem.Colors.text)
                }
                
                VStack(alignment: .leading) {
                    Text("Total Cores")
                        .font(DesignSystem.Typography.caption)
                        .foregroundColor(DesignSystem.Colors.textSecondary)
                    Text("48")
                        .font(DesignSystem.Typography.title)
                        .foregroundColor(DesignSystem.Colors.text)
                }
                
                VStack(alignment: .leading) {
                    Text("GPU Memory")
                        .font(DesignSystem.Typography.caption)
                        .foregroundColor(DesignSystem.Colors.textSecondary)
                    Text("32GB")
                        .font(DesignSystem.Typography.title)
                        .foregroundColor(DesignSystem.Colors.text)
                }
            }
            
            Divider()
            
            HStack {
                VStack(alignment: .leading) {
                    Text("Queue Depth")
                        .font(DesignSystem.Typography.caption)
                        .foregroundColor(DesignSystem.Colors.textSecondary)
                    Text("1,247 items")
                        .font(DesignSystem.Typography.mono)
                        .foregroundColor(DesignSystem.Colors.text)
                }
                
                Spacer()
                
                VStack(alignment: .trailing) {
                    Text("Throughput")
                        .font(DesignSystem.Typography.caption)
                        .foregroundColor(DesignSystem.Colors.textSecondary)
                    Text("3.2K/min")
                        .font(DesignSystem.Typography.mono)
                        .foregroundColor(DesignSystem.Colors.text)
                }
            }
        }
        .padding(DesignSystem.Spacing.md)
        .background(DesignSystem.Colors.surface)
        .cornerRadius(8)
        .overlay(
            RoundedRectangle(cornerRadius: 8)
                .stroke(DesignSystem.Colors.border, lineWidth: 1)
        )
    }
}

enum DeviceStatus {
    case active, idle, offline
    
    var color: Color {
        switch self {
        case .active: return DesignSystem.Colors.success
        case .idle: return DesignSystem.Colors.warning
        case .offline: return DesignSystem.Colors.textTertiary
        }
    }
    
    var label: String {
        switch self {
        case .active: return "Active"
        case .idle: return "Idle"
        case .offline: return "Offline"
        }
    }
}

struct DeviceRow: View {
    let name: String
    let type: String
    let status: DeviceStatus
    let cpu: Int
    let gpu: Int?
    
    var typeIcon: String {
        switch type {
        case "Desktop": return "laptopcomputer"
        case "Mobile": return "iphone"
        case "Watch": return "applewatch"
        case "Server": return "server.rack"
        default: return "desktopcomputer"
        }
    }
    
    var body: some View {
        HStack(spacing: DesignSystem.Spacing.md) {
            Image(systemName: typeIcon)
                .font(.system(size: 20))
                .foregroundColor(DesignSystem.Colors.text)
                .frame(width: 32)
            
            VStack(alignment: .leading, spacing: 2) {
                Text(name)
                    .font(DesignSystem.Typography.body)
                    .foregroundColor(DesignSystem.Colors.text)
                
                HStack(spacing: DesignSystem.Spacing.sm) {
                    Circle()
                        .fill(status.color)
                        .frame(width: 6, height: 6)
                    
                    Text(status.label)
                        .font(DesignSystem.Typography.caption)
                        .foregroundColor(DesignSystem.Colors.textSecondary)
                }
            }
            
            Spacer()
            
            VStack(alignment: .trailing, spacing: 2) {
                Text("CPU \(cpu)%")
                    .font(DesignSystem.Typography.mono)
                    .foregroundColor(DesignSystem.Colors.text)
                
                if let gpu = gpu {
                    Text("GPU \(gpu)%")
                        .font(DesignSystem.Typography.mono)
                        .foregroundColor(DesignSystem.Colors.accent)
                }
            }
        }
        .padding(DesignSystem.Spacing.md)
        .background(DesignSystem.Colors.surface)
    }
}

// MARK: - Preview

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
