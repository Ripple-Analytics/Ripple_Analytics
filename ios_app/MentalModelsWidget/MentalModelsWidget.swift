import WidgetKit
import SwiftUI

// MARK: - Widget Design System (M&S + Costco: Monochrome + Red)
struct WidgetDesign {
    static let accent = Color(red: 0.8, green: 0.1, blue: 0.1)
    static let gray900 = Color(white: 0.1)
    static let gray600 = Color(white: 0.3)
    static let gray500 = Color(white: 0.4)
    static let gray200 = Color(white: 0.8)
    static let white = Color.white
    
    static let metricFont = Font.system(size: 24, weight: .bold, design: .monospaced)
    static let smallMetricFont = Font.system(size: 18, weight: .bold, design: .monospaced)
    static let labelFont = Font.system(size: 9, weight: .medium, design: .default)
    static let bodyFont = Font.system(size: 11, weight: .regular, design: .default)
}

// MARK: - Timeline Entry
struct MentalModelsEntry: TimelineEntry {
    let date: Date
    let insightsCount: Int
    let modelsCount: Int
    let dataPointsProcessed: Int
    let isLearning: Bool
    let latestInsight: String?
    let lollapaloozaDetected: Bool
}

// MARK: - Timeline Provider
struct MentalModelsProvider: TimelineProvider {
    func placeholder(in context: Context) -> MentalModelsEntry {
        MentalModelsEntry(
            date: Date(),
            insightsCount: 42,
            modelsCount: 129,
            dataPointsProcessed: 10847,
            isLearning: true,
            latestInsight: "Confirmation bias detected",
            lollapaloozaDetected: false
        )
    }
    
    func getSnapshot(in context: Context, completion: @escaping (MentalModelsEntry) -> Void) {
        let entry = MentalModelsEntry(
            date: Date(),
            insightsCount: UserDefaults(suiteName: "group.com.ripple.mentalmodels")?.integer(forKey: "insightsCount") ?? 0,
            modelsCount: 129,
            dataPointsProcessed: UserDefaults(suiteName: "group.com.ripple.mentalmodels")?.integer(forKey: "dataPointsProcessed") ?? 0,
            isLearning: UserDefaults(suiteName: "group.com.ripple.mentalmodels")?.bool(forKey: "isLearning") ?? false,
            latestInsight: UserDefaults(suiteName: "group.com.ripple.mentalmodels")?.string(forKey: "latestInsight"),
            lollapaloozaDetected: UserDefaults(suiteName: "group.com.ripple.mentalmodels")?.bool(forKey: "lollapaloozaDetected") ?? false
        )
        completion(entry)
    }
    
    func getTimeline(in context: Context, completion: @escaping (Timeline<MentalModelsEntry>) -> Void) {
        let defaults = UserDefaults(suiteName: "group.com.ripple.mentalmodels")
        
        let entry = MentalModelsEntry(
            date: Date(),
            insightsCount: defaults?.integer(forKey: "insightsCount") ?? 0,
            modelsCount: 129,
            dataPointsProcessed: defaults?.integer(forKey: "dataPointsProcessed") ?? 0,
            isLearning: defaults?.bool(forKey: "isLearning") ?? false,
            latestInsight: defaults?.string(forKey: "latestInsight"),
            lollapaloozaDetected: defaults?.bool(forKey: "lollapaloozaDetected") ?? false
        )
        
        // Update every 15 minutes
        let nextUpdate = Calendar.current.date(byAdding: .minute, value: 15, to: Date())!
        let timeline = Timeline(entries: [entry], policy: .after(nextUpdate))
        completion(timeline)
    }
}

// MARK: - Small Widget View
struct SmallWidgetView: View {
    let entry: MentalModelsEntry
    
    var body: some View {
        VStack(alignment: .leading, spacing: 4) {
            // Header
            HStack {
                Circle()
                    .fill(entry.isLearning ? WidgetDesign.accent : WidgetDesign.gray500)
                    .frame(width: 6, height: 6)
                
                Text(entry.isLearning ? "LEARNING" : "IDLE")
                    .font(WidgetDesign.labelFont)
                    .foregroundColor(entry.isLearning ? WidgetDesign.accent : WidgetDesign.gray500)
                
                Spacer()
            }
            
            Spacer()
            
            // Main Metric
            Text("\(entry.insightsCount)")
                .font(WidgetDesign.metricFont)
                .foregroundColor(WidgetDesign.gray900)
            
            Text("INSIGHTS")
                .font(WidgetDesign.labelFont)
                .foregroundColor(WidgetDesign.gray500)
            
            Spacer()
            
            // Lollapalooza Alert
            if entry.lollapaloozaDetected {
                HStack(spacing: 4) {
                    Circle()
                        .fill(WidgetDesign.accent)
                        .frame(width: 4, height: 4)
                    
                    Text("LOLLAPALOOZA")
                        .font(WidgetDesign.labelFont)
                        .foregroundColor(WidgetDesign.accent)
                }
            }
        }
        .padding(12)
        .background(WidgetDesign.white)
    }
}

// MARK: - Medium Widget View
struct MediumWidgetView: View {
    let entry: MentalModelsEntry
    
    var body: some View {
        HStack(spacing: 0) {
            // Left: Metrics
            VStack(alignment: .leading, spacing: 8) {
                // Status
                HStack {
                    Circle()
                        .fill(entry.isLearning ? WidgetDesign.accent : WidgetDesign.gray500)
                        .frame(width: 6, height: 6)
                    
                    Text(entry.isLearning ? "LEARNING" : "IDLE")
                        .font(WidgetDesign.labelFont)
                        .foregroundColor(entry.isLearning ? WidgetDesign.accent : WidgetDesign.gray500)
                }
                
                Spacer()
                
                // Insights
                VStack(alignment: .leading, spacing: 2) {
                    Text("\(entry.insightsCount)")
                        .font(WidgetDesign.metricFont)
                        .foregroundColor(WidgetDesign.gray900)
                    
                    Text("INSIGHTS")
                        .font(WidgetDesign.labelFont)
                        .foregroundColor(WidgetDesign.gray500)
                }
                
                Spacer()
                
                // Data Points
                VStack(alignment: .leading, spacing: 2) {
                    Text(formatCompact(entry.dataPointsProcessed))
                        .font(WidgetDesign.smallMetricFont)
                        .foregroundColor(WidgetDesign.gray600)
                    
                    Text("DATA POINTS")
                        .font(WidgetDesign.labelFont)
                        .foregroundColor(WidgetDesign.gray500)
                }
            }
            .padding(12)
            .frame(maxWidth: .infinity, alignment: .leading)
            
            // Divider
            Rectangle()
                .fill(WidgetDesign.gray200)
                .frame(width: 1)
            
            // Right: Latest Insight
            VStack(alignment: .leading, spacing: 8) {
                Text("LATEST")
                    .font(WidgetDesign.labelFont)
                    .foregroundColor(WidgetDesign.gray500)
                
                if let insight = entry.latestInsight {
                    Text(insight)
                        .font(WidgetDesign.bodyFont)
                        .foregroundColor(WidgetDesign.gray900)
                        .lineLimit(3)
                } else {
                    Text("No insights yet")
                        .font(WidgetDesign.bodyFont)
                        .foregroundColor(WidgetDesign.gray500)
                }
                
                Spacer()
                
                if entry.lollapaloozaDetected {
                    HStack(spacing: 4) {
                        Rectangle()
                            .fill(WidgetDesign.accent)
                            .frame(width: 2)
                        
                        Text("Lollapalooza Effect")
                            .font(WidgetDesign.labelFont)
                            .foregroundColor(WidgetDesign.accent)
                    }
                }
                
                Text("129 MODELS")
                    .font(WidgetDesign.labelFont)
                    .foregroundColor(WidgetDesign.gray500)
            }
            .padding(12)
            .frame(maxWidth: .infinity, alignment: .leading)
        }
        .background(WidgetDesign.white)
    }
    
    private func formatCompact(_ num: Int) -> String {
        if num >= 1000000 { return String(format: "%.1fM", Double(num) / 1000000) }
        else if num >= 1000 { return String(format: "%.1fK", Double(num) / 1000) }
        return "\(num)"
    }
}

// MARK: - Large Widget View
struct LargeWidgetView: View {
    let entry: MentalModelsEntry
    
    var body: some View {
        VStack(spacing: 0) {
            // Header
            HStack {
                Text("MENTAL MODELS")
                    .font(Font.system(size: 11, weight: .semibold, design: .default))
                    .foregroundColor(WidgetDesign.gray900)
                
                Spacer()
                
                HStack(spacing: 4) {
                    Circle()
                        .fill(entry.isLearning ? WidgetDesign.accent : WidgetDesign.gray500)
                        .frame(width: 6, height: 6)
                    
                    Text(entry.isLearning ? "LEARNING" : "IDLE")
                        .font(WidgetDesign.labelFont)
                        .foregroundColor(entry.isLearning ? WidgetDesign.accent : WidgetDesign.gray500)
                }
            }
            .padding(.horizontal, 16)
            .padding(.vertical, 12)
            .background(WidgetDesign.gray200.opacity(0.5))
            
            // Metrics Grid
            HStack(spacing: 1) {
                MetricBox(label: "MODELS", value: "\(entry.modelsCount)", isHighlighted: false)
                MetricBox(label: "INSIGHTS", value: "\(entry.insightsCount)", isHighlighted: true)
                MetricBox(label: "DATA PTS", value: formatCompact(entry.dataPointsProcessed), isHighlighted: false)
            }
            .background(WidgetDesign.gray200)
            
            // Latest Insight
            VStack(alignment: .leading, spacing: 8) {
                Text("LATEST INSIGHT")
                    .font(WidgetDesign.labelFont)
                    .foregroundColor(WidgetDesign.gray500)
                
                if let insight = entry.latestInsight {
                    HStack(spacing: 8) {
                        if entry.lollapaloozaDetected {
                            Rectangle()
                                .fill(WidgetDesign.accent)
                                .frame(width: 3)
                        }
                        
                        Text(insight)
                            .font(Font.system(size: 13, weight: .regular, design: .default))
                            .foregroundColor(WidgetDesign.gray900)
                            .lineLimit(2)
                    }
                } else {
                    Text("Analyzing data...")
                        .font(Font.system(size: 13, weight: .regular, design: .default))
                        .foregroundColor(WidgetDesign.gray500)
                }
            }
            .padding(16)
            .frame(maxWidth: .infinity, alignment: .leading)
            
            Spacer()
            
            // Quick Actions
            HStack(spacing: 0) {
                WidgetAction(icon: "magnifyingglass", label: "Analyze")
                Divider().frame(height: 30).background(WidgetDesign.gray200)
                WidgetAction(icon: "doc.viewfinder", label: "Scan")
                Divider().frame(height: 30).background(WidgetDesign.gray200)
                WidgetAction(icon: "mic", label: "Voice")
            }
            .padding(.vertical, 12)
            .background(WidgetDesign.gray200.opacity(0.3))
        }
        .background(WidgetDesign.white)
    }
    
    private func formatCompact(_ num: Int) -> String {
        if num >= 1000000 { return String(format: "%.1fM", Double(num) / 1000000) }
        else if num >= 1000 { return String(format: "%.1fK", Double(num) / 1000) }
        return "\(num)"
    }
}

struct MetricBox: View {
    let label: String
    let value: String
    let isHighlighted: Bool
    
    var body: some View {
        VStack(spacing: 4) {
            Text(value)
                .font(WidgetDesign.smallMetricFont)
                .foregroundColor(isHighlighted ? WidgetDesign.accent : WidgetDesign.gray900)
            
            Text(label)
                .font(WidgetDesign.labelFont)
                .foregroundColor(WidgetDesign.gray500)
        }
        .frame(maxWidth: .infinity)
        .padding(.vertical, 12)
        .background(WidgetDesign.white)
    }
}

struct WidgetAction: View {
    let icon: String
    let label: String
    
    var body: some View {
        Link(destination: URL(string: "mentalmodels://action/\(label.lowercased())")!) {
            VStack(spacing: 4) {
                Image(systemName: icon)
                    .font(.system(size: 16, weight: .regular))
                    .foregroundColor(WidgetDesign.gray600)
                
                Text(label.uppercased())
                    .font(WidgetDesign.labelFont)
                    .foregroundColor(WidgetDesign.gray500)
            }
            .frame(maxWidth: .infinity)
        }
    }
}

// MARK: - Widget Configuration
@main
struct MentalModelsWidget: Widget {
    let kind: String = "MentalModelsWidget"
    
    var body: some WidgetConfiguration {
        StaticConfiguration(kind: kind, provider: MentalModelsProvider()) { entry in
            MentalModelsWidgetEntryView(entry: entry)
        }
        .configurationDisplayName("Mental Models")
        .description("Quick insights and learning status")
        .supportedFamilies([.systemSmall, .systemMedium, .systemLarge])
    }
}

struct MentalModelsWidgetEntryView: View {
    @Environment(\.widgetFamily) var family
    let entry: MentalModelsEntry
    
    var body: some View {
        switch family {
        case .systemSmall:
            SmallWidgetView(entry: entry)
        case .systemMedium:
            MediumWidgetView(entry: entry)
        case .systemLarge:
            LargeWidgetView(entry: entry)
        default:
            SmallWidgetView(entry: entry)
        }
    }
}

// MARK: - Preview
struct MentalModelsWidget_Previews: PreviewProvider {
    static var previews: some View {
        Group {
            SmallWidgetView(entry: MentalModelsEntry(
                date: Date(),
                insightsCount: 42,
                modelsCount: 129,
                dataPointsProcessed: 10847,
                isLearning: true,
                latestInsight: "Confirmation bias detected in recent analysis",
                lollapaloozaDetected: true
            ))
            .previewContext(WidgetPreviewContext(family: .systemSmall))
            
            MediumWidgetView(entry: MentalModelsEntry(
                date: Date(),
                insightsCount: 42,
                modelsCount: 129,
                dataPointsProcessed: 10847,
                isLearning: true,
                latestInsight: "Confirmation bias detected in recent analysis",
                lollapaloozaDetected: true
            ))
            .previewContext(WidgetPreviewContext(family: .systemMedium))
            
            LargeWidgetView(entry: MentalModelsEntry(
                date: Date(),
                insightsCount: 42,
                modelsCount: 129,
                dataPointsProcessed: 10847,
                isLearning: true,
                latestInsight: "Confirmation bias detected in recent analysis",
                lollapaloozaDetected: true
            ))
            .previewContext(WidgetPreviewContext(family: .systemLarge))
        }
    }
}
