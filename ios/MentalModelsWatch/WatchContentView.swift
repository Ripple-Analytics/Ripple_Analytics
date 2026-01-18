// Mental Models Apple Watch App
// Design: M&S + Costco - Clean, dense, monochrome + red accents
// Features: Complications, quick insights, haptic alerts, voice queries

import SwiftUI
import WatchKit

// MARK: - Design System (Watch)

struct WatchDesign {
    struct Colors {
        static let background = Color.black
        static let surface = Color(white: 0.12)
        static let text = Color.white
        static let textSecondary = Color(white: 0.7)
        static let textTertiary = Color(white: 0.5)
        static let accent = Color(red: 0.8, green: 0, blue: 0)  // Costco red
        static let success = Color.green
        static let warning = Color.orange
    }
    
    struct Typography {
        static let title = Font.system(size: 18, weight: .semibold)
        static let headline = Font.system(size: 15, weight: .semibold)
        static let body = Font.system(size: 13, weight: .regular)
        static let caption = Font.system(size: 11, weight: .regular)
        static let mono = Font.system(size: 12, weight: .medium, design: .monospaced)
    }
}

// MARK: - Main Watch View

struct WatchContentView: View {
    @State private var selectedTab = 0
    
    var body: some View {
        TabView(selection: $selectedTab) {
            DashboardWatchView()
                .tag(0)
            
            AlertsWatchView()
                .tag(1)
            
            QuickAnalysisView()
                .tag(2)
            
            MeshStatusWatchView()
                .tag(3)
        }
        .tabViewStyle(.page)
    }
}

// MARK: - Dashboard Watch View

struct DashboardWatchView: View {
    var body: some View {
        ScrollView {
            VStack(spacing: 8) {
                // Header
                HStack {
                    Text("Mental Models")
                        .font(WatchDesign.Typography.headline)
                        .foregroundColor(WatchDesign.Colors.text)
                    
                    Spacer()
                    
                    Circle()
                        .fill(WatchDesign.Colors.success)
                        .frame(width: 8, height: 8)
                }
                
                // Key Metrics Grid
                LazyVGrid(columns: [
                    GridItem(.flexible()),
                    GridItem(.flexible())
                ], spacing: 8) {
                    WatchMetricCard(title: "Queue", value: "1.2K", icon: "list.bullet")
                    WatchMetricCard(title: "CPU", value: "87%", icon: "cpu")
                    WatchMetricCard(title: "Alerts", value: "3", icon: "bell.fill", isAlert: true)
                    WatchMetricCard(title: "Models", value: "129", icon: "brain")
                }
                
                // Throughput
                HStack {
                    VStack(alignment: .leading, spacing: 2) {
                        Text("Throughput")
                            .font(WatchDesign.Typography.caption)
                            .foregroundColor(WatchDesign.Colors.textSecondary)
                        Text("3.2K/min")
                            .font(WatchDesign.Typography.mono)
                            .foregroundColor(WatchDesign.Colors.text)
                    }
                    
                    Spacer()
                    
                    // Mini sparkline
                    SparklineView(data: [0.4, 0.6, 0.5, 0.8, 0.7, 0.9, 0.85])
                        .frame(width: 50, height: 20)
                }
                .padding(8)
                .background(WatchDesign.Colors.surface)
                .cornerRadius(8)
            }
            .padding(.horizontal, 4)
        }
    }
}

struct WatchMetricCard: View {
    let title: String
    let value: String
    let icon: String
    var isAlert: Bool = false
    
    var body: some View {
        VStack(spacing: 4) {
            Image(systemName: icon)
                .font(.system(size: 14))
                .foregroundColor(isAlert ? WatchDesign.Colors.accent : WatchDesign.Colors.textSecondary)
            
            Text(value)
                .font(WatchDesign.Typography.headline)
                .foregroundColor(WatchDesign.Colors.text)
            
            Text(title)
                .font(WatchDesign.Typography.caption)
                .foregroundColor(WatchDesign.Colors.textSecondary)
        }
        .frame(maxWidth: .infinity)
        .padding(8)
        .background(WatchDesign.Colors.surface)
        .cornerRadius(8)
    }
}

struct SparklineView: View {
    let data: [Double]
    
    var body: some View {
        GeometryReader { geometry in
            Path { path in
                guard data.count > 1 else { return }
                
                let stepX = geometry.size.width / CGFloat(data.count - 1)
                let maxY = data.max() ?? 1
                let minY = data.min() ?? 0
                let rangeY = maxY - minY
                
                for (index, value) in data.enumerated() {
                    let x = CGFloat(index) * stepX
                    let y = geometry.size.height - (CGFloat((value - minY) / rangeY) * geometry.size.height)
                    
                    if index == 0 {
                        path.move(to: CGPoint(x: x, y: y))
                    } else {
                        path.addLine(to: CGPoint(x: x, y: y))
                    }
                }
            }
            .stroke(WatchDesign.Colors.accent, lineWidth: 1.5)
        }
    }
}

// MARK: - Alerts Watch View

struct AlertsWatchView: View {
    var body: some View {
        ScrollView {
            VStack(spacing: 8) {
                Text("Alerts")
                    .font(WatchDesign.Typography.headline)
                    .foregroundColor(WatchDesign.Colors.text)
                    .frame(maxWidth: .infinity, alignment: .leading)
                
                WatchAlertRow(
                    type: .lollapalooza,
                    title: "Lollapalooza",
                    description: "5 models converging",
                    time: "Now"
                )
                
                WatchAlertRow(
                    type: .risk,
                    title: "High Risk",
                    description: "Confirmation bias",
                    time: "10m"
                )
                
                WatchAlertRow(
                    type: .insight,
                    title: "New Pattern",
                    description: "Tech + Psychology",
                    time: "1h"
                )
            }
            .padding(.horizontal, 4)
        }
    }
}

enum WatchAlertType {
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
        case .lollapalooza: return WatchDesign.Colors.accent
        case .risk: return WatchDesign.Colors.warning
        case .insight: return WatchDesign.Colors.success
        }
    }
}

struct WatchAlertRow: View {
    let type: WatchAlertType
    let title: String
    let description: String
    let time: String
    
    var body: some View {
        HStack(spacing: 8) {
            Image(systemName: type.icon)
                .font(.system(size: 14))
                .foregroundColor(type.color)
            
            VStack(alignment: .leading, spacing: 2) {
                Text(title)
                    .font(WatchDesign.Typography.body)
                    .foregroundColor(WatchDesign.Colors.text)
                
                Text(description)
                    .font(WatchDesign.Typography.caption)
                    .foregroundColor(WatchDesign.Colors.textSecondary)
                    .lineLimit(1)
            }
            
            Spacer()
            
            Text(time)
                .font(WatchDesign.Typography.caption)
                .foregroundColor(WatchDesign.Colors.textTertiary)
        }
        .padding(8)
        .background(WatchDesign.Colors.surface)
        .cornerRadius(8)
    }
}

// MARK: - Quick Analysis View

struct QuickAnalysisView: View {
    @State private var isListening = false
    
    var body: some View {
        VStack(spacing: 12) {
            Text("Quick Analysis")
                .font(WatchDesign.Typography.headline)
                .foregroundColor(WatchDesign.Colors.text)
            
            // Voice Query Button
            Button(action: {
                withAnimation {
                    isListening.toggle()
                }
                if isListening {
                    WKInterfaceDevice.current().play(.start)
                }
            }) {
                VStack(spacing: 8) {
                    Image(systemName: isListening ? "waveform" : "mic.fill")
                        .font(.system(size: 28))
                        .foregroundColor(isListening ? WatchDesign.Colors.accent : WatchDesign.Colors.text)
                    
                    Text(isListening ? "Listening..." : "Tap to Ask")
                        .font(WatchDesign.Typography.caption)
                        .foregroundColor(WatchDesign.Colors.textSecondary)
                }
                .frame(maxWidth: .infinity)
                .padding(16)
                .background(WatchDesign.Colors.surface)
                .cornerRadius(12)
            }
            .buttonStyle(.plain)
            
            // Quick Actions
            HStack(spacing: 8) {
                QuickActionWatchButton(icon: "newspaper", label: "News")
                QuickActionWatchButton(icon: "chart.bar", label: "Stats")
            }
        }
        .padding(.horizontal, 4)
    }
}

struct QuickActionWatchButton: View {
    let icon: String
    let label: String
    
    var body: some View {
        Button(action: {
            WKInterfaceDevice.current().play(.click)
        }) {
            VStack(spacing: 4) {
                Image(systemName: icon)
                    .font(.system(size: 16))
                    .foregroundColor(WatchDesign.Colors.accent)
                
                Text(label)
                    .font(WatchDesign.Typography.caption)
                    .foregroundColor(WatchDesign.Colors.textSecondary)
            }
            .frame(maxWidth: .infinity)
            .padding(8)
            .background(WatchDesign.Colors.surface)
            .cornerRadius(8)
        }
        .buttonStyle(.plain)
    }
}

// MARK: - Mesh Status Watch View

struct MeshStatusWatchView: View {
    var body: some View {
        ScrollView {
            VStack(spacing: 8) {
                HStack {
                    Text("Compute Mesh")
                        .font(WatchDesign.Typography.headline)
                        .foregroundColor(WatchDesign.Colors.text)
                    
                    Spacer()
                    
                    HStack(spacing: 4) {
                        Circle()
                            .fill(WatchDesign.Colors.success)
                            .frame(width: 6, height: 6)
                        Text("4")
                            .font(WatchDesign.Typography.caption)
                            .foregroundColor(WatchDesign.Colors.textSecondary)
                    }
                }
                
                // Device List
                DeviceWatchRow(name: "MacBook", cpu: 87, gpu: 92)
                DeviceWatchRow(name: "iPhone", cpu: 23, gpu: nil)
                DeviceWatchRow(name: "Watch", cpu: 5, gpu: nil, isSelf: true)
                DeviceWatchRow(name: "Server", cpu: 95, gpu: 88)
                
                // Total Stats
                HStack {
                    VStack(alignment: .leading) {
                        Text("Cores")
                            .font(WatchDesign.Typography.caption)
                            .foregroundColor(WatchDesign.Colors.textSecondary)
                        Text("48")
                            .font(WatchDesign.Typography.mono)
                            .foregroundColor(WatchDesign.Colors.text)
                    }
                    
                    Spacer()
                    
                    VStack(alignment: .trailing) {
                        Text("GPU Mem")
                            .font(WatchDesign.Typography.caption)
                            .foregroundColor(WatchDesign.Colors.textSecondary)
                        Text("32GB")
                            .font(WatchDesign.Typography.mono)
                            .foregroundColor(WatchDesign.Colors.text)
                    }
                }
                .padding(8)
                .background(WatchDesign.Colors.surface)
                .cornerRadius(8)
            }
            .padding(.horizontal, 4)
        }
    }
}

struct DeviceWatchRow: View {
    let name: String
    let cpu: Int
    let gpu: Int?
    var isSelf: Bool = false
    
    var body: some View {
        HStack {
            VStack(alignment: .leading, spacing: 2) {
                HStack(spacing: 4) {
                    Text(name)
                        .font(WatchDesign.Typography.body)
                        .foregroundColor(WatchDesign.Colors.text)
                    
                    if isSelf {
                        Text("(this)")
                            .font(WatchDesign.Typography.caption)
                            .foregroundColor(WatchDesign.Colors.accent)
                    }
                }
                
                // CPU Bar
                GeometryReader { geometry in
                    ZStack(alignment: .leading) {
                        Rectangle()
                            .fill(WatchDesign.Colors.surface)
                            .frame(height: 3)
                        
                        Rectangle()
                            .fill(WatchDesign.Colors.text)
                            .frame(width: geometry.size.width * CGFloat(cpu) / 100, height: 3)
                    }
                    .cornerRadius(1.5)
                }
                .frame(height: 3)
            }
            
            Spacer()
            
            VStack(alignment: .trailing, spacing: 2) {
                Text("\(cpu)%")
                    .font(WatchDesign.Typography.mono)
                    .foregroundColor(WatchDesign.Colors.text)
                
                if let gpu = gpu {
                    Text("G:\(gpu)%")
                        .font(WatchDesign.Typography.caption)
                        .foregroundColor(WatchDesign.Colors.accent)
                }
            }
        }
        .padding(8)
        .background(WatchDesign.Colors.surface)
        .cornerRadius(8)
    }
}

// MARK: - Complications

struct ComplicationViews {
    // Circular complication
    struct CircularView: View {
        var body: some View {
            ZStack {
                Circle()
                    .stroke(WatchDesign.Colors.accent, lineWidth: 2)
                
                VStack(spacing: 0) {
                    Text("87")
                        .font(.system(size: 16, weight: .bold, design: .rounded))
                        .foregroundColor(WatchDesign.Colors.text)
                    
                    Text("%")
                        .font(.system(size: 8))
                        .foregroundColor(WatchDesign.Colors.textSecondary)
                }
            }
        }
    }
    
    // Rectangular complication
    struct RectangularView: View {
        var body: some View {
            HStack(spacing: 8) {
                VStack(alignment: .leading, spacing: 2) {
                    Text("Mental Models")
                        .font(.system(size: 12, weight: .semibold))
                        .foregroundColor(WatchDesign.Colors.text)
                    
                    Text("Queue: 1,247 • CPU: 87%")
                        .font(.system(size: 10))
                        .foregroundColor(WatchDesign.Colors.textSecondary)
                }
                
                Spacer()
                
                Image(systemName: "brain")
                    .font(.system(size: 16))
                    .foregroundColor(WatchDesign.Colors.accent)
            }
            .padding(8)
            .background(WatchDesign.Colors.surface)
            .cornerRadius(8)
        }
    }
    
    // Corner complication
    struct CornerView: View {
        var body: some View {
            VStack(spacing: 0) {
                Image(systemName: "brain")
                    .font(.system(size: 20))
                    .foregroundColor(WatchDesign.Colors.accent)
                
                Text("3")
                    .font(.system(size: 12, weight: .bold))
                    .foregroundColor(WatchDesign.Colors.text)
            }
        }
    }
}

// MARK: - Haptic Feedback Manager

class HapticManager {
    static let shared = HapticManager()
    
    func playLollapaloozaAlert() {
        // Strong haptic for Lollapalooza detection
        WKInterfaceDevice.current().play(.notification)
        
        // Follow up with success pattern
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.3) {
            WKInterfaceDevice.current().play(.success)
        }
    }
    
    func playRiskAlert() {
        // Warning haptic for high risk
        WKInterfaceDevice.current().play(.notification)
        WKInterfaceDevice.current().play(.retry)
    }
    
    func playInsightAlert() {
        // Gentle haptic for new insight
        WKInterfaceDevice.current().play(.click)
    }
    
    func playConfirmation() {
        WKInterfaceDevice.current().play(.success)
    }
}

// MARK: - Preview

struct WatchContentView_Previews: PreviewProvider {
    static var previews: some View {
        WatchContentView()
    }
}
