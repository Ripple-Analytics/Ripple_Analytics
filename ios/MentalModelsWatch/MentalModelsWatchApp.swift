// MentalModelsWatchApp.swift
// Mental Models System - watchOS App
// Quick access to models and decision logging on the wrist

import SwiftUI
import WatchKit

@main
struct MentalModelsWatchApp: App {
    @StateObject private var appState = WatchAppState()
    
    var body: some Scene {
        WindowGroup {
            NavigationStack {
                MainWatchView()
                    .environmentObject(appState)
            }
        }
    }
}

// MARK: - App State

class WatchAppState: ObservableObject {
    @Published var recentModels: [QuickModel] = [
        QuickModel(id: "1", name: "Inversion", emoji: "🔄"),
        QuickModel(id: "2", name: "Second-Order", emoji: "2️⃣"),
        QuickModel(id: "3", name: "Circle of Competence", emoji: "⭕"),
        QuickModel(id: "4", name: "Margin of Safety", emoji: "🛡️"),
        QuickModel(id: "5", name: "Sunk Cost", emoji: "💸")
    ]
    
    @Published var todayStats = WatchStats(
        modelsUsed: 3,
        decisionsLogged: 2,
        streakDays: 14
    )
    
    @Published var complications: [ComplicationData] = []
}

struct QuickModel: Identifiable {
    let id: String
    let name: String
    let emoji: String
}

struct WatchStats {
    let modelsUsed: Int
    let decisionsLogged: Int
    let streakDays: Int
}

struct ComplicationData: Identifiable {
    let id: String
    let title: String
    let value: String
}

// MARK: - Main Watch View

struct MainWatchView: View {
    @EnvironmentObject var appState: WatchAppState
    
    var body: some View {
        List {
            // Today's Stats
            Section {
                HStack {
                    StatBubble(value: "\(appState.todayStats.modelsUsed)", label: "Models", color: .red)
                    StatBubble(value: "\(appState.todayStats.decisionsLogged)", label: "Decisions", color: .blue)
                    StatBubble(value: "\(appState.todayStats.streakDays)", label: "Streak", color: .orange)
                }
                .listRowBackground(Color.clear)
            }
            
            // Quick Actions
            Section("Quick Actions") {
                NavigationLink {
                    QuickDecisionView()
                } label: {
                    Label("Log Decision", systemImage: "plus.circle.fill")
                        .foregroundColor(.red)
                }
                
                NavigationLink {
                    ModelBrowserView()
                } label: {
                    Label("Browse Models", systemImage: "brain")
                }
                
                NavigationLink {
                    AnalysisPromptView()
                } label: {
                    Label("Quick Analysis", systemImage: "waveform")
                }
            }
            
            // Recent Models
            Section("Recent Models") {
                ForEach(appState.recentModels) { model in
                    NavigationLink {
                        ModelQuickView(model: model)
                    } label: {
                        HStack {
                            Text(model.emoji)
                            Text(model.name)
                                .font(.caption)
                        }
                    }
                }
            }
        }
        .navigationTitle("Mental Models")
    }
}

struct StatBubble: View {
    let value: String
    let label: String
    let color: Color
    
    var body: some View {
        VStack(spacing: 2) {
            Text(value)
                .font(.title3)
                .fontWeight(.bold)
                .foregroundColor(color)
            
            Text(label)
                .font(.system(size: 9))
                .foregroundColor(.secondary)
        }
        .frame(maxWidth: .infinity)
    }
}

// MARK: - Quick Decision View

struct QuickDecisionView: View {
    @Environment(\.dismiss) var dismiss
    @State private var selectedModels: Set<String> = []
    @State private var note = ""
    
    let quickModels = [
        ("Inversion", "🔄"),
        ("Second-Order", "2️⃣"),
        ("Circle of Competence", "⭕"),
        ("Margin of Safety", "🛡️"),
        ("Opportunity Cost", "⚖️")
    ]
    
    var body: some View {
        ScrollView {
            VStack(spacing: 12) {
                Text("Select Models Used")
                    .font(.caption)
                    .foregroundColor(.secondary)
                
                // Model Selection Grid
                LazyVGrid(columns: [GridItem(.flexible()), GridItem(.flexible())], spacing: 8) {
                    ForEach(quickModels, id: \.0) { model in
                        Button {
                            if selectedModels.contains(model.0) {
                                selectedModels.remove(model.0)
                            } else {
                                selectedModels.insert(model.0)
                            }
                        } label: {
                            VStack {
                                Text(model.1)
                                    .font(.title3)
                                Text(model.0)
                                    .font(.system(size: 9))
                                    .lineLimit(1)
                            }
                            .frame(maxWidth: .infinity)
                            .padding(.vertical, 8)
                            .background(selectedModels.contains(model.0) ? Color.red.opacity(0.3) : Color.gray.opacity(0.2))
                            .cornerRadius(8)
                        }
                        .buttonStyle(.plain)
                    }
                }
                
                // Save Button
                Button {
                    // Save decision
                    WKInterfaceDevice.current().play(.success)
                    dismiss()
                } label: {
                    Text("Log Decision")
                        .frame(maxWidth: .infinity)
                }
                .buttonStyle(.borderedProminent)
                .tint(.red)
                .disabled(selectedModels.isEmpty)
            }
            .padding()
        }
        .navigationTitle("Log Decision")
    }
}

// MARK: - Model Browser View

struct ModelBrowserView: View {
    let categories = [
        ("Psychology", "🧠", 24),
        ("Economics", "📈", 18),
        ("Biology", "🧬", 12),
        ("Physics", "⚛️", 15),
        ("Systems", "🔗", 20),
        ("Math", "📐", 10)
    ]
    
    var body: some View {
        List {
            ForEach(categories, id: \.0) { category in
                NavigationLink {
                    CategoryModelsView(category: category.0)
                } label: {
                    HStack {
                        Text(category.1)
                        VStack(alignment: .leading) {
                            Text(category.0)
                                .font(.caption)
                            Text("\(category.2) models")
                                .font(.system(size: 10))
                                .foregroundColor(.secondary)
                        }
                    }
                }
            }
        }
        .navigationTitle("Categories")
    }
}

struct CategoryModelsView: View {
    let category: String
    
    let sampleModels = [
        "Confirmation Bias", "Sunk Cost", "Anchoring",
        "Availability Heuristic", "Loss Aversion"
    ]
    
    var body: some View {
        List {
            ForEach(sampleModels, id: \.self) { model in
                NavigationLink {
                    ModelDetailWatchView(modelName: model)
                } label: {
                    Text(model)
                        .font(.caption)
                }
            }
        }
        .navigationTitle(category)
    }
}

// MARK: - Model Quick View

struct ModelQuickView: View {
    let model: QuickModel
    
    var body: some View {
        ScrollView {
            VStack(spacing: 12) {
                Text(model.emoji)
                    .font(.largeTitle)
                
                Text(model.name)
                    .font(.headline)
                
                Text("A mental model for better decision making by considering the boundaries of your knowledge.")
                    .font(.caption)
                    .foregroundColor(.secondary)
                    .multilineTextAlignment(.center)
                
                Divider()
                
                VStack(alignment: .leading, spacing: 8) {
                    Text("Key Points")
                        .font(.caption)
                        .fontWeight(.semibold)
                    
                    BulletPoint(text: "Know what you know")
                    BulletPoint(text: "Stay within boundaries")
                    BulletPoint(text: "Expand deliberately")
                }
                
                Button {
                    // Mark as used
                    WKInterfaceDevice.current().play(.click)
                } label: {
                    Label("Use Now", systemImage: "checkmark.circle")
                        .frame(maxWidth: .infinity)
                }
                .buttonStyle(.borderedProminent)
                .tint(.red)
            }
            .padding()
        }
        .navigationTitle(model.name)
    }
}

struct BulletPoint: View {
    let text: String
    
    var body: some View {
        HStack(alignment: .top, spacing: 4) {
            Text("•")
                .foregroundColor(.red)
            Text(text)
                .font(.system(size: 11))
        }
    }
}

// MARK: - Model Detail Watch View

struct ModelDetailWatchView: View {
    let modelName: String
    
    var body: some View {
        ScrollView {
            VStack(spacing: 12) {
                Text(modelName)
                    .font(.headline)
                    .multilineTextAlignment(.center)
                
                Text("Description of the mental model and how to apply it in decision making.")
                    .font(.caption)
                    .foregroundColor(.secondary)
                    .multilineTextAlignment(.center)
                
                HStack {
                    VStack {
                        Text("47")
                            .font(.title3)
                            .fontWeight(.bold)
                        Text("Uses")
                            .font(.system(size: 9))
                            .foregroundColor(.secondary)
                    }
                    
                    Divider()
                        .frame(height: 30)
                    
                    VStack {
                        Text("85%")
                            .font(.title3)
                            .fontWeight(.bold)
                            .foregroundColor(.green)
                        Text("Effective")
                            .font(.system(size: 9))
                            .foregroundColor(.secondary)
                    }
                }
                .padding()
                .background(Color.gray.opacity(0.2))
                .cornerRadius(8)
                
                Button {
                    WKInterfaceDevice.current().play(.click)
                } label: {
                    Label("Apply", systemImage: "checkmark")
                        .frame(maxWidth: .infinity)
                }
                .buttonStyle(.borderedProminent)
                .tint(.red)
            }
            .padding()
        }
    }
}

// MARK: - Analysis Prompt View

struct AnalysisPromptView: View {
    @State private var isRecording = false
    @State private var transcribedText = ""
    
    var body: some View {
        ScrollView {
            VStack(spacing: 16) {
                Text("Describe your situation")
                    .font(.caption)
                    .foregroundColor(.secondary)
                
                // Voice Input Button
                Button {
                    isRecording.toggle()
                    if isRecording {
                        WKInterfaceDevice.current().play(.start)
                    } else {
                        WKInterfaceDevice.current().play(.stop)
                        // Simulate transcription
                        transcribedText = "Considering a new investment opportunity..."
                    }
                } label: {
                    VStack {
                        Image(systemName: isRecording ? "stop.circle.fill" : "mic.circle.fill")
                            .font(.largeTitle)
                            .foregroundColor(isRecording ? .red : .blue)
                        
                        Text(isRecording ? "Tap to Stop" : "Tap to Speak")
                            .font(.caption)
                    }
                }
                .buttonStyle(.plain)
                
                if !transcribedText.isEmpty {
                    Text(transcribedText)
                        .font(.caption)
                        .padding()
                        .background(Color.gray.opacity(0.2))
                        .cornerRadius(8)
                    
                    Button {
                        // Analyze
                        WKInterfaceDevice.current().play(.success)
                    } label: {
                        Text("Analyze")
                            .frame(maxWidth: .infinity)
                    }
                    .buttonStyle(.borderedProminent)
                    .tint(.red)
                }
            }
            .padding()
        }
        .navigationTitle("Quick Analysis")
    }
}

// MARK: - Complications

struct ComplicationViews {
    // Circular complication showing streak
    static func circularStreak(days: Int) -> some View {
        ZStack {
            Circle()
                .stroke(Color.red.opacity(0.3), lineWidth: 4)
            
            Circle()
                .trim(from: 0, to: CGFloat(min(days, 30)) / 30)
                .stroke(Color.red, style: StrokeStyle(lineWidth: 4, lineCap: .round))
                .rotationEffect(.degrees(-90))
            
            VStack(spacing: 0) {
                Text("\(days)")
                    .font(.system(size: 16, weight: .bold))
                Text("days")
                    .font(.system(size: 8))
                    .foregroundColor(.secondary)
            }
        }
    }
    
    // Rectangular complication showing today's stats
    static func rectangularStats(models: Int, decisions: Int) -> some View {
        HStack {
            VStack(alignment: .leading) {
                Text("Today")
                    .font(.system(size: 10))
                    .foregroundColor(.secondary)
                HStack(spacing: 8) {
                    Label("\(models)", systemImage: "brain")
                    Label("\(decisions)", systemImage: "doc")
                }
                .font(.system(size: 12, weight: .semibold))
            }
            Spacer()
        }
    }
}

#Preview {
    MainWatchView()
        .environmentObject(WatchAppState())
}
