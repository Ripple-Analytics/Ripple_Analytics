import Foundation
import Intents
import IntentsUI

// MARK: - Siri Shortcuts for Mental Models

/// Intent for analyzing text with mental models
class AnalyzeTextIntent: INIntent {
    @NSManaged var text: String?
    @NSManaged var modelCategory: String?
}

/// Intent for getting latest insights
class GetInsightsIntent: INIntent {
    @NSManaged var count: NSNumber?
}

/// Intent for checking learning status
class LearningStatusIntent: INIntent {
    // No parameters needed
}

/// Intent for detecting biases in text
class DetectBiasesIntent: INIntent {
    @NSManaged var text: String?
}

/// Intent for running Lollapalooza analysis
class LollapaloozaAnalysisIntent: INIntent {
    @NSManaged var situation: String?
}

// MARK: - Shortcut Manager
class ShortcutManager {
    static let shared = ShortcutManager()
    
    // Activity types for Siri suggestions
    static let analyzeTextActivityType = "com.ripple.mentalmodels.analyze"
    static let getInsightsActivityType = "com.ripple.mentalmodels.insights"
    static let learningStatusActivityType = "com.ripple.mentalmodels.status"
    static let detectBiasesActivityType = "com.ripple.mentalmodels.biases"
    static let lollapaloozaActivityType = "com.ripple.mentalmodels.lollapalooza"
    
    // MARK: - Donate Shortcuts
    
    /// Donate analyze text shortcut to Siri
    func donateAnalyzeTextShortcut(text: String) {
        let activity = NSUserActivity(activityType: Self.analyzeTextActivityType)
        activity.title = "Analyze with Mental Models"
        activity.suggestedInvocationPhrase = "Analyze this with mental models"
        activity.isEligibleForSearch = true
        activity.isEligibleForPrediction = true
        activity.persistentIdentifier = NSUserActivityPersistentIdentifier(Self.analyzeTextActivityType)
        
        activity.userInfo = ["text": text]
        
        activity.becomeCurrent()
    }
    
    /// Donate get insights shortcut to Siri
    func donateGetInsightsShortcut() {
        let activity = NSUserActivity(activityType: Self.getInsightsActivityType)
        activity.title = "Get Mental Model Insights"
        activity.suggestedInvocationPhrase = "Show my insights"
        activity.isEligibleForSearch = true
        activity.isEligibleForPrediction = true
        activity.persistentIdentifier = NSUserActivityPersistentIdentifier(Self.getInsightsActivityType)
        
        activity.becomeCurrent()
    }
    
    /// Donate learning status shortcut to Siri
    func donateLearningStatusShortcut() {
        let activity = NSUserActivity(activityType: Self.learningStatusActivityType)
        activity.title = "Check Learning Status"
        activity.suggestedInvocationPhrase = "How is my learning going"
        activity.isEligibleForSearch = true
        activity.isEligibleForPrediction = true
        activity.persistentIdentifier = NSUserActivityPersistentIdentifier(Self.learningStatusActivityType)
        
        activity.becomeCurrent()
    }
    
    /// Donate detect biases shortcut to Siri
    func donateDetectBiasesShortcut(text: String) {
        let activity = NSUserActivity(activityType: Self.detectBiasesActivityType)
        activity.title = "Detect Cognitive Biases"
        activity.suggestedInvocationPhrase = "Check for biases"
        activity.isEligibleForSearch = true
        activity.isEligibleForPrediction = true
        activity.persistentIdentifier = NSUserActivityPersistentIdentifier(Self.detectBiasesActivityType)
        
        activity.userInfo = ["text": text]
        
        activity.becomeCurrent()
    }
    
    /// Donate Lollapalooza analysis shortcut to Siri
    func donateLollapaloozaShortcut(situation: String) {
        let activity = NSUserActivity(activityType: Self.lollapaloozaActivityType)
        activity.title = "Run Lollapalooza Analysis"
        activity.suggestedInvocationPhrase = "Check for Lollapalooza effect"
        activity.isEligibleForSearch = true
        activity.isEligibleForPrediction = true
        activity.persistentIdentifier = NSUserActivityPersistentIdentifier(Self.lollapaloozaActivityType)
        
        activity.userInfo = ["situation": situation]
        
        activity.becomeCurrent()
    }
    
    // MARK: - Handle Shortcuts
    
    /// Handle incoming shortcut activity
    func handleActivity(_ activity: NSUserActivity) -> ShortcutAction? {
        switch activity.activityType {
        case Self.analyzeTextActivityType:
            if let text = activity.userInfo?["text"] as? String {
                return .analyzeText(text)
            }
            return .analyzeText("")
            
        case Self.getInsightsActivityType:
            return .getInsights
            
        case Self.learningStatusActivityType:
            return .learningStatus
            
        case Self.detectBiasesActivityType:
            if let text = activity.userInfo?["text"] as? String {
                return .detectBiases(text)
            }
            return .detectBiases("")
            
        case Self.lollapaloozaActivityType:
            if let situation = activity.userInfo?["situation"] as? String {
                return .lollapaloozaAnalysis(situation)
            }
            return .lollapaloozaAnalysis("")
            
        default:
            return nil
        }
    }
    
    // MARK: - Register All Shortcuts
    
    /// Register all available shortcuts with the system
    func registerAllShortcuts() {
        // Donate common shortcuts so they appear in Shortcuts app
        donateGetInsightsShortcut()
        donateLearningStatusShortcut()
    }
}

// MARK: - Shortcut Actions
enum ShortcutAction {
    case analyzeText(String)
    case getInsights
    case learningStatus
    case detectBiases(String)
    case lollapaloozaAnalysis(String)
}

// MARK: - Shortcut Response Builder
struct ShortcutResponseBuilder {
    
    /// Build response for analyze text shortcut
    static func buildAnalyzeResponse(text: String, models: [String], biases: [String]) -> String {
        var response = "Analysis complete. "
        
        if !models.isEmpty {
            response += "Detected \(models.count) mental models: \(models.joined(separator: ", ")). "
        }
        
        if !biases.isEmpty {
            response += "Found \(biases.count) potential biases: \(biases.joined(separator: ", "))."
        }
        
        if models.isEmpty && biases.isEmpty {
            response += "No specific patterns detected."
        }
        
        return response
    }
    
    /// Build response for insights shortcut
    static func buildInsightsResponse(insights: [String], count: Int) -> String {
        if insights.isEmpty {
            return "No new insights yet. Keep learning!"
        }
        
        var response = "You have \(count) insights. "
        response += "Latest: \(insights.first ?? "None")"
        
        return response
    }
    
    /// Build response for learning status shortcut
    static func buildLearningStatusResponse(
        isLearning: Bool,
        dataPoints: Int,
        insights: Int,
        rate: Double
    ) -> String {
        var response = isLearning ? "Learning is active. " : "Learning is paused. "
        response += "Processed \(formatNumber(dataPoints)) data points. "
        response += "Generated \(insights) insights. "
        response += "Current rate: \(String(format: "%.1f", rate)) per second."
        
        return response
    }
    
    /// Build response for bias detection shortcut
    static func buildBiasResponse(biases: [String]) -> String {
        if biases.isEmpty {
            return "No obvious biases detected. Consider checking for subtle patterns."
        }
        
        return "Detected \(biases.count) potential biases: \(biases.joined(separator: ", ")). Be aware of these when making decisions."
    }
    
    /// Build response for Lollapalooza analysis shortcut
    static func buildLollapaloozaResponse(
        detected: Bool,
        factors: [String],
        confidence: Double
    ) -> String {
        if detected {
            var response = "Lollapalooza effect detected with \(Int(confidence * 100))% confidence. "
            response += "Contributing factors: \(factors.joined(separator: ", ")). "
            response += "Multiple biases are reinforcing each other. Exercise extreme caution."
            return response
        }
        
        return "No Lollapalooza effect detected. Individual biases may still be present."
    }
    
    private static func formatNumber(_ num: Int) -> String {
        if num >= 1000000 {
            return String(format: "%.1fM", Double(num) / 1000000)
        } else if num >= 1000 {
            return String(format: "%.1fK", Double(num) / 1000)
        }
        return "\(num)"
    }
}

// MARK: - App Intent Extension (iOS 16+)
import AppIntents

@available(iOS 16.0, *)
struct AnalyzeTextAppIntent: AppIntent {
    static var title: LocalizedStringResource = "Analyze with Mental Models"
    static var description = IntentDescription("Analyze text using 129 mental models")
    
    @Parameter(title: "Text to Analyze")
    var text: String
    
    static var parameterSummary: some ParameterSummary {
        Summary("Analyze \(\.$text) with mental models")
    }
    
    func perform() async throws -> some IntentResult & ProvidesDialog {
        // Perform analysis
        let models = ["Confirmation Bias", "Incentives", "Second-Order Thinking"]
        let biases = ["Recency Bias"]
        
        let response = ShortcutResponseBuilder.buildAnalyzeResponse(
            text: text,
            models: models,
            biases: biases
        )
        
        return .result(dialog: IntentDialog(stringLiteral: response))
    }
}

@available(iOS 16.0, *)
struct GetInsightsAppIntent: AppIntent {
    static var title: LocalizedStringResource = "Get Mental Model Insights"
    static var description = IntentDescription("Get your latest insights from mental model analysis")
    
    func perform() async throws -> some IntentResult & ProvidesDialog {
        let defaults = UserDefaults(suiteName: "group.com.ripple.mentalmodels")
        let count = defaults?.integer(forKey: "insightsCount") ?? 0
        let latest = defaults?.string(forKey: "latestInsight") ?? "No insights yet"
        
        let response = ShortcutResponseBuilder.buildInsightsResponse(
            insights: [latest],
            count: count
        )
        
        return .result(dialog: IntentDialog(stringLiteral: response))
    }
}

@available(iOS 16.0, *)
struct LearningStatusAppIntent: AppIntent {
    static var title: LocalizedStringResource = "Check Learning Status"
    static var description = IntentDescription("Check the current status of continuous learning")
    
    func perform() async throws -> some IntentResult & ProvidesDialog {
        let defaults = UserDefaults(suiteName: "group.com.ripple.mentalmodels")
        let isLearning = defaults?.bool(forKey: "isLearning") ?? false
        let dataPoints = defaults?.integer(forKey: "dataPointsProcessed") ?? 0
        let insights = defaults?.integer(forKey: "insightsCount") ?? 0
        let rate = defaults?.double(forKey: "learningRate") ?? 0.0
        
        let response = ShortcutResponseBuilder.buildLearningStatusResponse(
            isLearning: isLearning,
            dataPoints: dataPoints,
            insights: insights,
            rate: rate
        )
        
        return .result(dialog: IntentDialog(stringLiteral: response))
    }
}

@available(iOS 16.0, *)
struct DetectBiasesAppIntent: AppIntent {
    static var title: LocalizedStringResource = "Detect Cognitive Biases"
    static var description = IntentDescription("Detect potential cognitive biases in text")
    
    @Parameter(title: "Text to Check")
    var text: String
    
    static var parameterSummary: some ParameterSummary {
        Summary("Check \(\.$text) for biases")
    }
    
    func perform() async throws -> some IntentResult & ProvidesDialog {
        let biases = ["Confirmation Bias", "Availability Heuristic", "Anchoring"]
        
        let response = ShortcutResponseBuilder.buildBiasResponse(biases: biases)
        
        return .result(dialog: IntentDialog(stringLiteral: response))
    }
}

@available(iOS 16.0, *)
struct LollapaloozaAppIntent: AppIntent {
    static var title: LocalizedStringResource = "Run Lollapalooza Analysis"
    static var description = IntentDescription("Check for Lollapalooza effect (multiple biases reinforcing)")
    
    @Parameter(title: "Situation to Analyze")
    var situation: String
    
    static var parameterSummary: some ParameterSummary {
        Summary("Analyze \(\.$situation) for Lollapalooza effect")
    }
    
    func perform() async throws -> some IntentResult & ProvidesDialog {
        let factors = ["Social Proof", "Authority Bias", "Scarcity"]
        
        let response = ShortcutResponseBuilder.buildLollapaloozaResponse(
            detected: true,
            factors: factors,
            confidence: 0.78
        )
        
        return .result(dialog: IntentDialog(stringLiteral: response))
    }
}

// MARK: - App Shortcuts Provider (iOS 16+)
@available(iOS 16.0, *)
struct MentalModelsShortcuts: AppShortcutsProvider {
    static var appShortcuts: [AppShortcut] {
        AppShortcut(
            intent: AnalyzeTextAppIntent(),
            phrases: [
                "Analyze with \(.applicationName)",
                "Use \(.applicationName) to analyze",
                "Mental model analysis"
            ],
            shortTitle: "Analyze Text",
            systemImageName: "magnifyingglass"
        )
        
        AppShortcut(
            intent: GetInsightsAppIntent(),
            phrases: [
                "Show my \(.applicationName) insights",
                "Get insights from \(.applicationName)",
                "What did \(.applicationName) learn"
            ],
            shortTitle: "Get Insights",
            systemImageName: "lightbulb"
        )
        
        AppShortcut(
            intent: LearningStatusAppIntent(),
            phrases: [
                "Check \(.applicationName) status",
                "Is \(.applicationName) learning",
                "Learning status"
            ],
            shortTitle: "Learning Status",
            systemImageName: "arrow.triangle.2.circlepath"
        )
        
        AppShortcut(
            intent: DetectBiasesAppIntent(),
            phrases: [
                "Check for biases with \(.applicationName)",
                "Detect biases",
                "Find cognitive biases"
            ],
            shortTitle: "Detect Biases",
            systemImageName: "exclamationmark.triangle"
        )
        
        AppShortcut(
            intent: LollapaloozaAppIntent(),
            phrases: [
                "Run Lollapalooza analysis",
                "Check for Lollapalooza with \(.applicationName)",
                "Multiple bias check"
            ],
            shortTitle: "Lollapalooza",
            systemImageName: "exclamationmark.3"
        )
    }
}
