import Foundation
import CoreData

// MARK: - Offline Cache Manager
/// Manages local caching for offline mode with Core Data persistence
class OfflineCacheManager: ObservableObject {
    static let shared = OfflineCacheManager()
    
    @Published var isOffline: Bool = false
    @Published var lastSyncDate: Date?
    @Published var pendingUploads: Int = 0
    @Published var cachedModelsCount: Int = 129
    @Published var cachedInsightsCount: Int = 0
    
    private let userDefaults = UserDefaults(suiteName: "group.com.ripple.mentalmodels")
    private let fileManager = FileManager.default
    private let cacheDirectory: URL
    
    // MARK: - Initialization
    
    init() {
        // Set up cache directory
        let paths = fileManager.urls(for: .cachesDirectory, in: .userDomainMask)
        cacheDirectory = paths[0].appendingPathComponent("MentalModelsCache")
        
        // Create cache directory if needed
        try? fileManager.createDirectory(at: cacheDirectory, withIntermediateDirectories: true)
        
        // Load cached state
        loadCachedState()
        
        // Monitor network status
        startNetworkMonitoring()
    }
    
    // MARK: - Network Monitoring
    
    private func startNetworkMonitoring() {
        // In production, use NWPathMonitor
        // For now, check periodically
        Timer.scheduledTimer(withTimeInterval: 30, repeats: true) { [weak self] _ in
            self?.checkNetworkStatus()
        }
    }
    
    private func checkNetworkStatus() {
        // Simplified network check - in production use NWPathMonitor
        let url = URL(string: "https://api.ripple-analytics.com/health")!
        var request = URLRequest(url: url)
        request.timeoutInterval = 5
        
        URLSession.shared.dataTask(with: request) { [weak self] _, response, error in
            DispatchQueue.main.async {
                let wasOffline = self?.isOffline ?? false
                self?.isOffline = error != nil || (response as? HTTPURLResponse)?.statusCode != 200
                
                // Sync when coming back online
                if wasOffline && !(self?.isOffline ?? true) {
                    self?.syncPendingData()
                }
            }
        }.resume()
    }
    
    // MARK: - Cache State Management
    
    private func loadCachedState() {
        lastSyncDate = userDefaults?.object(forKey: "lastSyncDate") as? Date
        pendingUploads = userDefaults?.integer(forKey: "pendingUploads") ?? 0
        cachedInsightsCount = userDefaults?.integer(forKey: "cachedInsightsCount") ?? 0
    }
    
    private func saveCachedState() {
        userDefaults?.set(lastSyncDate, forKey: "lastSyncDate")
        userDefaults?.set(pendingUploads, forKey: "pendingUploads")
        userDefaults?.set(cachedInsightsCount, forKey: "cachedInsightsCount")
    }
    
    // MARK: - Mental Models Cache
    
    /// Cache all mental models for offline access
    func cacheMentalModels(_ models: [CachedMentalModel]) {
        let encoder = JSONEncoder()
        if let data = try? encoder.encode(models) {
            let fileURL = cacheDirectory.appendingPathComponent("mental_models.json")
            try? data.write(to: fileURL)
            cachedModelsCount = models.count
        }
    }
    
    /// Load cached mental models
    func loadCachedMentalModels() -> [CachedMentalModel] {
        let fileURL = cacheDirectory.appendingPathComponent("mental_models.json")
        guard let data = try? Data(contentsOf: fileURL) else {
            return defaultMentalModels
        }
        
        let decoder = JSONDecoder()
        return (try? decoder.decode([CachedMentalModel].self, from: data)) ?? defaultMentalModels
    }
    
    // MARK: - Insights Cache
    
    /// Cache insights for offline access
    func cacheInsights(_ insights: [CachedInsight]) {
        let encoder = JSONEncoder()
        if let data = try? encoder.encode(insights) {
            let fileURL = cacheDirectory.appendingPathComponent("insights.json")
            try? data.write(to: fileURL)
            cachedInsightsCount = insights.count
            saveCachedState()
        }
    }
    
    /// Load cached insights
    func loadCachedInsights() -> [CachedInsight] {
        let fileURL = cacheDirectory.appendingPathComponent("insights.json")
        guard let data = try? Data(contentsOf: fileURL) else {
            return []
        }
        
        let decoder = JSONDecoder()
        return (try? decoder.decode([CachedInsight].self, from: data)) ?? []
    }
    
    /// Add new insight (queued for upload when online)
    func addInsight(_ insight: CachedInsight) {
        var insights = loadCachedInsights()
        insights.insert(insight, at: 0)
        cacheInsights(insights)
        
        if isOffline {
            pendingUploads += 1
            saveCachedState()
        }
    }
    
    // MARK: - Analysis Cache
    
    /// Cache analysis result for offline access
    func cacheAnalysis(_ analysis: CachedAnalysis) {
        var analyses = loadCachedAnalyses()
        analyses.insert(analysis, at: 0)
        
        // Keep only last 100 analyses
        if analyses.count > 100 {
            analyses = Array(analyses.prefix(100))
        }
        
        let encoder = JSONEncoder()
        if let data = try? encoder.encode(analyses) {
            let fileURL = cacheDirectory.appendingPathComponent("analyses.json")
            try? data.write(to: fileURL)
        }
        
        if isOffline {
            pendingUploads += 1
            saveCachedState()
        }
    }
    
    /// Load cached analyses
    func loadCachedAnalyses() -> [CachedAnalysis] {
        let fileURL = cacheDirectory.appendingPathComponent("analyses.json")
        guard let data = try? Data(contentsOf: fileURL) else {
            return []
        }
        
        let decoder = JSONDecoder()
        return (try? decoder.decode([CachedAnalysis].self, from: data)) ?? []
    }
    
    // MARK: - Pending Data Queue
    
    /// Queue data for upload when online
    func queueForUpload(_ item: PendingUpload) {
        var queue = loadPendingQueue()
        queue.append(item)
        
        let encoder = JSONEncoder()
        if let data = try? encoder.encode(queue) {
            let fileURL = cacheDirectory.appendingPathComponent("pending_queue.json")
            try? data.write(to: fileURL)
        }
        
        pendingUploads = queue.count
        saveCachedState()
    }
    
    /// Load pending upload queue
    func loadPendingQueue() -> [PendingUpload] {
        let fileURL = cacheDirectory.appendingPathComponent("pending_queue.json")
        guard let data = try? Data(contentsOf: fileURL) else {
            return []
        }
        
        let decoder = JSONDecoder()
        return (try? decoder.decode([PendingUpload].self, from: data)) ?? []
    }
    
    // MARK: - Sync
    
    /// Sync pending data when online
    func syncPendingData() {
        guard !isOffline else { return }
        
        let queue = loadPendingQueue()
        guard !queue.isEmpty else { return }
        
        // Process each pending item
        for item in queue {
            uploadPendingItem(item) { [weak self] success in
                if success {
                    self?.removePendingItem(item)
                }
            }
        }
        
        lastSyncDate = Date()
        saveCachedState()
    }
    
    private func uploadPendingItem(_ item: PendingUpload, completion: @escaping (Bool) -> Void) {
        // In production, upload to server
        // For now, simulate success
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
            completion(true)
        }
    }
    
    private func removePendingItem(_ item: PendingUpload) {
        var queue = loadPendingQueue()
        queue.removeAll { $0.id == item.id }
        
        let encoder = JSONEncoder()
        if let data = try? encoder.encode(queue) {
            let fileURL = cacheDirectory.appendingPathComponent("pending_queue.json")
            try? data.write(to: fileURL)
        }
        
        pendingUploads = queue.count
        saveCachedState()
    }
    
    // MARK: - Cache Management
    
    /// Clear all cached data
    func clearCache() {
        try? fileManager.removeItem(at: cacheDirectory)
        try? fileManager.createDirectory(at: cacheDirectory, withIntermediateDirectories: true)
        
        cachedInsightsCount = 0
        pendingUploads = 0
        lastSyncDate = nil
        saveCachedState()
    }
    
    /// Get cache size in bytes
    func getCacheSize() -> Int64 {
        var size: Int64 = 0
        
        if let enumerator = fileManager.enumerator(at: cacheDirectory, includingPropertiesForKeys: [.fileSizeKey]) {
            for case let fileURL as URL in enumerator {
                if let fileSize = try? fileURL.resourceValues(forKeys: [.fileSizeKey]).fileSize {
                    size += Int64(fileSize)
                }
            }
        }
        
        return size
    }
    
    /// Format cache size for display
    func formattedCacheSize() -> String {
        let size = getCacheSize()
        
        if size >= 1_000_000_000 {
            return String(format: "%.1f GB", Double(size) / 1_000_000_000)
        } else if size >= 1_000_000 {
            return String(format: "%.1f MB", Double(size) / 1_000_000)
        } else if size >= 1_000 {
            return String(format: "%.1f KB", Double(size) / 1_000)
        }
        return "\(size) B"
    }
    
    // MARK: - Default Mental Models
    
    private var defaultMentalModels: [CachedMentalModel] {
        [
            CachedMentalModel(id: "1", name: "Circle of Competence", category: "Thinking", thinker: "Buffett", failureModes: 5),
            CachedMentalModel(id: "2", name: "Inversion", category: "Thinking", thinker: "Munger", failureModes: 5),
            CachedMentalModel(id: "3", name: "First Principles", category: "Thinking", thinker: "Aristotle", failureModes: 5),
            CachedMentalModel(id: "4", name: "Confirmation Bias", category: "Psychology", thinker: "Wason", failureModes: 5),
            CachedMentalModel(id: "5", name: "Compound Interest", category: "Math", thinker: "Einstein", failureModes: 5),
            CachedMentalModel(id: "6", name: "Network Effects", category: "Economics", thinker: "Metcalfe", failureModes: 5),
            CachedMentalModel(id: "7", name: "Margin of Safety", category: "Investing", thinker: "Graham", failureModes: 5),
            CachedMentalModel(id: "8", name: "Second-Order Thinking", category: "Thinking", thinker: "Marks", failureModes: 5),
            CachedMentalModel(id: "9", name: "Incentives", category: "Psychology", thinker: "Munger", failureModes: 5),
            CachedMentalModel(id: "10", name: "Opportunity Cost", category: "Economics", thinker: "Various", failureModes: 5)
        ]
    }
}

// MARK: - Cached Data Models

struct CachedMentalModel: Codable, Identifiable {
    let id: String
    let name: String
    let category: String
    let thinker: String
    let failureModes: Int
}

struct CachedInsight: Codable, Identifiable {
    let id: String
    let title: String
    let detail: String
    let timestamp: Date
    let isImportant: Bool
    let relatedModels: [String]
}

struct CachedAnalysis: Codable, Identifiable {
    let id: String
    let inputText: String
    let timestamp: Date
    let detectedModels: [String]
    let detectedBiases: [String]
    let lollapaloozaScore: Double
    let confidence: Double
}

struct PendingUpload: Codable, Identifiable {
    let id: String
    let type: UploadType
    let data: Data
    let timestamp: Date
    
    enum UploadType: String, Codable {
        case insight
        case analysis
        case feedback
        case sensorData
    }
}

// MARK: - Offline Mode View Modifier
import SwiftUI

struct OfflineBannerModifier: ViewModifier {
    @ObservedObject var cacheManager = OfflineCacheManager.shared
    
    func body(content: Content) -> some View {
        VStack(spacing: 0) {
            if cacheManager.isOffline {
                HStack(spacing: 8) {
                    Image(systemName: "wifi.slash")
                        .font(.system(size: 12, weight: .medium))
                    
                    Text("OFFLINE MODE")
                        .font(.system(size: 9, weight: .semibold))
                    
                    Spacer()
                    
                    if cacheManager.pendingUploads > 0 {
                        Text("\(cacheManager.pendingUploads) pending")
                            .font(.system(size: 9, weight: .regular))
                    }
                }
                .foregroundColor(.white)
                .padding(.horizontal, 16)
                .padding(.vertical, 6)
                .background(Color(red: 0.8, green: 0.1, blue: 0.1))
            }
            
            content
        }
    }
}

extension View {
    func offlineBanner() -> some View {
        modifier(OfflineBannerModifier())
    }
}

// MARK: - Sync Status View
struct SyncStatusView: View {
    @ObservedObject var cacheManager = OfflineCacheManager.shared
    
    var body: some View {
        VStack(alignment: .leading, spacing: 12) {
            HStack {
                Text("SYNC STATUS")
                    .font(.system(size: 9, weight: .medium))
                    .foregroundColor(Color(white: 0.4))
                
                Spacer()
                
                Circle()
                    .fill(cacheManager.isOffline ? Color(red: 0.8, green: 0.1, blue: 0.1) : Color(white: 0.4))
                    .frame(width: 6, height: 6)
                
                Text(cacheManager.isOffline ? "OFFLINE" : "ONLINE")
                    .font(.system(size: 9, weight: .medium))
                    .foregroundColor(cacheManager.isOffline ? Color(red: 0.8, green: 0.1, blue: 0.1) : Color(white: 0.4))
            }
            
            HStack {
                VStack(alignment: .leading, spacing: 4) {
                    Text("Last Sync")
                        .font(.system(size: 11, weight: .regular))
                        .foregroundColor(Color(white: 0.5))
                    
                    if let date = cacheManager.lastSyncDate {
                        Text(formatDate(date))
                            .font(.system(size: 13, weight: .regular))
                            .foregroundColor(Color(white: 0.1))
                    } else {
                        Text("Never")
                            .font(.system(size: 13, weight: .regular))
                            .foregroundColor(Color(white: 0.5))
                    }
                }
                
                Spacer()
                
                VStack(alignment: .trailing, spacing: 4) {
                    Text("Pending")
                        .font(.system(size: 11, weight: .regular))
                        .foregroundColor(Color(white: 0.5))
                    
                    Text("\(cacheManager.pendingUploads)")
                        .font(.system(size: 18, weight: .bold, design: .monospaced))
                        .foregroundColor(cacheManager.pendingUploads > 0 ? Color(red: 0.8, green: 0.1, blue: 0.1) : Color(white: 0.1))
                }
            }
            
            HStack {
                VStack(alignment: .leading, spacing: 4) {
                    Text("Cache Size")
                        .font(.system(size: 11, weight: .regular))
                        .foregroundColor(Color(white: 0.5))
                    
                    Text(cacheManager.formattedCacheSize())
                        .font(.system(size: 13, weight: .regular))
                        .foregroundColor(Color(white: 0.1))
                }
                
                Spacer()
                
                Button(action: { cacheManager.syncPendingData() }) {
                    Text("SYNC NOW")
                        .font(.system(size: 9, weight: .semibold))
                        .foregroundColor(.white)
                        .padding(.horizontal, 12)
                        .padding(.vertical, 6)
                        .background(cacheManager.isOffline ? Color(white: 0.5) : Color(red: 0.8, green: 0.1, blue: 0.1))
                        .cornerRadius(4)
                }
                .disabled(cacheManager.isOffline)
            }
        }
        .padding(16)
        .background(Color.white)
    }
    
    private func formatDate(_ date: Date) -> String {
        let formatter = RelativeDateTimeFormatter()
        formatter.unitsStyle = .abbreviated
        return formatter.localizedString(for: date, relativeTo: Date())
    }
}
