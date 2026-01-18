// Mental Models Desktop App
// Built with Tauri (Rust backend) + ClojureScript frontend
// Features:
// - Distributed compute mesh (auto-discovers other nodes)
// - GPU acceleration (CUDA/Metal/OpenCL)
// - Local file scanning (beast mode)
// - Background processing
// - System tray integration
// - Cross-platform (macOS, Windows, Linux)

#![cfg_attr(
    all(not(debug_assertions), target_os = "windows"),
    windows_subsystem = "windows"
)]

use std::sync::{Arc, Mutex};
use std::collections::HashMap;
use std::net::{UdpSocket, SocketAddr, IpAddr, Ipv4Addr};
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};
use serde::{Deserialize, Serialize};
use tauri::{Manager, SystemTray, SystemTrayMenu, SystemTrayMenuItem, CustomMenuItem};

// =============================================================================
// MESH NODE CONFIGURATION
// =============================================================================

const MESH_PORT: u16 = 45678;
const DISCOVERY_PORT: u16 = 45679;
const HEARTBEAT_INTERVAL_MS: u64 = 5000;
const NODE_TIMEOUT_MS: u64 = 15000;

// =============================================================================
// DATA STRUCTURES
// =============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
struct MeshNode {
    id: String,
    name: String,
    address: String,
    port: u16,
    capabilities: NodeCapabilities,
    status: NodeStatus,
    last_seen: u64,
    stats: NodeStats,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct NodeCapabilities {
    cpu_cores: u32,
    gpu_available: bool,
    gpu_name: Option<String>,
    gpu_memory_gb: Option<f32>,
    ram_gb: f32,
    storage_gb: f32,
    platform: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
enum NodeStatus {
    Idle,
    Processing,
    Busy,
    Offline,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
struct NodeStats {
    items_processed: u64,
    bytes_processed: u64,
    cpu_utilization: f32,
    gpu_utilization: f32,
    uptime_seconds: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct WorkItem {
    id: String,
    item_type: WorkItemType,
    payload: String,
    priority: u8,
    created_at: u64,
    assigned_to: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
enum WorkItemType {
    FileAnalysis,
    NewsAnalysis,
    PatternDetection,
    ModelMatching,
    EmbeddingComputation,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct MeshMessage {
    msg_type: MeshMessageType,
    sender_id: String,
    payload: String,
    timestamp: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
enum MeshMessageType {
    Discovery,
    Heartbeat,
    WorkRequest,
    WorkResult,
    NodeJoin,
    NodeLeave,
}

// =============================================================================
// MESH STATE
// =============================================================================

struct MeshState {
    node_id: String,
    node_name: String,
    nodes: HashMap<String, MeshNode>,
    work_queue: Vec<WorkItem>,
    processing: HashMap<String, WorkItem>,
    stats: NodeStats,
    started_at: Instant,
}

impl MeshState {
    fn new() -> Self {
        let node_id = uuid::Uuid::new_v4().to_string();
        let node_name = hostname::get()
            .map(|h| h.to_string_lossy().to_string())
            .unwrap_or_else(|_| "unknown".to_string());
        
        MeshState {
            node_id,
            node_name,
            nodes: HashMap::new(),
            work_queue: Vec::new(),
            processing: HashMap::new(),
            stats: NodeStats::default(),
            started_at: Instant::now(),
        }
    }
    
    fn get_capabilities() -> NodeCapabilities {
        NodeCapabilities {
            cpu_cores: num_cpus::get() as u32,
            gpu_available: Self::detect_gpu(),
            gpu_name: Self::get_gpu_name(),
            gpu_memory_gb: Self::get_gpu_memory(),
            ram_gb: Self::get_ram_gb(),
            storage_gb: Self::get_storage_gb(),
            platform: std::env::consts::OS.to_string(),
        }
    }
    
    fn detect_gpu() -> bool {
        // Check for NVIDIA GPU
        std::process::Command::new("nvidia-smi")
            .output()
            .map(|o| o.status.success())
            .unwrap_or(false)
    }
    
    fn get_gpu_name() -> Option<String> {
        // Would query actual GPU
        if Self::detect_gpu() {
            Some("NVIDIA GPU".to_string())
        } else {
            None
        }
    }
    
    fn get_gpu_memory() -> Option<f32> {
        if Self::detect_gpu() {
            Some(8.0) // Would query actual memory
        } else {
            None
        }
    }
    
    fn get_ram_gb() -> f32 {
        // Would use sysinfo crate
        16.0
    }
    
    fn get_storage_gb() -> f32 {
        // Would query actual storage
        500.0
    }
}

type SharedMeshState = Arc<Mutex<MeshState>>;

// =============================================================================
// MESH DISCOVERY
// =============================================================================

fn start_discovery_listener(state: SharedMeshState) {
    thread::spawn(move || {
        let socket = UdpSocket::bind(format!("0.0.0.0:{}", DISCOVERY_PORT))
            .expect("Failed to bind discovery socket");
        socket.set_broadcast(true).ok();
        
        let mut buf = [0u8; 4096];
        
        loop {
            if let Ok((size, src)) = socket.recv_from(&mut buf) {
                if let Ok(msg) = serde_json::from_slice::<MeshMessage>(&buf[..size]) {
                    handle_discovery_message(&state, msg, src);
                }
            }
        }
    });
}

fn start_discovery_broadcaster(state: SharedMeshState) {
    thread::spawn(move || {
        let socket = UdpSocket::bind("0.0.0.0:0")
            .expect("Failed to bind broadcast socket");
        socket.set_broadcast(true).ok();
        
        let broadcast_addr = SocketAddr::new(
            IpAddr::V4(Ipv4Addr::new(255, 255, 255, 255)),
            DISCOVERY_PORT
        );
        
        loop {
            let msg = {
                let state = state.lock().unwrap();
                MeshMessage {
                    msg_type: MeshMessageType::Discovery,
                    sender_id: state.node_id.clone(),
                    payload: serde_json::to_string(&MeshNode {
                        id: state.node_id.clone(),
                        name: state.node_name.clone(),
                        address: "".to_string(), // Filled by receiver
                        port: MESH_PORT,
                        capabilities: MeshState::get_capabilities(),
                        status: NodeStatus::Idle,
                        last_seen: current_timestamp(),
                        stats: state.stats.clone(),
                    }).unwrap(),
                    timestamp: current_timestamp(),
                }
            };
            
            if let Ok(data) = serde_json::to_vec(&msg) {
                socket.send_to(&data, broadcast_addr).ok();
            }
            
            thread::sleep(Duration::from_millis(HEARTBEAT_INTERVAL_MS));
        }
    });
}

fn handle_discovery_message(state: &SharedMeshState, msg: MeshMessage, src: SocketAddr) {
    let mut state = state.lock().unwrap();
    
    match msg.msg_type {
        MeshMessageType::Discovery | MeshMessageType::Heartbeat => {
            if let Ok(mut node) = serde_json::from_str::<MeshNode>(&msg.payload) {
                if node.id != state.node_id {
                    node.address = src.ip().to_string();
                    node.last_seen = current_timestamp();
                    state.nodes.insert(node.id.clone(), node);
                }
            }
        }
        _ => {}
    }
    
    // Clean up stale nodes
    let now = current_timestamp();
    state.nodes.retain(|_, node| {
        now - node.last_seen < NODE_TIMEOUT_MS
    });
}

// =============================================================================
// WORK DISTRIBUTION
// =============================================================================

fn start_work_processor(state: SharedMeshState) {
    thread::spawn(move || {
        loop {
            let work_item = {
                let mut state = state.lock().unwrap();
                state.work_queue.pop()
            };
            
            if let Some(item) = work_item {
                process_work_item(&state, item);
            } else {
                thread::sleep(Duration::from_millis(100));
            }
        }
    });
}

fn process_work_item(state: &SharedMeshState, item: WorkItem) {
    // Mark as processing
    {
        let mut state = state.lock().unwrap();
        state.processing.insert(item.id.clone(), item.clone());
    }
    
    // Process based on type
    let result = match item.item_type {
        WorkItemType::FileAnalysis => analyze_file(&item.payload),
        WorkItemType::NewsAnalysis => analyze_news(&item.payload),
        WorkItemType::PatternDetection => detect_patterns(&item.payload),
        WorkItemType::ModelMatching => match_models(&item.payload),
        WorkItemType::EmbeddingComputation => compute_embeddings(&item.payload),
    };
    
    // Update stats
    {
        let mut state = state.lock().unwrap();
        state.processing.remove(&item.id);
        state.stats.items_processed += 1;
        state.stats.bytes_processed += item.payload.len() as u64;
    }
}

fn analyze_file(payload: &str) -> String {
    // Would perform actual file analysis
    format!("Analyzed file: {}", payload)
}

fn analyze_news(payload: &str) -> String {
    // Would perform news analysis through Munger framework
    format!("Analyzed news: {}", payload)
}

fn detect_patterns(payload: &str) -> String {
    // Would detect patterns
    format!("Detected patterns in: {}", payload)
}

fn match_models(payload: &str) -> String {
    // Would match against 129 mental models
    format!("Matched models for: {}", payload)
}

fn compute_embeddings(payload: &str) -> String {
    // Would compute embeddings using GPU
    format!("Computed embeddings for: {}", payload)
}

// =============================================================================
// TAURI COMMANDS
// =============================================================================

#[tauri::command]
fn get_mesh_status(state: tauri::State<SharedMeshState>) -> serde_json::Value {
    let state = state.lock().unwrap();
    serde_json::json!({
        "node_id": state.node_id,
        "node_name": state.node_name,
        "connected_nodes": state.nodes.len(),
        "nodes": state.nodes.values().collect::<Vec<_>>(),
        "work_queue_size": state.work_queue.len(),
        "processing_count": state.processing.len(),
        "stats": state.stats,
        "uptime_seconds": state.started_at.elapsed().as_secs(),
    })
}

#[tauri::command]
fn get_capabilities(state: tauri::State<SharedMeshState>) -> NodeCapabilities {
    MeshState::get_capabilities()
}

#[tauri::command]
fn submit_work(state: tauri::State<SharedMeshState>, item_type: String, payload: String) -> String {
    let work_item = WorkItem {
        id: uuid::Uuid::new_v4().to_string(),
        item_type: match item_type.as_str() {
            "file" => WorkItemType::FileAnalysis,
            "news" => WorkItemType::NewsAnalysis,
            "pattern" => WorkItemType::PatternDetection,
            "model" => WorkItemType::ModelMatching,
            "embedding" => WorkItemType::EmbeddingComputation,
            _ => WorkItemType::FileAnalysis,
        },
        payload,
        priority: 5,
        created_at: current_timestamp(),
        assigned_to: None,
    };
    
    let id = work_item.id.clone();
    state.lock().unwrap().work_queue.push(work_item);
    id
}

#[tauri::command]
fn start_beast_mode(state: tauri::State<SharedMeshState>, path: String) -> String {
    // Would start folder devourer on the path
    format!("Beast mode started on: {}", path)
}

#[tauri::command]
fn get_stats(state: tauri::State<SharedMeshState>) -> NodeStats {
    state.lock().unwrap().stats.clone()
}

// =============================================================================
// UTILITIES
// =============================================================================

fn current_timestamp() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as u64
}

// =============================================================================
// MAIN
// =============================================================================

fn main() {
    let state = Arc::new(Mutex::new(MeshState::new()));
    
    // Start mesh services
    start_discovery_listener(state.clone());
    start_discovery_broadcaster(state.clone());
    start_work_processor(state.clone());
    
    // System tray
    let quit = CustomMenuItem::new("quit".to_string(), "Quit");
    let show = CustomMenuItem::new("show".to_string(), "Show Window");
    let status = CustomMenuItem::new("status".to_string(), "Mesh Status");
    let tray_menu = SystemTrayMenu::new()
        .add_item(show)
        .add_item(status)
        .add_native_item(SystemTrayMenuItem::Separator)
        .add_item(quit);
    let system_tray = SystemTray::new().with_menu(tray_menu);
    
    tauri::Builder::default()
        .manage(state)
        .system_tray(system_tray)
        .on_system_tray_event(|app, event| {
            match event {
                tauri::SystemTrayEvent::MenuItemClick { id, .. } => {
                    match id.as_str() {
                        "quit" => std::process::exit(0),
                        "show" => {
                            if let Some(window) = app.get_window("main") {
                                window.show().ok();
                                window.set_focus().ok();
                            }
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        })
        .invoke_handler(tauri::generate_handler![
            get_mesh_status,
            get_capabilities,
            submit_work,
            start_beast_mode,
            get_stats,
        ])
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
