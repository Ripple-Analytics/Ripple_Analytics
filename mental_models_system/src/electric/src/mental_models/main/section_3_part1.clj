(ns mental-models.main.section-3-part1
  "Main Module - Section 3 Part1"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [ring.adapter.jetty :as jetty])
            #?(:clj [cheshire.core :as json])
            [mental-models.models :as models]
            [mental-models.algorithms :as algo]))

;; ============================================

(def index-html
  "<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
    <title>Mental Models System - Electric Clojure</title>
    <script src=\"https://cdn.tailwindcss.com\"></script>
    <link rel=\"stylesheet\" href=\"https://unpkg.com/leaflet@1.9.4/dist/leaflet.css\" />
    <script src=\"https://unpkg.com/leaflet@1.9.4/dist/leaflet.js\"></script>
    <style>
        /* M&S + Costco Design Language: Monochrome + Strategic Red Accents ONLY */
        :root {
            --black: #000000;
            --gray-900: #1a1a1a;
            --gray-800: #2d2d2d;
            --gray-700: #404040;
            --gray-600: #525252;
            --gray-500: #6b6b6b;
            --gray-400: #8a8a8a;
            --gray-300: #a3a3a3;
            --gray-200: #d4d4d4;
            --gray-100: #e5e5e5;
            --gray-50: #f5f5f5;
            --white: #ffffff;
            --accent-red: #cc1a1a;
            --accent-red-light: #fee2e2;
        }
        /* Value Line-style density */
        body { font-size: 11px; line-height: 1.3; background: var(--gray-50); color: var(--gray-900); }
        .dense-table td, .dense-table th { padding: 2px 4px; border-bottom: 1px solid var(--gray-200); }
        .metric-card { text-align: center; padding: 8px; background: var(--white); border: 1px solid var(--gray-200); }
        .metric-value { font-size: 18px; font-weight: bold; color: var(--black); font-family: ui-monospace, monospace; }
        .metric-value.accent { color: var(--accent-red); }
        .metric-label { font-size: 10px; color: var(--gray-500); text-transform: uppercase; letter-spacing: 0.5px; }
        #world-map { height: 400px; width: 100%; }
        .model-popup { max-width: 300px; }
        .model-popup h4 { font-weight: bold; margin-bottom: 4px; color: var(--black); }
        .model-popup .category { color: var(--gray-500); font-size: 10px; }
        /* Beast Mode Toggle */
        .beast-mode-toggle { display: flex; align-items: center; gap: 8px; padding: 8px 16px; background: var(--gray-900); }
        .beast-mode-toggle.active { background: var(--accent-red); }
        .beast-mode-label { color: var(--white); font-size: 11px; font-weight: 600; text-transform: uppercase; letter-spacing: 1px; }
        /* Navigation */
        .nav-tab { color: var(--gray-600); border-bottom: 2px solid transparent; }
        .nav-tab:hover { background: var(--gray-100); }
        .nav-tab.active { color: var(--black); border-bottom-color: var(--accent-red); background: var(--white); font-weight: 600; }
        /* Buttons */
        .btn-primary { background: var(--gray-900); color: var(--white); }
        .btn-primary:hover { background: var(--black); }
        .btn-accent { background: var(--accent-red); color: var(--white); }
        .btn-accent:hover { background: #b31515; }
        .btn-secondary { background: var(--gray-200); color: var(--gray-900); }
        .btn-secondary:hover { background: var(--gray-300); }
        /* Status indicators */
        .status-active { color: var(--accent-red); }
        .status-inactive { color: var(--gray-400); }
            /* Progress bars */
            .progress-bar { background: var(--gray-200); }
            .progress-fill { background: var(--gray-700); }
            .progress-fill.accent { background: var(--accent-red); }
            /* Clickable metrics with data provenance */
            .metric-value.clickable { cursor: pointer; text-decoration: underline; text-decoration-style: dotted; }
            .metric-value.clickable:hover { color: var(--accent-red); }
            /* Data Provenance Modal */
            .provenance-modal { display: none; position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: rgba(0,0,0,0.5); z-index: 1000; }
            .provenance-modal.active { display: flex; justify-content: center; align-items: center; }
            .provenance-content { background: var(--white); border: 2px solid var(--gray-900); max-width: 500px; width: 90%; max-height: 80vh; overflow-y: auto; }
            .provenance-header { background: var(--gray-900); color: var(--white); padding: 12px 16px; font-weight: bold; display: flex; justify-content: space-between; align-items: center; }
            .provenance-close { cursor: pointer; font-size: 18px; }
            .provenance-body { padding: 16px; }
            .provenance-row { display: flex; border-bottom: 1px solid var(--gray-200); padding: 8px 0; }
            .provenance-label { font-weight: bold; width: 120px; color: var(--gray-600); font-size: 10px; text-transform: uppercase; }
            .provenance-value { flex: 1; font-family: ui-monospace, monospace; font-size: 11px; word-break: break-all; }
            .provenance-value.na { color: var(--accent-red); font-weight: bold; }
            .provenance-value.real { color: #166534; }
            .provenance-formula { background: var(--gray-100); padding: 8px; margin-top: 8px; font-family: ui-monospace, monospace; font-size: 10px; border-left: 3px solid var(--accent-red); }
        </style>
</head>
<body class=\"bg-gray-50\">
    <!-- Data Provenance Modal - Click any metric to see where the data comes from -->
    <div id=\"provenance-modal\" class=\"provenance-modal\" onclick=\"if(event.target===this)closeProvenance()\">
        <div class=\"provenance-content\">
            <div class=\"provenance-header\">
                <span id=\"provenance-title\">DATA PROVENANCE</span>
                <span class=\"provenance-close\" onclick=\"closeProvenance()\">&times;</span>
            </div>
            <div class=\"provenance-body\" id=\"provenance-body\">
                <!-- Populated dynamically -->
            </div>
        </div>
    </div>
    
    <div id=\"app\">
        <!-- Header with Beast Mode Toggle -->
        <div class=\"flex justify-between items-center\" style=\"background: var(--gray-900);\">
            <div class=\"px-4 py-2\">
                <h1 class=\"text-lg font-bold\" style=\"color: var(--white);\">MENTAL MODELS SYSTEM</h1>
                <p class=\"text-xs\" style=\"color: var(--gray-400);\">Electric Clojure - Reactive Full-Stack | <span id=\"header-models\">NA</span> Models | <span id=\"header-failures\">NA</span> Failure Modes</p>
            </div>
            <div class=\"beast-mode-toggle\" id=\"beast-mode-btn\" onclick=\"toggleBeastMode()\">
                <span class=\"beast-mode-label\">BEAST MODE</span>
                <span id=\"beast-mode-indicator\" style=\"width: 8px; height: 8px; border-radius: 50%; background: var(--gray-500);\"></span>
            </div>
        </div>
        
                <!-- Navigation - Munger's Latticework Organization -->
                <!-- Organized by: 1) Overview, 2) INVERSION (failures first!), 3) Latticework (models), 4) Multi-disciplinary Analysis, 5) Tools -->
                <div class=\"flex flex-wrap border-b\" style=\"border-color: var(--gray-200); background: var(--gray-100);\">
                    <!-- OVERVIEW: Start with the big picture -->
                    <div class=\"flex items-center\" style=\"border-right: 1px solid var(--gray-300);\">
                        <span class=\"px-2 text-xs font-bold\" style=\"color: var(--gray-500);\">OVERVIEW</span>
                        <button class=\"nav-tab active px-3 py-2 text-xs\" onclick=\"showTab('dashboard')\" title=\"Big picture metrics and status\">DASHBOARD</button>
                    </div>
                    <!-- INVERSION: What can go wrong? (Munger: 'Invert, always invert') -->
                    <div class=\"flex items-center\" style=\"border-right: 1px solid var(--gray-300);\">
                        <span class=\"px-2 text-xs font-bold\" style=\"color: var(--accent-red);\">INVERSION</span>
                        <button class=\"nav-tab px-3 py-2 text-xs\" onclick=\"showTab('models')\" title=\"Mental models with failure modes - what can go wrong?\">MODELS + FAILURES</button>
                        <button class=\"nav-tab px-3 py-2 text-xs\" onclick=\"showTab('techdebt')\" title=\"Technical debt - what's broken or fragile?\">TECH DEBT</button>
                    </div>
                    <!-- LATTICEWORK: Multi-disciplinary analysis -->
                    <div class=\"flex items-center\" style=\"border-right: 1px solid var(--gray-300);\">
                        <span class=\"px-2 text-xs font-bold\" style=\"color: var(--gray-500);\">LATTICEWORK</span>
                        <button class=\"nav-tab px-3 py-2 text-xs\" onclick=\"showTab('analysis')\" title=\"Cross-disciplinary analysis\">ANALYSIS</button>
                        <button class=\"nav-tab px-3 py-2 text-xs\" onclick=\"showTab('statistics')\" title=\"Quantitative reasoning\">STATISTICS</button>
                        <button class=\"nav-tab px-3 py-2 text-xs\" onclick=\"showTab('map')\" title=\"Spatial/geographic patterns\">WORLD MAP</button>
                    </div>
                    <!-- TOOLS: Processing and automation -->
                    <div class=\"flex items-center\">
                        <span class=\"px-2 text-xs font-bold\" style=\"color: var(--gray-500);\">TOOLS</span>
                        <button class=\"nav-tab px-3 py-2 text-xs\" onclick=\"showTab('data')\" title=\"Data ingestion and processing\">DATA</button>
                        <button class=\"nav-tab px-3 py-2 text-xs\" onclick=\"showTab('llm')\" title=\"LLM-powered analysis\">LLM</button>
                        <button class=\"nav-tab px-3 py-2 text-xs\" onclick=\"showTab('distributed')\" title=\"Distributed processing at scale\">DISTRIBUTED</button>
                    </div>
                </div>
                <!-- Munger's Organizing Principles Legend -->
                <div class=\"px-4 py-1 text-xs\" style=\"background: var(--gray-50); border-bottom: 1px solid var(--gray-200); color: var(--gray-500);\">
                    <strong>Munger's Latticework:</strong> 
                    <span style=\"color: var(--black);\">OVERVIEW</span> (big picture) | 
                    <span style=\"color: var(--accent-red);\">INVERSION</span> (what can go wrong - always invert!) | 
                    <span style=\"color: var(--black);\">LATTICEWORK</span> (multi-disciplinary connections) | 
                    <span style=\"color: var(--black);\">TOOLS</span> (processing power)
                </div>
        
        <!-- Dashboard -->
        <div id=\"dashboard\" class=\"p-4\">
                                                <div class=\"grid grid-cols-6 gap-4 mb-4\">
                                                    <div class=\"metric-card bg-white border rounded shadow-sm\">
