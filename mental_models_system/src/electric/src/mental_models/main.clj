(ns mental-models.main
  "Mental Models System - Electric Clojure Server
   
   Main entry point for the Electric Clojure application.
   Starts the server and serves the reactive UI.
   
   Includes LM Studio integration for local LLM inference.
   Includes Tech Debt Eliminator with DAG visualization.
   Includes Petabyte-scale distributed processing.
   Includes 24/7 continuous learning system."
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.util.response :as response]
            [mental-models.models :as models]
            [mental-models.analysis :as analysis]
            [mental-models.statistics :as stats]
            [mental-models.data-processing :as data]
            [mental-models.tech-debt :as tech-debt]
            [mental-models.db :as db]
            [mental-models.distributed :as distributed]
            [mental-models.continuous :as continuous]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.net HttpURLConnection URL]
           [java.io BufferedReader InputStreamReader OutputStreamWriter])
  (:gen-class))

;; ============================================
;; LM Studio Integration
;; ============================================

(def lm-studio-config
  "Configuration for LM Studio connection."
  {:base-url (or (System/getenv "LM_STUDIO_URL") "http://localhost:1234")
   :model (or (System/getenv "LM_STUDIO_MODEL") "local-model")
   :timeout 60000})

(defn call-lm-studio
  "Call LM Studio's OpenAI-compatible API.
   Returns the response text or nil on error."
  [prompt & {:keys [system-prompt max-tokens temperature]
             :or {system-prompt "You are a helpful assistant that analyzes situations using mental models."
                  max-tokens 1000
                  temperature 0.7}}]
  (try
    (let [url (URL. (str (:base-url lm-studio-config) "/v1/chat/completions"))
          conn (doto (.openConnection url)
                 (.setRequestMethod "POST")
                 (.setRequestProperty "Content-Type" "application/json")
                 (.setDoOutput true)
                 (.setConnectTimeout (:timeout lm-studio-config))
                 (.setReadTimeout (:timeout lm-studio-config)))
          request-body (cheshire.core/generate-string
                        {:model (:model lm-studio-config)
                         :messages [{:role "system" :content system-prompt}
                                    {:role "user" :content prompt}]
                         :max_tokens max-tokens
                         :temperature temperature})]
      (with-open [writer (OutputStreamWriter. (.getOutputStream conn))]
        (.write writer request-body)
        (.flush writer))
      (if (= 200 (.getResponseCode conn))
        (with-open [reader (BufferedReader. (InputStreamReader. (.getInputStream conn)))]
          (let [response (cheshire.core/parse-string (slurp reader) true)]
            (get-in response [:choices 0 :message :content])))
        (do
          (println "LM Studio error:" (.getResponseCode conn))
          nil)))
    (catch Exception e
      (println "LM Studio connection error:" (.getMessage e))
      nil)))

(defn analyze-with-llm
  "Analyze a situation using LM Studio and mental models."
  [situation model-names]
  (let [models-context (if (empty? model-names)
                         (take 10 (models/get-all-models))
                         (map models/get-model model-names))
        models-str (str/join "\n\n" 
                            (map (fn [m]
                                   (str "**" (:name m) "** (" (:category m) ")\n"
                                        "Description: " (:description m) "\n"
                                        "Key Insight: " (:key-insight m) "\n"
                                        "Application: " (:application m)))
                                 models-context))
        prompt (str "Analyze the following situation using these mental models:\n\n"
                   "MENTAL MODELS:\n" models-str "\n\n"
                   "SITUATION:\n" situation "\n\n"
                   "Provide a comprehensive analysis that:\n"
                   "1. Identifies which mental models are most relevant\n"
                   "2. Explains how each relevant model applies\n"
                   "3. Identifies potential failure modes to watch for\n"
                   "4. Provides actionable recommendations\n"
                   "5. Notes any lollapalooza effects (multiple models reinforcing each other)")]
    (if-let [response (call-lm-studio prompt)]
      {:success true
       :analysis response
       :models-used (map :name models-context)
       :llm-powered true}
      {:success false
       :error "Could not connect to LM Studio. Make sure it's running on localhost:1234"
       :fallback (analysis/latticework-analyze model-names situation)
       :llm-powered false})))

(defn detect-biases-with-llm
  "Detect cognitive biases in text using LM Studio."
  [text]
  (let [bias-models ["confirmation-bias" "hindsight-bias" "availability-heuristic" 
                     "loss-aversion" "social-proof" "anchoring-negotiation"
                     "dunning-kruger" "status-quo-bias" "narrative-fallacy"]
        prompt (str "Analyze the following text for cognitive biases:\n\n"
                   "TEXT:\n" text "\n\n"
                   "Look for these specific biases:\n"
                   "- Confirmation bias: seeking confirming evidence\n"
                   "- Hindsight bias: believing past was predictable\n"
                   "- Availability heuristic: overweighting recent/vivid events\n"
                   "- Loss aversion: overweighting losses vs gains\n"
                   "- Social proof: following the crowd\n"
                   "- Anchoring: being influenced by first numbers\n"
                   "- Dunning-Kruger: overconfidence without competence\n"
                   "- Status quo bias: preferring current state\n"
                   "- Narrative fallacy: creating stories for random events\n\n"
                   "For each bias detected, explain:\n"
                   "1. What specific phrases or patterns indicate the bias\n"
                   "2. How severe the bias appears (low/medium/high)\n"
                   "3. Recommendations to counteract the bias")]
    (if-let [response (call-lm-studio prompt)]
      {:success true
       :analysis response
       :llm-powered true}
      {:success false
       :error "Could not connect to LM Studio"
       :fallback (analysis/detect-biases text)
       :llm-powered false})))

(defn generate-decision-checklist-with-llm
  "Generate a decision checklist using LM Studio."
  [decision-context]
  (let [prompt (str "Create a comprehensive decision checklist for the following situation:\n\n"
                   "DECISION CONTEXT:\n" decision-context "\n\n"
                   "Generate a checklist that includes:\n"
                   "1. Key questions to answer before deciding\n"
                   "2. Information gaps to fill\n"
                   "3. Stakeholders to consult\n"
                   "4. Potential failure modes to consider\n"
                   "5. Second-order effects to anticipate\n"
                   "6. Reversibility assessment\n"
                   "7. Opportunity cost analysis\n"
                   "8. Pre-mortem: what could go wrong?\n"
                   "9. Kill criteria: when to abandon this path\n"
                   "10. Success metrics: how will you know it worked?")]
    (if-let [response (call-lm-studio prompt)]
      {:success true
       :checklist response
       :llm-powered true}
      {:success false
       :error "Could not connect to LM Studio"
       :fallback (analysis/decision-checklist decision-context)
       :llm-powered false})))

(defn classify-document-with-llm
  "Classify a document by relevant mental models using LM Studio."
  [text]
  (let [categories (models/get-all-categories)
        prompt (str "Classify the following document by which mental models are most relevant:\n\n"
                   "DOCUMENT:\n" text "\n\n"
                   "Available categories: " (str/join ", " categories) "\n\n"
                   "For each relevant mental model:\n"
                   "1. Name the model and category\n"
                   "2. Explain why it's relevant to this document\n"
                   "3. Rate relevance (1-10)\n"
                   "4. Identify specific passages that relate to the model\n\n"
                   "Also identify:\n"
                   "- The primary theme/topic of the document\n"
                   "- Key decisions or situations described\n"
                   "- Potential biases in the author's perspective")]
    (if-let [response (call-lm-studio prompt)]
      {:success true
       :classification response
       :llm-powered true}
      {:success false
       :error "Could not connect to LM Studio"
       :fallback (data/classify-by-mental-models text)
       :llm-powered false})))

;; ============================================
;; HTML Template
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
                                                        <div class=\"metric-value clickable\" id=\"total-models\" onclick=\"showProvenance('total-models')\">NA</div>
                                                        <div class=\"metric-label\">Models (click for source)</div>
                                                    </div>
                                                    <div class=\"metric-card bg-white border rounded shadow-sm\">
                                                        <div class=\"metric-value clickable\" id=\"total-failures\" onclick=\"showProvenance('total-failures')\">NA</div>
                                                        <div class=\"metric-label\">Failure Modes (click for source)</div>
                                                    </div>
                                                    <div class=\"metric-card bg-white border rounded shadow-sm\">
                                                        <div class=\"metric-value clickable\" id=\"total-categories\" onclick=\"showProvenance('total-categories')\">NA</div>
                                                        <div class=\"metric-label\">Categories (click for source)</div>
                                                    </div>
                                                    <div class=\"metric-card bg-white border rounded shadow-sm\">
                                                        <div class=\"metric-value clickable\" id=\"avg-failures\" onclick=\"showProvenance('avg-failures')\">NA</div>
                                                        <div class=\"metric-label\">Avg Failures/Model (click for source)</div>
                                                    </div>
                                                    <div class=\"metric-card bg-white border rounded shadow-sm\">
                                                        <div class=\"metric-value clickable\" id=\"coverage\" onclick=\"showProvenance('coverage')\">NA</div>
                                                        <div class=\"metric-label\">Coverage (click for source)</div>
                                                    </div>
                                                    <div class=\"metric-card bg-white border rounded shadow-sm\">
                                                        <div class=\"metric-value clickable\" id=\"api-status\" onclick=\"showProvenance('api-status')\">NA</div>
                                                        <div class=\"metric-label\">Status (click for source)</div>
                                                    </div>
                                                </div>
            
            <div class=\"grid grid-cols-2 gap-4\">
                <!-- Categories -->
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Categories</div>
                    <div class=\"p-3\">
                        <table class=\"w-full dense-table text-xs\">
                            <thead>
                                <tr class=\"bg-gray-50\">
                                    <th class=\"text-left\">Category</th>
                                    <th class=\"text-left\">Models</th>
                                    <th class=\"text-left\">Coverage</th>
                                </tr>
                            </thead>
                            <tbody id=\"categories-table\"></tbody>
                        </table>
                    </div>
                </div>
                
                <!-- Quick Analysis -->
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Quick Analysis</div>
                    <div class=\"p-3\">
                        <textarea id=\"analysis-input\" class=\"w-full border rounded px-2 py-1 text-xs h-20\" placeholder=\"Enter situation to analyze...\"></textarea>
                        <div class=\"flex gap-2 mt-2\">
                                                        <button class=\"btn-primary px-3 py-1 text-xs rounded\" onclick=\"runAnalysis()\">ANALYZE</button>
                                                        <button class=\"btn-secondary px-3 py-1 text-xs rounded\" onclick=\"detectBiases()\">DETECT BIASES</button>
                        </div>
                        <div id=\"analysis-result\" class=\"mt-2 text-xs\"></div>
                    </div>
                </div>
            </div>
        </div>
        
        <!-- Models Tab -->
        <div id=\"models\" class=\"p-4 hidden\">
            <div class=\"grid grid-cols-2 gap-4\">
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">
                        Mental Models
                        <input type=\"text\" id=\"model-search\" class=\"ml-4 border rounded px-2 py-0.5 text-xs\" placeholder=\"Search...\" oninput=\"filterModels()\">
                    </div>
                    <div class=\"p-3 max-h-96 overflow-y-auto\">
                        <table class=\"w-full dense-table text-xs\">
                            <thead>
                                <tr class=\"bg-gray-50\">
                                    <th class=\"text-left\">Name</th>
                                    <th class=\"text-left\">Category</th>
                                    <th class=\"text-left\">Originator</th>
                                    <th class=\"text-left\">Failures</th>
                                </tr>
                            </thead>
                            <tbody id=\"models-table\"></tbody>
                        </table>
                    </div>
                </div>
                
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Model Detail</div>
                    <div class=\"p-3\" id=\"model-detail\">
                        <p class=\"text-gray-500 text-xs\">Select a model to view details</p>
                    </div>
                </div>
            </div>
        </div>
        
        <!-- Analysis Tab -->
        <div id=\"analysis\" class=\"p-4 hidden\">
            <div class=\"grid grid-cols-2 gap-4\">
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Comprehensive Analysis</div>
                    <div class=\"p-3\">
                        <textarea id=\"comprehensive-input\" class=\"w-full border rounded px-2 py-1 text-xs h-32\" placeholder=\"Enter situation for comprehensive analysis...\"></textarea>
                        <div class=\"flex gap-2 mt-2\">
                                                        <button class=\"btn-primary px-3 py-1 text-xs rounded\" onclick=\"runLatticework()\">LATTICEWORK</button>
                                                        <button class=\"btn-accent px-3 py-1 text-xs rounded\" onclick=\"runLollapalooza()\">LOLLAPALOOZA</button>
                                                        <button class=\"btn-primary px-3 py-1 text-xs rounded\" onclick=\"runInversion()\">INVERSION</button>
                                                        <button class=\"btn-primary px-3 py-1 text-xs rounded\" onclick=\"runTwoTrack()\">TWO-TRACK</button>
                        </div>
                    </div>
                </div>
                
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Analysis Results</div>
                    <div class=\"p-3\" id=\"comprehensive-result\">
                        <p class=\"text-gray-500 text-xs\">Run an analysis to see results</p>
                    </div>
                </div>
            </div>
        </div>
        
        <!-- Statistics Tab -->
        <div id=\"statistics\" class=\"p-4 hidden\">
            <div class=\"bg-white border rounded shadow-sm\">
                <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Statistical Analysis</div>
                <div class=\"p-3\">
                    <div class=\"grid grid-cols-2 gap-4\">
                        <div>
                            <label class=\"text-xs font-semibold\">X Values (comma-separated)</label>
                            <input type=\"text\" id=\"stats-x\" class=\"w-full border rounded px-2 py-1 text-xs mt-1\" placeholder=\"1, 2, 3, 4, 5\">
                        </div>
                        <div>
                            <label class=\"text-xs font-semibold\">Y Values (comma-separated)</label>
                            <input type=\"text\" id=\"stats-y\" class=\"w-full border rounded px-2 py-1 text-xs mt-1\" placeholder=\"2, 4, 5, 4, 5\">
                        </div>
                    </div>
                    <button class=\"btn-primary px-3 py-1 text-xs rounded mt-2\" onclick=\"runStatistics()\">CALCULATE</button>
                    <div id=\"stats-result\" class=\"mt-4\"></div>
                </div>
            </div>
        </div>
        
        <!-- Data Tab -->
        <div id=\"data\" class=\"p-4 hidden\">
            <div class=\"bg-white border rounded shadow-sm\">
                <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Data Processing</div>
                <div class=\"p-3\">
                    <textarea id=\"data-input\" class=\"w-full border rounded px-2 py-1 text-xs h-32\" placeholder=\"Paste text to analyze...\"></textarea>
                    <div class=\"flex gap-2 mt-2\">
                                                <button class=\"btn-primary px-3 py-1 text-xs rounded\" onclick=\"analyzeDocument()\">ANALYZE DOCUMENT</button>
                                                <button class=\"btn-primary px-3 py-1 text-xs rounded\" onclick=\"extractEntities()\">EXTRACT ENTITIES</button>
                                                <button class=\"btn-primary px-3 py-1 text-xs rounded\" onclick=\"classifyText()\">CLASSIFY BY MODELS</button>
                    </div>
                    <div id=\"data-result\" class=\"mt-4\"></div>
                </div>
            </div>
        </div>
        
        <!-- World Map Tab -->
        <div id=\"map\" class=\"p-4 hidden\">
            <div class=\"grid grid-cols-3 gap-4\">
                <div class=\"col-span-2 bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Mental Models World Map</div>
                    <div class=\"p-2\">
                        <div id=\"world-map\"></div>
                    </div>
                </div>
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Case Studies by Location</div>
                    <div class=\"p-3 max-h-96 overflow-y-auto\" id=\"map-case-studies\">
                        <p class=\"text-gray-500 text-xs\">Click a marker to see case studies</p>
                    </div>
                </div>
            </div>
            <div class=\"mt-4 bg-white border rounded shadow-sm\">
                <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Add Case Study</div>
                <div class=\"p-3\">
                    <div class=\"grid grid-cols-4 gap-4\">
                        <input type=\"text\" id=\"case-title\" class=\"border rounded px-2 py-1 text-xs\" placeholder=\"Title\">
                        <input type=\"text\" id=\"case-lat\" class=\"border rounded px-2 py-1 text-xs\" placeholder=\"Latitude\">
                        <input type=\"text\" id=\"case-lng\" class=\"border rounded px-2 py-1 text-xs\" placeholder=\"Longitude\">
                        <select id=\"case-model\" class=\"border rounded px-2 py-1 text-xs\"></select>
                    </div>
                    <textarea id=\"case-description\" class=\"w-full border rounded px-2 py-1 text-xs mt-2 h-16\" placeholder=\"Description...\"></textarea>
                    <button class=\"btn-accent px-3 py-1 text-xs rounded mt-2\" onclick=\"addCaseStudy()\">ADD TO MAP</button>
                </div>
            </div>
        </div>
        
        <!-- LLM Tab -->
        <div id=\"llm\" class=\"p-4 hidden\">
            <div class=\"grid grid-cols-2 gap-4\">
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">LM Studio Analysis</div>
                    <div class=\"p-3\">
                        <div class=\"mb-2\">
                            <span class=\"text-xs\">Status: </span>
                            <span id=\"llm-status\" class=\"text-xs bg-gray-200 px-2 py-0.5 rounded\">Checking...</span>
                        </div>
                        <textarea id=\"llm-input\" class=\"w-full border rounded px-2 py-1 text-xs h-32\" placeholder=\"Enter situation for LLM-powered analysis...\"></textarea>
                        <div class=\"flex gap-2 mt-2\">
                                                        <button class=\"btn-primary px-3 py-1 text-xs rounded\" onclick=\"runLLMAnalysis()\">ANALYZE WITH LLM</button>
                                                        <button class=\"btn-primary px-3 py-1 text-xs rounded\" onclick=\"runLLMBiases()\">DETECT BIASES</button>
                                                        <button class=\"btn-accent px-3 py-1 text-xs rounded\" onclick=\"runLLMChecklist()\">GENERATE CHECKLIST</button>
                        </div>
                    </div>
                </div>
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">LLM Results</div>
                    <div class=\"p-3 max-h-96 overflow-y-auto\" id=\"llm-result\">
                        <p class=\"text-gray-500 text-xs\">Run an LLM analysis to see results</p>
                    </div>
                </div>
            </div>
        </div>
        
        <!-- Tech Debt Tab -->
        <div id=\"techdebt\" class=\"p-4 hidden\">
            <div class=\"grid grid-cols-3 gap-4 mb-4\">
                <div class=\"metric-card bg-white border rounded shadow-sm\">
                    <div class=\"metric-value\" id=\"td-total-nodes\">0</div>
                    <div class=\"metric-label\">Total Nodes</div>
                </div>
                <div class=\"metric-card bg-white border rounded shadow-sm\">
                    <div class=\"metric-value\" id=\"td-total-tangles\">0</div>
                    <div class=\"metric-label\">Tangles Detected</div>
                </div>
                <div class=\"metric-card bg-white border rounded shadow-sm\">
                    <div class=\"metric-value\" id=\"td-pure-functions\">0</div>
                    <div class=\"metric-label\">Pure Functions</div>
                </div>
            </div>
            
            <div class=\"grid grid-cols-2 gap-4\">
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">DAG Visualization</div>
                    <div class=\"p-2\">
                        <svg id=\"dag-viz\" width=\"100%\" height=\"300\"></svg>
                        <div class=\"flex gap-2 mt-2 text-xs\">
                            <span class=\"flex items-center\"><span class=\"w-3 h-3 bg-green-500 rounded-full mr-1\"></span>Normal</span>
                            <span class=\"flex items-center\"><span class=\"w-3 h-3 bg-red-500 rounded-full mr-1\"></span>Tangle</span>
                            <span class=\"flex items-center\"><span class=\"w-3 h-3 bg-yellow-500 rounded-full mr-1\"></span>High Coupling</span>
                            <span class=\"flex items-center\"><span class=\"w-3 h-3 bg-orange-500 rounded-full mr-1\"></span>High Complexity</span>
                        </div>
                    </div>
                </div>
                
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Refactoring Suggestions</div>
                    <div class=\"p-3 max-h-64 overflow-y-auto\" id=\"refactoring-suggestions\">
                        <p class=\"text-gray-500 text-xs\">Analyze code to see suggestions</p>
                    </div>
                </div>
            </div>
            
            <div class=\"mt-4 bg-white border rounded shadow-sm\">
                <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Analyze Code</div>
                <div class=\"p-3\">
                    <textarea id=\"code-input\" class=\"w-full border rounded px-2 py-1 text-xs h-32 font-mono\" placeholder=\"Paste code or JSON dependency graph...\"></textarea>
                    <div class=\"flex gap-2 mt-2\">
                                                <button class=\"btn-primary px-3 py-1 text-xs rounded\" onclick=\"analyzeCodeDAG()\">ANALYZE DAG</button>
                                                <button class=\"btn-accent px-3 py-1 text-xs rounded\" onclick=\"detectTangles()\">DETECT TANGLES</button>
                                                <button class=\"btn-primary px-3 py-1 text-xs rounded\" onclick=\"generateRefactoringPlan()\">GENERATE PLAN</button>
                                                <button class=\"btn-primary px-3 py-1 text-xs rounded\" onclick=\"llmRefactorSuggestion()\">LLM REFACTOR</button>
                    </div>
                </div>
            </div>
            
            <div class=\"mt-4 bg-white border rounded shadow-sm\">
                <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Refactoring Plan</div>
                <div class=\"p-3\" id=\"refactoring-plan\">
                    <p class=\"text-gray-500 text-xs\">Generate a plan to see prioritized refactoring steps</p>
                </div>
            </div>
        </div>
        <!-- Distributed Tab -->
        <div id=\"distributed\" class=\"p-4 hidden\">
                                                <div class=\"grid grid-cols-4 gap-4 mb-4\">
                                                    <div class=\"metric-card rounded shadow-sm\">
                                                        <div class=\"metric-value clickable\" id=\"dist-workers\" onclick=\"showProvenance('dist-workers')\">NA</div>
                                                        <div class=\"metric-label\">Active Workers (click for source)</div>
                                                    </div>
                                                    <div class=\"metric-card rounded shadow-sm\">
                                                        <div class=\"metric-value clickable\" id=\"dist-tasks\" onclick=\"showProvenance('dist-tasks')\">NA</div>
                                                        <div class=\"metric-label\">Tasks Queued (click for source)</div>
                                                    </div>
                                                    <div class=\"metric-card rounded shadow-sm\">
                                                        <div class=\"metric-value accent clickable\" id=\"dist-throughput\" onclick=\"showProvenance('dist-throughput')\">NA</div>
                                                        <div class=\"metric-label\">Tasks/Second (click for source)</div>
                                                    </div>
                                                    <div class=\"metric-card rounded shadow-sm\">
                                                        <div class=\"metric-value clickable\" id=\"dist-data\" onclick=\"showProvenance('dist-data')\">NA</div>
                                                        <div class=\"metric-label\">Data Processed (click for source)</div>
                                                    </div>
                                                </div>
            
            <div class=\"grid grid-cols-2 gap-4\">
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">WORKER NODES</div>
                    <div class=\"p-3 max-h-64 overflow-y-auto\" id=\"worker-list\">
                        <p class=\"text-gray-500 text-xs\">No workers connected. Start workers to begin distributed processing.</p>
                    </div>
                </div>
                
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">TASK QUEUE</div>
                    <div class=\"p-3 max-h-64 overflow-y-auto\" id=\"task-queue\">
                        <p class=\"text-gray-500 text-xs\">No tasks in queue.</p>
                    </div>
                </div>
            </div>
            
            <div class=\"mt-4 bg-white border rounded shadow-sm\">
                <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">DISTRIBUTED PROCESSING CONTROLS</div>
                <div class=\"p-3\">
                    <div class=\"grid grid-cols-4 gap-4\">
                        <div>
                            <label class=\"text-xs font-semibold\">Data Source Path</label>
                            <input type=\"text\" id=\"dist-path\" class=\"w-full border rounded px-2 py-1 text-xs mt-1\" placeholder=\"/path/to/data\">
                        </div>
                        <div>
                            <label class=\"text-xs font-semibold\">Worker Count</label>
                            <input type=\"number\" id=\"dist-workers-count\" class=\"w-full border rounded px-2 py-1 text-xs mt-1\" value=\"4\" min=\"1\" max=\"1000\">
                        </div>
                        <div>
                            <label class=\"text-xs font-semibold\">Batch Size</label>
                            <input type=\"number\" id=\"dist-batch\" class=\"w-full border rounded px-2 py-1 text-xs mt-1\" value=\"1000\" min=\"1\">
                        </div>
                        <div>
                            <label class=\"text-xs font-semibold\">Processing Mode</label>
                            <select id=\"dist-mode\" class=\"w-full border rounded px-2 py-1 text-xs mt-1\">
                                <option value=\"parallel\">Parallel</option>
                                <option value=\"sequential\">Sequential</option>
                                <option value=\"streaming\">Streaming</option>
                            </select>
                        </div>
                    </div>
                    <div class=\"flex gap-2 mt-4\">
                        <button class=\"btn-accent px-3 py-1 text-xs rounded\" onclick=\"startDistributed()\">START PROCESSING</button>
                        <button class=\"btn-secondary px-3 py-1 text-xs rounded\" onclick=\"stopDistributed()\">STOP</button>
                        <button class=\"btn-primary px-3 py-1 text-xs rounded\" onclick=\"scaleWorkers()\">SCALE WORKERS</button>
                    </div>
                </div>
            </div>
            
            <div class=\"mt-4 bg-white border rounded shadow-sm\">
                <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">PROCESSING LOG</div>
                <div class=\"p-3 max-h-48 overflow-y-auto font-mono text-xs\" id=\"dist-log\" style=\"background: var(--gray-900); color: var(--gray-300);\">
                    <p>[READY] Distributed processing system initialized</p>
                    <p>[INFO] Waiting for tasks...</p>
                </div>
            </div>
        </div>
    </div>
    
    <script>
        // Beast Mode state
        let beastModeActive = false;
        
        function toggleBeastMode() {
            beastModeActive = !beastModeActive;
            const btn = document.getElementById('beast-mode-btn');
            const indicator = document.getElementById('beast-mode-indicator');
            
            if (beastModeActive) {
                btn.classList.add('active');
                indicator.style.background = 'var(--white)';
                document.body.style.setProperty('--accent-red', '#ff0000');
                addToLog('[BEAST MODE] ACTIVATED - Maximum processing enabled');
                // Start continuous learning
                startContinuousLearning();
            } else {
                btn.classList.remove('active');
                indicator.style.background = 'var(--gray-500)';
                document.body.style.setProperty('--accent-red', '#cc1a1a');
                addToLog('[BEAST MODE] Deactivated');
                stopContinuousLearning();
            }
        }
        
        function addToLog(message) {
            const log = document.getElementById('dist-log');
            if (log) {
                const p = document.createElement('p');
                p.textContent = message;
                log.appendChild(p);
                log.scrollTop = log.scrollHeight;
            }
        }
        
        let continuousLearningInterval = null;
        
                function startContinuousLearning() {
                    if (continuousLearningInterval) return;
                    // NOTE: This should be replaced with real API calls to get actual learning metrics
                    // For now, show NA to indicate no real data is available
                    document.getElementById('dist-throughput').textContent = 'NA';
                    addToLog('[LEARNING] Waiting for real metrics from backend...');
                    // TODO: Replace with real API polling
                    // continuousLearningInterval = setInterval(async () => {
                    //     const response = await fetch('/api/learning/status');
                    //     const data = await response.json();
                    //     document.getElementById('dist-throughput').textContent = data.throughput || 'NA';
                    // }, 2000);
                }
        
        function stopContinuousLearning() {
            if (continuousLearningInterval) {
                clearInterval(continuousLearningInterval);
                continuousLearningInterval = null;
            }
        }
        
        // Tab switching with nav highlighting
        function showTab(tabId) {
            document.querySelectorAll('#app > div:not(:first-child):not(:nth-child(2))').forEach(el => el.classList.add('hidden'));
            document.getElementById(tabId).classList.remove('hidden');
            
            // Update nav tabs
            document.querySelectorAll('.nav-tab').forEach(tab => tab.classList.remove('active'));
            event.target.classList.add('active');
        }
        
        // Load data on page load
        async function loadData() {
            const response = await fetch('/api/models');
            const data = await response.json();
            
            document.getElementById('total-models').textContent = data.total_models;
            document.getElementById('total-failures').textContent = data.total_failure_modes;
            document.getElementById('total-categories').textContent = Object.keys(data.categories).length;
            
            // Populate categories table
            const categoriesTable = document.getElementById('categories-table');
            categoriesTable.innerHTML = '';
            for (const [cat, models] of Object.entries(data.categories)) {
                const row = document.createElement('tr');
                row.innerHTML = `
                    <td>${cat}</td>
                    <td>${models.length}</td>
                    <td><div class=\"bg-gray-200 h-2 rounded\"><div class=\"bg-blue-600 h-2 rounded\" style=\"width: ${Math.min(100, models.length * 10)}%\"></div></div></td>
                `;
                categoriesTable.appendChild(row);
            }
            
            // Populate models table
            const modelsTable = document.getElementById('models-table');
            modelsTable.innerHTML = '';
            for (const [name, model] of Object.entries(data.models)) {
                const row = document.createElement('tr');
                row.className = 'hover:bg-blue-50 cursor-pointer';
                row.onclick = () => showModelDetail(model);
                row.innerHTML = `
                    <td>${model.name}</td>
                    <td>${model.category}</td>
                    <td>${model.originator || '-'}</td>
                    <td><span class=\"bg-red-100 text-red-800 px-1 rounded\">${model.failure_modes?.length || 0}</span></td>
                `;
                modelsTable.appendChild(row);
            }
        }
        
        function showModelDetail(model) {
            const detail = document.getElementById('model-detail');
            detail.innerHTML = `
                <h3 class=\"font-bold text-sm\">${model.name}</h3>
                <p class=\"text-xs text-gray-600 mt-1\">${model.description}</p>
                <p class=\"text-xs mt-2\"><strong>Key Insight:</strong> ${model.key_insight}</p>
                <p class=\"text-xs mt-1\"><strong>Application:</strong> ${model.application}</p>
                <h4 class=\"font-semibold text-xs mt-3\">Failure Modes:</h4>
                <table class=\"w-full dense-table text-xs mt-1\">
                    <thead><tr class=\"bg-gray-50\"><th>Name</th><th>Severity</th><th>Description</th></tr></thead>
                    <tbody>
                        ${(model.failure_modes || []).map(fm => `
                            <tr>
                                <td>${fm.name}</td>
                                <td><span class=\"${fm.severity === 'high' || fm.severity === 'critical' ? 'bg-red-100 text-red-800' : 'bg-yellow-100 text-yellow-800'} px-1 rounded\">${fm.severity}</span></td>
                                <td>${fm.description}</td>
                            </tr>
                        `).join('')}
                    </tbody>
                </table>
            `;
        }
        
        function filterModels() {
            const query = document.getElementById('model-search').value.toLowerCase();
            document.querySelectorAll('#models-table tr').forEach(row => {
                const text = row.textContent.toLowerCase();
                row.style.display = text.includes(query) ? '' : 'none';
            });
        }
        
        async function runAnalysis() {
            const input = document.getElementById('analysis-input').value;
            const response = await fetch('/api/analysis/latticework', {
                method: 'POST',
                headers: {'Content-Type': 'application/json'},
                body: JSON.stringify({context: input})
            });
            const data = await response.json();
            document.getElementById('analysis-result').innerHTML = `
                <div class=\"bg-blue-50 p-2 rounded\">
                    <p><strong>Models Applied:</strong> ${data.models_applied}</p>
                    <p><strong>Confidence:</strong> ${(data.combined_confidence * 100).toFixed(1)}%</p>
                    <p><strong>Recommendation:</strong> ${data.recommendation?.message || 'N/A'}</p>
                </div>
            `;
        }
        
        async function detectBiases() {
            const input = document.getElementById('analysis-input').value;
            const response = await fetch('/api/analysis/bias-detection', {
                method: 'POST',
                headers: {'Content-Type': 'application/json'},
                body: JSON.stringify({text: input})
            });
            const data = await response.json();
            document.getElementById('analysis-result').innerHTML = `
                <div class=\"bg-yellow-50 p-2 rounded\">
                    <p><strong>Risk Level:</strong> ${data.risk_level}</p>
                    <p><strong>Biases Detected:</strong> ${data.total_biases}</p>
                    ${data.biases_detected?.map(b => `<p class=\"text-xs\">- ${b.bias}: ${b.triggers.join(', ')}</p>`).join('') || ''}
                </div>
            `;
        }
        
        async function runStatistics() {
            const xStr = document.getElementById('stats-x').value;
            const yStr = document.getElementById('stats-y').value;
            const x = xStr.split(',').map(n => parseFloat(n.trim())).filter(n => !isNaN(n));
            const y = yStr.split(',').map(n => parseFloat(n.trim())).filter(n => !isNaN(n));
            
            const response = await fetch('/api/statistics/correlation', {
                method: 'POST',
                headers: {'Content-Type': 'application/json'},
                body: JSON.stringify({x, y})
            });
            const data = await response.json();
            document.getElementById('stats-result').innerHTML = `
                <div class=\"grid grid-cols-4 gap-4\">
                    <div class=\"metric-card bg-gray-50 rounded\">
                        <div class=\"metric-value\">${data.pearson?.toFixed(3) || 'N/A'}</div>
                        <div class=\"metric-label\">Pearson r</div>
                    </div>
                    <div class=\"metric-card bg-gray-50 rounded\">
                        <div class=\"metric-value\">${data.spearman?.toFixed(3) || 'N/A'}</div>
                        <div class=\"metric-label\">Spearman rho</div>
                    </div>
                    <div class=\"metric-card bg-gray-50 rounded\">
                        <div class=\"metric-value\">${data.r_squared?.toFixed(3) || 'N/A'}</div>
                        <div class=\"metric-label\">R-squared</div>
                    </div>
                    <div class=\"metric-card bg-gray-50 rounded\">
                        <div class=\"metric-value\">${data.pearson_interpretation || 'N/A'}</div>
                        <div class=\"metric-label\">Interpretation</div>
                    </div>
                </div>
            `;
        }
        
        async function analyzeDocument() {
            const input = document.getElementById('data-input').value;
            const response = await fetch('/api/data/analyze', {
                method: 'POST',
                headers: {'Content-Type': 'application/json'},
                body: JSON.stringify({text: input})
            });
            const data = await response.json();
            document.getElementById('data-result').innerHTML = `
                <div class=\"grid grid-cols-4 gap-4\">
                    <div class=\"metric-card bg-gray-50 rounded\">
                        <div class=\"metric-value\">${data.word_count || 0}</div>
                        <div class=\"metric-label\">Words</div>
                    </div>
                    <div class=\"metric-card bg-gray-50 rounded\">
                        <div class=\"metric-value\">${data.sentence_count || 0}</div>
                        <div class=\"metric-label\">Sentences</div>
                    </div>
                    <div class=\"metric-card bg-gray-50 rounded\">
                        <div class=\"metric-value\">${data.estimated_reading_time_minutes?.toFixed(1) || 0}</div>
                        <div class=\"metric-label\">Reading Time (min)</div>
                    </div>
                    <div class=\"metric-card bg-gray-50 rounded\">
                        <div class=\"metric-value\">${data.complexity_estimate || 'N/A'}</div>
                        <div class=\"metric-label\">Complexity</div>
                    </div>
                </div>
            `;
        }
        
        // World Map functionality
        let worldMap = null;
        let caseStudies = [];
        const modelMarkers = [];
        
        // Case studies loaded from database (no fake/sample data)
        // All data must be real and user-added
        
        function initMap() {
            if (worldMap) return;
            
            worldMap = L.map('world-map').setView([20, 0], 2);
            L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
                attribution: '© OpenStreetMap contributors'
            }).addTo(worldMap);
            
            // Load real case studies from database
            loadCaseStudiesFromDB();
            
            // Populate model dropdown
            loadData().then(() => {
                const select = document.getElementById('case-model');
                select.innerHTML = '<option value=\"\">Select Model...</option>';
                for (const [name, model] of Object.entries(window.modelsData || {})) {
                    const option = document.createElement('option');
                    option.value = model.name;
                    option.textContent = model.name + ' (' + model.category + ')';
                    select.appendChild(option);
                }
            });
        }
        
        function addMarkerToMap(caseStudy) {
            const marker = L.marker([caseStudy.lat, caseStudy.lng]).addTo(worldMap);
            marker.bindPopup(`
                <div class=\"model-popup\">
                    <h4>${caseStudy.title}</h4>
                    <div class=\"category\">Model: ${caseStudy.model}</div>
                    <p style=\"font-size: 11px; margin-top: 4px;\">${caseStudy.description}</p>
                </div>
            `);
            marker.on('click', () => showCaseStudyDetail(caseStudy));
            modelMarkers.push(marker);
            caseStudies.push(caseStudy);
        }
        
        function showCaseStudyDetail(cs) {
            document.getElementById('map-case-studies').innerHTML = `
                <h4 class=\"font-bold text-sm\">${cs.title}</h4>
                <p class=\"text-xs text-gray-600\">Model: ${cs.model}</p>
                <p class=\"text-xs mt-2\">${cs.description}</p>
                <p class=\"text-xs mt-2 text-gray-500\">Location: ${cs.lat.toFixed(4)}, ${cs.lng.toFixed(4)}</p>
            `;
        }
        
        function addCaseStudy() {
            const title = document.getElementById('case-title').value;
            const lat = parseFloat(document.getElementById('case-lat').value);
            const lng = parseFloat(document.getElementById('case-lng').value);
            const model = document.getElementById('case-model').value;
            const description = document.getElementById('case-description').value;
            
            if (!title || isNaN(lat) || isNaN(lng)) {
                alert('Please fill in title, latitude, and longitude');
                return;
            }
            
            addMarkerToMap({ lat, lng, title, model, description });
            
            // Clear form
            document.getElementById('case-title').value = '';
            document.getElementById('case-lat').value = '';
            document.getElementById('case-lng').value = '';
            document.getElementById('case-description').value = '';
        }
        
        // Load real case studies from database
        async function loadCaseStudiesFromDB() {
            try {
                const response = await fetch('/api/case-studies');
                if (response.ok) {
                    const data = await response.json();
                    if (data.case_studies && Array.isArray(data.case_studies)) {
                        data.case_studies.forEach(cs => addMarkerToMap(cs));
                    }
                }
            } catch (e) {
                console.log('No case studies loaded from database yet');
            }
        }
        
        // LLM functionality
        async function checkLLMStatus() {
            try {
                const response = await fetch('/api/llm/status');
                const data = await response.json();
                const statusEl = document.getElementById('llm-status');
                if (data.status === 'connected') {
                    statusEl.textContent = 'Connected';
                    statusEl.className = 'text-xs bg-green-100 text-green-800 px-2 py-0.5 rounded';
                } else {
                    statusEl.textContent = 'Disconnected';
                    statusEl.className = 'text-xs bg-red-100 text-red-800 px-2 py-0.5 rounded';
                }
            } catch (e) {
                document.getElementById('llm-status').textContent = 'Error';
            }
        }
        
        async function runLLMAnalysis() {
            const input = document.getElementById('llm-input').value;
            document.getElementById('llm-result').innerHTML = '<p class=\"text-gray-500 text-xs\">Analyzing with LLM...</p>';
            
            const response = await fetch('/api/llm/analyze', {
                method: 'POST',
                headers: {'Content-Type': 'application/json'},
                body: JSON.stringify({situation: input, models: []})
            });
            const data = await response.json();
            
            if (data.success) {
                document.getElementById('llm-result').innerHTML = `
                    <div class=\"bg-purple-50 p-2 rounded\">
                        <p class=\"text-xs font-semibold\">LLM-Powered Analysis</p>
                        <pre class=\"text-xs mt-2 whitespace-pre-wrap\">${data.analysis}</pre>
                        <p class=\"text-xs mt-2 text-gray-500\">Models used: ${data.models_used?.join(', ') || 'N/A'}</p>
                    </div>
                `;
            } else {
                document.getElementById('llm-result').innerHTML = `
                    <div class=\"bg-yellow-50 p-2 rounded\">
                        <p class=\"text-xs text-yellow-800\">${data.error}</p>
                        <p class=\"text-xs mt-2\">Fallback analysis:</p>
                        <pre class=\"text-xs mt-1\">${JSON.stringify(data.fallback, null, 2)}</pre>
                    </div>
                `;
            }
        }
        
        async function runLLMBiases() {
            const input = document.getElementById('llm-input').value;
            document.getElementById('llm-result').innerHTML = '<p class=\"text-gray-500 text-xs\">Detecting biases with LLM...</p>';
            
            const response = await fetch('/api/llm/biases', {
                method: 'POST',
                headers: {'Content-Type': 'application/json'},
                body: JSON.stringify({text: input})
            });
            const data = await response.json();
            
            if (data.success) {
                document.getElementById('llm-result').innerHTML = `
                    <div class=\"bg-purple-50 p-2 rounded\">
                        <p class=\"text-xs font-semibold\">LLM Bias Detection</p>
                        <pre class=\"text-xs mt-2 whitespace-pre-wrap\">${data.analysis}</pre>
                    </div>
                `;
            } else {
                document.getElementById('llm-result').innerHTML = `
                    <div class=\"bg-yellow-50 p-2 rounded\">
                        <p class=\"text-xs text-yellow-800\">${data.error}</p>
                    </div>
                `;
            }
        }
        
        async function runLLMChecklist() {
            const input = document.getElementById('llm-input').value;
            document.getElementById('llm-result').innerHTML = '<p class=\"text-gray-500 text-xs\">Generating checklist with LLM...</p>';
            
            const response = await fetch('/api/llm/checklist', {
                method: 'POST',
                headers: {'Content-Type': 'application/json'},
                body: JSON.stringify({context: input})
            });
            const data = await response.json();
            
            if (data.success) {
                document.getElementById('llm-result').innerHTML = `
                    <div class=\"bg-purple-50 p-2 rounded\">
                        <p class=\"text-xs font-semibold\">Decision Checklist</p>
                        <pre class=\"text-xs mt-2 whitespace-pre-wrap\">${data.checklist}</pre>
                    </div>
                `;
            } else {
                document.getElementById('llm-result').innerHTML = `
                    <div class=\"bg-yellow-50 p-2 rounded\">
                        <p class=\"text-xs text-yellow-800\">${data.error}</p>
                    </div>
                `;
            }
        }
        
        // Modified tab switching to initialize map when needed
        function showTab(tabId) {
            document.querySelectorAll('#app > div:not(:first-child):not(:nth-child(2))').forEach(el => el.classList.add('hidden'));
            document.getElementById(tabId).classList.remove('hidden');
            
            if (tabId === 'map') {
                setTimeout(initMap, 100);
            }
            if (tabId === 'llm') {
                checkLLMStatus();
            }
        }
        
                // Store models data globally for map dropdown
                window.modelsData = {};
        
                // DATA PROVENANCE SYSTEM - Track where every metric comes from
                // This is CRITICAL for a life-critical system - no fake data allowed
                window.dataProvenance = {
                    'total-models': {
                        label: 'Total Models',
                        source: 'NA - Not yet loaded',
                        endpoint: '/api/models',
                        field: 'data.total_models',
                        rawValue: null,
                        timestamp: null,
                        isReal: false
                    },
                    'total-failures': {
                        label: 'Total Failure Modes',
                        source: 'NA - Not yet loaded',
                        endpoint: '/api/models',
                        field: 'data.total_failure_modes',
                        rawValue: null,
                        timestamp: null,
                        isReal: false
                    },
                    'total-categories': {
                        label: 'Total Categories',
                        source: 'NA - Not yet loaded',
                        endpoint: '/api/models',
                        field: 'Object.keys(data.categories).length',
                        formula: 'Count of unique category keys from API response',
                        rawValue: null,
                        timestamp: null,
                        isReal: false
                    },
                    'avg-failures': {
                        label: 'Average Failures per Model',
                        source: 'NA - Not yet loaded',
                        endpoint: '/api/models',
                        field: 'Calculated',
                        formula: 'total_failure_modes / total_models',
                        rawValue: null,
                        numerator: null,
                        denominator: null,
                        timestamp: null,
                        isReal: false
                    },
                    'coverage': {
                        label: 'Coverage Percentage',
                        source: 'NA - Not yet loaded',
                        endpoint: '/api/models',
                        field: 'Calculated',
                        formula: '(models_with_failure_modes / total_models) * 100',
                        rawValue: null,
                        numerator: null,
                        denominator: null,
                        timestamp: null,
                        isReal: false
                    },
                                    'api-status': {
                                        label: 'API Status',
                                        source: 'NA - Not yet loaded',
                                        endpoint: '/api/models',
                                        field: 'HTTP response status',
                                        rawValue: null,
                                        timestamp: null,
                                        isReal: false
                                    },
                                    // Distributed Processing Metrics - all show NA until real backend is connected
                                    'dist-workers': {
                                        label: 'Active Workers',
                                        source: 'NA - No distributed backend connected',
                                        endpoint: '/api/distributed/workers (NOT IMPLEMENTED)',
                                        field: 'User input from form',
                                        rawValue: null,
                                        timestamp: null,
                                        isReal: false,
                                        note: 'This metric shows the number of workers configured by the user. Real worker status requires a distributed backend.'
                                    },
                                    'dist-tasks': {
                                        label: 'Tasks Queued',
                                        source: 'NA - No distributed backend connected',
                                        endpoint: '/api/distributed/tasks (NOT IMPLEMENTED)',
                                        field: 'N/A - requires real task queue',
                                        rawValue: null,
                                        timestamp: null,
                                        isReal: false,
                                        note: 'This metric requires a real distributed task queue backend (e.g., Redis, RabbitMQ) to show actual queued tasks.'
                                    },
                                    'dist-throughput': {
                                        label: 'Tasks Per Second',
                                        source: 'NA - No distributed backend connected',
                                        endpoint: '/api/distributed/throughput (NOT IMPLEMENTED)',
                                        field: 'N/A - requires real processing metrics',
                                        rawValue: null,
                                        timestamp: null,
                                        isReal: false,
                                        note: 'This metric requires real-time monitoring of task processing to show actual throughput.'
                                    },
                                    'dist-data': {
                                        label: 'Data Processed',
                                        source: 'NA - No distributed backend connected',
                                        endpoint: '/api/distributed/data (NOT IMPLEMENTED)',
                                        field: 'N/A - requires real data tracking',
                                        rawValue: null,
                                        timestamp: null,
                                        isReal: false,
                                        note: 'This metric requires tracking actual data processed by workers to show real values.'
                                    }
                                };
        
                function showProvenance(metricId) {
                    const prov = window.dataProvenance[metricId];
                    if (!prov) {
                        alert('No provenance data available for this metric');
                        return;
                    }
            
                    const modal = document.getElementById('provenance-modal');
                    const title = document.getElementById('provenance-title');
                    const body = document.getElementById('provenance-body');
            
                    title.textContent = 'DATA PROVENANCE: ' + prov.label;
            
                    let html = '';
            
                    // Data status
                    html += '<div class=\"provenance-row\">';
                    html += '<div class=\"provenance-label\">Data Status</div>';
                    html += '<div class=\"provenance-value ' + (prov.isReal ? 'real' : 'na') + '\">' + (prov.isReal ? 'REAL DATA' : 'NO DATA AVAILABLE') + '</div>';
                    html += '</div>';
            
                    // Current value
                    html += '<div class=\"provenance-row\">';
                    html += '<div class=\"provenance-label\">Current Value</div>';
                    html += '<div class=\"provenance-value\">' + document.getElementById(metricId).textContent + '</div>';
                    html += '</div>';
            
                    // Source
                    html += '<div class=\"provenance-row\">';
                    html += '<div class=\"provenance-label\">Source</div>';
                    html += '<div class=\"provenance-value\">' + prov.source + '</div>';
                    html += '</div>';
            
                    // API Endpoint
                    html += '<div class=\"provenance-row\">';
                    html += '<div class=\"provenance-label\">API Endpoint</div>';
                    html += '<div class=\"provenance-value\">' + prov.endpoint + '</div>';
                    html += '</div>';
            
                    // Field
                    html += '<div class=\"provenance-row\">';
                    html += '<div class=\"provenance-label\">Data Field</div>';
                    html += '<div class=\"provenance-value\">' + prov.field + '</div>';
                    html += '</div>';
            
                    // Raw value
                    if (prov.rawValue !== null) {
                        html += '<div class=\"provenance-row\">';
                        html += '<div class=\"provenance-label\">Raw Value</div>';
                        html += '<div class=\"provenance-value\">' + JSON.stringify(prov.rawValue) + '</div>';
                        html += '</div>';
                    }
            
                    // Formula (if calculated)
                    if (prov.formula) {
                        html += '<div class=\"provenance-formula\">';
                        html += '<strong>Calculation:</strong> ' + prov.formula;
                        if (prov.numerator !== null && prov.denominator !== null) {
                            html += '<br><strong>Values:</strong> ' + prov.numerator + ' / ' + prov.denominator;
                        }
                        html += '</div>';
                    }
            
                                    // Timestamp
                                    html += '<div class=\"provenance-row\">';
                                    html += '<div class=\"provenance-label\">Last Updated</div>';
                                    html += '<div class=\"provenance-value\">' + (prov.timestamp || 'Never') + '</div>';
                                    html += '</div>';
                    
                                    // Note (for metrics that need explanation)
                                    if (prov.note) {
                                        html += '<div class=\"provenance-formula\" style=\"margin-top: 12px; border-left-color: var(--gray-500);\">';
                                        html += '<strong>Note:</strong> ' + prov.note;
                                        html += '</div>';
                                    }
            
                                    body.innerHTML = html;
                                    modal.classList.add('active');
                                }
        
                function closeProvenance() {
                    document.getElementById('provenance-modal').classList.remove('active');
                }
        
                                                                // Modified loadData to store models - ALL REAL CHECKS, NO HARDCODED VALUES
                                                        async function loadDataAndStore() {
                                                            const timestamp = new Date().toISOString();
                                                            try {
                                                                const response = await fetch('/api/models');
                                                                const data = await response.json();
                                                                window.modelsData = data.models;
                
                                                                // Calculate REAL metrics from actual data
                                                                const totalModels = data.total_models || 0;
                                                                const totalFailures = data.total_failure_modes || 0;
                                                                const totalCategories = Object.keys(data.categories || {}).length;
                        
                                                                // Calculate REAL average failures per model
                                                                const avgFailures = totalModels > 0 ? (totalFailures / totalModels).toFixed(1) : 'NA';
                        
                                                                // Calculate REAL coverage (models with at least one failure mode)
                                                                let modelsWithFailures = 0;
                                                                for (const [name, model] of Object.entries(data.models || {})) {
                                                                    if (model.failure_modes && model.failure_modes.length > 0) {
                                                                        modelsWithFailures++;
                                                                    }
                                                                }
                                                                const coverage = totalModels > 0 ? Math.round((modelsWithFailures / totalModels) * 100) + '%' : 'NA';
                        
                                                                // UPDATE DATA PROVENANCE - Track where every metric comes from
                                                                window.dataProvenance['total-models'].source = 'API Response: /api/models';
                                                                window.dataProvenance['total-models'].rawValue = data.total_models;
                                                                window.dataProvenance['total-models'].timestamp = timestamp;
                                                                window.dataProvenance['total-models'].isReal = totalModels > 0;
                                    
                                                                window.dataProvenance['total-failures'].source = 'API Response: /api/models';
                                                                window.dataProvenance['total-failures'].rawValue = data.total_failure_modes;
                                                                window.dataProvenance['total-failures'].timestamp = timestamp;
                                                                window.dataProvenance['total-failures'].isReal = totalFailures > 0;
                                    
                                                                window.dataProvenance['total-categories'].source = 'Calculated from API Response';
                                                                window.dataProvenance['total-categories'].rawValue = Object.keys(data.categories || {});
                                                                window.dataProvenance['total-categories'].timestamp = timestamp;
                                                                window.dataProvenance['total-categories'].isReal = totalCategories > 0;
                                    
                                                                window.dataProvenance['avg-failures'].source = 'Calculated from API Response';
                                                                window.dataProvenance['avg-failures'].rawValue = avgFailures;
                                                                window.dataProvenance['avg-failures'].numerator = totalFailures;
                                                                window.dataProvenance['avg-failures'].denominator = totalModels;
                                                                window.dataProvenance['avg-failures'].timestamp = timestamp;
                                                                window.dataProvenance['avg-failures'].isReal = totalModels > 0;
                                    
                                                                window.dataProvenance['coverage'].source = 'Calculated from API Response';
                                                                window.dataProvenance['coverage'].rawValue = coverage;
                                                                window.dataProvenance['coverage'].numerator = modelsWithFailures;
                                                                window.dataProvenance['coverage'].denominator = totalModels;
                                                                window.dataProvenance['coverage'].timestamp = timestamp;
                                                                window.dataProvenance['coverage'].isReal = totalModels > 0;
                                    
                                                                window.dataProvenance['api-status'].source = 'HTTP Response Status';
                                                                window.dataProvenance['api-status'].rawValue = response.status;
                                                                window.dataProvenance['api-status'].timestamp = timestamp;
                                                                window.dataProvenance['api-status'].isReal = true;
                        
                                                                // Update dashboard counts from REAL API data - show NA if no data
                                                                document.getElementById('total-models').textContent = totalModels > 0 ? totalModels : 'NA';
                                                                document.getElementById('total-failures').textContent = totalFailures > 0 ? totalFailures : 'NA';
                                                                document.getElementById('total-categories').textContent = totalCategories > 0 ? totalCategories : 'NA';
                                                                document.getElementById('avg-failures').textContent = avgFailures;
                                                                document.getElementById('coverage').textContent = coverage;
                                                                document.getElementById('api-status').textContent = 'Live';
                        
                                                                // Update header counts dynamically from REAL data
                                                                document.getElementById('header-models').textContent = totalModels > 0 ? totalModels : 'NA';
                                                                document.getElementById('header-failures').textContent = totalFailures > 0 ? totalFailures : 'NA';
                                                            } catch (error) {
                                                                console.error('Failed to load models:', error);
                                                                // Update provenance to show error state
                                                                const errorTimestamp = new Date().toISOString();
                                                                for (const key of Object.keys(window.dataProvenance)) {
                                                                    window.dataProvenance[key].source = 'ERROR: API call failed - ' + error.message;
                                                                    window.dataProvenance[key].timestamp = errorTimestamp;
                                                                    window.dataProvenance[key].isReal = false;
                                                                }
                                                                // Show NA for all metrics on error - NEVER show fake data
                                                                document.getElementById('total-models').textContent = 'NA';
                                                                document.getElementById('total-failures').textContent = 'NA';
                                                                document.getElementById('total-categories').textContent = 'NA';
                                                                document.getElementById('avg-failures').textContent = 'NA';
                                                                document.getElementById('coverage').textContent = 'NA';
                                                                document.getElementById('api-status').textContent = 'Error';
                                                                document.getElementById('header-models').textContent = 'NA';
                                                                document.getElementById('header-failures').textContent = 'NA';
                                                                return;
                                                            }
            
                        // Populate categories table
            const categoriesTable = document.getElementById('categories-table');
            categoriesTable.innerHTML = '';
            for (const [cat, models] of Object.entries(data.categories)) {
                const row = document.createElement('tr');
                row.innerHTML = `
                    <td>${cat}</td>
                    <td>${models.length}</td>
                    <td><div class=\"bg-gray-200 h-2 rounded\"><div class=\"bg-blue-600 h-2 rounded\" style=\"width: ${Math.min(100, models.length * 10)}%\"></div></div></td>
                `;
                categoriesTable.appendChild(row);
            }
            
            // Populate models table
            const modelsTable = document.getElementById('models-table');
            modelsTable.innerHTML = '';
            for (const [name, model] of Object.entries(data.models)) {
                const row = document.createElement('tr');
                row.className = 'hover:bg-blue-50 cursor-pointer';
                row.onclick = () => showModelDetail(model);
                row.innerHTML = `
                    <td>${model.name}</td>
                    <td>${model.category}</td>
                    <td>${model.originator || '-'}</td>
                    <td><span class=\"bg-red-100 text-red-800 px-1 rounded\">${model.failure_modes?.length || 0}</span></td>
                `;
                modelsTable.appendChild(row);
            }
        }
        
        // Load data on page load
        loadDataAndStore();
        
        // ============================================
        // Distributed Processing Functions
        // ============================================
        
        let distributedProcessing = false;
        let workerCount = 0;
        
        async function startDistributed() {
            const path = document.getElementById('dist-path').value;
            const workers = parseInt(document.getElementById('dist-workers-count').value);
            const batch = parseInt(document.getElementById('dist-batch').value);
            const mode = document.getElementById('dist-mode').value;
            
            if (!path) {
                alert('Please enter a data source path');
                return;
            }
            
            distributedProcessing = true;
            workerCount = workers;
            
            addToLog('[START] Initializing distributed processing...');
            addToLog('[CONFIG] Path: ' + path + ', Workers: ' + workers + ', Batch: ' + batch + ', Mode: ' + mode);
            
                        // Update metrics - show NA until real data available
                        document.getElementById('dist-workers').textContent = workers;
                        document.getElementById('dist-tasks').textContent = 'NA'; // Will be updated with real task count
            
                        // Show worker status - CPU will show NA until real monitoring available
                        const workerList = document.getElementById('worker-list');
                        workerList.innerHTML = '';
                        for (let i = 0; i < Math.min(workers, 10); i++) {
                            const div = document.createElement('div');
                            div.className = 'flex justify-between items-center py-1 border-b text-xs';
                            div.innerHTML = `
                                <span>Worker-${i + 1}</span>
                                <span style=\"color: var(--accent-red);\">PENDING</span>
                                <span>NA</span>
                            `;
                            workerList.appendChild(div);
                        }
            if (workers > 10) {
                const more = document.createElement('p');
                more.className = 'text-xs text-gray-500 mt-2';
                more.textContent = '... and ' + (workers - 10) + ' more workers';
                workerList.appendChild(more);
            }
            
            addToLog('[READY] ' + workers + ' workers initialized');
            
            // Start processing simulation
            simulateProcessing();
        }
        
                function stopDistributed() {
                    distributedProcessing = false;
                    addToLog('[STOP] Distributed processing stopped');
                    document.getElementById('dist-workers').textContent = 'NA';
                    document.getElementById('dist-tasks').textContent = 'NA';
                    document.getElementById('dist-throughput').textContent = 'NA';
                }
        
        function scaleWorkers() {
            const newCount = parseInt(document.getElementById('dist-workers-count').value);
            addToLog('[SCALE] Scaling workers from ' + workerCount + ' to ' + newCount);
            workerCount = newCount;
            document.getElementById('dist-workers').textContent = newCount;
        }
        
                function simulateProcessing() {
                    // NOTE: This function should be replaced with real API calls to get actual processing metrics
                    // For now, show NA to indicate no real data is available
                    if (!distributedProcessing) return;
            
                    // TODO: Replace with real API call to /api/distributed/status
                    // const response = await fetch('/api/distributed/status');
                    // const data = await response.json();
            
                    // Show NA until real metrics are available from the backend
                    document.getElementById('dist-throughput').textContent = 'NA';
                    document.getElementById('dist-tasks').textContent = 'NA';
                    document.getElementById('dist-data').textContent = 'NA';
            
            // Add to task queue display
            const taskQueue = document.getElementById('task-queue');
            if (tasks > 0) {
                taskQueue.innerHTML = `
                    <div class=\"text-xs\">
                        <p>Pending: ${tasks} tasks</p>
                        <p>Processing: ${throughput}/sec</p>
                        <p style=\"color: var(--accent-red);\">ETA: ${Math.ceil(tasks / throughput)} seconds</p>
                    </div>
                `;
            } else {
                taskQueue.innerHTML = '<p class=\"text-xs\" style=\"color: var(--accent-red);\">All tasks completed!</p>';
                distributedProcessing = false;
                addToLog('[COMPLETE] All tasks processed successfully');
                return;
            }
            
            setTimeout(simulateProcessing, 1000);
        }
        
        // ============================================
        // Tech Debt Functions
        // ============================================
        
        // Store current DAG data
        window.currentDAG = null;
        
        // Parse code input to DAG format
        function parseCodeToDAG(input) {
            try {
                // Try to parse as JSON first
                return JSON.parse(input);
            } catch (e) {
                // Parse as simple dependency format: function_name: dep1, dep2, dep3
                const lines = input.split('\\n').filter(l => l.trim());
                const functions = {};
                const dependencies = [];
                
                lines.forEach((line, idx) => {
                    const match = line.match(/^([\\w-]+):\\s*(.*)$/);
                    if (match) {
                        const funcName = match[1];
                        const deps = match[2].split(',').map(d => d.trim()).filter(d => d);
                                                functions[funcName] = {
                                                    name: funcName,
                                                    type: 'function',
                                                    complexity: 'NA' // TODO: Calculate real complexity from code analysis
                                                };
                        deps.forEach(dep => {
                            dependencies.push({ from: funcName, to: dep, type: 'calls' });
                        });
                    }
                });
                
                return { functions, dependencies };
            }
        }
        
        // Analyze DAG
        async function analyzeCodeDAG() {
            const input = document.getElementById('code-input').value;
            if (!input.trim()) {
                alert('Please enter code or dependency graph');
                return;
            }
            
            const dagData = parseCodeToDAG(input);
            window.currentDAG = dagData;
            
            const response = await fetch('/api/techdebt/analyze', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ dag: dagData })
            });
            const data = await response.json();
            
            // Update metrics
            document.getElementById('td-total-nodes').textContent = data.total_nodes || 0;
            document.getElementById('td-total-tangles').textContent = data.tangles?.length || 0;
            document.getElementById('td-pure-functions').textContent = data.pure_functions || 0;
            
            // Update suggestions
            const suggestions = document.getElementById('refactoring-suggestions');
            if (data.tangles && data.tangles.length > 0) {
                suggestions.innerHTML = data.tangles.map((t, i) => `
                    <div class=\"mb-2 p-2 bg-red-50 rounded\">
                        <p class=\"text-xs font-semibold\">Tangle ${i + 1}: ${t.nodes?.length || 0} nodes</p>
                        <p class=\"text-xs text-gray-600\">${t.suggestion || 'Consider extracting pure functions'}</p>
                    </div>
                `).join('');
            } else {
                suggestions.innerHTML = '<p class=\"text-green-600 text-xs\">No tangles detected - code is well-structured!</p>';
            }
            
            // Visualize DAG
            visualizeDAG(data);
        }
        
        // Detect tangles
        async function detectTangles() {
            const input = document.getElementById('code-input').value;
            if (!input.trim()) {
                alert('Please enter code or dependency graph');
                return;
            }
            
            const dagData = parseCodeToDAG(input);
            window.currentDAG = dagData;
            
            const response = await fetch('/api/techdebt/tangles', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ dag: dagData })
            });
            const data = await response.json();
            
            document.getElementById('td-total-tangles').textContent = data.length || 0;
            
            const suggestions = document.getElementById('refactoring-suggestions');
            if (data.length > 0) {
                suggestions.innerHTML = data.map((t, i) => `
                    <div class=\"mb-2 p-2 bg-red-50 rounded cursor-pointer\" onclick=\"selectTangle(${i})\">
                        <p class=\"text-xs font-semibold\">Tangle ${i + 1}</p>
                        <p class=\"text-xs\">Nodes: ${t.nodes?.join(', ') || 'Unknown'}</p>
                        <p class=\"text-xs text-gray-600\">Severity: ${t.severity || 'Medium'}</p>
                    </div>
                `).join('');
            } else {
                suggestions.innerHTML = '<p class=\"text-green-600 text-xs\">No tangles detected!</p>';
            }
        }
        
        // Generate refactoring plan
        async function generateRefactoringPlan() {
            const input = document.getElementById('code-input').value;
            if (!input.trim()) {
                alert('Please enter code or dependency graph');
                return;
            }
            
            const dagData = parseCodeToDAG(input);
            
            const response = await fetch('/api/techdebt/plan', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ dag: dagData })
            });
            const data = await response.json();
            
            const planDiv = document.getElementById('refactoring-plan');
            if (data.steps && data.steps.length > 0) {
                planDiv.innerHTML = `
                    <div class=\"text-xs\">
                        <p class=\"font-semibold mb-2\">Priority: ${data.priority || 'Medium'}</p>
                        <ol class=\"list-decimal list-inside space-y-1\">
                            ${data.steps.map(s => `<li>${s}</li>`).join('')}
                        </ol>
                    </div>
                `;
            } else {
                planDiv.innerHTML = '<p class=\"text-green-600 text-xs\">No refactoring needed - code is clean!</p>';
            }
        }
        
        // LLM refactoring suggestion
        async function llmRefactorSuggestion() {
            const input = document.getElementById('code-input').value;
            if (!input.trim()) {
                alert('Please enter code or dependency graph');
                return;
            }
            
            const dagData = parseCodeToDAG(input);
            
            document.getElementById('refactoring-suggestions').innerHTML = '<p class=\"text-xs text-gray-500\">Asking LLM for suggestions...</p>';
            
            const response = await fetch('/api/techdebt/llm-refactor', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ dag: dagData, tangle: {} })
            });
            const data = await response.json();
            
            const suggestions = document.getElementById('refactoring-suggestions');
            if (data.success) {
                suggestions.innerHTML = `
                    <div class=\"bg-purple-50 p-2 rounded\">
                        <p class=\"text-xs font-semibold\">LLM Refactoring Suggestion</p>
                        <pre class=\"text-xs mt-2 whitespace-pre-wrap\">${data['refactoring-suggestion']}</pre>
                    </div>
                `;
            } else {
                suggestions.innerHTML = `
                    <div class=\"bg-yellow-50 p-2 rounded\">
                        <p class=\"text-xs text-yellow-800\">LLM not available. Generated prompt:</p>
                        <pre class=\"text-xs mt-2 whitespace-pre-wrap bg-gray-100 p-2 rounded\">${data.prompt || 'No prompt generated'}</pre>
                    </div>
                `;
            }
        }
        
        // Visualize DAG using SVG
        function visualizeDAG(data) {
            const svg = document.getElementById('dag-viz');
            svg.innerHTML = '';
            
            const nodes = data.nodes || [];
            const edges = data.edges || [];
            const tangles = new Set((data.tangles || []).flatMap(t => t.nodes || []));
            
            if (nodes.length === 0) {
                svg.innerHTML = '<text x=\"50%\" y=\"50%\" text-anchor=\"middle\" class=\"text-xs fill-gray-400\">No nodes to visualize</text>';
                return;
            }
            
            const width = svg.clientWidth || 400;
            const height = 300;
            const nodeRadius = 20;
            
            // Simple force-directed layout simulation
            const positions = {};
            nodes.forEach((node, i) => {
                const angle = (2 * Math.PI * i) / nodes.length;
                const radius = Math.min(width, height) / 3;
                positions[node.id] = {
                    x: width / 2 + radius * Math.cos(angle),
                    y: height / 2 + radius * Math.sin(angle)
                };
            });
            
            // Draw edges
            edges.forEach(edge => {
                const from = positions[edge.from];
                const to = positions[edge.to];
                if (from && to) {
                    const line = document.createElementNS('http://www.w3.org/2000/svg', 'line');
                    line.setAttribute('x1', from.x);
                    line.setAttribute('y1', from.y);
                    line.setAttribute('x2', to.x);
                    line.setAttribute('y2', to.y);
                    line.setAttribute('stroke', '#ccc');
                    line.setAttribute('stroke-width', '1');
                    line.setAttribute('marker-end', 'url(#arrowhead)');
                    svg.appendChild(line);
                }
            });
            
            // Add arrowhead marker
            const defs = document.createElementNS('http://www.w3.org/2000/svg', 'defs');
            defs.innerHTML = `
                <marker id=\"arrowhead\" markerWidth=\"10\" markerHeight=\"7\" refX=\"10\" refY=\"3.5\" orient=\"auto\">
                    <polygon points=\"0 0, 10 3.5, 0 7\" fill=\"#ccc\" />
                </marker>
            `;
            svg.appendChild(defs);
            
            // Draw nodes
            nodes.forEach(node => {
                const pos = positions[node.id];
                if (!pos) return;
                
                const group = document.createElementNS('http://www.w3.org/2000/svg', 'g');
                
                const circle = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
                circle.setAttribute('cx', pos.x);
                circle.setAttribute('cy', pos.y);
                circle.setAttribute('r', nodeRadius);
                
                // Color based on status
                let fill = '#22c55e'; // green - normal
                if (tangles.has(node.id)) {
                    fill = '#ef4444'; // red - tangle
                } else if (node.coupling > 5) {
                    fill = '#eab308'; // yellow - high coupling
                } else if (node.complexity > 7) {
                    fill = '#f97316'; // orange - high complexity
                }
                
                circle.setAttribute('fill', fill);
                circle.setAttribute('stroke', '#fff');
                circle.setAttribute('stroke-width', '2');
                group.appendChild(circle);
                
                const text = document.createElementNS('http://www.w3.org/2000/svg', 'text');
                text.setAttribute('x', pos.x);
                text.setAttribute('y', pos.y + 4);
                text.setAttribute('text-anchor', 'middle');
                text.setAttribute('fill', '#fff');
                text.setAttribute('font-size', '8');
                text.textContent = (node.name || node.id).substring(0, 4);
                group.appendChild(text);
                
                svg.appendChild(group);
            });
        }
        
        // Select a tangle for detailed view
        window.selectedTangle = null;
        function selectTangle(index) {
            window.selectedTangle = index;
            // Could highlight in visualization
        }
    </script>
</body>
</html>")

;; ============================================
;; API Handlers
;; ============================================

(defn json-response [data]
  (-> (response/response (cheshire.core/generate-string data))
      (response/content-type "application/json")))

(defn handle-get-models [request]
  (json-response (models/export-all-models)))

(defn handle-latticework [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        context (get body :context "")
        model-names (get body :models [])]
    (json-response (analysis/latticework-analyze model-names context))))

(defn handle-lollapalooza [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        context (get body :context "")
        model-names (get body :models [])]
    (json-response (analysis/detect-lollapalooza model-names context))))

(defn handle-inversion [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        problem (get body :problem "")]
    (json-response (analysis/invert problem))))

(defn handle-two-track [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        situation (get body :situation "")]
    (json-response (analysis/two-track-analysis situation))))

(defn handle-bias-detection [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        text (get body :text "")]
    (json-response (analysis/detect-biases text))))

(defn handle-correlation [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        xs (get body :x [])
        ys (get body :y [])]
    (json-response (stats/correlation-analysis xs ys))))

(defn handle-document-analysis [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        text (get body :text "")]
    (json-response (data/analyze-document text))))

;; LLM-powered handlers
(defn handle-llm-analyze [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        situation (get body :situation "")
        model-names (get body :models [])]
    (json-response (analyze-with-llm situation model-names))))

(defn handle-llm-biases [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        text (get body :text "")]
    (json-response (detect-biases-with-llm text))))

(defn handle-llm-checklist [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        context (get body :context "")]
    (json-response (generate-decision-checklist-with-llm context))))

(defn handle-llm-classify [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        text (get body :text "")]
    (json-response (classify-document-with-llm text))))

(defn handle-llm-status [request]
  (json-response {:lm_studio_url (:base-url lm-studio-config)
                  :model (:model lm-studio-config)
                  :status (if (call-lm-studio "test" :max-tokens 5) "connected" "disconnected")}))

;; Tech Debt handlers
(defn handle-analyze-dag [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        dag-data (get body :dag {:nodes {} :edges []})]
    (let [dag (tech-debt/build-dag-from-code dag-data)]
      (json-response (tech-debt/analyze-codebase dag)))))

(defn handle-detect-tangles [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        dag-data (get body :dag {:nodes {} :edges []})]
    (let [dag (tech-debt/build-dag-from-code dag-data)]
      (json-response (tech-debt/detect-tangles dag)))))

(defn handle-refactoring-plan [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        dag-data (get body :dag {:nodes {} :edges []})]
    (let [dag (tech-debt/build-dag-from-code dag-data)
          analysis (tech-debt/analyze-codebase dag)]
      (json-response (tech-debt/generate-refactoring-plan analysis)))))

(defn handle-llm-refactor [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        tangle (get body :tangle {})
        dag-data (get body :dag {:nodes {} :edges []})]
    (let [dag (tech-debt/build-dag-from-code dag-data)
          prompt (tech-debt/generate-llm-refactoring-prompt tangle dag)]
      (if-let [response (call-lm-studio prompt)]
        (json-response {:success true :refactoring-suggestion response})
        (json-response {:success false :error "LM Studio not available"
                       :prompt prompt})))))

(defn handle-dag-visualization [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        dag-data (get body :dag {:nodes {} :edges []})]
    (let [dag (tech-debt/build-dag-from-code dag-data)]
      (json-response (tech-debt/export-dag-for-visualization dag)))))

;; ============================================
;; Routes
;; ============================================

(defn app [request]
  (let [uri (:uri request)
        method (:request-method request)]
    (cond
      ;; Index
      (and (= method :get) (= uri "/"))
      (-> (response/response index-html)
          (response/content-type "text/html"))
      
      ;; Health
      (and (= method :get) (= uri "/health"))
      (json-response {:status "healthy" :service "mental-models-electric"})
      
      ;; Models API
      (and (= method :get) (= uri "/api/models"))
      (handle-get-models request)
      
      ;; Analysis API
      (and (= method :post) (= uri "/api/analysis/latticework"))
      (handle-latticework request)
      
      (and (= method :post) (= uri "/api/analysis/lollapalooza"))
      (handle-lollapalooza request)
      
      (and (= method :post) (= uri "/api/analysis/inversion"))
      (handle-inversion request)
      
      (and (= method :post) (= uri "/api/analysis/two-track"))
      (handle-two-track request)
      
      (and (= method :post) (= uri "/api/analysis/bias-detection"))
      (handle-bias-detection request)
      
      ;; Statistics API
      (and (= method :post) (= uri "/api/statistics/correlation"))
      (handle-correlation request)
      
      ;; Data API
      (and (= method :post) (= uri "/api/data/analyze"))
      (handle-document-analysis request)
      
      ;; LLM API (LM Studio integration)
      (and (= method :get) (= uri "/api/llm/status"))
      (handle-llm-status request)
      
      (and (= method :post) (= uri "/api/llm/analyze"))
      (handle-llm-analyze request)
      
      (and (= method :post) (= uri "/api/llm/biases"))
      (handle-llm-biases request)
      
      (and (= method :post) (= uri "/api/llm/checklist"))
      (handle-llm-checklist request)
      
      (and (= method :post) (= uri "/api/llm/classify"))
      (handle-llm-classify request)
      
      ;; Tech Debt API
      (and (= method :post) (= uri "/api/techdebt/analyze"))
      (handle-analyze-dag request)
      
      (and (= method :post) (= uri "/api/techdebt/tangles"))
      (handle-detect-tangles request)
      
      (and (= method :post) (= uri "/api/techdebt/plan"))
      (handle-refactoring-plan request)
      
      (and (= method :post) (= uri "/api/techdebt/llm-refactor"))
      (handle-llm-refactor request)
      
      (and (= method :post) (= uri "/api/techdebt/visualize"))
      (handle-dag-visualization request)
      
      ;; Distributed Processing API
      (and (= method :get) (= uri "/api/distributed/status"))
      (json-response (distributed/get-cluster-metrics))
      
      (and (= method :get) (= uri "/api/distributed/throughput"))
      (json-response {:throughput (distributed/get-throughput)})
      
      (and (= method :post) (= uri "/api/distributed/submit"))
      (let [body (parse-json-body request)
            work-type (keyword (get body "type" "analyze"))
            data (get body "data")
            priority (keyword (get body "priority" "normal"))]
        (json-response (distributed/submit-work work-type data :priority priority)))
      
      (and (= method :post) (= uri "/api/distributed/submit-bulk"))
      (let [body (parse-json-body request)
            work-items (get body "items" [])]
        (json-response (distributed/submit-bulk-work work-items)))
      
      (and (= method :post) (= uri "/api/distributed/start-workers"))
      (let [body (parse-json-body request)
            num-workers (get body "workers" 4)]
        (distributed/start-workers! num-workers)
        (json-response {:status "started" :workers num-workers}))
      
      (and (= method :post) (= uri "/api/distributed/stop"))
      (do
        (distributed/shutdown!)
        (json-response {:status "stopped"}))
      
      (and (= method :post) (= uri "/api/distributed/scale"))
      (let [body (parse-json-body request)
            target-workers (get body "target" 8)]
        (distributed/scale-workers! target-workers)
        (json-response {:status "scaling" :target target-workers}))
      
      ;; Continuous Learning API
      (and (= method :get) (= uri "/api/continuous/status"))
      (json-response (continuous/get-system-status))
      
      (and (= method :post) (= uri "/api/continuous/start"))
      (do
        (continuous/start-all-systems!)
        (json-response {:status "started"}))
      
      (and (= method :post) (= uri "/api/continuous/stop"))
      (do
        (continuous/stop-all-systems!)
        (json-response {:status "stopped"}))
      
      (and (= method :post) (= uri "/api/continuous/scraper/start"))
      (let [body (parse-json-body request)
            scraper-id (keyword (get body "id" "default"))
            urls (get body "urls" [])
            depth (get body "depth" 2)
            interval (get body "interval" 3600000)]
        (continuous/start-scraper! scraper-id urls :depth depth :interval interval)
        (json-response {:status "started" :scraper-id scraper-id}))
      
      (and (= method :post) (= uri "/api/continuous/file-watcher/start"))
      (let [body (parse-json-body request)
            watcher-id (keyword (get body "id" "default"))
            directories (get body "directories" [])]
        (continuous/start-file-watcher! watcher-id directories)
        (json-response {:status "started" :watcher-id watcher-id}))
      
      (and (= method :post) (= uri "/api/continuous/sensor/start"))
      (let [body (parse-json-body request)
            device-id (get body "device-id")
            sensor-types (map keyword (get body "sensors" ["accelerometer"]))]
        (continuous/start-sensor-collector! device-id sensor-types)
        (json-response {:status "started" :device-id device-id}))
      
      (and (= method :post) (= uri "/api/continuous/petabyte"))
      (let [body (parse-json-body request)
            data-source (get body "source")
            parallel-factor (get body "parallel" 1000)]
        (json-response (continuous/process-petabyte-dataset! data-source :parallel-factor parallel-factor)))
      
      ;; Database API
      (and (= method :get) (= uri "/api/db/health"))
      (json-response (db/health-check))
      
      (and (= method :get) (= uri "/api/db/stats"))
      (json-response (db/get-stats))
      
      (and (= method :post) (= uri "/api/db/analysis"))
      (let [body (parse-json-body request)]
        (db/save-analysis! body)
        (json-response {:status "saved"}))
      
      (and (= method :get) (= uri "/api/db/analyses"))
      (let [params (:query-params request)
            limit (Integer/parseInt (get params "limit" "100"))]
        (json-response {:analyses (db/get-analyses :limit limit)}))
      
      ;; 404
      :else
      (-> (response/response "Not found")
          (response/status 404)))))

;; ============================================
;; Server
;; ============================================

(defn init-distributed-systems!
  "Initialize distributed processing and continuous learning systems.
   Called on server startup if enabled via environment variables."
  []
  (let [enable-distributed (= "true" (System/getenv "ENABLE_DISTRIBUTED"))
        enable-continuous (= "true" (System/getenv "ENABLE_CONTINUOUS"))
        enable-db (= "true" (System/getenv "ENABLE_DATABASE"))
        db-url (System/getenv "DATABASE_URL")]
    
    ;; Initialize database if enabled and URL provided
    (when (and enable-db db-url)
      (println "Initializing database connection...")
      (try
        (db/init-pool! {:jdbcUrl db-url})
        (db/init-schema!)
        (println "Database initialized successfully")
        (catch Exception e
          (println "Warning: Database initialization failed:" (.getMessage e)))))
    
    ;; Initialize distributed processing if enabled
    (when enable-distributed
      (println "Initializing distributed processing system...")
      (distributed/init!)
      (let [num-workers (Integer/parseInt (or (System/getenv "NUM_WORKERS") "4"))]
        (distributed/start-workers! num-workers)
        (println (str "Started " num-workers " distributed workers"))))
    
    ;; Initialize continuous learning if enabled
    (when enable-continuous
      (println "Initializing continuous learning system...")
      (continuous/start-all-systems!)
      (println "Continuous learning system started"))))

(defn start-server [& {:keys [port] :or {port 8000}}]
  (println "")
  (println "========================================")
  (println "  Mental Models System - Electric Clojure")
  (println "  Petabyte-Scale Distributed Processing")
  (println "========================================")
  (println "")
  (println (str "Starting server on port " port "..."))
  (println (str "Models loaded: " (count @models/!models)))
  (println (str "Failure modes: " (count @models/!failure-modes)))
  (println (str "Categories: " (count @models/!categories)))
  (println "")
  
  ;; Initialize distributed systems
  (init-distributed-systems!)
  
  (println "")
  (println "API Endpoints:")
  (println "  /api/distributed/status    - Get cluster metrics")
  (println "  /api/distributed/submit    - Submit work to cluster")
  (println "  /api/continuous/status     - Get continuous learning status")
  (println "  /api/continuous/start      - Start all continuous systems")
  (println "  /api/db/health             - Check database health")
  (println "")
  (println (str "Open http://localhost:" port " in your browser"))
  (println "")
  (jetty/run-jetty (wrap-defaults app site-defaults)
                   {:port port :join? false}))

(defn -main [& args]
  (let [port (Integer/parseInt (or (System/getenv "PORT") "8000"))]
    (start-server :port port)))
