(ns mental-models.main
  "Mental Models System - Electric Clojure Server
   
   Main entry point for the Electric Clojure application.
   Starts the server and serves the reactive UI."
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.util.response :as response]
            [mental-models.models :as models]
            [mental-models.analysis :as analysis]
            [mental-models.statistics :as stats]
            [mental-models.data-processing :as data])
  (:gen-class))

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
    <style>
        /* Value Line-style density */
        body { font-size: 11px; line-height: 1.3; }
        .dense-table td, .dense-table th { padding: 2px 4px; }
        .metric-card { text-align: center; padding: 8px; }
        .metric-value { font-size: 18px; font-weight: bold; color: #2563eb; }
        .metric-label { font-size: 10px; color: #6b7280; }
    </style>
</head>
<body class=\"bg-gray-50\">
    <div id=\"app\">
        <!-- Header -->
        <div class=\"bg-gray-900 text-white px-4 py-2\">
            <h1 class=\"text-lg font-bold\">Mental Models System</h1>
            <p class=\"text-xs text-gray-400\">Electric Clojure - Reactive Full-Stack</p>
        </div>
        
        <!-- Navigation -->
        <div class=\"flex border-b border-gray-300 bg-gray-100\">
            <button class=\"px-4 py-2 text-xs font-semibold bg-white border-b-2 border-blue-600\" onclick=\"showTab('dashboard')\">Dashboard</button>
            <button class=\"px-4 py-2 text-xs text-gray-600 hover:bg-gray-200\" onclick=\"showTab('models')\">Models</button>
            <button class=\"px-4 py-2 text-xs text-gray-600 hover:bg-gray-200\" onclick=\"showTab('analysis')\">Analysis</button>
            <button class=\"px-4 py-2 text-xs text-gray-600 hover:bg-gray-200\" onclick=\"showTab('statistics')\">Statistics</button>
            <button class=\"px-4 py-2 text-xs text-gray-600 hover:bg-gray-200\" onclick=\"showTab('data')\">Data</button>
        </div>
        
        <!-- Dashboard -->
        <div id=\"dashboard\" class=\"p-4\">
            <div class=\"grid grid-cols-6 gap-4 mb-4\">
                <div class=\"metric-card bg-white border rounded shadow-sm\">
                    <div class=\"metric-value\" id=\"total-models\">0</div>
                    <div class=\"metric-label\">Models</div>
                </div>
                <div class=\"metric-card bg-white border rounded shadow-sm\">
                    <div class=\"metric-value\" id=\"total-failures\">0</div>
                    <div class=\"metric-label\">Failure Modes</div>
                </div>
                <div class=\"metric-card bg-white border rounded shadow-sm\">
                    <div class=\"metric-value\" id=\"total-categories\">0</div>
                    <div class=\"metric-label\">Categories</div>
                </div>
                <div class=\"metric-card bg-white border rounded shadow-sm\">
                    <div class=\"metric-value\">5.0</div>
                    <div class=\"metric-label\">Avg Failures/Model</div>
                </div>
                <div class=\"metric-card bg-white border rounded shadow-sm\">
                    <div class=\"metric-value\">100%</div>
                    <div class=\"metric-label\">Coverage</div>
                </div>
                <div class=\"metric-card bg-white border rounded shadow-sm\">
                    <div class=\"metric-value\">Live</div>
                    <div class=\"metric-label\">Status</div>
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
                            <button class=\"bg-blue-600 text-white px-3 py-1 text-xs rounded hover:bg-blue-700\" onclick=\"runAnalysis()\">Analyze</button>
                            <button class=\"bg-gray-200 text-gray-800 px-3 py-1 text-xs rounded hover:bg-gray-300\" onclick=\"detectBiases()\">Detect Biases</button>
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
                            <button class=\"bg-blue-600 text-white px-3 py-1 text-xs rounded\" onclick=\"runLatticework()\">Latticework</button>
                            <button class=\"bg-blue-600 text-white px-3 py-1 text-xs rounded\" onclick=\"runLollapalooza()\">Lollapalooza</button>
                            <button class=\"bg-blue-600 text-white px-3 py-1 text-xs rounded\" onclick=\"runInversion()\">Inversion</button>
                            <button class=\"bg-blue-600 text-white px-3 py-1 text-xs rounded\" onclick=\"runTwoTrack()\">Two-Track</button>
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
                    <button class=\"bg-blue-600 text-white px-3 py-1 text-xs rounded mt-2\" onclick=\"runStatistics()\">Calculate</button>
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
                        <button class=\"bg-blue-600 text-white px-3 py-1 text-xs rounded\" onclick=\"analyzeDocument()\">Analyze Document</button>
                        <button class=\"bg-blue-600 text-white px-3 py-1 text-xs rounded\" onclick=\"extractEntities()\">Extract Entities</button>
                        <button class=\"bg-blue-600 text-white px-3 py-1 text-xs rounded\" onclick=\"classifyText()\">Classify by Models</button>
                    </div>
                    <div id=\"data-result\" class=\"mt-4\"></div>
                </div>
            </div>
        </div>
    </div>
    
    <script>
        // Tab switching
        function showTab(tabId) {
            document.querySelectorAll('#app > div:not(:first-child):not(:nth-child(2))').forEach(el => el.classList.add('hidden'));
            document.getElementById(tabId).classList.remove('hidden');
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
        
        // Load data on page load
        loadData();
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
      
      ;; 404
      :else
      (-> (response/response "Not found")
          (response/status 404)))))

;; ============================================
;; Server
;; ============================================

(defn start-server [& {:keys [port] :or {port 8000}}]
  (println "")
  (println "========================================")
  (println "  Mental Models System - Electric Clojure")
  (println "========================================")
  (println "")
  (println (str "Starting server on port " port "..."))
  (println (str "Models loaded: " (count @models/!models)))
  (println (str "Failure modes: " (count @models/!failure-modes)))
  (println (str "Categories: " (count @models/!categories)))
  (println "")
  (println (str "Open http://localhost:" port " in your browser"))
  (println "")
  (jetty/run-jetty (wrap-defaults app site-defaults)
                   {:port port :join? false}))

(defn -main [& args]
  (let [port (Integer/parseInt (or (System/getenv "PORT") "8000"))]
    (start-server :port port)))
