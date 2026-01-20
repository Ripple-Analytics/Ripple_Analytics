(ns mental-models.main.section-3-part4
  "Main Module - Section 3 Part4"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [ring.adapter.jetty :as jetty])
            #?(:clj [cheshire.core :as json])
            [mental-models.models :as models]
            [mental-models.algorithms :as algo]))

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
