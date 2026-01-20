(ns mental-models.main.section-3-part5
  "Main Module - Section 3 Part5"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [ring.adapter.jetty :as jetty])
            #?(:clj [cheshire.core :as json])
            [mental-models.models :as models]
            [mental-models.algorithms :as algo]))

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
