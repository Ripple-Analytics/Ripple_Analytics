(ns mental-models.main.section-3-part6
  "Main Module - Section 3 Part6"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [ring.adapter.jetty :as jetty])
            #?(:clj [cheshire.core :as json])
            [mental-models.models :as models]
            [mental-models.algorithms :as algo]))

            
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
