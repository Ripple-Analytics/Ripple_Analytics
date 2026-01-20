(ns mental-models.main.section-3-part7
  "Main Module - Section 3 Part7"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [ring.adapter.jetty :as jetty])
            #?(:clj [cheshire.core :as json])
            [mental-models.models :as models]
            [mental-models.algorithms :as algo]))

        
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
