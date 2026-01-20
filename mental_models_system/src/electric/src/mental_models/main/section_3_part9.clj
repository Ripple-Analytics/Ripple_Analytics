(ns mental-models.main.section-3-part9
  "Main Module - Section 3 Part9"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [ring.adapter.jetty :as jetty])
            #?(:clj [cheshire.core :as json])
            [mental-models.models :as models]
            [mental-models.algorithms :as algo]))

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
