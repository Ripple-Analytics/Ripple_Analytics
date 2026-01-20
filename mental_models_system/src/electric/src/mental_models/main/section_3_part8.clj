(ns mental-models.main.section-3-part8
  "Main Module - Section 3 Part8"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [ring.adapter.jetty :as jetty])
            #?(:clj [cheshire.core :as json])
            [mental-models.models :as models]
            [mental-models.algorithms :as algo]))

        
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
