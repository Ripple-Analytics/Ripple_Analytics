(ns mental-models.main.section-3-part10
  "Main Module - Section 3 Part10"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [ring.adapter.jetty :as jetty])
            #?(:clj [cheshire.core :as json])
            [mental-models.models :as models]
            [mental-models.algorithms :as algo]))

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
