(ns mental-models.main.section-3-part11
  "Main Module - Section 3 Part11"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [ring.adapter.jetty :as jetty])
            #?(:clj [cheshire.core :as json])
            [mental-models.models :as models]
            [mental-models.algorithms :as algo]))

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

