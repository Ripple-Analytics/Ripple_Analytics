(ns mental-models.refactor.visualizer
  "DAG Visualizer - D3.js Force-Directed Graph
   Interactive visualization of codebase dependency structure"
  #?(:cljs (:require-macros [mental-models.refactor.visualizer]))
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            #?(:clj [mental-models.refactor.dag :as dag])
            #?(:clj [cheshire.core :as json])))

;; -- D3.js Integration -------------------------------------------------------

(def d3-script "https://d3js.org/d3.v7.min.js")

(def visualization-styles
  "svg {
     font-family: 'Inter', -apple-system, sans-serif;
     background: #fafafa;
   }
   .node {
     cursor: pointer;
   }
   .node circle {
     stroke: #fff;
     stroke-width: 2px;
     transition: all 0.2s;
   }
   .node:hover circle {
     stroke: #dc2626;
     stroke-width: 3px;
   }
   .node text {
     font-size: 10px;
     fill: #525252;
     pointer-events: none;
   }
   .link {
     stroke: #d4d4d4;
     stroke-opacity: 0.6;
     fill: none;
   }
   .link.require {
     stroke: #525252;
     stroke-dasharray: 4,2;
   }
   .link.reference {
     stroke: #a3a3a3;
   }
   .link.highlighted {
     stroke: #dc2626;
     stroke-width: 2px;
     stroke-opacity: 1;
   }
   .tangle-region {
     fill: #fef2f2;
     stroke: #dc2626;
     stroke-width: 2px;
     stroke-dasharray: 5,5;
     opacity: 0.3;
   }
   .tooltip {
     position: absolute;
     background: white;
     border: 1px solid #e5e5e5;
     border-radius: 4px;
     padding: 8px 12px;
     font-size: 12px;
     box-shadow: 0 2px 8px rgba(0,0,0,0.1);
     pointer-events: none;
     z-index: 1000;
   }
   .tooltip h4 {
     margin: 0 0 4px 0;
     font-size: 13px;
     font-weight: 600;
   }
   .tooltip .stat {
     display: flex;
     justify-content: space-between;
     gap: 16px;
     color: #525252;
   }
   .controls {
     position: absolute;
     top: 16px;
     left: 16px;
     display: flex;
     flex-direction: column;
     gap: 8px;
     z-index: 100;
   }
   .control-btn {
     padding: 6px 12px;
     background: white;
     border: 1px solid #e5e5e5;
     border-radius: 4px;
     font-size: 12px;
     cursor: pointer;
     transition: all 0.15s;
   }
   .control-btn:hover {
     background: #f0f0f0;
   }
   .control-btn.active {
     background: #dc2626;
     color: white;
     border-color: #dc2626;
   }
   .legend {
     position: absolute;
     bottom: 16px;
     right: 16px;
     background: white;
     border: 1px solid #e5e5e5;
     border-radius: 4px;
     padding: 12px;
     font-size: 11px;
   }
   .legend-item {
     display: flex;
     align-items: center;
     gap: 8px;
     margin: 4px 0;
   }
   .legend-circle {
     width: 12px;
     height: 12px;
     border-radius: 50%;
   }")

;; -- Color Schemes -----------------------------------------------------------

(def node-colors
  {:namespace "#525252"
   :defn "#3b82f6"
   :defn- "#6366f1"
   :def "#10b981"
   :defmacro "#f59e0b"
   :defmulti "#8b5cf6"
   :defprotocol "#ec4899"
   :e/defn "#dc2626"
   :default "#a3a3a3"})

(defn get-node-color [node-type]
  (get node-colors (keyword node-type) (:default node-colors)))

(defn get-node-size [node]
  (let [base-size 6
        complexity-factor (min 3 (/ (or (:complexity node) 1) 10))
        loc-factor (min 2 (/ (or (:loc node) 10) 50))]
    (+ base-size (* 2 complexity-factor) loc-factor)))

;; -- D3 Visualization Code ---------------------------------------------------

(def d3-visualization-code
  "function initDAGVisualization(containerId, data, options = {}) {
    const container = document.getElementById(containerId);
    if (!container || !data) return;
    
    const width = options.width || container.clientWidth || 800;
    const height = options.height || container.clientHeight || 600;
    
    // Clear existing
    d3.select('#' + containerId).selectAll('*').remove();
    
    const svg = d3.select('#' + containerId)
      .append('svg')
      .attr('width', width)
      .attr('height', height)
      .attr('viewBox', [0, 0, width, height]);
    
    // Add zoom behavior
    const g = svg.append('g');
    svg.call(d3.zoom()
      .extent([[0, 0], [width, height]])
      .scaleExtent([0.1, 4])
      .on('zoom', (event) => g.attr('transform', event.transform)));
    
    // Create force simulation
    const simulation = d3.forceSimulation(data.nodes)
      .force('link', d3.forceLink(data.links).id(d => d.id).distance(80))
      .force('charge', d3.forceManyBody().strength(-200))
      .force('center', d3.forceCenter(width / 2, height / 2))
      .force('collision', d3.forceCollide().radius(d => getNodeSize(d) + 5));
    
    // Draw links
    const link = g.append('g')
      .selectAll('line')
      .data(data.links)
      .join('line')
      .attr('class', d => 'link ' + d.type)
      .attr('stroke-width', 1);
    
    // Draw nodes
    const node = g.append('g')
      .selectAll('g')
      .data(data.nodes)
      .join('g')
      .attr('class', 'node')
      .call(d3.drag()
        .on('start', dragstarted)
        .on('drag', dragged)
        .on('end', dragended));
    
    node.append('circle')
      .attr('r', d => getNodeSize(d))
      .attr('fill', d => getNodeColor(d.type));
    
    node.append('text')
      .attr('dx', d => getNodeSize(d) + 4)
      .attr('dy', 3)
      .text(d => d.label);
    
    // Tooltip
    const tooltip = d3.select('body').append('div')
      .attr('class', 'tooltip')
      .style('opacity', 0);
    
    node.on('mouseover', (event, d) => {
      tooltip.transition().duration(200).style('opacity', 1);
      tooltip.html(`
        <h4>${d.label}</h4>
        <div class='stat'><span>Type:</span><span>${d.type}</span></div>
        <div class='stat'><span>LOC:</span><span>${d.loc || 'N/A'}</span></div>
        <div class='stat'><span>Complexity:</span><span>${d.complexity || 'N/A'}</span></div>
        <div class='stat'><span>In-degree:</span><span>${d.inDegree || 0}</span></div>
        <div class='stat'><span>Out-degree:</span><span>${d.outDegree || 0}</span></div>
      `)
      .style('left', (event.pageX + 10) + 'px')
      .style('top', (event.pageY - 10) + 'px');
      
      // Highlight connected
      link.classed('highlighted', l => l.source.id === d.id || l.target.id === d.id);
    })
    .on('mouseout', () => {
      tooltip.transition().duration(200).style('opacity', 0);
      link.classed('highlighted', false);
    });
    
    // Update positions on tick
    simulation.on('tick', () => {
      link
        .attr('x1', d => d.source.x)
        .attr('y1', d => d.source.y)
        .attr('x2', d => d.target.x)
        .attr('y2', d => d.target.y);
      
      node.attr('transform', d => `translate(${d.x},${d.y})`);
    });
    
    // Drag functions
    function dragstarted(event) {
      if (!event.active) simulation.alphaTarget(0.3).restart();
      event.subject.fx = event.subject.x;
      event.subject.fy = event.subject.y;
    }
    
    function dragged(event) {
      event.subject.fx = event.x;
      event.subject.fy = event.y;
    }
    
    function dragended(event) {
      if (!event.active) simulation.alphaTarget(0);
      event.subject.fx = null;
      event.subject.fy = null;
    }
    
    function getNodeSize(d) {
      const base = 6;
      const complexityFactor = Math.min(3, (d.complexity || 1) / 10);
      const locFactor = Math.min(2, (d.loc || 10) / 50);
      return base + 2 * complexityFactor + locFactor;
    }
    
    function getNodeColor(type) {
      const colors = {
        'namespace': '#525252',
        'defn': '#3b82f6',
        'defn-': '#6366f1',
        'def': '#10b981',
        'defmacro': '#f59e0b',
        'defmulti': '#8b5cf6',
        'defprotocol': '#ec4899',
        'e/defn': '#dc2626'
      };
      return colors[type] || '#a3a3a3';
    }
    
    // Return control functions
    return {
      highlightTangles: function(tangles) {
        // Draw tangle regions
        const tangleGroup = g.insert('g', ':first-child');
        tangles.forEach(tangle => {
          const nodePositions = tangle.nodes.map(id => 
            data.nodes.find(n => n.id === id)
          ).filter(n => n);
          
          if (nodePositions.length > 2) {
            const hull = d3.polygonHull(nodePositions.map(n => [n.x, n.y]));
            if (hull) {
              tangleGroup.append('path')
                .attr('class', 'tangle-region')
                .attr('d', 'M' + hull.join('L') + 'Z');
            }
          }
        });
      },
      focusNode: function(nodeId) {
        const targetNode = data.nodes.find(n => n.id === nodeId);
        if (targetNode) {
          svg.transition().duration(750).call(
            d3.zoom().transform,
            d3.zoomIdentity.translate(width/2, height/2).scale(2).translate(-targetNode.x, -targetNode.y)
          );
        }
      },
      resetView: function() {
        svg.transition().duration(750).call(
          d3.zoom().transform,
          d3.zoomIdentity
        );
      }
    };
  }")

;; -- Electric UI Components --------------------------------------------------

(e/defn DAGVisualizerStyles []
  (dom/style (dom/text visualization-styles)))

(e/defn DAGControls [on-action]
  (dom/div
    (dom/props {:class "controls"})
    (dom/button
      (dom/props {:class "control-btn"})
      (dom/on "click" (fn [_] (on-action :reset-view)))
      (dom/text "Reset View"))
    (dom/button
      (dom/props {:class "control-btn"})
      (dom/on "click" (fn [_] (on-action :show-tangles)))
      (dom/text "Show Tangles"))
    (dom/button
      (dom/props {:class "control-btn"})
      (dom/on "click" (fn [_] (on-action :show-hubs)))
      (dom/text "Show Hubs"))
    (dom/button
      (dom/props {:class "control-btn"})
      (dom/on "click" (fn [_] (on-action :show-orphans)))
      (dom/text "Show Orphans"))))

(e/defn DAGLegend []
  (dom/div
    (dom/props {:class "legend"})
    (dom/div
      (dom/props {:style {:font-weight "600" :margin-bottom "8px"}})
      (dom/text "Node Types"))
    (e/for [[type color] [["Namespace" "#525252"]
                          ["defn" "#3b82f6"]
                          ["def" "#10b981"]
                          ["defmacro" "#f59e0b"]
                          ["Electric" "#dc2626"]]]
      (dom/div
        (dom/props {:class "legend-item"})
        (dom/div
          (dom/props {:class "legend-circle"
                      :style {:background color}}))
        (dom/text type)))))

(e/defn DAGStats [metadata]
  (dom/div
    (dom/props {:class "card"
                :style {:margin-bottom "16px"}})
    (dom/div
      (dom/props {:style {:display "grid"
                          :grid-template-columns "repeat(5, 1fr)"
                          :gap "16px"}})
      (dom/div
        (dom/props {:class "metric"})
        (dom/span (dom/props {:class "metric-label"}) (dom/text "Files"))
        (dom/span (dom/props {:class "metric-value"}) (dom/text (str (:file-count metadata)))))
      (dom/div
        (dom/props {:class "metric"})
        (dom/span (dom/props {:class "metric-label"}) (dom/text "Definitions"))
        (dom/span (dom/props {:class "metric-value"}) (dom/text (str (:definition-count metadata)))))
      (dom/div
        (dom/props {:class "metric"})
        (dom/span (dom/props {:class "metric-label"}) (dom/text "Edges"))
        (dom/span (dom/props {:class "metric-value"}) (dom/text (str (:edge-count metadata)))))
      (dom/div
        (dom/props {:class "metric"})
        (dom/span (dom/props {:class "metric-label"}) (dom/text "Total LOC"))
        (dom/span (dom/props {:class "metric-value"}) (dom/text (str (:total-loc metadata)))))
      (dom/div
        (dom/props {:class "metric"})
        (dom/span (dom/props {:class "metric-label"}) (dom/text "Complexity"))
        (dom/span (dom/props {:class "metric-value"}) (dom/text (str (:total-complexity metadata))))))))

(e/defn DAGVisualizerPage []
  (let [dag-data (e/server
                  (let [analysis (dag/analyze-codebase "src/mental_models")]
                    (dag/dag->json (:dag analysis))))]
    (dom/div
      (dom/props {:style {:padding "24px"}})
      
      ;; Include D3.js
      (dom/script (dom/props {:src d3-script}))
      
      (DAGVisualizerStyles.)
      
      ;; Header
      (dom/div
        (dom/props {:style {:display "flex"
                            :justify-content "space-between"
                            :align-items "center"
                            :margin-bottom "16px"}})
        (dom/div
          (dom/h3
            (dom/props {:style {:font-size "16px" :font-weight "600" :margin "0"}})
            (dom/text "Codebase DAG Visualization"))
          (dom/p
            (dom/props {:style {:font-size "12px" :color "#525252" :margin "4px 0 0"}})
            (dom/text "Interactive dependency graph for tech debt analysis"))))
      
      ;; Stats
      (DAGStats. (:metadata dag-data))
      
      ;; Visualization container
      (dom/div
        (dom/props {:style {:position "relative"
                            :height "600px"
                            :border "1px solid #e5e5e5"
                            :border-radius "4px"
                            :background "#fafafa"}})
        (dom/div
          (dom/props {:id "dag-container"
                      :style {:width "100%" :height "100%"}}))
        (DAGControls. (fn [action] (println "Action:" action)))
        (DAGLegend.))
      
      ;; Initialize visualization
      (dom/script
        (dom/text d3-visualization-code))
      
      (dom/script
        (dom/text
         (str "setTimeout(function() {
                 if (window.d3 && !window.dagViz) {
                   var data = " (json/generate-string dag-data) ";
                   window.dagViz = initDAGVisualization('dag-container', data);
                 }
               }, 500);"))))))
