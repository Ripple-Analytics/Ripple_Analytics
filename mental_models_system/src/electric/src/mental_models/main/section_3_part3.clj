(ns mental-models.main.section-3-part3
  "Main Module - Section 3 Part3"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [ring.adapter.jetty :as jetty])
            #?(:clj [cheshire.core :as json])
            [mental-models.models :as models]
            [mental-models.algorithms :as algo]))

        </div>
        
        <!-- World Map Tab -->
        <div id=\"map\" class=\"p-4 hidden\">
            <div class=\"grid grid-cols-3 gap-4\">
                <div class=\"col-span-2 bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Mental Models World Map</div>
                    <div class=\"p-2\">
                        <div id=\"world-map\"></div>
                    </div>
                </div>
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Case Studies by Location</div>
                    <div class=\"p-3 max-h-96 overflow-y-auto\" id=\"map-case-studies\">
                        <p class=\"text-gray-500 text-xs\">Click a marker to see case studies</p>
                    </div>
                </div>
            </div>
            <div class=\"mt-4 bg-white border rounded shadow-sm\">
                <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Add Case Study</div>
                <div class=\"p-3\">
                    <div class=\"grid grid-cols-4 gap-4\">
                        <input type=\"text\" id=\"case-title\" class=\"border rounded px-2 py-1 text-xs\" placeholder=\"Title\">
                        <input type=\"text\" id=\"case-lat\" class=\"border rounded px-2 py-1 text-xs\" placeholder=\"Latitude\">
                        <input type=\"text\" id=\"case-lng\" class=\"border rounded px-2 py-1 text-xs\" placeholder=\"Longitude\">
                        <select id=\"case-model\" class=\"border rounded px-2 py-1 text-xs\"></select>
                    </div>
                    <textarea id=\"case-description\" class=\"w-full border rounded px-2 py-1 text-xs mt-2 h-16\" placeholder=\"Description...\"></textarea>
                    <button class=\"btn-accent px-3 py-1 text-xs rounded mt-2\" onclick=\"addCaseStudy()\">ADD TO MAP</button>
                </div>
            </div>
        </div>
        
        <!-- LLM Tab -->
        <div id=\"llm\" class=\"p-4 hidden\">
            <div class=\"grid grid-cols-2 gap-4\">
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">LM Studio Analysis</div>
                    <div class=\"p-3\">
                        <div class=\"mb-2\">
                            <span class=\"text-xs\">Status: </span>
                            <span id=\"llm-status\" class=\"text-xs bg-gray-200 px-2 py-0.5 rounded\">Checking...</span>
                        </div>
                        <textarea id=\"llm-input\" class=\"w-full border rounded px-2 py-1 text-xs h-32\" placeholder=\"Enter situation for LLM-powered analysis...\"></textarea>
                        <div class=\"flex gap-2 mt-2\">
                                                        <button class=\"btn-primary px-3 py-1 text-xs rounded\" onclick=\"runLLMAnalysis()\">ANALYZE WITH LLM</button>
                                                        <button class=\"btn-primary px-3 py-1 text-xs rounded\" onclick=\"runLLMBiases()\">DETECT BIASES</button>
                                                        <button class=\"btn-accent px-3 py-1 text-xs rounded\" onclick=\"runLLMChecklist()\">GENERATE CHECKLIST</button>
                        </div>
                    </div>
                </div>
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">LLM Results</div>
                    <div class=\"p-3 max-h-96 overflow-y-auto\" id=\"llm-result\">
                        <p class=\"text-gray-500 text-xs\">Run an LLM analysis to see results</p>
                    </div>
                </div>
            </div>
        </div>
        
        <!-- Tech Debt Tab -->
        <div id=\"techdebt\" class=\"p-4 hidden\">
            <div class=\"grid grid-cols-3 gap-4 mb-4\">
                <div class=\"metric-card bg-white border rounded shadow-sm\">
                    <div class=\"metric-value\" id=\"td-total-nodes\">0</div>
                    <div class=\"metric-label\">Total Nodes</div>
                </div>
                <div class=\"metric-card bg-white border rounded shadow-sm\">
                    <div class=\"metric-value\" id=\"td-total-tangles\">0</div>
                    <div class=\"metric-label\">Tangles Detected</div>
                </div>
                <div class=\"metric-card bg-white border rounded shadow-sm\">
                    <div class=\"metric-value\" id=\"td-pure-functions\">0</div>
                    <div class=\"metric-label\">Pure Functions</div>
                </div>
            </div>
            
            <div class=\"grid grid-cols-2 gap-4\">
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">DAG Visualization</div>
                    <div class=\"p-2\">
                        <svg id=\"dag-viz\" width=\"100%\" height=\"300\"></svg>
                        <div class=\"flex gap-2 mt-2 text-xs\">
                            <span class=\"flex items-center\"><span class=\"w-3 h-3 bg-green-500 rounded-full mr-1\"></span>Normal</span>
                            <span class=\"flex items-center\"><span class=\"w-3 h-3 bg-red-500 rounded-full mr-1\"></span>Tangle</span>
                            <span class=\"flex items-center\"><span class=\"w-3 h-3 bg-yellow-500 rounded-full mr-1\"></span>High Coupling</span>
                            <span class=\"flex items-center\"><span class=\"w-3 h-3 bg-orange-500 rounded-full mr-1\"></span>High Complexity</span>
                        </div>
                    </div>
                </div>
                
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Refactoring Suggestions</div>
                    <div class=\"p-3 max-h-64 overflow-y-auto\" id=\"refactoring-suggestions\">
                        <p class=\"text-gray-500 text-xs\">Analyze code to see suggestions</p>
                    </div>
                </div>
            </div>
            
            <div class=\"mt-4 bg-white border rounded shadow-sm\">
                <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Analyze Code</div>
                <div class=\"p-3\">
                    <textarea id=\"code-input\" class=\"w-full border rounded px-2 py-1 text-xs h-32 font-mono\" placeholder=\"Paste code or JSON dependency graph...\"></textarea>
                    <div class=\"flex gap-2 mt-2\">
                                                <button class=\"btn-primary px-3 py-1 text-xs rounded\" onclick=\"analyzeCodeDAG()\">ANALYZE DAG</button>
                                                <button class=\"btn-accent px-3 py-1 text-xs rounded\" onclick=\"detectTangles()\">DETECT TANGLES</button>
                                                <button class=\"btn-primary px-3 py-1 text-xs rounded\" onclick=\"generateRefactoringPlan()\">GENERATE PLAN</button>
                                                <button class=\"btn-primary px-3 py-1 text-xs rounded\" onclick=\"llmRefactorSuggestion()\">LLM REFACTOR</button>
                    </div>
                </div>
            </div>
            
            <div class=\"mt-4 bg-white border rounded shadow-sm\">
                <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Refactoring Plan</div>
                <div class=\"p-3\" id=\"refactoring-plan\">
                    <p class=\"text-gray-500 text-xs\">Generate a plan to see prioritized refactoring steps</p>
                </div>
            </div>
        </div>
        <!-- Distributed Tab -->
        <div id=\"distributed\" class=\"p-4 hidden\">
                                                <div class=\"grid grid-cols-4 gap-4 mb-4\">
                                                    <div class=\"metric-card rounded shadow-sm\">
                                                        <div class=\"metric-value clickable\" id=\"dist-workers\" onclick=\"showProvenance('dist-workers')\">NA</div>
                                                        <div class=\"metric-label\">Active Workers (click for source)</div>
                                                    </div>
                                                    <div class=\"metric-card rounded shadow-sm\">
                                                        <div class=\"metric-value clickable\" id=\"dist-tasks\" onclick=\"showProvenance('dist-tasks')\">NA</div>
                                                        <div class=\"metric-label\">Tasks Queued (click for source)</div>
                                                    </div>
                                                    <div class=\"metric-card rounded shadow-sm\">
                                                        <div class=\"metric-value accent clickable\" id=\"dist-throughput\" onclick=\"showProvenance('dist-throughput')\">NA</div>
                                                        <div class=\"metric-label\">Tasks/Second (click for source)</div>
                                                    </div>
                                                    <div class=\"metric-card rounded shadow-sm\">
                                                        <div class=\"metric-value clickable\" id=\"dist-data\" onclick=\"showProvenance('dist-data')\">NA</div>
                                                        <div class=\"metric-label\">Data Processed (click for source)</div>
                                                    </div>
                                                </div>
            
            <div class=\"grid grid-cols-2 gap-4\">
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">WORKER NODES</div>
                    <div class=\"p-3 max-h-64 overflow-y-auto\" id=\"worker-list\">
                        <p class=\"text-gray-500 text-xs\">No workers connected. Start workers to begin distributed processing.</p>
                    </div>
                </div>
                
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">TASK QUEUE</div>
