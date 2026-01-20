(ns mental-models.main.section-3-part2
  "Main Module - Section 3 Part2"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [ring.adapter.jetty :as jetty])
            #?(:clj [cheshire.core :as json])
            [mental-models.models :as models]
            [mental-models.algorithms :as algo]))

                                                        <div class=\"metric-value clickable\" id=\"total-models\" onclick=\"showProvenance('total-models')\">NA</div>
                                                        <div class=\"metric-label\">Models (click for source)</div>
                                                    </div>
                                                    <div class=\"metric-card bg-white border rounded shadow-sm\">
                                                        <div class=\"metric-value clickable\" id=\"total-failures\" onclick=\"showProvenance('total-failures')\">NA</div>
                                                        <div class=\"metric-label\">Failure Modes (click for source)</div>
                                                    </div>
                                                    <div class=\"metric-card bg-white border rounded shadow-sm\">
                                                        <div class=\"metric-value clickable\" id=\"total-categories\" onclick=\"showProvenance('total-categories')\">NA</div>
                                                        <div class=\"metric-label\">Categories (click for source)</div>
                                                    </div>
                                                    <div class=\"metric-card bg-white border rounded shadow-sm\">
                                                        <div class=\"metric-value clickable\" id=\"avg-failures\" onclick=\"showProvenance('avg-failures')\">NA</div>
                                                        <div class=\"metric-label\">Avg Failures/Model (click for source)</div>
                                                    </div>
                                                    <div class=\"metric-card bg-white border rounded shadow-sm\">
                                                        <div class=\"metric-value clickable\" id=\"coverage\" onclick=\"showProvenance('coverage')\">NA</div>
                                                        <div class=\"metric-label\">Coverage (click for source)</div>
                                                    </div>
                                                    <div class=\"metric-card bg-white border rounded shadow-sm\">
                                                        <div class=\"metric-value clickable\" id=\"api-status\" onclick=\"showProvenance('api-status')\">NA</div>
                                                        <div class=\"metric-label\">Status (click for source)</div>
                                                    </div>
                                                </div>
            
            <div class=\"grid grid-cols-2 gap-4\">
                <!-- Categories -->
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Categories</div>
                    <div class=\"p-3\">
                        <table class=\"w-full dense-table text-xs\">
                            <thead>
                                <tr class=\"bg-gray-50\">
                                    <th class=\"text-left\">Category</th>
                                    <th class=\"text-left\">Models</th>
                                    <th class=\"text-left\">Coverage</th>
                                </tr>
                            </thead>
                            <tbody id=\"categories-table\"></tbody>
                        </table>
                    </div>
                </div>
                
                <!-- Quick Analysis -->
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Quick Analysis</div>
                    <div class=\"p-3\">
                        <textarea id=\"analysis-input\" class=\"w-full border rounded px-2 py-1 text-xs h-20\" placeholder=\"Enter situation to analyze...\"></textarea>
                        <div class=\"flex gap-2 mt-2\">
                                                        <button class=\"btn-primary px-3 py-1 text-xs rounded\" onclick=\"runAnalysis()\">ANALYZE</button>
                                                        <button class=\"btn-secondary px-3 py-1 text-xs rounded\" onclick=\"detectBiases()\">DETECT BIASES</button>
                        </div>
                        <div id=\"analysis-result\" class=\"mt-2 text-xs\"></div>
                    </div>
                </div>
            </div>
        </div>
        
        <!-- Models Tab -->
        <div id=\"models\" class=\"p-4 hidden\">
            <div class=\"grid grid-cols-2 gap-4\">
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">
                        Mental Models
                        <input type=\"text\" id=\"model-search\" class=\"ml-4 border rounded px-2 py-0.5 text-xs\" placeholder=\"Search...\" oninput=\"filterModels()\">
                    </div>
                    <div class=\"p-3 max-h-96 overflow-y-auto\">
                        <table class=\"w-full dense-table text-xs\">
                            <thead>
                                <tr class=\"bg-gray-50\">
                                    <th class=\"text-left\">Name</th>
                                    <th class=\"text-left\">Category</th>
                                    <th class=\"text-left\">Originator</th>
                                    <th class=\"text-left\">Failures</th>
                                </tr>
                            </thead>
                            <tbody id=\"models-table\"></tbody>
                        </table>
                    </div>
                </div>
                
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Model Detail</div>
                    <div class=\"p-3\" id=\"model-detail\">
                        <p class=\"text-gray-500 text-xs\">Select a model to view details</p>
                    </div>
                </div>
            </div>
        </div>
        
        <!-- Analysis Tab -->
        <div id=\"analysis\" class=\"p-4 hidden\">
            <div class=\"grid grid-cols-2 gap-4\">
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Comprehensive Analysis</div>
                    <div class=\"p-3\">
                        <textarea id=\"comprehensive-input\" class=\"w-full border rounded px-2 py-1 text-xs h-32\" placeholder=\"Enter situation for comprehensive analysis...\"></textarea>
                        <div class=\"flex gap-2 mt-2\">
                                                        <button class=\"btn-primary px-3 py-1 text-xs rounded\" onclick=\"runLatticework()\">LATTICEWORK</button>
                                                        <button class=\"btn-accent px-3 py-1 text-xs rounded\" onclick=\"runLollapalooza()\">LOLLAPALOOZA</button>
                                                        <button class=\"btn-primary px-3 py-1 text-xs rounded\" onclick=\"runInversion()\">INVERSION</button>
                                                        <button class=\"btn-primary px-3 py-1 text-xs rounded\" onclick=\"runTwoTrack()\">TWO-TRACK</button>
                        </div>
                    </div>
                </div>
                
                <div class=\"bg-white border rounded shadow-sm\">
                    <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Analysis Results</div>
                    <div class=\"p-3\" id=\"comprehensive-result\">
                        <p class=\"text-gray-500 text-xs\">Run an analysis to see results</p>
                    </div>
                </div>
            </div>
        </div>
        
        <!-- Statistics Tab -->
        <div id=\"statistics\" class=\"p-4 hidden\">
            <div class=\"bg-white border rounded shadow-sm\">
                <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Statistical Analysis</div>
                <div class=\"p-3\">
                    <div class=\"grid grid-cols-2 gap-4\">
                        <div>
                            <label class=\"text-xs font-semibold\">X Values (comma-separated)</label>
                            <input type=\"text\" id=\"stats-x\" class=\"w-full border rounded px-2 py-1 text-xs mt-1\" placeholder=\"1, 2, 3, 4, 5\">
                        </div>
                        <div>
                            <label class=\"text-xs font-semibold\">Y Values (comma-separated)</label>
                            <input type=\"text\" id=\"stats-y\" class=\"w-full border rounded px-2 py-1 text-xs mt-1\" placeholder=\"2, 4, 5, 4, 5\">
                        </div>
                    </div>
                    <button class=\"btn-primary px-3 py-1 text-xs rounded mt-2\" onclick=\"runStatistics()\">CALCULATE</button>
                    <div id=\"stats-result\" class=\"mt-4\"></div>
                </div>
            </div>
        </div>
        
        <!-- Data Tab -->
        <div id=\"data\" class=\"p-4 hidden\">
            <div class=\"bg-white border rounded shadow-sm\">
                <div class=\"bg-gray-100 px-3 py-2 font-semibold text-xs border-b\">Data Processing</div>
                <div class=\"p-3\">
                    <textarea id=\"data-input\" class=\"w-full border rounded px-2 py-1 text-xs h-32\" placeholder=\"Paste text to analyze...\"></textarea>
                    <div class=\"flex gap-2 mt-2\">
                                                <button class=\"btn-primary px-3 py-1 text-xs rounded\" onclick=\"analyzeDocument()\">ANALYZE DOCUMENT</button>
                                                <button class=\"btn-primary px-3 py-1 text-xs rounded\" onclick=\"extractEntities()\">EXTRACT ENTITIES</button>
                                                <button class=\"btn-primary px-3 py-1 text-xs rounded\" onclick=\"classifyText()\">CLASSIFY BY MODELS</button>
                    </div>
                    <div id=\"data-result\" class=\"mt-4\"></div>
                </div>
            </div>
