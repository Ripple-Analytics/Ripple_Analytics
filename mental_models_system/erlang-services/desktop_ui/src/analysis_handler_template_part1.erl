%%%-------------------------------------------------------------------
%%% @doc Analysis Handler Template - Part 1
%%% @end
%%%-------------------------------------------------------------------
-module(analysis_handler_template_part1).

-export([content/0]).

content() ->
    <<"

        <!-- Status Bar - Information Dense Header -->
        <div class=\"card\" style=\"padding: 10px 15px; margin-bottom: 10px;\">
            <div style=\"display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 10px;\">
                <div style=\"display: flex; align-items: center; gap: 15px;\">
                    <span style=\"font-weight: bold; color: #4361ee;\">Mental Models Analysis</span>
                    <span id=\"analysis-status\" class=\"status-healthy\" style=\"font-size: 11px;\">Ready</span>
                </div>
                <div style=\"display: flex; align-items: center; gap: 15px; font-size: 11px; color: #666;\">
                    <span>Last Analysis: <span id=\"last-analysis-time\">Never</span></span>
                    <span>|</span>
                    <span>Models in Registry: <span id=\"model-count\">Loading...</span></span>
                    <button class=\"btn btn-secondary\" style=\"font-size: 10px; padding: 4px 8px;\" onclick=\"refreshSystemStatus()\">Refresh Status</button>
                </div>
            </div>
        </div>

        <!-- Data Import Section -->
        <div class=\"card\" style=\"background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; margin-bottom: 10px;\">
            <div style=\"display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 10px;\">
                <div>
                    <h3 style=\"margin: 0; color: white; font-size: 14px;\">Batch Data Import</h3>
                    <p style=\"margin: 5px 0 0 0; font-size: 11px; opacity: 0.9;\">Upload files or paste data for wholesale analysis</p>
                </div>
                <div style=\"display: flex; gap: 8px; align-items: center;\">
                    <input type=\"file\" id=\"file-upload\" accept=\".txt,.csv,.json,.md\" multiple style=\"display: none;\" onchange=\"handleFileUpload(event)\">
                    <button class=\"btn\" style=\"background: white; color: #667eea; font-size: 11px; padding: 6px 12px;\" onclick=\"document.getElementById('file-upload').click()\">Upload Files</button>
                    <button class=\"btn\" style=\"background: rgba(255,255,255,0.2); border: 1px solid white; font-size: 11px; padding: 6px 12px;\" onclick=\"showPasteModal()\">Paste Bulk Data</button>
                    <span id=\"file-count\" style=\"font-size: 11px;\"></span>
                </div>
            </div>
            <div id=\"import-preview\" style=\"margin-top: 10px; display: none;\">
                <div style=\"background: rgba(255,255,255,0.1); padding: 8px; border-radius: 6px; max-height: 100px; overflow-y: auto;\">
                    <div id=\"import-files-list\" style=\"font-size: 11px;\"></div>
                </div>
                <div style=\"margin-top: 8px; display: flex; gap: 8px;\">
                    <button class=\"btn\" style=\"background: #28a745; font-size: 11px; padding: 6px 12px;\" onclick=\"processBatchData()\">Process All Files</button>
                    <button class=\"btn\" style=\"background: rgba(255,255,255,0.2); font-size: 11px; padding: 6px 12px;\" onclick=\"clearImportedFiles()\">Clear</button>
                </div>
            </div>
        </div>
        
        <!-- Paste Modal -->
        <div id=\"paste-modal\" style=\"display: none; position: fixed; top: 0; left: 0; right: 0; bottom: 0; background: rgba(0,0,0,0.5); z-index: 1000; align-items: center; justify-content: center;\">
            <div style=\"background: white; padding: 20px; border-radius: 12px; max-width: 600px; width: 90%; max-height: 80vh; overflow-y: auto;\">
                <h3 style=\"margin: 0 0 10px 0;\">Paste Bulk Data</h3>
                <p style=\"color: #666; font-size: 12px;\">Paste multiple entries separated by blank lines, or CSV/JSON data:</p>
                <textarea id=\"bulk-paste-area\" style=\"width: 100%; height: 250px; margin: 10px 0; padding: 10px; border: 1px solid #ddd; border-radius: 6px; font-family: monospace; font-size: 12px;\" placeholder=\"Paste your data here...\\n\\nSupported formats:\\n- Plain text (entries separated by blank lines)\\n- CSV (with headers)\\n- JSON array of objects with text field\"></textarea>
                <div style=\"display: flex; gap: 8px; justify-content: flex-end;\">
                    <button class=\"btn btn-secondary\" onclick=\"closePasteModal()\">Cancel</button>
                    <button class=\"btn\" onclick=\"importPastedData()\">Import Data</button>
                </div>
            </div>
        </div>

        <!-- Main Analysis Card -->
        <div class=\"card\">
            <h2>Text Analysis Input</h2>
            <p style=\"font-size: 12px; color: #666; margin-bottom: 10px;\">Enter text to identify mental models, detect cognitive biases, and discover patterns using multiple analysis methods.</p>
            
            <!-- Quick Templates Row -->
            <div style=\"margin-bottom: 12px; padding: 10px; background: #f8f9fa; border-radius: 6px;\">
                <label style=\"font-weight: bold; display: block; margin-bottom: 6px; font-size: 12px;\">Quick Templates:</label>
                <div style=\"display: flex; gap: 6px; flex-wrap: wrap;\">
                    <button class=\"btn btn-secondary\" style=\"font-size: 11px; padding: 4px 10px;\" onclick=\"loadTemplate('decision')\">Decision Analysis</button>
                    <button class=\"btn btn-secondary\" style=\"font-size: 11px; padding: 4px 10px;\" onclick=\"loadTemplate('problem')\">Problem Solving</button>
                    <button class=\"btn btn-secondary\" style=\"font-size: 11px; padding: 4px 10px;\" onclick=\"loadTemplate('strategy')\">Strategy Review</button>
                    <button class=\"btn btn-secondary\" style=\"font-size: 11px; padding: 4px 10px;\" onclick=\"loadTemplate('negotiation')\">Negotiation Preparation</button>
                    <button class=\"btn btn-secondary\" style=\"font-size: 11px; padding: 4px 10px;\" onclick=\"loadTemplate('investment')\">Investment Analysis</button>
                    <button class=\"btn btn-secondary\" style=\"font-size: 11px; padding: 4px 10px;\" onclick=\"loadTemplate('meeting')\">Meeting Notes</button>
                    <button class=\"btn btn-secondary\" style=\"font-size: 11px; padding: 4px 10px;\" onclick=\"loadTemplate('munger')\">Munger Checklist</button>
                </div>
            </div>
            
            <!-- Text Input -->
            <textarea id=\"analysis-text\" placeholder=\"Enter text to analyze...\" style=\"min-height: 150px;\"></textarea>
            
            <!-- Analysis Options Row -->
            <div style=\"margin-top: 10px; padding: 10px; background: #f8f9fa; border-radius: 6px;\">
                <div style=\"display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 10px;\">
                    <div style=\"display: flex; align-items: center; gap: 10px;\">
                        <label style=\"font-size: 12px;\">Top Models:</label>
                        <select id=\"top-n-select\" style=\"padding: 4px 8px; border-radius: 4px; border: 1px solid #ddd;\">
                            <option value=\"5\">5</option>
                            <option value=\"10\" selected>10</option>
                            <option value=\"15\">15</option>
                            <option value=\"20\">20</option>
                        </select>
                    </div>
                    <div style=\"display: flex; align-items: center; gap: 8px;\">
                        <label style=\"font-size: 12px;\">
                            <input type=\"checkbox\" id=\"auto-save-history\" checked> Auto-save to history
                        </label>
                        <label style=\"font-size: 12px;\">
                            <input type=\"checkbox\" id=\"show-explanations\" checked> Show explanations
                        </label>
                    </div>
                </div>
            </div>
            
            <!-- Analysis Buttons - Primary Actions -->
            <div style=\"margin-top: 15px;\">
                <div style=\"display: flex; gap: 8px; flex-wrap: wrap; margin-bottom: 10px;\">
                    <button class=\"btn\" onclick=\"runFullAnalysis()\" style=\"background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);\">
                        Run Comprehensive Analysis (Recommended)
                    </button>
                </div>
                
                <!-- Secondary Analysis Methods -->
                <div style=\"display: flex; gap: 8px; flex-wrap: wrap;\">
                    <button class=\"btn btn-secondary\" onclick=\"analyzeText()\">Detect Mental Models</button>
                    <button class=\"btn btn-secondary\" onclick=\"detectBiases()\">Detect Cognitive Biases</button>
                    <button class=\"btn btn-secondary\" onclick=\"runBayesianAnalysis()\">Bayesian Probability Analysis</button>
                    <button class=\"btn btn-secondary\" onclick=\"runPatternAnalysis()\">Extract Patterns and Insights</button>
                    <button class=\"btn btn-secondary\" onclick=\"clearInput()\" style=\"background: #6c757d;\">Clear Input</button>
                </div>
            </div>
        </div>
        
        <!-- Results Container -->
        <div id=\"results\"></div>
        
        <!-- Export Section -->
        <div id=\"export-buttons\" style=\"display: none; margin-top: 15px;\">
            <div class=\"card\">
                <div style=\"display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 10px;\">
                    <div>
                        <h3 style=\"margin: 0; font-size: 14px;\">Export Analysis Results</h3>
                        <p style=\"margin: 5px 0 0 0; font-size: 11px; color: #666;\">Download or share your analysis in various formats</p>
                    </div>
                    <div style=\"display: flex; gap: 8px; flex-wrap: wrap;\">
                        <button class=\"btn\" onclick=\"exportHTMLReport()\" style=\"font-size: 12px; padding: 6px 12px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);\">Export HTML Report</button>
                        <button class=\"btn btn-secondary\" onclick=\"exportJSON()\" style=\"font-size: 12px; padding: 6px 12px;\">Export as JSON</button>
                        <button class=\"btn btn-secondary\" onclick=\"exportPDF()\" style=\"font-size: 12px; padding: 6px 12px;\">Export as PDF</button>
                        <button class=\"btn btn-secondary\" onclick=\"exportMarkdown()\" style=\"font-size: 12px; padding: 6px 12px;\">Export as Markdown</button>
                        <button class=\"btn btn-secondary\" onclick=\"copyToClipboard()\" style=\"font-size: 12px; padding: 6px 12px;\">Copy to Clipboard</button>
                    </div>
                </div>
            </div>
        </div>
        
        <script>
            let lastAnalysisResult = null;
            let lastAnalysisType = null;
            let analysisStartTime = null;
            let importedFiles = [];
            let batchResults = [];
            
            // ========== Data Import Functions ==========
            
    ">>.
