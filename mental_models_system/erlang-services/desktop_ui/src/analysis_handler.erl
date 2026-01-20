%%%-------------------------------------------------------------------
%%% @doc Analysis Handler - Mental model analysis page with comprehensive features
%%% 
%%% Provides an information-dense UI for mental model analysis following
%%% Charlie Munger's latticework approach. Features include:
%%% - Multiple analysis methods (Models, Biases, Bayesian, Patterns, Full)
%%% - Lollapalooza effect detection
%%% - Export capabilities (JSON, PDF, Clipboard)
%%% - Analysis history tracking
%%% @end
%%%-------------------------------------------------------------------
-module(analysis_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Content = [
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
                    <div style=\"display: flex; gap: 8px;\">
                        <button class=\"btn\" onclick=\"exportJSON()\" style=\"font-size: 12px; padding: 6px 12px;\">Export as JSON</button>
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
            
            function handleFileUpload(event) {
                const files = Array.from(event.target.files);
                files.forEach(file => {
                    const reader = new FileReader();
                    reader.onload = (e) => {
                        importedFiles.push({
                            name: file.name,
                            type: file.type || getFileType(file.name),
                            content: e.target.result,
                            size: file.size
                        });
                        updateImportPreview();
                    };
                    reader.readAsText(file);
                });
            }
            
            function getFileType(filename) {
                const ext = filename.split('.').pop().toLowerCase();
                const types = {txt: 'text/plain', csv: 'text/csv', json: 'application/json', md: 'text/markdown'};
                return types[ext] || 'text/plain';
            }
            
            function updateImportPreview() {
                const preview = document.getElementById('import-preview');
                const list = document.getElementById('import-files-list');
                const count = document.getElementById('file-count');
                
                if (importedFiles.length > 0) {
                    preview.style.display = 'block';
                    count.textContent = importedFiles.length + ' file(s)';
                    list.innerHTML = importedFiles.map((f, i) => 
                        '<div style=\"display: flex; justify-content: space-between; align-items: center; padding: 3px 0;\">' +
                        '<span>' + f.name + ' (' + formatSize(f.size) + ')</span>' +
                        '<button onclick=\"removeFile(' + i + ')\" style=\"background: none; border: none; color: white; cursor: pointer; font-size: 14px;\">x</button>' +
                        '</div>'
                    ).join('');
                } else {
                    preview.style.display = 'none';
                    count.textContent = '';
                }
            }
            
            function formatSize(bytes) {
                if (bytes < 1024) return bytes + ' B';
                if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(1) + ' KB';
                return (bytes / (1024 * 1024)).toFixed(1) + ' MB';
            }
            
            function removeFile(index) {
                importedFiles.splice(index, 1);
                updateImportPreview();
            }
            
            function clearImportedFiles() {
                importedFiles = [];
                document.getElementById('file-upload').value = '';
                updateImportPreview();
            }
            
            function showPasteModal() {
                document.getElementById('paste-modal').style.display = 'flex';
            }
            
            function closePasteModal() {
                document.getElementById('paste-modal').style.display = 'none';
            }
            
            function importPastedData() {
                const data = document.getElementById('bulk-paste-area').value.trim();
                if (!data) { alert('Please paste some data first'); return; }
                
                try {
                    const jsonData = JSON.parse(data);
                    if (Array.isArray(jsonData)) {
                        jsonData.forEach((item, i) => {
                            const text = typeof item === 'string' ? item : (item.text || item.content || JSON.stringify(item));
                            importedFiles.push({ name: 'item-' + (i + 1) + '.txt', type: 'text/plain', content: text, size: text.length });
                        });
                    }
                } catch (e) {
                    if (data.includes(',') && data.includes('\\n')) {
                        const lines = data.split('\\n');
                        const headers = lines[0].split(',').map(h => h.trim());
                        const textCol = headers.findIndex(h => h.toLowerCase().includes('text') || h.toLowerCase().includes('content'));
                        if (textCol >= 0) {
                            lines.slice(1).forEach((line, i) => {
                                const cols = line.split(',');
                                if (cols[textCol]) {
                                    importedFiles.push({ name: 'row-' + (i + 1) + '.txt', type: 'text/plain', content: cols[textCol].replace(/^\"|\"$/g, ''), size: cols[textCol].length });
                                }
                            });
                        } else {
                            lines.forEach((line, i) => {
                                if (line.trim()) importedFiles.push({ name: 'line-' + (i + 1) + '.txt', type: 'text/plain', content: line.trim(), size: line.length });
                            });
                        }
                    } else {
                        const entries = data.split(/\\n\\s*\\n/).filter(e => e.trim());
                        entries.forEach((entry, i) => {
                            importedFiles.push({ name: 'entry-' + (i + 1) + '.txt', type: 'text/plain', content: entry.trim(), size: entry.length });
                        });
                    }
                }
                updateImportPreview();
                closePasteModal();
                document.getElementById('bulk-paste-area').value = '';
            }
            
            async function processBatchData() {
                if (importedFiles.length === 0) { alert('No files to process'); return; }
                
                setStatus('Processing batch...', 'yellow');
                document.getElementById('results').innerHTML = '<div class=\"loading\">Processing ' + importedFiles.length + ' items...</div>';
                batchResults = [];
                
                for (let i = 0; i < importedFiles.length; i++) {
                    const file = importedFiles[i];
                    document.getElementById('results').innerHTML = '<div class=\"loading\">Processing item ' + (i + 1) + ' of ' + importedFiles.length + '...</div>';
                    
                    try {
                        const res = await fetch('/api/analysis/comprehensive', {
                            method: 'POST',
                            headers: {'Content-Type': 'application/json'},
                            body: JSON.stringify({text: file.content, top_n: 5})
                        });
                        const data = await res.json();
                        batchResults.push({ file: file.name, success: true, result: data });
                    } catch (e) {
                        batchResults.push({ file: file.name, success: false, error: e.message });
                    }
                }
                
                displayBatchResults();
                setStatus('Batch complete', 'green');
            }
            
            function displayBatchResults() {
                let html = '<div class=\"card\">';
                html += '<h2>Batch Analysis Results</h2>';
                html += '<p style=\"font-size: 12px; color: #666;\">Processed ' + batchResults.length + ' items</p>';
                html += '<div style=\"margin-top: 10px; display: flex; gap: 8px;\">';
                html += '<button class=\"btn\" onclick=\"exportBatchJSON()\" style=\"font-size: 11px;\">Export All as JSON</button>';
                html += '<button class=\"btn btn-secondary\" onclick=\"exportBatchCSV()\" style=\"font-size: 11px;\">Export Summary CSV</button>';
                html += '</div></div>';
                
                batchResults.forEach((item) => {
                    html += '<div class=\"card\" style=\"margin-top: 10px;\">';
                    html += '<div style=\"display: flex; justify-content: space-between; align-items: center;\">';
                    html += '<h4 style=\"margin: 0;\">' + item.file + '</h4>';
                    html += '<span style=\"font-size: 11px; color: ' + (item.success ? '#28a745' : '#dc3545') + ';\">' + (item.success ? 'Success' : 'Failed') + '</span>';
                    html += '</div>';
                    
                    if (item.success && item.result) {
                        const lolla = item.result.lollapalooza || {};
                        if (lolla.detected) {
                            html += '<div style=\"background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); color: white; padding: 8px; border-radius: 6px; margin: 8px 0; font-size: 12px;\">';
                            html += '<strong>Lollapalooza!</strong> ' + lolla.convergence_count + ' models (' + lolla.strength + ')';
                            html += '</div>';
                        }
                        const models = item.result.analysis || [];
                        if (models.length > 0) {
                            html += '<div style=\"display: flex; flex-wrap: wrap; gap: 4px; margin-top: 8px;\">';
                            models.slice(0, 5).forEach(m => {
                                html += '<span style=\"background: #e9ecef; padding: 3px 8px; border-radius: 4px; font-size: 11px;\">' + m.name + '</span>';
                            });
                            html += '</div>';
                        }
                    }
                    html += '</div>';
                });
                
                document.getElementById('results').innerHTML = html;
                document.getElementById('export-buttons').style.display = 'none';
                lastAnalysisResult = batchResults;
                lastAnalysisType = 'batch';
            }
            
            function exportBatchJSON() {
                const blob = new Blob([JSON.stringify(batchResults, null, 2)], {type: 'application/json'});
                const url = URL.createObjectURL(blob);
                const a = document.createElement('a'); a.href = url;
                a.download = 'batch-analysis-' + new Date().toISOString().slice(0,10) + '.json';
                a.click();
            }
            
            function exportBatchCSV() {
                let csv = 'File,Success,Lollapalooza,Convergence,TopModel,TopScore\\n';
                batchResults.forEach(item => {
                    const lolla = item.result?.lollapalooza || {};
                    const top = item.result?.analysis?.[0] || {};
                    csv += [item.file, item.success, lolla.detected || false, lolla.convergence_count || 0, top.name || '', top.relevance || 0].join(',') + '\\n';
                });
                const blob = new Blob([csv], {type: 'text/csv'});
                const url = URL.createObjectURL(blob);
                const a = document.createElement('a'); a.href = url;
                a.download = 'batch-summary-' + new Date().toISOString().slice(0,10) + '.csv';
                a.click();
            }
            
            // ========== Core Functions ==========
            
            // Initialize on page load
            document.addEventListener('DOMContentLoaded', function() {
                refreshSystemStatus();
                loadLastAnalysisTime();
            });
            
            async function refreshSystemStatus() {
                try {
                    const res = await fetch('/api/analysis/models');
                    const data = await res.json();
                    document.getElementById('model-count').textContent = (data.models || []).length;
                } catch (e) {
                    document.getElementById('model-count').textContent = 'Error';
                }
            }
            
            function loadLastAnalysisTime() {
                const lastTime = localStorage.getItem('lastAnalysisTime');
                if (lastTime) {
                    document.getElementById('last-analysis-time').textContent = new Date(lastTime).toLocaleString();
                }
            }
            
            function updateLastAnalysisTime() {
                const now = new Date().toISOString();
                localStorage.setItem('lastAnalysisTime', now);
                document.getElementById('last-analysis-time').textContent = new Date(now).toLocaleString();
            }
            
            function setStatus(status, color) {
                const statusEl = document.getElementById('analysis-status');
                statusEl.textContent = status;
                statusEl.className = color === 'green' ? 'status-healthy' : 
                                     color === 'yellow' ? 'status-unknown' : 
                                     color === 'red' ? 'status-unhealthy' : 'status-healthy';
            }
            
            function clearInput() {
                document.getElementById('analysis-text').value = '';
                document.getElementById('results').innerHTML = '';
                document.getElementById('export-buttons').style.display = 'none';
                setStatus('Ready', 'green');
            }
            
            function getTopN() {
                return parseInt(document.getElementById('top-n-select').value) || 10;
            }
            
            const templates = {
                decision: 'DECISION ANALYSIS\\n\\nDecision to make: [Describe the decision]\\n\\nOptions:\\n1. [Option A]\\n2. [Option B]\\n3. [Option C]\\n\\nPros and Cons:\\n- Option A: [pros/cons]\\n- Option B: [pros/cons]\\n- Option C: [pros/cons]\\n\\nKey factors to consider:\\n- [Factor 1]\\n- [Factor 2]\\n\\nTimeline: [When decision needs to be made]\\n\\nStakeholders affected: [Who is impacted]',
                problem: 'PROBLEM SOLVING\\n\\nProblem statement: [Describe the problem clearly]\\n\\nCurrent situation: [What is happening now]\\n\\nDesired outcome: [What should be happening]\\n\\nRoot causes identified:\\n1. [Cause 1]\\n2. [Cause 2]\\n\\nPotential solutions:\\n1. [Solution 1]\\n2. [Solution 2]\\n\\nConstraints: [Time, budget, resources]\\n\\nSuccess metrics: [How will we know it is solved]',
                strategy: 'STRATEGY REVIEW\\n\\nObjective: [What are we trying to achieve]\\n\\nCurrent strategy: [Describe current approach]\\n\\nMarket conditions: [External factors]\\n\\nCompetitive landscape: [Key competitors and their moves]\\n\\nStrengths to leverage:\\n- [Strength 1]\\n- [Strength 2]\\n\\nWeaknesses to address:\\n- [Weakness 1]\\n- [Weakness 2]\\n\\nOpportunities identified:\\n- [Opportunity 1]\\n\\nThreats to mitigate:\\n- [Threat 1]\\n\\nProposed changes: [What should we do differently]',
                negotiation: 'NEGOTIATION PREPARATION\\n\\nNegotiation context: [What is being negotiated]\\n\\nOur position: [What we want]\\n\\nTheir likely position: [What they want]\\n\\nOur BATNA (Best Alternative): [What we do if no deal]\\n\\nTheir likely BATNA: [What they do if no deal]\\n\\nKey interests (ours):\\n- [Interest 1]\\n- [Interest 2]\\n\\nKey interests (theirs):\\n- [Interest 1]\\n- [Interest 2]\\n\\nPotential trade-offs: [What can we give up]\\n\\nDeal breakers: [What we cannot accept]\\n\\nOpening offer: [Where to start]\\n\\nTarget outcome: [Ideal result]',
                investment: 'INVESTMENT ANALYSIS\\n\\nInvestment opportunity: [Describe the investment]\\n\\nAmount: [How much]\\n\\nExpected return: [ROI expectations]\\n\\nTime horizon: [Investment period]\\n\\nRisk factors:\\n1. [Risk 1]\\n2. [Risk 2]\\n3. [Risk 3]\\n\\nMitigation strategies: [How to reduce risks]\\n\\nMarket analysis: [Industry trends]\\n\\nCompetitive moat: [What protects this investment]\\n\\nExit strategy: [How and when to exit]\\n\\nAlternative investments considered: [Other options]',
                meeting: 'MEETING NOTES\\n\\nDate: [Date]\\nAttendees: [Who was present]\\nPurpose: [Why we met]\\n\\nKey discussion points:\\n1. [Topic 1]: [Summary]\\n2. [Topic 2]: [Summary]\\n3. [Topic 3]: [Summary]\\n\\nDecisions made:\\n- [Decision 1]\\n- [Decision 2]\\n\\nAction items:\\n- [Action 1] - Owner: [Name] - Due: [Date]\\n- [Action 2] - Owner: [Name] - Due: [Date]\\n\\nOpen questions:\\n- [Question 1]\\n\\nNext steps: [What happens next]\\n\\nFollow-up meeting: [If scheduled]',
                munger: 'MUNGER INVESTMENT CHECKLIST\\n\\nCompany/Opportunity: [Name]\\n\\n1. UNDERSTANDING THE BUSINESS\\n- Can I explain this business to a child? [Yes/No]\\n- What is the core value proposition?\\n- How does it make money?\\n- What are the unit economics?\\n\\n2. COMPETITIVE ADVANTAGE (MOAT)\\n- Does it have pricing power?\\n- Network effects?\\n- Switching costs?\\n- Cost advantages?\\n- Intangible assets (brand, patents)?\\n\\n3. MANAGEMENT QUALITY\\n- Are they owner-operators?\\n- Capital allocation track record?\\n- Insider ownership?\\n- Compensation aligned with shareholders?\\n\\n4. FINANCIAL STRENGTH\\n- Debt levels reasonable?\\n- Free cash flow positive?\\n- Return on invested capital?\\n- Margin trends?\\n\\n5. VALUATION\\n- What is it worth?\\n- Margin of safety?\\n- What could go wrong?\\n\\n6. INVERSION - What would make this fail?\\n- [Failure mode 1]\\n- [Failure mode 2]\\n- [Failure mode 3]'
            };
            
            function loadTemplate(type) {
                if (templates[type]) {
                    document.getElementById('analysis-text').value = templates[type];
                }
            }
            
            async function saveToHistory(type, inputText, models, biases) {
                if (!document.getElementById('auto-save-history').checked) return;
                try {
                    await fetch('/api/storage/history', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({
                            type: type,
                            input_text: inputText,
                            models: models || [],
                            biases: biases || []
                        })
                    });
                } catch (e) {
                    console.log('Failed to save to history:', e);
                }
            }
            
            async function analyzeText() {
                const text = document.getElementById('analysis-text').value;
                if (!text.trim()) {
                    alert('Please enter some text to analyze');
                    return;
                }
                
                setStatus('Analyzing...', 'yellow');
                analysisStartTime = Date.now();
                document.getElementById('results').innerHTML = '<div class=\"loading\">Detecting mental models...</div>';
                document.getElementById('export-buttons').style.display = 'none';
                
                try {
                    const res = await fetch('/api/analysis/analyze', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({text: text, top_n: getTopN()})
                    });
                    const data = await res.json();
                    lastAnalysisResult = data;
                    lastAnalysisType = 'models';
                    
                    const elapsed = Date.now() - analysisStartTime;
                    saveToHistory('models', text, data.models || [], []);
                    updateLastAnalysisTime();
                    
                    let html = '<div class=\"card\"><h2>Mental Model Detection Results</h2>';
                    html += '<p style=\"font-size: 11px; color: #666;\">Analysis completed in ' + elapsed + 'ms | Method: ' + (data.method || 'keyword_matching') + '</p>';
                    
                    if (data.models && data.models.length > 0) {
                        html += '<p style=\"margin-top: 10px;\">Found <strong>' + data.models.length + '</strong> relevant mental models:</p>';
                        html += '<div style=\"display: grid; gap: 10px; margin-top: 15px;\">';
                        for (const model of data.models) {
                            const relevance = model.relevance || 0;
                            const relevanceColor = relevance >= 70 ? '#28a745' : relevance >= 40 ? '#ffc107' : '#6c757d';
                            html += '<div class=\"model-card\">';
                            html += '<div style=\"display: flex; justify-content: space-between; align-items: flex-start;\">';
                            html += '<div><h4 style=\"margin: 0;\">' + model.name + '</h4>';
                            html += '<span class=\"category\">' + model.category + '</span></div>';
                            if (relevance > 0) {
                                html += '<span style=\"background: ' + relevanceColor + '; color: white; padding: 3px 10px; border-radius: 12px; font-size: 11px; white-space: nowrap;\">' + relevance + '%</span>';
                            }
                            html += '</div>';
                            html += '<p style=\"margin-top: 8px; font-size: 13px;\">' + model.description + '</p>';
                            html += '</div>';
                        }
                        html += '</div>';
                    } else {
                        html += '<p>No specific mental models detected. Try providing more context.</p>';
                    }
                    html += '</div>';
                    
                    document.getElementById('results').innerHTML = html;
                    document.getElementById('export-buttons').style.display = 'block';
                    setStatus('Complete', 'green');
                } catch (e) {
                    document.getElementById('results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                    setStatus('Error', 'red');
                }
            }
            
            async function detectBiases() {
                const text = document.getElementById('analysis-text').value;
                if (!text.trim()) {
                    alert('Please enter some text to analyze');
                    return;
                }
                
                setStatus('Detecting biases...', 'yellow');
                analysisStartTime = Date.now();
                document.getElementById('results').innerHTML = '<div class=\"loading\">Scanning for cognitive biases...</div>';
                document.getElementById('export-buttons').style.display = 'none';
                
                try {
                    const res = await fetch('/api/analysis/detect-biases', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({text: text})
                    });
                    const data = await res.json();
                    lastAnalysisResult = data;
                    lastAnalysisType = 'biases';
                    
                    const elapsed = Date.now() - analysisStartTime;
                    saveToHistory('biases', text, [], data.biases || []);
                    updateLastAnalysisTime();
                    
                    let html = '<div class=\"card\"><h2>Cognitive Bias Detection Results</h2>';
                    html += '<p style=\"font-size: 11px; color: #666;\">Analysis completed in ' + elapsed + 'ms</p>';
                    
                    if (data.biases && data.biases.length > 0) {
                        html += '<p style=\"margin-top: 10px;\">Found <strong>' + data.biases.length + '</strong> potential cognitive biases:</p>';
                        html += '<div style=\"display: grid; gap: 10px; margin-top: 15px;\">';
                        for (const bias of data.biases) {
                            const severityColor = bias.severity === 'high' ? '#dc3545' : 
                                                  bias.severity === 'medium' ? '#ffc107' : '#28a745';
                            html += '<div class=\"model-card\" style=\"border-left: 4px solid ' + severityColor + ';\">';
                            html += '<div style=\"display: flex; justify-content: space-between; align-items: center;\">';
                            html += '<h4 style=\"margin: 0;\">' + bias.bias.replace(/_/g, ' ').replace(/\\b\\w/g, l => l.toUpperCase()) + '</h4>';
                            html += '<span style=\"background: ' + severityColor + '; color: white; padding: 3px 10px; border-radius: 12px; font-size: 11px;\">' + bias.severity + ' severity</span>';
                            html += '</div>';
                            if (bias.evidence && bias.evidence.length > 0) {
                                html += '<p style=\"margin-top: 8px; font-size: 12px;\"><strong>Evidence:</strong> ' + bias.evidence.join(', ') + '</p>';
                            }
                            html += '</div>';
                        }
                        html += '</div>';
                    } else {
                        html += '<p>No obvious cognitive biases detected in the text.</p>';
                    }
                    html += '</div>';
                    
                    document.getElementById('results').innerHTML = html;
                    document.getElementById('export-buttons').style.display = 'block';
                    setStatus('Complete', 'green');
                } catch (e) {
                    document.getElementById('results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                    setStatus('Error', 'red');
                }
            }
            
            async function runBayesianAnalysis() {
                const text = document.getElementById('analysis-text').value;
                if (!text.trim()) {
                    alert('Please enter some text to analyze');
                    return;
                }
                
                setStatus('Running Bayesian analysis...', 'yellow');
                analysisStartTime = Date.now();
                document.getElementById('results').innerHTML = '<div class=\"loading\">Calculating posterior probabilities using Bayesian inference...</div>';
                document.getElementById('export-buttons').style.display = 'none';
                
                try {
                    const res = await fetch('/api/analysis/bayesian', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({text: text, top_n: getTopN()})
                    });
                    const data = await res.json();
                    lastAnalysisResult = data;
                    lastAnalysisType = 'bayesian';
                    
                    const elapsed = Date.now() - analysisStartTime;
                    saveToHistory('bayesian', text, data.models || [], []);
                    updateLastAnalysisTime();
                    
                    let html = '<div class=\"card\">';
                    html += '<h2>Bayesian Probability Analysis Results</h2>';
                    html += '<p style=\"font-size: 11px; color: #666;\">Analysis completed in ' + elapsed + 'ms | Evidence terms extracted: ' + (data.evidence_extracted || 0) + '</p>';
                    html += '</div>';
                    
                    if (data.models && data.models.length > 0) {
                        html += '<div class=\"card\"><h2>Models Ranked by Posterior Probability</h2>';
                        html += '<p style=\"font-size: 12px; color: #666;\">P(Model|Evidence) calculated using Bayes\\' theorem</p>';
                        html += '<div style=\"display: grid; gap: 10px; margin-top: 15px;\">';
                        for (const model of data.models) {
                            const score = model.bayesian_score || 0;
                            const scoreColor = score >= 70 ? '#28a745' : score >= 40 ? '#ffc107' : '#6c757d';
                            html += '<div class=\"model-card\">';
                            html += '<div style=\"display: flex; justify-content: space-between; align-items: flex-start;\">';
                            html += '<div><h4 style=\"margin: 0;\">' + model.name + '</h4>';
                            html += '<span class=\"category\">' + model.category + '</span></div>';
                            html += '<span style=\"background: ' + scoreColor + '; color: white; padding: 3px 10px; border-radius: 12px; font-size: 11px;\">' + score + '% posterior</span>';
                            html += '</div>';
                            html += '<p style=\"margin-top: 8px; font-size: 13px;\">' + model.description + '</p>';
                            html += '<div style=\"margin-top: 10px; padding: 10px; background: #f8f9fa; border-radius: 6px; font-size: 11px;\">';
                            html += '<div style=\"display: grid; grid-template-columns: repeat(4, 1fr); gap: 8px; text-align: center;\">';
                            html += '<div><strong>Prior</strong><br>' + (model.prior || 0) + '%</div>';
                            html += '<div><strong>Likelihood</strong><br>' + (model.likelihood || 0) + '%</div>';
                            html += '<div><strong>Evidence</strong><br>' + (model.evidence_count || 0) + ' matches</div>';
                            html += '<div><strong>95% CI</strong><br>[' + (model.confidence_lower || 0) + '%, ' + (model.confidence_upper || 0) + '%]</div>';
                            html += '</div></div>';
                            html += '</div>';
                        }
                        html += '</div>';
                    } else {
                        html += '<div class=\"card\"><p>No models found with significant posterior probability.</p></div>';
                    }
                    
                    if (document.getElementById('show-explanations').checked) {
                        html += '<div class=\"card\" style=\"background: #f8f9fa;\">';
                        html += '<h3 style=\"font-size: 14px;\">About Bayesian Analysis</h3>';
                        html += '<p style=\"font-size: 12px;\">Bayesian inference updates beliefs using Bayes\\' theorem:</p>';
                        html += '<p style=\"text-align: center; font-family: monospace; margin: 10px 0; font-size: 13px; background: white; padding: 10px; border-radius: 4px;\">P(Model|Evidence) = P(Evidence|Model) × P(Model) / P(Evidence)</p>';
                        html += '<ul style=\"font-size: 12px; margin: 0; padding-left: 20px;\">';
                        html += '<li><strong>Prior:</strong> Base probability before seeing evidence</li>';
                        html += '<li><strong>Likelihood:</strong> Probability of evidence given the model</li>';
                        html += '<li><strong>Posterior:</strong> Updated probability after evidence</li>';
                        html += '</ul></div>';
                    }
                    
                    document.getElementById('results').innerHTML = html;
                    document.getElementById('export-buttons').style.display = 'block';
                    setStatus('Complete', 'green');
                } catch (e) {
                    document.getElementById('results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                    setStatus('Error', 'red');
                }
            }
            
            async function runPatternAnalysis() {
                const text = document.getElementById('analysis-text').value;
                if (!text.trim()) {
                    alert('Please enter some text to analyze');
                    return;
                }
                
                setStatus('Extracting patterns...', 'yellow');
                analysisStartTime = Date.now();
                document.getElementById('results').innerHTML = '<div class=\"loading\">Extracting patterns, insights, and generating inverted perspectives...</div>';
                document.getElementById('export-buttons').style.display = 'none';
                
                try {
                    const res = await fetch('/api/analysis/patterns', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({text: text, action: 'all'})
                    });
                    const data = await res.json();
                    lastAnalysisResult = data;
                    lastAnalysisType = 'patterns';
                    
                    const elapsed = Date.now() - analysisStartTime;
                    saveToHistory('patterns', text, [], []);
                    updateLastAnalysisTime();
                    
                    let html = '';
                    
                    // Inversions section (Munger's Inversion)
                    const inversions = data.inversions || [];
                    if (inversions.length > 0) {
                        html += '<div class=\"card\" style=\"background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%); color: white;\">';
                        html += '<h2 style=\"color: white; margin-bottom: 5px;\">Inverted Perspective (Munger\\'s Inversion)</h2>';
                        html += '<p style=\"font-style: italic; font-size: 12px; opacity: 0.9;\">' + (data.munger_quote || '\"Invert, always invert.\" - Charlie Munger') + '</p>';
                        html += '<div style=\"margin-top: 15px;\">';
                        for (const inv of inversions) {
                            html += '<div style=\"background: rgba(255,255,255,0.15); padding: 12px; border-radius: 8px; margin-bottom: 10px;\">';
                            html += '<p style=\"margin: 0;\"><strong>Original:</strong> ' + inv.original + '</p>';
                            html += '<p style=\"margin: 8px 0 0 0;\"><strong>Inverted:</strong> ' + inv.inverted + '</p>';
                            html += '<p style=\"margin: 8px 0 0 0; font-style: italic; opacity: 0.9;\"><strong>Key Question:</strong> ' + inv.question + '</p>';
                            html += '</div>';
                        }
                        html += '</div></div>';
                    }
                    
                    // Insights section
                    const insights = data.insights || [];
                    if (insights.length > 0) {
                        html += '<div class=\"card\">';
                        html += '<h2>Key Insights Extracted</h2>';
                        html += '<p style=\"font-size: 12px; color: #666;\">Actionable insights identified from your text:</p>';
                        html += '<div style=\"display: grid; gap: 10px; margin-top: 15px;\">';
                        for (const insight of insights) {
                            const typeColor = insight.type === 'absolute' ? '#dc3545' : 
                                              insight.type === 'causal' ? '#17a2b8' : 
                                              insight.type === 'contrast' ? '#6f42c1' : '#6c757d';
                            html += '<div class=\"model-card\" style=\"border-left: 4px solid ' + typeColor + ';\">';
                            html += '<h4 style=\"margin: 0; font-size: 13px;\">' + (insight.type || 'Insight').replace(/_/g, ' ').replace(/\\b\\w/g, l => l.toUpperCase()) + '</h4>';
                            html += '<p style=\"margin: 8px 0 0 0;\">' + insight.insight + '</p>';
                            if (insight.action) {
                                html += '<p style=\"margin: 8px 0 0 0; font-size: 12px; color: #666;\"><strong>Suggested Action:</strong> ' + insight.action + '</p>';
                            }
                            html += '</div>';
                        }
                        html += '</div></div>';
                    }
                    
                    // Patterns section
                    const patterns = data.patterns || [];
                    if (patterns.length > 0) {
                        html += '<div class=\"card\">';
                        html += '<h2>Detected Patterns</h2>';
                        html += '<p style=\"font-size: 12px; color: #666;\">Recurring patterns identified in your text:</p>';
                        html += '<div style=\"display: grid; gap: 10px; margin-top: 15px;\">';
                        for (const pattern of patterns) {
                            const confidence = pattern.confidence || 0;
                            const confColor = confidence >= 75 ? '#28a745' : confidence >= 50 ? '#ffc107' : '#6c757d';
                            html += '<div class=\"model-card\">';
                            html += '<div style=\"display: flex; justify-content: space-between; align-items: center;\">';
                            html += '<h4 style=\"margin: 0;\">' + pattern.pattern + '</h4>';
                            html += '<span style=\"background: ' + confColor + '; color: white; padding: 3px 10px; border-radius: 12px; font-size: 11px;\">' + confidence + '% confidence</span>';
                            html += '</div>';
                            html += '<span class=\"category\">' + (pattern.type || 'Pattern').replace(/_/g, ' ').replace(/\\b\\w/g, l => l.toUpperCase()) + '</span>';
                            html += '<p style=\"margin-top: 8px;\">' + pattern.description + '</p>';
                            html += '</div>';
                        }
                        html += '</div></div>';
                    }
                    
                    // Summary stats
                    html += '<div class=\"card\" style=\"background: #f8f9fa;\">';
                    html += '<p style=\"font-size: 11px; color: #666; margin: 0;\">Analysis completed in ' + elapsed + 'ms | ';
                    html += 'Inversions: ' + inversions.length + ' | Insights: ' + insights.length + ' | Patterns: ' + patterns.length + '</p>';
                    html += '</div>';
                    
                    if (inversions.length === 0 && insights.length === 0 && patterns.length === 0) {
                        html = '<div class=\"card\"><p>No significant patterns detected. Try providing more detailed text with clear statements and reasoning.</p></div>';
                    }
                    
                    document.getElementById('results').innerHTML = html;
                    document.getElementById('export-buttons').style.display = 'block';
                    setStatus('Complete', 'green');
                } catch (e) {
                    document.getElementById('results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                    setStatus('Error', 'red');
                }
            }
            
            async function runFullAnalysis() {
                const text = document.getElementById('analysis-text').value;
                if (!text.trim()) {
                    alert('Please enter some text to analyze');
                    return;
                }
                
                setStatus('Running comprehensive analysis...', 'yellow');
                analysisStartTime = Date.now();
                document.getElementById('results').innerHTML = '<div class=\"loading\">Running comprehensive analysis with Lollapalooza detection, pattern extraction, and bias scanning...</div>';
                document.getElementById('export-buttons').style.display = 'none';
                
                try {
                    // Run all analyses in parallel for speed
                    const [comprehensiveRes, biasesRes, patternsRes] = await Promise.all([
                        fetch('/api/analysis/comprehensive', {
                            method: 'POST',
                            headers: {'Content-Type': 'application/json'},
                            body: JSON.stringify({text: text, top_n: getTopN()})
                        }),
                        fetch('/api/analysis/detect-biases', {
                            method: 'POST',
                            headers: {'Content-Type': 'application/json'},
                            body: JSON.stringify({text: text})
                        }),
                        fetch('/api/analysis/patterns', {
                            method: 'POST',
                            headers: {'Content-Type': 'application/json'},
                            body: JSON.stringify({text: text, action: 'all'})
                        })
                    ]);
                    
                    const comprehensiveData = await comprehensiveRes.json();
                    const biasesData = await biasesRes.json();
                    const patternsData = await patternsRes.json();
                    
                    const elapsed = Date.now() - analysisStartTime;
                    
                    lastAnalysisResult = {
                        comprehensive: comprehensiveData,
                        biases: biasesData,
                        patterns: patternsData,
                        timestamp: new Date().toISOString(),
                        inputText: text.substring(0, 500) + (text.length > 500 ? '...' : ''),
                        analysisTime: elapsed
                    };
                    lastAnalysisType = 'full';
                    
                    const models = comprehensiveData.analysis || [];
                    saveToHistory('full', text, models, biasesData.biases || []);
                    updateLastAnalysisTime();
                    
                    let html = '';
                    
                    // Lollapalooza Alert (if detected) - Most Important
                    const lollapalooza = comprehensiveData.lollapalooza || {};
                    if (lollapalooza.detected) {
                        html += '<div class=\"card\" style=\"background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white;\">';
                        html += '<h2 style=\"color: white; margin-bottom: 10px;\">LOLLAPALOOZA EFFECT DETECTED</h2>';
                        html += '<p style=\"font-size: 15px;\">Multiple mental models (<strong>' + lollapalooza.convergence_count + '</strong>) are converging with <strong>' + (lollapalooza.strength || 'significant').toUpperCase() + '</strong> intensity.</p>';
                        html += '<div style=\"margin-top: 15px; padding: 12px; background: rgba(255,255,255,0.15); border-radius: 8px;\">';
                        html += '<p style=\"margin: 0;\"><strong>Converging Models:</strong> ' + (lollapalooza.converging_models || []).join(', ') + '</p>';
                        if (lollapalooza.cross_domain) {
                            html += '<p style=\"margin: 8px 0 0 0;\"><strong>Cross-Domain:</strong> Models span categories: ' + (lollapalooza.categories_involved || []).join(', ') + '</p>';
                        }
                        html += '<p style=\"margin: 8px 0 0 0;\"><strong>Convergence Score:</strong> ' + (lollapalooza.convergence_score || 0) + '%</p>';
                        html += '</div>';
                        html += '<p style=\"margin-top: 15px; font-style: italic; font-size: 12px; opacity: 0.9;\">\"When several models combine, you get lollapalooza effects; this is when two, three, or four forces are all operating in the same direction.\" - Charlie Munger</p>';
                        html += '</div>';
                    }
                    
                    // Summary Dashboard - Information Dense
                    html += '<div class=\"card\">';
                    html += '<h2>Analysis Summary</h2>';
                    html += '<p style=\"font-size: 11px; color: #666; margin-bottom: 15px;\">Comprehensive analysis completed in ' + elapsed + 'ms</p>';
                    html += '<div style=\"display: grid; grid-template-columns: repeat(auto-fit, minmax(120px, 1fr)); gap: 12px;\">';
                    
                    const convergence = comprehensiveData.convergence || {};
                    html += '<div style=\"text-align: center; padding: 12px; background: #f8f9fa; border-radius: 8px;\">';
                    html += '<div style=\"font-size: 22px; font-weight: bold; color: #4361ee;\">' + (convergence.total_models || 0) + '</div>';
                    html += '<div style=\"font-size: 11px; color: #666;\">Models Analyzed</div></div>';
                    
                    html += '<div style=\"text-align: center; padding: 12px; background: #f8f9fa; border-radius: 8px;\">';
                    html += '<div style=\"font-size: 22px; font-weight: bold; color: #28a745;\">' + (convergence.high_scoring || 0) + '</div>';
                    html += '<div style=\"font-size: 11px; color: #666;\">High Scoring (70%+)</div></div>';
                    
                    html += '<div style=\"text-align: center; padding: 12px; background: #f8f9fa; border-radius: 8px;\">';
                    html += '<div style=\"font-size: 22px; font-weight: bold; color: #17a2b8;\">' + ((convergence.mean_score || 0).toFixed(1)) + '%</div>';
                    html += '<div style=\"font-size: 11px; color: #666;\">Mean Score</div></div>';
                    
                    html += '<div style=\"text-align: center; padding: 12px; background: #f8f9fa; border-radius: 8px;\">';
                    html += '<div style=\"font-size: 22px; font-weight: bold; color: #fd7e14;\">' + (biasesData.biases || []).length + '</div>';
                    html += '<div style=\"font-size: 11px; color: #666;\">Biases Detected</div></div>';
                    
                    html += '<div style=\"text-align: center; padding: 12px; background: #f8f9fa; border-radius: 8px;\">';
                    html += '<div style=\"font-size: 22px; font-weight: bold; color: #6f42c1;\">' + (patternsData.patterns || []).length + '</div>';
                    html += '<div style=\"font-size: 11px; color: #666;\">Patterns Found</div></div>';
                    
                    html += '<div style=\"text-align: center; padding: 12px; background: #f8f9fa; border-radius: 8px;\">';
                    html += '<div style=\"font-size: 22px; font-weight: bold; color: #20c997;\">' + (patternsData.insights || []).length + '</div>';
                    html += '<div style=\"font-size: 11px; color: #666;\">Insights</div></div>';
                    
                    html += '</div></div>';
                    
                    // Recommendations (if any)
                    const recommendations = comprehensiveData.recommendations || [];
                    if (recommendations.length > 0) {
                        html += '<div class=\"card\">';
                        html += '<h2>Recommendations</h2>';
                        html += '<div style=\"display: grid; gap: 10px;\">';
                        for (const rec of recommendations) {
                            const priorityColor = rec.priority === 'high' ? '#dc3545' : rec.priority === 'medium' ? '#ffc107' : '#28a745';
                            html += '<div class=\"model-card\" style=\"border-left: 4px solid ' + priorityColor + ';\">';
                            html += '<div style=\"display: flex; justify-content: space-between; align-items: center;\">';
                            html += '<h4 style=\"margin: 0; font-size: 13px;\">' + (rec.type || 'Insight').replace(/_/g, ' ').replace(/\\b\\w/g, l => l.toUpperCase()) + '</h4>';
                            html += '<span style=\"font-size: 10px; color: ' + priorityColor + '; text-transform: uppercase;\">' + rec.priority + ' priority</span>';
                            html += '</div>';
                            html += '<p style=\"margin: 8px 0 0 0;\">' + rec.message + '</p>';
                            if (rec.action) {
                                html += '<p style=\"margin: 8px 0 0 0; font-size: 12px; background: #f8f9fa; padding: 8px; border-radius: 4px;\"><strong>Action:</strong> ' + rec.action + '</p>';
                            }
                            html += '</div>';
                        }
                        html += '</div></div>';
                    }
                    
                    // Inversions (from patterns)
                    const inversions = patternsData.inversions || [];
                    if (inversions.length > 0) {
                        html += '<div class=\"card\" style=\"background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%); color: white;\">';
                        html += '<h2 style=\"color: white;\">Inverted Perspectives</h2>';
                        html += '<p style=\"font-size: 12px; opacity: 0.9;\">\"Invert, always invert.\" - Charlie Munger</p>';
                        html += '<div style=\"margin-top: 15px;\">';
                        for (const inv of inversions) {
                            html += '<div style=\"background: rgba(255,255,255,0.15); padding: 12px; border-radius: 8px; margin-bottom: 10px;\">';
                            html += '<p style=\"margin: 0;\"><strong>Original:</strong> ' + inv.original + '</p>';
                            html += '<p style=\"margin: 8px 0 0 0;\"><strong>Inverted:</strong> ' + inv.inverted + '</p>';
                            html += '</div>';
                        }
                        html += '</div></div>';
                    }
                    
                    // Mental Models section
                    const modelsData = comprehensiveData.analysis || [];
                    if (modelsData.length > 0) {
                        html += '<div class=\"card\"><h2>Mental Models Detected (' + modelsData.length + ')</h2>';
                        html += '<div style=\"display: grid; gap: 10px; margin-top: 15px;\">';
                        for (const model of modelsData) {
                            const relevance = model.relevance || 0;
                            const relevanceColor = relevance >= 70 ? '#28a745' : relevance >= 40 ? '#ffc107' : '#6c757d';
                            html += '<div class=\"model-card\">';
                            html += '<div style=\"display: flex; justify-content: space-between; align-items: flex-start;\">';
                            html += '<div><h4 style=\"margin: 0;\">' + model.name + '</h4>';
                            html += '<span class=\"category\">' + model.category + '</span></div>';
                            html += '<span style=\"background: ' + relevanceColor + '; color: white; padding: 3px 10px; border-radius: 12px; font-size: 11px;\">' + relevance + '%</span>';
                            html += '</div>';
                            html += '<p style=\"margin-top: 8px; font-size: 13px;\">' + model.description + '</p>';
                            html += '</div>';
                        }
                        html += '</div></div>';
                    }
                    
                    // Failure Modes
                    const failureModes = comprehensiveData.failure_modes || [];
                    if (failureModes.length > 0) {
                        html += '<div class=\"card\">';
                        html += '<h2>Potential Failure Modes</h2>';
                        html += '<p style=\"font-size: 12px; color: #666;\">Watch out for these pitfalls based on detected models:</p>';
                        html += '<div style=\"display: grid; gap: 10px; margin-top: 15px;\">';
                        for (const fm of failureModes) {
                            const riskColor = fm.risk === 'critical' ? '#dc3545' : fm.risk === 'high' ? '#fd7e14' : fm.risk === 'medium' ? '#ffc107' : '#28a745';
                            html += '<div class=\"model-card\" style=\"border-left: 4px solid ' + riskColor + ';\">';
                            html += '<div style=\"display: flex; justify-content: space-between; align-items: center;\">';
                            html += '<h4 style=\"margin: 0;\">' + fm.mode + '</h4>';
                            html += '<span style=\"background: ' + riskColor + '; color: white; padding: 3px 10px; border-radius: 12px; font-size: 11px;\">' + fm.risk + '</span>';
                            html += '</div>';
                            html += '<p style=\"margin: 5px 0 0 0; font-size: 12px; color: #666;\">Source: ' + fm.source_model + '</p>';
                            html += '</div>';
                        }
                        html += '</div></div>';
                    }
                    
                    // Text Analysis section (keyword-based detection)
                    const textAnalysis = comprehensiveData.text_analysis || {};
                    const textModels = textAnalysis.top_models || [];
                    if (textModels.length > 0) {
                        html += '<div class="card">';
                        html += '<h2>Keyword-Based Detection</h2>';
                        html += '<p style="font-size: 12px; color: #666;">Text analysis detected ' + (textAnalysis.models || []).length + ' patterns across ' + (textAnalysis.text_length || 0) + ' characters:</p>';
                        html += '<div style="display: grid; gap: 10px; margin-top: 15px;">';
                        for (const tm of textModels) {
                            const score = tm.score || 0;
                            const scoreColor = score >= 70 ? '#28a745' : score >= 40 ? '#ffc107' : '#6c757d';
                            html += '<div class="model-card">';
                            html += '<div style="display: flex; justify-content: space-between; align-items: flex-start;">';
                            html += '<div><h4 style="margin: 0;">' + tm.name + '</h4>';
                            html += '<span class="category">' + tm.category + '</span></div>';
                            html += '<span style="background: ' + scoreColor + '; color: white; padding: 3px 10px; border-radius: 12px; font-size: 11px;">' + score + '%</span>';
                            html += '</div>';
                            html += '<p style="margin-top: 8px; font-size: 12px;"><strong>Confidence:</strong> ' + (tm.confidence || 'Low') + '</p>';
                            if (tm.evidence) {
                                html += '<p style="font-size: 11px; color: #666;">Evidence: ' + (tm.evidence.keywords || 0) + ' keywords, ' + (tm.evidence.patterns || 0) + ' patterns</p>';
                            }
                            html += '</div>';
                        }
                        html += '</div></div>';
                    }
                    
                    // Cognitive Biases
                    if (biasesData.biases && biasesData.biases.length > 0) {
                        html += '<div class=\"card\"><h2>Cognitive Biases Detected (' + biasesData.biases.length + ')</h2>';
                        html += '<div style=\"display: grid; gap: 10px; margin-top: 15px;\">';
                        for (const bias of biasesData.biases) {
                            const severityColor = bias.severity === 'high' ? '#dc3545' : 
                                                  bias.severity === 'medium' ? '#ffc107' : '#28a745';
                            html += '<div class=\"model-card\" style=\"border-left: 4px solid ' + severityColor + ';\">';
                            html += '<div style=\"display: flex; justify-content: space-between; align-items: center;\">';
                            html += '<h4 style=\"margin: 0;\">' + bias.bias.replace(/_/g, ' ').replace(/\\b\\w/g, l => l.toUpperCase()) + '</h4>';
                            html += '<span style=\"background: ' + severityColor + '; color: white; padding: 3px 10px; border-radius: 12px; font-size: 11px;\">' + bias.severity + '</span>';
                            html += '</div>';
                            html += '</div>';
                        }
                        html += '</div></div>';
                    }
                    
                    document.getElementById('results').innerHTML = html;
                    document.getElementById('export-buttons').style.display = 'block';
                    setStatus('Complete', 'green');
                } catch (e) {
                    document.getElementById('results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                    setStatus('Error', 'red');
                }
            }
            
            function exportJSON() {
                if (!lastAnalysisResult) {
                    alert('No analysis results to export');
                    return;
                }
                
                const exportData = {
                    exportedAt: new Date().toISOString(),
                    analysisType: lastAnalysisType,
                    version: '2.0',
                    results: lastAnalysisResult
                };
                
                const blob = new Blob([JSON.stringify(exportData, null, 2)], {type: 'application/json'});
                const url = URL.createObjectURL(blob);
                const a = document.createElement('a');
                a.href = url;
                a.download = 'mental-models-analysis-' + new Date().toISOString().split('T')[0] + '.json';
                a.click();
                URL.revokeObjectURL(url);
            }
            
            function exportMarkdown() {
                if (!lastAnalysisResult) {
                    alert('No analysis results to export');
                    return;
                }
                
                let md = '# Mental Models Analysis Report\\n\\n';
                md += '**Generated:** ' + new Date().toLocaleString() + '\\n';
                md += '**Analysis Type:** ' + lastAnalysisType + '\\n\\n';
                
                if (lastAnalysisType === 'full') {
                    const comp = lastAnalysisResult.comprehensive || {};
                    const lolla = comp.lollapalooza || {};
                    
                    if (lolla.detected) {
                        md += '## LOLLAPALOOZA EFFECT DETECTED\\n\\n';
                        md += '**Converging Models:** ' + (lolla.converging_models || []).join(', ') + '\\n';
                        md += '**Strength:** ' + lolla.strength + '\\n';
                        md += '**Convergence Score:** ' + lolla.convergence_score + '%\\n\\n';
                    }
                    
                    md += '## Mental Models\\n\\n';
                    for (const model of (comp.analysis || [])) {
                        md += '### ' + model.name + ' (' + model.relevance + '%)\\n';
                        md += '**Category:** ' + model.category + '\\n\\n';
                        md += model.description + '\\n\\n';
                    }
                    
                    const biases = lastAnalysisResult.biases?.biases || [];
                    if (biases.length > 0) {
                        md += '## Cognitive Biases\\n\\n';
                        for (const bias of biases) {
                            md += '- **' + bias.bias.replace(/_/g, ' ') + '** (Severity: ' + bias.severity + ')\\n';
                        }
                        md += '\\n';
                    }
                }
                
                const blob = new Blob([md], {type: 'text/markdown'});
                const url = URL.createObjectURL(blob);
                const a = document.createElement('a');
                a.href = url;
                a.download = 'mental-models-analysis-' + new Date().toISOString().split('T')[0] + '.md';
                a.click();
                URL.revokeObjectURL(url);
            }
            
            function exportPDF() {
                if (!lastAnalysisResult) {
                    alert('No analysis results to export');
                    return;
                }
                
                let content = '<html><head><title>Mental Models Analysis Report</title>';
                content += '<style>';
                content += 'body{font-family:Arial,sans-serif;padding:40px;max-width:800px;margin:0 auto;font-size:12px;}';
                content += 'h1{color:#4361ee;font-size:24px;}h2{color:#3f37c9;margin-top:25px;font-size:18px;}';
                content += '.model{border:1px solid #e0e0e0;padding:12px;margin:8px 0;border-radius:6px;}';
                content += '.category{background:#4895ef;color:white;padding:2px 8px;border-radius:4px;font-size:11px;}';
                content += '.lolla{background:linear-gradient(135deg,#667eea,#764ba2);color:white;padding:20px;border-radius:8px;margin-bottom:20px;}';
                content += '</style></head><body>';
                content += '<h1>Mental Models Analysis Report</h1>';
                content += '<p>Generated: ' + new Date().toLocaleString() + '</p>';
                
                if (lastAnalysisType === 'full') {
                    const comp = lastAnalysisResult.comprehensive || {};
                    const lolla = comp.lollapalooza || {};
                    
                    if (lolla.detected) {
                        content += '<div class=\"lolla\">';
                        content += '<h2 style=\"color:white;margin-top:0;\">LOLLAPALOOZA EFFECT DETECTED</h2>';
                        content += '<p><strong>Converging Models:</strong> ' + (lolla.converging_models || []).join(', ') + '</p>';
                        content += '<p><strong>Strength:</strong> ' + lolla.strength + ' | <strong>Score:</strong> ' + lolla.convergence_score + '%</p>';
                        content += '</div>';
                    }
                    
                    content += '<h2>Mental Models Detected</h2>';
                    for (const model of (comp.analysis || [])) {
                        content += '<div class=\"model\">';
                        content += '<h3 style=\"margin:0 0 5px 0;\">' + model.name + ' <span style=\"color:#28a745;\">(' + model.relevance + '%)</span></h3>';
                        content += '<span class=\"category\">' + model.category + '</span>';
                        content += '<p style=\"margin-top:10px;\">' + model.description + '</p>';
                        content += '</div>';
                    }
                    
                    const biases = lastAnalysisResult.biases?.biases || [];
                    if (biases.length > 0) {
                        content += '<h2>Cognitive Biases Detected</h2>';
                        for (const bias of biases) {
                            content += '<div class=\"model\">';
                            content += '<strong>' + bias.bias.replace(/_/g, ' ') + '</strong> - Severity: ' + bias.severity;
                            content += '</div>';
                        }
                    }
                }
                
                content += '</body></html>';
                
                const printWindow = window.open('', '_blank');
                printWindow.document.write(content);
                printWindow.document.close();
                printWindow.print();
            }
            
            function copyToClipboard() {
                if (!lastAnalysisResult) {
                    alert('No analysis results to copy');
                    return;
                }
                
                let text = 'MENTAL MODELS ANALYSIS REPORT\\n';
                text += '============================\\n';
                text += 'Generated: ' + new Date().toLocaleString() + '\\n\\n';
                
                if (lastAnalysisType === 'full') {
                    const comp = lastAnalysisResult.comprehensive || {};
                    const lolla = comp.lollapalooza || {};
                    
                    if (lolla.detected) {
                        text += '*** LOLLAPALOOZA EFFECT DETECTED ***\\n';
                        text += 'Converging Models: ' + (lolla.converging_models || []).join(', ') + '\\n';
                        text += 'Strength: ' + lolla.strength + '\\n\\n';
                    }
                    
                    text += 'MENTAL MODELS:\\n';
                    for (const model of (comp.analysis || [])) {
                        text += '- ' + model.name + ' (' + model.relevance + '%) - ' + model.category + '\\n';
                        text += '  ' + model.description + '\\n\\n';
                    }
                    
                    const biases = lastAnalysisResult.biases?.biases || [];
                    if (biases.length > 0) {
                        text += 'COGNITIVE BIASES:\\n';
                        for (const bias of biases) {
                            text += '- ' + bias.bias.replace(/_/g, ' ') + ' (Severity: ' + bias.severity + ')\\n';
                        }
                    }
                } else if (lastAnalysisType === 'models') {
                    text += 'MENTAL MODELS:\\n';
                    for (const model of (lastAnalysisResult.models || [])) {
                        text += '- ' + model.name + ' (' + model.category + ')\\n';
                        text += '  ' + model.description + '\\n\\n';
                    }
                } else if (lastAnalysisType === 'biases') {
                    text += 'COGNITIVE BIASES:\\n';
                    for (const bias of (lastAnalysisResult.biases || [])) {
                        text += '- ' + bias.bias.replace(/_/g, ' ') + ' (Severity: ' + bias.severity + ')\\n';
                    }
                }
                
                navigator.clipboard.writeText(text).then(() => {
                    alert('Results copied to clipboard!');
                }).catch(err => {
                    console.error('Failed to copy:', err);
                    alert('Failed to copy to clipboard');
                });
            }
        </script>">>
    ],
    Html = html_templates:base_layout(<<"Analysis">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.
