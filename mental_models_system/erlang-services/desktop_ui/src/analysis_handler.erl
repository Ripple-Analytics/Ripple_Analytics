%%%-------------------------------------------------------------------
%%% @doc Analysis Handler - Mental model analysis page with export
%%%-------------------------------------------------------------------
-module(analysis_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Content = [
        <<"<div class=\"card\">
            <h2>Analyze Text</h2>
            <p>Enter text to identify relevant mental models and detect cognitive biases.</p>
            <br>
            
            <!-- Data Import Section -->
            <div style=\"margin-bottom: 20px; padding: 15px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border-radius: 8px; color: white;\">
                <h3 style=\"margin: 0 0 10px 0; color: white;\">Import Data</h3>
                <p style=\"margin: 0 0 15px 0; font-size: 14px;\">Upload files or paste data for batch analysis</p>
                <div style=\"display: flex; gap: 10px; flex-wrap: wrap; align-items: center;\">
                    <input type=\"file\" id=\"file-upload\" accept=\".txt,.csv,.json,.md\" multiple style=\"display: none;\" onchange=\"handleFileUpload(event)\">
                    <button class=\"btn\" style=\"background: white; color: #667eea;\" onclick=\"document.getElementById('file-upload').click()\">Upload Files</button>
                    <button class=\"btn\" style=\"background: rgba(255,255,255,0.2); border: 1px solid white;\" onclick=\"showPasteModal()\">Paste Bulk Data</button>
                    <button class=\"btn\" style=\"background: rgba(255,255,255,0.2); border: 1px solid white;\" onclick=\"loadFromURL()\">Load from URL</button>
                    <span id=\"file-count\" style=\"font-size: 12px;\"></span>
                </div>
                <div id=\"import-preview\" style=\"margin-top: 10px; display: none;\">
                    <div style=\"background: rgba(255,255,255,0.1); padding: 10px; border-radius: 6px; max-height: 150px; overflow-y: auto;\">
                        <div id=\"import-files-list\"></div>
                    </div>
                    <div style=\"margin-top: 10px; display: flex; gap: 10px;\">
                        <button class=\"btn\" style=\"background: #28a745;\" onclick=\"processBatchData()\">Process All Files</button>
                        <button class=\"btn\" style=\"background: rgba(255,255,255,0.2);\" onclick=\"clearImportedFiles()\">Clear</button>
                    </div>
                </div>
            </div>
            
            <div style=\"margin-bottom: 15px;\">
                <label style=\"font-weight: bold; display: block; margin-bottom: 8px;\">Quick Templates:</label>
                <div style=\"display: flex; gap: 8px; flex-wrap: wrap;\">
                    <button class=\"btn btn-secondary\" style=\"font-size: 12px; padding: 6px 12px;\" onclick=\"loadTemplate('decision')\">Decision Analysis</button>
                    <button class=\"btn btn-secondary\" style=\"font-size: 12px; padding: 6px 12px;\" onclick=\"loadTemplate('problem')\">Problem Solving</button>
                    <button class=\"btn btn-secondary\" style=\"font-size: 12px; padding: 6px 12px;\" onclick=\"loadTemplate('strategy')\">Strategy Review</button>
                    <button class=\"btn btn-secondary\" style=\"font-size: 12px; padding: 6px 12px;\" onclick=\"loadTemplate('negotiation')\">Negotiation Prep</button>
                    <button class=\"btn btn-secondary\" style=\"font-size: 12px; padding: 6px 12px;\" onclick=\"loadTemplate('investment')\">Investment Analysis</button>
                    <button class=\"btn btn-secondary\" style=\"font-size: 12px; padding: 6px 12px;\" onclick=\"loadTemplate('meeting')\">Meeting Notes</button>
                </div>
            </div>
            
            <textarea id=\"analysis-text\" placeholder=\"Enter text to analyze or import data above...\"></textarea>
            <div style=\"display: flex; gap: 10px; margin-top: 10px; flex-wrap: wrap;\">
                <button class=\"btn\" onclick=\"analyzeText()\">Analyze for Models</button>
                <button class=\"btn btn-secondary\" onclick=\"detectBiases()\">Detect Biases</button>
                <button class=\"btn btn-secondary\" onclick=\"runBayesianAnalysis()\">Bayesian Analysis</button>
                <button class=\"btn btn-secondary\" onclick=\"runPatternAnalysis()\">Pattern Analysis</button>
                <button class=\"btn btn-secondary\" onclick=\"runFullAnalysis()\">Full Analysis</button>
                <button class=\"btn btn-secondary\" onclick=\"document.getElementById('analysis-text').value=''\">Clear</button>
            </div>
        </div>
        
        <!-- Paste Modal -->
        <div id=\"paste-modal\" style=\"display: none; position: fixed; top: 0; left: 0; right: 0; bottom: 0; background: rgba(0,0,0,0.5); z-index: 1000; align-items: center; justify-content: center;\">
            <div style=\"background: white; padding: 25px; border-radius: 12px; max-width: 600px; width: 90%; max-height: 80vh; overflow-y: auto;\">
                <h3 style=\"margin: 0 0 15px 0;\">Paste Bulk Data</h3>
                <p style=\"color: #666; font-size: 14px;\">Paste multiple entries separated by blank lines, or CSV/JSON data:</p>
                <textarea id=\"bulk-paste-area\" style=\"width: 100%; height: 300px; margin: 15px 0; padding: 10px; border: 1px solid #ddd; border-radius: 6px; font-family: monospace;\" placeholder=\"Paste your data here...\\n\\nSupported formats:\\n- Plain text (entries separated by blank lines)\\n- CSV (with headers)\\n- JSON array of objects with 'text' field\"></textarea>
                <div style=\"display: flex; gap: 10px; justify-content: flex-end;\">
                    <button class=\"btn btn-secondary\" onclick=\"closePasteModal()\">Cancel</button>
                    <button class=\"btn\" onclick=\"importPastedData()\">Import Data</button>
                </div>
            </div>
        </div>
        <div id=\"results\"></div>
        <div id=\"export-buttons\" style=\"display: none; margin-top: 15px;\">
            <div class=\"card\">
                <h2>Export Results</h2>
                <p>Download your analysis results in different formats:</p>
                <div style=\"display: flex; gap: 10px; margin-top: 10px;\">
                    <button class=\"btn\" onclick=\"exportJSON()\">Export as JSON</button>
                    <button class=\"btn btn-secondary\" onclick=\"exportPDF()\">Export as PDF</button>
                    <button class=\"btn btn-secondary\" onclick=\"copyToClipboard()\">Copy to Clipboard</button>
                </div>
            </div>
        </div>
        <script>
            let lastAnalysisResult = null;
            let lastAnalysisType = null;
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
                    count.textContent = importedFiles.length + ' file(s) ready';
                    list.innerHTML = importedFiles.map((f, i) => 
                        '<div style=\"display: flex; justify-content: space-between; align-items: center; padding: 5px 0; border-bottom: 1px solid rgba(255,255,255,0.1);\">' +
                        '<span>' + f.name + ' (' + formatSize(f.size) + ')</span>' +
                        '<button onclick=\"removeFile(' + i + ')\" style=\"background: none; border: none; color: white; cursor: pointer;\">x</button>' +
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
                if (!data) {
                    alert('Please paste some data first');
                    return;
                }
                
                // Try to parse as JSON first
                try {
                    const jsonData = JSON.parse(data);
                    if (Array.isArray(jsonData)) {
                        jsonData.forEach((item, i) => {
                            const text = typeof item === 'string' ? item : (item.text || item.content || JSON.stringify(item));
                            importedFiles.push({
                                name: 'pasted-item-' + (i + 1) + '.txt',
                                type: 'text/plain',
                                content: text,
                                size: text.length
                            });
                        });
                    }
                } catch (e) {
                    // Try CSV parsing
                    if (data.includes(',') && data.includes('\\n')) {
                        const lines = data.split('\\n');
                        const headers = lines[0].split(',').map(h => h.trim());
                        const textCol = headers.findIndex(h => h.toLowerCase().includes('text') || h.toLowerCase().includes('content'));
                        
                        if (textCol >= 0) {
                            lines.slice(1).forEach((line, i) => {
                                const cols = line.split(',');
                                if (cols[textCol]) {
                                    importedFiles.push({
                                        name: 'csv-row-' + (i + 1) + '.txt',
                                        type: 'text/plain',
                                        content: cols[textCol].replace(/^\"|\"$/g, ''),
                                        size: cols[textCol].length
                                    });
                                }
                            });
                        } else {
                            // Use whole lines as entries
                            lines.forEach((line, i) => {
                                if (line.trim()) {
                                    importedFiles.push({
                                        name: 'line-' + (i + 1) + '.txt',
                                        type: 'text/plain',
                                        content: line.trim(),
                                        size: line.length
                                    });
                                }
                            });
                        }
                    } else {
                        // Split by blank lines
                        const entries = data.split(/\\n\\s*\\n/).filter(e => e.trim());
                        entries.forEach((entry, i) => {
                            importedFiles.push({
                                name: 'entry-' + (i + 1) + '.txt',
                                type: 'text/plain',
                                content: entry.trim(),
                                size: entry.length
                            });
                        });
                    }
                }
                
                updateImportPreview();
                closePasteModal();
                document.getElementById('bulk-paste-area').value = '';
            }
            
            async function loadFromURL() {
                const url = prompt('Enter URL to fetch data from (must return text, JSON, or CSV):');
                if (!url) return;
                
                try {
                    document.getElementById('file-count').textContent = 'Loading...';
                    const res = await fetch('/api/proxy?url=' + encodeURIComponent(url));
                    const text = await res.text();
                    
                    importedFiles.push({
                        name: url.split('/').pop() || 'url-data.txt',
                        type: 'text/plain',
                        content: text,
                        size: text.length
                    });
                    updateImportPreview();
                } catch (e) {
                    alert('Failed to load URL: ' + e.message);
                    document.getElementById('file-count').textContent = '';
                }
            }
            
            async function processBatchData() {
                if (importedFiles.length === 0) {
                    alert('No files to process');
                    return;
                }
                
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
                        batchResults.push({
                            file: file.name,
                            success: true,
                            result: data
                        });
                    } catch (e) {
                        batchResults.push({
                            file: file.name,
                            success: false,
                            error: e.message
                        });
                    }
                }
                
                displayBatchResults();
            }
            
            function displayBatchResults() {
                let html = '<div class=\"card\">';
                html += '<h2>Batch Analysis Results</h2>';
                html += '<p>Processed ' + batchResults.length + ' items</p>';
                html += '<div style=\"margin-top: 15px;\">';
                html += '<button class=\"btn\" onclick=\"exportBatchJSON()\">Export All as JSON</button>';
                html += '<button class=\"btn btn-secondary\" style=\"margin-left: 10px;\" onclick=\"exportBatchCSV()\">Export Summary CSV</button>';
                html += '</div>';
                html += '</div>';
                
                batchResults.forEach((item, i) => {
                    html += '<div class=\"card\" style=\"margin-top: 15px;\">';
                    html += '<h3 style=\"display: flex; justify-content: space-between;\">';
                    html += '<span>' + item.file + '</span>';
                    html += '<span style=\"font-size: 12px; color: ' + (item.success ? '#28a745' : '#dc3545') + ';\">' + (item.success ? 'Success' : 'Failed') + '</span>';
                    html += '</h3>';
                    
                    if (item.success && item.result) {
                        const lolla = item.result.lollapalooza || {};
                        if (lolla.detected) {
                            html += '<div style=\"background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); color: white; padding: 10px; border-radius: 6px; margin: 10px 0;\">';
                            html += '<strong>Lollapalooza Detected!</strong> ' + lolla.convergence_count + ' models converging (' + lolla.strength + ')';
                            html += '</div>';
                        }
                        
                        const models = item.result.analysis || [];
                        if (models.length > 0) {
                            html += '<p><strong>Top Models:</strong></p>';
                            html += '<div style=\"display: flex; flex-wrap: wrap; gap: 5px; margin-top: 5px;\">';
                            models.slice(0, 5).forEach(m => {
                                html += '<span style=\"background: #e9ecef; padding: 4px 8px; border-radius: 4px; font-size: 12px;\">' + m.name + ' (' + (m.relevance || 0) + '%)</span>';
                            });
                            html += '</div>';
                        }
                    } else if (!item.success) {
                        html += '<p style=\"color: #dc3545;\">Error: ' + item.error + '</p>';
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
                const a = document.createElement('a');
                a.href = url;
                a.download = 'batch-analysis-' + new Date().toISOString().slice(0,10) + '.json';
                a.click();
            }
            
            function exportBatchCSV() {
                let csv = 'File,Success,Lollapalooza,Convergence Count,Top Model,Top Score\\n';
                batchResults.forEach(item => {
                    const lolla = item.result?.lollapalooza || {};
                    const topModel = item.result?.analysis?.[0] || {};
                    csv += [
                        item.file,
                        item.success,
                        lolla.detected || false,
                        lolla.convergence_count || 0,
                        topModel.name || '',
                        topModel.relevance || 0
                    ].join(',') + '\\n';
                });
                
                const blob = new Blob([csv], {type: 'text/csv'});
                const url = URL.createObjectURL(blob);
                const a = document.createElement('a');
                a.href = url;
                a.download = 'batch-summary-' + new Date().toISOString().slice(0,10) + '.csv';
                a.click();
            }
            
            // ========== Templates ==========
            
            const templates = {
                decision: 'DECISION ANALYSIS\\n\\nDecision to make: [Describe the decision]\\n\\nOptions:\\n1. [Option A]\\n2. [Option B]\\n3. [Option C]\\n\\nPros and Cons:\\n- Option A: [pros/cons]\\n- Option B: [pros/cons]\\n- Option C: [pros/cons]\\n\\nKey factors to consider:\\n- [Factor 1]\\n- [Factor 2]\\n\\nTimeline: [When decision needs to be made]\\n\\nStakeholders affected: [Who is impacted]',
                problem: 'PROBLEM SOLVING\\n\\nProblem statement: [Describe the problem clearly]\\n\\nCurrent situation: [What is happening now]\\n\\nDesired outcome: [What should be happening]\\n\\nRoot causes identified:\\n1. [Cause 1]\\n2. [Cause 2]\\n\\nPotential solutions:\\n1. [Solution 1]\\n2. [Solution 2]\\n\\nConstraints: [Time, budget, resources]\\n\\nSuccess metrics: [How will we know it is solved]',
                strategy: 'STRATEGY REVIEW\\n\\nObjective: [What are we trying to achieve]\\n\\nCurrent strategy: [Describe current approach]\\n\\nMarket conditions: [External factors]\\n\\nCompetitive landscape: [Key competitors and their moves]\\n\\nStrengths to leverage:\\n- [Strength 1]\\n- [Strength 2]\\n\\nWeaknesses to address:\\n- [Weakness 1]\\n- [Weakness 2]\\n\\nOpportunities identified:\\n- [Opportunity 1]\\n\\nThreats to mitigate:\\n- [Threat 1]\\n\\nProposed changes: [What should we do differently]',
                negotiation: 'NEGOTIATION PREPARATION\\n\\nNegotiation context: [What is being negotiated]\\n\\nOur position: [What we want]\\n\\nTheir likely position: [What they want]\\n\\nOur BATNA (Best Alternative): [What we do if no deal]\\n\\nTheir likely BATNA: [What they do if no deal]\\n\\nKey interests (ours):\\n- [Interest 1]\\n- [Interest 2]\\n\\nKey interests (theirs):\\n- [Interest 1]\\n- [Interest 2]\\n\\nPotential trade-offs: [What can we give up]\\n\\nDeal breakers: [What we cannot accept]\\n\\nOpening offer: [Where to start]\\n\\nTarget outcome: [Ideal result]',
                investment: 'INVESTMENT ANALYSIS\\n\\nInvestment opportunity: [Describe the investment]\\n\\nAmount: [How much]\\n\\nExpected return: [ROI expectations]\\n\\nTime horizon: [Investment period]\\n\\nRisk factors:\\n1. [Risk 1]\\n2. [Risk 2]\\n3. [Risk 3]\\n\\nMitigation strategies: [How to reduce risks]\\n\\nMarket analysis: [Industry trends]\\n\\nCompetitive moat: [What protects this investment]\\n\\nExit strategy: [How and when to exit]\\n\\nAlternative investments considered: [Other options]',
                meeting: 'MEETING NOTES\\n\\nDate: [Date]\\nAttendees: [Who was present]\\nPurpose: [Why we met]\\n\\nKey discussion points:\\n1. [Topic 1]: [Summary]\\n2. [Topic 2]: [Summary]\\n3. [Topic 3]: [Summary]\\n\\nDecisions made:\\n- [Decision 1]\\n- [Decision 2]\\n\\nAction items:\\n- [Action 1] - Owner: [Name] - Due: [Date]\\n- [Action 2] - Owner: [Name] - Due: [Date]\\n\\nOpen questions:\\n- [Question 1]\\n\\nNext steps: [What happens next]\\n\\nFollow-up meeting: [If scheduled]'
            };
            
            function loadTemplate(type) {
                if (templates[type]) {
                    document.getElementById('analysis-text').value = templates[type];
                }
            }
            
            async function saveToHistory(type, inputText, models, biases) {
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
                
                document.getElementById('results').innerHTML = '<div class=\"loading\">Analyzing...</div>';
                document.getElementById('export-buttons').style.display = 'none';
                
                try {
                    const res = await fetch('/api/analysis/analyze', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({text: text, top_n: 5})
                    });
                    const data = await res.json();
                    lastAnalysisResult = data;
                    lastAnalysisType = 'models';
                    
                    // Save to history
                    saveToHistory('models', text, data.models || [], []);
                    
                    let html = '<div class=\"card\"><h2>Analysis Results</h2>';
                    if (data.models && data.models.length > 0) {
                        html += '<p>Found ' + data.models.length + ' relevant mental models:</p><br>';
                        for (const model of data.models) {
                            html += '<div class=\"model-card\">';
                            html += '<h4>' + model.name + '</h4>';
                            html += '<span class=\"category\">' + model.category + '</span>';
                            html += '<p>' + model.description + '</p>';
                            if (model.relevance) {
                                html += '<p><strong>Relevance:</strong> ' + model.relevance + '</p>';
                            }
                            html += '</div>';
                        }
                    } else {
                        html += '<p>No specific mental models detected. Try providing more context.</p>';
                    }
                    html += '<p style=\"margin-top:15px;font-size:12px;color:#666;\">Method: ' + (data.method || 'keyword_matching') + '</p>';
                    html += '</div>';
                    document.getElementById('results').innerHTML = html;
                    document.getElementById('export-buttons').style.display = 'block';
                } catch (e) {
                    document.getElementById('results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function detectBiases() {
                const text = document.getElementById('analysis-text').value;
                if (!text.trim()) {
                    alert('Please enter some text to analyze');
                    return;
                }
                
                document.getElementById('results').innerHTML = '<div class=\"loading\">Detecting biases...</div>';
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
                    
                    // Save to history
                    saveToHistory('biases', text, [], data.biases || []);
                    
                    let html = '<div class=\"card\"><h2>Bias Detection Results</h2>';
                    if (data.biases && data.biases.length > 0) {
                        html += '<p>Found ' + data.biases.length + ' potential cognitive biases:</p><br>';
                        for (const bias of data.biases) {
                            const severityClass = bias.severity === 'high' ? 'status-unhealthy' : 
                                                  bias.severity === 'medium' ? 'status-unknown' : 'status-healthy';
                            html += '<div class=\"model-card\">';
                            html += '<h4>' + bias.bias.replace(/_/g, ' ').replace(/\\b\\w/g, l => l.toUpperCase()) + '</h4>';
                            html += '<span class=\"' + severityClass + '\">Severity: ' + bias.severity + '</span>';
                            if (bias.evidence) {
                                html += '<p><strong>Evidence:</strong> ' + bias.evidence.join(', ') + '</p>';
                            }
                            html += '</div>';
                        }
                    } else {
                        html += '<p>No obvious cognitive biases detected.</p>';
                    }
                    html += '</div>';
                    document.getElementById('results').innerHTML = html;
                    document.getElementById('export-buttons').style.display = 'block';
                } catch (e) {
                    document.getElementById('results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function runBayesianAnalysis() {
                const text = document.getElementById('analysis-text').value;
                if (!text.trim()) {
                    alert('Please enter some text to analyze');
                    return;
                }
                
                document.getElementById('results').innerHTML = '<div class=\"loading\">Running Bayesian analysis...</div>';
                document.getElementById('export-buttons').style.display = 'none';
                
                try {
                    const res = await fetch('/api/analysis/bayesian', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({text: text, top_n: 10})
                    });
                    const data = await res.json();
                    lastAnalysisResult = data;
                    lastAnalysisType = 'bayesian';
                    
                    // Save to history
                    saveToHistory('bayesian', text, data.models || [], []);
                    
                    let html = '<div class=\"card\">';
                    html += '<h2>Bayesian Analysis Results</h2>';
                    html += '<p>Using Bayes\\' theorem to calculate posterior probabilities for mental model relevance.</p>';
                    html += '<p style=\"font-size: 12px; color: #666; margin-top: 5px;\">Evidence extracted: ' + (data.evidence_extracted || 0) + ' terms</p>';
                    html += '</div>';
                    
                    if (data.models && data.models.length > 0) {
                        html += '<div class=\"card\"><h2>Models by Posterior Probability</h2>';
                        html += '<p>Models ranked by P(Model|Evidence) using Bayesian inference:</p><br>';
                        for (const model of data.models) {
                            const score = model.bayesian_score || 0;
                            const scoreColor = score >= 70 ? '#28a745' : score >= 40 ? '#ffc107' : '#6c757d';
                            html += '<div class=\"model-card\">';
                            html += '<div style=\"display: flex; justify-content: space-between; align-items: center;\">';
                            html += '<h4>' + model.name + '</h4>';
                            html += '<span style=\"background: ' + scoreColor + '; color: white; padding: 4px 12px; border-radius: 20px; font-size: 12px;\">' + score + '% posterior</span>';
                            html += '</div>';
                            html += '<span class=\"category\">' + model.category + '</span>';
                            html += '<p>' + model.description + '</p>';
                            html += '<div style=\"margin-top: 10px; padding: 10px; background: #f8f9fa; border-radius: 6px; font-size: 12px;\">';
                            html += '<div style=\"display: grid; grid-template-columns: repeat(3, 1fr); gap: 10px;\">';
                            html += '<div><strong>Prior:</strong> ' + (model.prior || 0) + '%</div>';
                            html += '<div><strong>Likelihood:</strong> ' + (model.likelihood || 0) + '%</div>';
                            html += '<div><strong>Evidence:</strong> ' + (model.evidence_count || 0) + ' matches</div>';
                            html += '</div>';
                            html += '<div style=\"margin-top: 8px;\"><strong>95% CI:</strong> [' + (model.confidence_lower || 0) + '%, ' + (model.confidence_upper || 0) + '%]</div>';
                            html += '</div>';
                            html += '</div>';
                        }
                        html += '</div>';
                    } else {
                        html += '<div class=\"card\"><p>No models found with significant posterior probability.</p></div>';
                    }
                    
                    html += '<div class=\"card\">';
                    html += '<h2>About Bayesian Analysis</h2>';
                    html += '<p>Bayesian inference uses Bayes\\' theorem to update our beliefs about which mental models apply:</p>';
                    html += '<p style=\"text-align: center; font-family: monospace; margin: 15px 0; font-size: 14px;\">P(Model|Evidence) = P(Evidence|Model) × P(Model) / P(Evidence)</p>';
                    html += '<ul style=\"margin-top: 10px;\">';
                    html += '<li><strong>Prior P(Model):</strong> Base probability that a model applies (based on category)</li>';
                    html += '<li><strong>Likelihood P(Evidence|Model):</strong> How likely we\\'d see this evidence if the model applies</li>';
                    html += '<li><strong>Posterior P(Model|Evidence):</strong> Updated probability after seeing the evidence</li>';
                    html += '<li><strong>95% CI:</strong> Confidence interval for the posterior estimate</li>';
                    html += '</ul>';
                    html += '</div>';
                    
                    document.getElementById('results').innerHTML = html;
                    document.getElementById('export-buttons').style.display = 'block';
                } catch (e) {
                    document.getElementById('results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function runPatternAnalysis() {
                const text = document.getElementById('analysis-text').value;
                if (!text.trim()) {
                    alert('Please enter some text to analyze');
                    return;
                }
                
                document.getElementById('results').innerHTML = '<div class=\"loading\">Extracting patterns and generating insights...</div>';
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
                    
                    // Save to history
                    saveToHistory('patterns', text, [], []);
                    
                    let html = '';
                    
                    // Inversions section (Munger's Inversion)
                    const inversions = data.inversions || [];
                    if (inversions.length > 0) {
                        html += '<div class=\"card\" style=\"background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%); color: white;\">';
                        html += '<h2 style=\"color: white;\">Inverted Perspective (Munger\\'s Inversion)</h2>';
                        html += '<p style=\"font-style: italic;\">' + (data.munger_quote || '') + '</p><br>';
                        for (const inv of inversions) {
                            html += '<div style=\"background: rgba(255,255,255,0.15); padding: 15px; border-radius: 8px; margin-bottom: 10px;\">';
                            html += '<p><strong>Original:</strong> ' + inv.original + '</p>';
                            html += '<p><strong>Inverted:</strong> ' + inv.inverted + '</p>';
                            html += '<p style=\"margin-top: 10px; font-style: italic;\"><strong>Key Question:</strong> ' + inv.question + '</p>';
                            html += '</div>';
                        }
                        html += '</div>';
                    }
                    
                    // Insights section
                    const insights = data.insights || [];
                    if (insights.length > 0) {
                        html += '<div class=\"card\">';
                        html += '<h2>Key Insights</h2>';
                        html += '<p>Actionable insights extracted from your text:</p><br>';
                        for (const insight of insights) {
                            const typeColor = insight.type === 'absolute' ? '#dc3545' : insight.type === 'causal' ? '#17a2b8' : '#6c757d';
                            html += '<div class=\"model-card\" style=\"border-left: 4px solid ' + typeColor + ';\">';
                            html += '<h4>' + (insight.type || 'Insight').replace(/_/g, ' ').replace(/\\b\\w/g, l => l.toUpperCase()) + '</h4>';
                            html += '<p>' + insight.insight + '</p>';
                            if (insight.action) {
                                html += '<p style=\"margin-top: 10px;\"><strong>Action:</strong> ' + insight.action + '</p>';
                            }
                            html += '</div>';
                        }
                        html += '</div>';
                    }
                    
                    // Patterns section
                    const patterns = data.patterns || [];
                    if (patterns.length > 0) {
                        html += '<div class=\"card\">';
                        html += '<h2>Detected Patterns</h2>';
                        html += '<p>Recurring patterns identified in your text:</p><br>';
                        for (const pattern of patterns) {
                            const confidence = pattern.confidence || 0;
                            const confColor = confidence >= 75 ? '#28a745' : confidence >= 50 ? '#ffc107' : '#6c757d';
                            html += '<div class=\"model-card\">';
                            html += '<div style=\"display: flex; justify-content: space-between; align-items: center;\">';
                            html += '<h4>' + pattern.pattern + '</h4>';
                            html += '<span style=\"background: ' + confColor + '; color: white; padding: 4px 12px; border-radius: 20px; font-size: 12px;\">' + confidence + '% confidence</span>';
                            html += '</div>';
                            html += '<span class=\"category\">' + (pattern.type || 'Pattern').replace(/_/g, ' ').replace(/\\b\\w/g, l => l.toUpperCase()) + '</span>';
                            html += '<p>' + pattern.description + '</p>';
                            html += '</div>';
                        }
                        html += '</div>';
                    }
                    
                    if (inversions.length === 0 && insights.length === 0 && patterns.length === 0) {
                        html += '<div class=\"card\"><p>No significant patterns detected. Try providing more detailed text.</p></div>';
                    }
                    
                    document.getElementById('results').innerHTML = html;
                    document.getElementById('export-buttons').style.display = 'block';
                } catch (e) {
                    document.getElementById('results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function runFullAnalysis() {
                const text = document.getElementById('analysis-text').value;
                if (!text.trim()) {
                    alert('Please enter some text to analyze');
                    return;
                }
                
                document.getElementById('results').innerHTML = '<div class=\"loading\">Running comprehensive analysis with Lollapalooza detection...</div>';
                document.getElementById('export-buttons').style.display = 'none';
                
                try {
                    // Use comprehensive analysis endpoint for Lollapalooza detection
                    const [comprehensiveRes, biasesRes] = await Promise.all([
                        fetch('/api/analysis/comprehensive', {
                            method: 'POST',
                            headers: {'Content-Type': 'application/json'},
                            body: JSON.stringify({text: text, top_n: 10})
                        }),
                        fetch('/api/analysis/detect-biases', {
                            method: 'POST',
                            headers: {'Content-Type': 'application/json'},
                            body: JSON.stringify({text: text})
                        })
                    ]);
                    
                    const comprehensiveData = await comprehensiveRes.json();
                    const biasesData = await biasesRes.json();
                    
                    lastAnalysisResult = {
                        comprehensive: comprehensiveData,
                        biases: biasesData,
                        timestamp: new Date().toISOString(),
                        inputText: text.substring(0, 200) + (text.length > 200 ? '...' : '')
                    };
                    lastAnalysisType = 'full';
                    
                    // Save to history
                    const models = comprehensiveData.analysis || [];
                    saveToHistory('full', text, models, biasesData.biases || []);
                    
                    let html = '';
                    
                    // Lollapalooza Alert (if detected)
                    const lollapalooza = comprehensiveData.lollapalooza || {};
                    if (lollapalooza.detected) {
                        html += '<div class=\"card\" style=\"background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white;\">';
                        html += '<h2 style=\"color: white;\">LOLLAPALOOZA EFFECT DETECTED!</h2>';
                        html += '<p style=\"font-size: 16px;\">Multiple mental models (' + lollapalooza.convergence_count + ') are converging with <strong>' + lollapalooza.strength + '</strong> intensity.</p>';
                        html += '<p><strong>Converging Models:</strong> ' + (lollapalooza.converging_models || []).join(', ') + '</p>';
                        if (lollapalooza.cross_domain) {
                            html += '<p><strong>Cross-Domain Analysis:</strong> Models span multiple categories: ' + (lollapalooza.categories_involved || []).join(', ') + '</p>';
                        }
                        html += '<p style=\"margin-top: 15px; font-style: italic;\">\"When several models combine, you get lollapalooza effects; this is when two, three, or four forces are all operating in the same direction.\" - Charlie Munger</p>';
                        html += '</div>';
                    }
                    
                    // Recommendations (if any)
                    const recommendations = comprehensiveData.recommendations || [];
                    if (recommendations.length > 0) {
                        html += '<div class=\"card\">';
                        html += '<h2>Recommendations</h2>';
                        for (const rec of recommendations) {
                            const priorityColor = rec.priority === 'high' ? '#dc3545' : rec.priority === 'medium' ? '#ffc107' : '#28a745';
                            html += '<div class=\"model-card\" style=\"border-left: 4px solid ' + priorityColor + ';\">';
                            html += '<h4>' + (rec.type || 'Insight').replace(/_/g, ' ').replace(/\\b\\w/g, l => l.toUpperCase()) + '</h4>';
                            html += '<p>' + rec.message + '</p>';
                            if (rec.action) {
                                html += '<p style=\"margin-top: 10px;\"><strong>Action:</strong> ' + rec.action + '</p>';
                            }
                            html += '</div>';
                        }
                        html += '</div>';
                    }
                    
                    // Convergence Metrics
                    const convergence = comprehensiveData.convergence || {};
                    if (convergence.total_models > 0) {
                        html += '<div class=\"card\">';
                        html += '<h2>Convergence Metrics</h2>';
                        html += '<div style=\"display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 15px;\">';
                        html += '<div style=\"text-align: center; padding: 15px; background: #f8f9fa; border-radius: 8px;\"><div style=\"font-size: 24px; font-weight: bold; color: #4361ee;\">' + convergence.total_models + '</div><div>Models Analyzed</div></div>';
                        html += '<div style=\"text-align: center; padding: 15px; background: #f8f9fa; border-radius: 8px;\"><div style=\"font-size: 24px; font-weight: bold; color: #28a745;\">' + convergence.high_scoring + '</div><div>High Scoring (70%+)</div></div>';
                        html += '<div style=\"text-align: center; padding: 15px; background: #f8f9fa; border-radius: 8px;\"><div style=\"font-size: 24px; font-weight: bold; color: #6c757d;\">' + (convergence.mean_score || 0).toFixed(1) + '%</div><div>Mean Score</div></div>';
                        html += '<div style=\"text-align: center; padding: 15px; background: #f8f9fa; border-radius: 8px;\"><div style=\"font-size: 24px; font-weight: bold; color: #17a2b8;\">' + (convergence.max_score || 0) + '%</div><div>Max Score</div></div>';
                        html += '</div></div>';
                    }
                    
                    // Mental Models section
                    html += '<div class=\"card\"><h2>Mental Models Detected</h2>';
                    const modelsData = comprehensiveData.analysis || [];
                    if (modelsData.length > 0) {
                        html += '<p>Found ' + modelsData.length + ' relevant mental models:</p><br>';
                        for (const model of modelsData) {
                            const relevance = model.relevance || 0;
                            const relevanceColor = relevance >= 70 ? '#28a745' : relevance >= 40 ? '#ffc107' : '#6c757d';
                            html += '<div class=\"model-card\">';
                            html += '<div style=\"display: flex; justify-content: space-between; align-items: center;\">';
                            html += '<h4>' + model.name + '</h4>';
                            html += '<span style=\"background: ' + relevanceColor + '; color: white; padding: 4px 12px; border-radius: 20px; font-size: 12px;\">' + relevance + '% relevance</span>';
                            html += '</div>';
                            html += '<span class=\"category\">' + model.category + '</span>';
                            html += '<p>' + model.description + '</p>';
                            html += '</div>';
                        }
                    } else {
                        html += '<p>No specific mental models detected.</p>';
                    }
                    html += '</div>';
                    
                    // Failure Modes (if any)
                    const failureModes = comprehensiveData.failure_modes || [];
                    if (failureModes.length > 0) {
                        html += '<div class=\"card\">';
                        html += '<h2>Potential Failure Modes</h2>';
                        html += '<p>Based on detected models, watch out for these potential pitfalls:</p><br>';
                        for (const fm of failureModes) {
                            const riskColor = fm.risk === 'critical' ? '#dc3545' : fm.risk === 'high' ? '#fd7e14' : fm.risk === 'medium' ? '#ffc107' : '#28a745';
                            html += '<div class=\"model-card\" style=\"border-left: 4px solid ' + riskColor + ';\">';
                            html += '<div style=\"display: flex; justify-content: space-between; align-items: center;\">';
                            html += '<h4>' + fm.mode + '</h4>';
                            html += '<span style=\"background: ' + riskColor + '; color: white; padding: 4px 12px; border-radius: 20px; font-size: 12px;\">' + fm.risk + ' risk</span>';
                            html += '</div>';
                            html += '<p>Source: ' + fm.source_model + ' (Score: ' + fm.model_score + '%)</p>';
                            html += '</div>';
                        }
                        html += '</div>';
                    }
                    
                    // Text Analysis section (keyword-based detection)
                    const textAnalysis = comprehensiveData.text_analysis || {};
                    const textModels = textAnalysis.top_models || [];
                    if (textModels.length > 0) {
                        html += '<div class=\"card\">';
                        html += '<h2>Keyword-Based Detection</h2>';
                        html += '<p>Text analysis detected ' + (textAnalysis.models || []).length + ' patterns across ' + (textAnalysis.text_length || 0) + ' characters:</p><br>';
                        for (const tm of textModels) {
                            const score = tm.score || 0;
                            const scoreColor = score >= 70 ? '#28a745' : score >= 40 ? '#ffc107' : '#6c757d';
                            html += '<div class=\"model-card\">';
                            html += '<div style=\"display: flex; justify-content: space-between; align-items: center;\">';
                            html += '<h4>' + tm.name + '</h4>';
                            html += '<span style=\"background: ' + scoreColor + '; color: white; padding: 4px 12px; border-radius: 20px; font-size: 12px;\">' + score + '% match</span>';
                            html += '</div>';
                            html += '<span class=\"category\">' + tm.category + '</span>';
                            html += '<p style=\"margin-top: 8px;\"><strong>Confidence:</strong> ' + (tm.confidence || 'Low') + '</p>';
                            if (tm.evidence) {
                                html += '<p style=\"font-size: 12px; color: #666;\">Evidence: ' + (tm.evidence.keywords || 0) + ' keywords, ' + (tm.evidence.patterns || 0) + ' patterns</p>';
                            }
                            html += '</div>';
                        }
                        html += '</div>';
                    }
                    
                    // Biases section
                    html += '<div class=\"card\"><h2>Cognitive Biases</h2>';
                    if (biasesData.biases && biasesData.biases.length > 0) {
                        html += '<p>Found ' + biasesData.biases.length + ' potential cognitive biases:</p><br>';
                        for (const bias of biasesData.biases) {
                            const severityClass = bias.severity === 'high' ? 'status-unhealthy' : 
                                                  bias.severity === 'medium' ? 'status-unknown' : 'status-healthy';
                            html += '<div class=\"model-card\">';
                            html += '<h4>' + bias.bias.replace(/_/g, ' ').replace(/\\b\\w/g, l => l.toUpperCase()) + '</h4>';
                            html += '<span class=\"' + severityClass + '\">Severity: ' + bias.severity + '</span>';
                            html += '</div>';
                        }
                    } else {
                        html += '<p>No obvious cognitive biases detected.</p>';
                    }
                    html += '</div>';
                    
                    document.getElementById('results').innerHTML = html;
                    document.getElementById('export-buttons').style.display = 'block';
                } catch (e) {
                    document.getElementById('results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
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
            
            function exportPDF() {
                if (!lastAnalysisResult) {
                    alert('No analysis results to export');
                    return;
                }
                
                // Create printable HTML
                let content = '<html><head><title>Mental Models Analysis</title>';
                content += '<style>body{font-family:Arial,sans-serif;padding:40px;max-width:800px;margin:0 auto;}';
                content += 'h1{color:#4361ee;}h2{color:#3f37c9;margin-top:30px;}';
                content += '.model{border:1px solid #e0e0e0;padding:15px;margin:10px 0;border-radius:8px;}';
                content += '.category{background:#4895ef;color:white;padding:2px 8px;border-radius:4px;font-size:12px;}';
                content += '.severity-high{color:#dc3545;}.severity-medium{color:#ffc107;}.severity-low{color:#28a745;}';
                content += '</style></head><body>';
                content += '<h1>Mental Models Analysis Report</h1>';
                content += '<p>Generated: ' + new Date().toLocaleString() + '</p>';
                
                if (lastAnalysisType === 'models' || lastAnalysisType === 'full') {
                    const models = lastAnalysisType === 'full' ? lastAnalysisResult.models.models : lastAnalysisResult.models;
                    content += '<h2>Mental Models Detected</h2>';
                    if (models && models.length > 0) {
                        for (const model of models) {
                            content += '<div class=\"model\">';
                            content += '<h3>' + model.name + '</h3>';
                            content += '<span class=\"category\">' + model.category + '</span>';
                            content += '<p>' + model.description + '</p>';
                            content += '</div>';
                        }
                    } else {
                        content += '<p>No mental models detected.</p>';
                    }
                }
                
                if (lastAnalysisType === 'biases' || lastAnalysisType === 'full') {
                    const biases = lastAnalysisType === 'full' ? lastAnalysisResult.biases.biases : lastAnalysisResult.biases;
                    content += '<h2>Cognitive Biases Detected</h2>';
                    if (biases && biases.length > 0) {
                        for (const bias of biases) {
                            content += '<div class=\"model\">';
                            content += '<h3>' + bias.bias.replace(/_/g, ' ').replace(/\\b\\w/g, l => l.toUpperCase()) + '</h3>';
                            content += '<p class=\"severity-' + bias.severity + '\">Severity: ' + bias.severity + '</p>';
                            content += '</div>';
                        }
                    } else {
                        content += '<p>No cognitive biases detected.</p>';
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
                
                let text = 'Mental Models Analysis Report\\n';
                text += '========================\\n\\n';
                
                if (lastAnalysisType === 'models' || lastAnalysisType === 'full') {
                    const models = lastAnalysisType === 'full' ? lastAnalysisResult.models.models : lastAnalysisResult.models;
                    text += 'MENTAL MODELS:\\n';
                    if (models && models.length > 0) {
                        for (const model of models) {
                            text += '- ' + model.name + ' (' + model.category + ')\\n';
                            text += '  ' + model.description + '\\n\\n';
                        }
                    }
                }
                
                if (lastAnalysisType === 'biases' || lastAnalysisType === 'full') {
                    const biases = lastAnalysisType === 'full' ? lastAnalysisResult.biases.biases : lastAnalysisResult.biases;
                    text += 'COGNITIVE BIASES:\\n';
                    if (biases && biases.length > 0) {
                        for (const bias of biases) {
                            text += '- ' + bias.bias.replace(/_/g, ' ') + ' (Severity: ' + bias.severity + ')\\n';
                        }
                    }
                }
                
                navigator.clipboard.writeText(text).then(() => {
                    alert('Results copied to clipboard!');
                }).catch(err => {
                    console.error('Failed to copy:', err);
                });
            }
        </script>">>
    ],
    Html = html_templates:base_layout(<<"Analysis">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.
