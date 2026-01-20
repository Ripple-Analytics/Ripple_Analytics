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
            
            <textarea id=\"analysis-text\" placeholder=\"Enter text to analyze...\"></textarea>
            <div style=\"display: flex; gap: 10px; margin-top: 10px; flex-wrap: wrap;\">
                <button class=\"btn\" onclick=\"analyzeText()\">Analyze for Models</button>
                <button class=\"btn btn-secondary\" onclick=\"detectBiases()\">Detect Biases</button>
                <button class=\"btn btn-secondary\" onclick=\"runFullAnalysis()\">Full Analysis</button>
                <button class=\"btn btn-secondary\" onclick=\"document.getElementById('analysis-text').value=''\">Clear</button>
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
            
            async function runFullAnalysis() {
                const text = document.getElementById('analysis-text').value;
                if (!text.trim()) {
                    alert('Please enter some text to analyze');
                    return;
                }
                
                document.getElementById('results').innerHTML = '<div class=\"loading\">Running full analysis...</div>';
                document.getElementById('export-buttons').style.display = 'none';
                
                try {
                    const [modelsRes, biasesRes] = await Promise.all([
                        fetch('/api/analysis/analyze', {
                            method: 'POST',
                            headers: {'Content-Type': 'application/json'},
                            body: JSON.stringify({text: text, top_n: 5})
                        }),
                        fetch('/api/analysis/detect-biases', {
                            method: 'POST',
                            headers: {'Content-Type': 'application/json'},
                            body: JSON.stringify({text: text})
                        })
                    ]);
                    
                    const modelsData = await modelsRes.json();
                    const biasesData = await biasesRes.json();
                    
                    lastAnalysisResult = {
                        models: modelsData,
                        biases: biasesData,
                        timestamp: new Date().toISOString(),
                        inputText: text.substring(0, 200) + (text.length > 200 ? '...' : '')
                    };
                    lastAnalysisType = 'full';
                    
                    // Save to history
                    saveToHistory('full', text, modelsData.models || [], biasesData.biases || []);
                    
                    let html = '<div class=\"card\"><h2>Full Analysis Results</h2>';
                    
                    // Models section
                    html += '<h3 style=\"margin-top: 20px;\">Mental Models</h3>';
                    if (modelsData.models && modelsData.models.length > 0) {
                        html += '<p>Found ' + modelsData.models.length + ' relevant mental models:</p><br>';
                        for (const model of modelsData.models) {
                            html += '<div class=\"model-card\">';
                            html += '<h4>' + model.name + '</h4>';
                            html += '<span class=\"category\">' + model.category + '</span>';
                            html += '<p>' + model.description + '</p>';
                            html += '</div>';
                        }
                    } else {
                        html += '<p>No specific mental models detected.</p>';
                    }
                    
                    // Biases section
                    html += '<h3 style=\"margin-top: 20px;\">Cognitive Biases</h3>';
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
