%%%-------------------------------------------------------------------
%%% @doc Analysis Handler Template - Part 4
%%% @end
%%%-------------------------------------------------------------------
-module(analysis_handler_template_part4).

-export([content/0]).

content() ->
    <<"
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
    ">>.
