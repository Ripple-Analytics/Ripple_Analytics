%%%-------------------------------------------------------------------
%%% @doc Analysis Handler Template - Part 5
%%% @end
%%%-------------------------------------------------------------------
-module(analysis_handler_template_part5).

-export([content/0]).

content() ->
    <<"
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
    ">>.
