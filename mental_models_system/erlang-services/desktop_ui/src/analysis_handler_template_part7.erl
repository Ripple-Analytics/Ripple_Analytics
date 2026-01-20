%%%-------------------------------------------------------------------
%%% @doc Analysis Handler Template - Part 7
%%% @end
%%%-------------------------------------------------------------------
-module(analysis_handler_template_part7).

-export([content/0]).

content() ->
    <<"
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
                        html += '<div class=\"card\">';
                        html += '<h2>Keyword-Based Detection</h2>';
                        html += '<p style=\"font-size: 12px; color: #666;\">Text analysis detected ' + (textAnalysis.models || []).length + ' patterns across ' + (textAnalysis.text_length || 0) + ' characters:</p>';
                        html += '<div style=\"display: grid; gap: 10px; margin-top: 15px;\">';
                        for (const tm of textModels) {
                            const score = tm.score || 0;
                            const scoreColor = score >= 70 ? '#28a745' : score >= 40 ? '#ffc107' : '#6c757d';
                            html += '<div class=\"model-card\">';
                            html += '<div style=\"display: flex; justify-content: space-between; align-items: flex-start;\">';
                            html += '<div><h4 style=\"margin: 0;\">' + tm.name + '</h4>';
                            html += '<span class=\"category\">' + tm.category + '</span></div>';
                            html += '<span style=\"background: ' + scoreColor + '; color: white; padding: 3px 10px; border-radius: 12px; font-size: 11px;\">' + score + '%</span>';
                            html += '</div>';
                            html += '<p style=\"margin-top: 8px; font-size: 12px;\"><strong>Confidence:</strong> ' + (tm.confidence || 'Low') + '</p>';
                            if (tm.evidence) {
                                html += '<p style=\"font-size: 11px; color: #666;\">Evidence: ' + (tm.evidence.keywords || 0) + ' keywords, ' + (tm.evidence.patterns || 0) + ' patterns</p>';
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
                
    ">>.
