%%%-------------------------------------------------------------------
%%% @doc Folder Handler Template - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(folder_handler_template_part2).

-export([content/0]).

content() ->
    <<"
                        html += '<div class=\"card\">';
                        html += '<h2>Analysis Summary</h2>';
                        html += '<div style=\"display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 15px; margin-top: 15px;\">';
                        html += '<div style=\"text-align: center; padding: 15px; background: #f8f9fa; border-radius: 8px;\"><div style=\"font-size: 24px; font-weight: bold; color: #4361ee;\">' + data.total_files + '</div><div>Total Files</div></div>';
                        html += '<div style=\"text-align: center; padding: 15px; background: #f8f9fa; border-radius: 8px;\"><div style=\"font-size: 24px; font-weight: bold; color: #28a745;\">' + data.analyzed + '</div><div>Analyzed</div></div>';
                        if (data.failed > 0) {
                            html += '<div style=\"text-align: center; padding: 15px; background: #f8f9fa; border-radius: 8px;\"><div style=\"font-size: 24px; font-weight: bold; color: #dc3545;\">' + data.failed + '</div><div>Failed</div></div>';
                        }
                        html += '<div style=\"text-align: center; padding: 15px; background: #f8f9fa; border-radius: 8px;\"><div style=\"font-size: 24px; font-weight: bold; color: #764ba2;\">' + (data.summary.lollapalooza_count || 0) + '</div><div>Lollapalooza Effects</div></div>';
                        html += '</div></div>';
                        
                        // Lollapalooza alert
                        if (data.summary.lollapalooza_files && data.summary.lollapalooza_files.length > 0) {
                            html += '<div class=\"card\" style=\"background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white;\">';
                            html += '<h2 style=\"color: white;\">Lollapalooza Effects Detected!</h2>';
                            html += '<p>The following files have 3+ high-scoring mental models converging:</p>';
                            html += '<ul style=\"margin-top: 10px;\">';
                            for (const file of data.summary.lollapalooza_files) {
                                html += '<li>' + file + '</li>';
                            }
                            html += '</ul></div>';
                        }
                        
                        // Top models across all files
                        if (data.summary.top_models && data.summary.top_models.length > 0) {
                            html += '<div class=\"card\">';
                            html += '<h2>Most Common Mental Models</h2>';
                            html += '<p>Models detected across all analyzed files:</p><br>';
                            for (const model of data.summary.top_models) {
                                html += '<div class=\"model-card\">';
                                html += '<div style=\"display: flex; justify-content: space-between; align-items: center;\">';
                                html += '<h4>' + model.name + '</h4>';
                                html += '<span style=\"background: #4361ee; color: white; padding: 4px 12px; border-radius: 20px; font-size: 12px;\">' + model.count + ' files</span>';
                                html += '</div></div>';
                            }
                            html += '</div>';
                        }
                        
                        // Individual file results
                        if (data.results && data.results.length > 0) {
                            html += '<div class=\"card\">';
                            html += '<h2>Individual File Results</h2>';
                            html += '<p>Click on a file to see detailed analysis:</p><br>';
                            
                            for (const result of data.results) {
                                const hasLollapalooza = result.lollapalooza_detected;
                                const borderColor = hasLollapalooza ? '#764ba2' : '#e0e0e0';
                                html += '<div class=\"model-card\" style=\"border-left: 4px solid ' + borderColor + '; cursor: pointer;\" onclick=\"toggleFileDetails(this)\">';
                                html += '<div style=\"display: flex; justify-content: space-between; align-items: center;\">';
                                html += '<h4>' + result.file_name + '</h4>';
                                html += '<div>';
                                if (hasLollapalooza) {
                                    html += '<span style=\"background: #764ba2; color: white; padding: 4px 8px; border-radius: 4px; font-size: 11px; margin-right: 5px;\">LOLLAPALOOZA</span>';
                                }
                                html += '<span style=\"background: #4361ee; color: white; padding: 4px 8px; border-radius: 4px; font-size: 11px;\">' + (result.model_count || 0) + ' models</span>';
                                html += '</div></div>';
                                
                                // Hidden details
                                html += '<div class=\"file-details\" style=\"display: none; margin-top: 15px; padding-top: 15px; border-top: 1px solid #eee;\">';
                                
                                if (result.models && result.models.length > 0) {
                                    html += '<p><strong>Top Models:</strong></p>';
                                    html += '<ul style=\"margin: 5px 0 15px 20px;\">';
                                    for (const model of result.models.slice(0, 5)) {
                                        html += '<li>' + model.name + ' (' + (model.score || 0) + '%)</li>';
                                    }
                                    html += '</ul>';
                                }
                                
                                if (result.patterns && result.patterns.length > 0) {
                                    html += '<p><strong>Patterns:</strong> ' + result.pattern_count + ' detected</p>';
                                }
                                
                                if (result.insights && result.insights.length > 0) {
                                    html += '<p><strong>Insights:</strong> ' + result.insight_count + ' found</p>';
                                }
                                
                                html += '</div></div>';
                            }
                            html += '</div>';
                        }
                        
                        document.getElementById('analysis-results').innerHTML = html;
                    } else {
                        document.getElementById('analysis-results').innerHTML = '<div class=\"alert alert-error\">Error: ' + (data.error || 'Unknown error') + '</div>';
                    }
                } catch (e) {
                    document.getElementById('analysis-results').innerHTML = '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function analyzeFile(filePath) {
                const analysisType = document.getElementById('analysis-type').value;
                
                document.getElementById('analysis-results').innerHTML = '<div class=\"loading\">Analyzing file...</div>';
                
                try {
                    const res = await fetch('/api/analysis/folder', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({
                            action: 'analyze_file',
                            file: filePath,
                            analysis_type: analysisType
                        })
                    });
                    const data = await res.json();
                    
                    if (data.success) {
                        let html = '<div class=\"card\">';
                        html += '<h2>Analysis: ' + data.file_name + '</h2>';
                        html += '<p style=\"font-size: 12px; color: #666;\">File size: ' + formatSize(data.file_size) + '</p>';
                        
                        if (data.lollapalooza_detected) {
                            html += '<div style=\"background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 15px; border-radius: 8px; margin: 15px 0;\">';
                            html += '<strong>LOLLAPALOOZA EFFECT DETECTED!</strong> ' + data.high_scoring_count + ' high-scoring models converging.';
                            html += '</div>';
                        }
                        
                        if (data.models && data.models.length > 0) {
                            html += '<h3 style=\"margin-top: 20px;\">Mental Models (' + data.model_count + ')</h3>';
                            for (const model of data.models) {
                                const score = model.score || 0;
                                const scoreColor = score >= 70 ? '#28a745' : score >= 40 ? '#ffc107' : '#6c757d';
                                html += '<div class=\"model-card\">';
                                html += '<div style=\"display: flex; justify-content: space-between; align-items: center;\">';
                                html += '<h4>' + model.name + '</h4>';
                                html += '<span style=\"background: ' + scoreColor + '; color: white; padding: 4px 12px; border-radius: 20px; font-size: 12px;\">' + score + '%</span>';
                                html += '</div>';
                                html += '<span class=\"category\">' + model.category + '</span>';
                                html += '</div>';
                            }
                        }
                        
                        if (data.patterns && data.patterns.length > 0) {
                            html += '<h3 style=\"margin-top: 20px;\">Patterns (' + data.pattern_count + ')</h3>';
                            for (const pattern of data.patterns) {
                                html += '<div class=\"model-card\">';
                                html += '<h4>' + pattern.pattern + '</h4>';
                                html += '<p>' + pattern.description + '</p>';
                                html += '</div>';
                            }
                        }
                        
                        if (data.insights && data.insights.length > 0) {
                            html += '<h3 style=\"margin-top: 20px;\">Insights (' + data.insight_count + ')</h3>';
                            for (const insight of data.insights) {
                                html += '<div class=\"model-card\">';
                                html += '<h4>' + (insight.type || 'Insight') + '</h4>';
                                html += '<p>' + insight.insight + '</p>';
    ">>.
