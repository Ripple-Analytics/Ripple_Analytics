%%%-------------------------------------------------------------------
%%% @doc Analysis Handler Template - Part 6
%%% @end
%%%-------------------------------------------------------------------
-module(analysis_handler_template_part6).

-export([content/0]).

content() ->
    <<"
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
                    
                    // Lollapalooza Alert (if detected) - Most Important - Enhanced Display
                    const lollapalooza = comprehensiveData.lollapalooza || {};
                    if (lollapalooza.detected) {
                        const strengthColors = {
                            'extreme': 'linear-gradient(135deg, #ff0844 0%, #ffb199 100%)',
                            'strong': 'linear-gradient(135deg, #667eea 0%, #764ba2 100%)',
                            'moderate': 'linear-gradient(135deg, #11998e 0%, #38ef7d 100%)',
                            'weak': 'linear-gradient(135deg, #4facfe 0%, #00f2fe 100%)'
                        };
                        const strength = (lollapalooza.strength || 'moderate').toLowerCase();
                        const bgGradient = strengthColors[strength] || strengthColors.moderate;
                        
                        html += '<div class=\"card\" style=\"background: ' + bgGradient + '; color: white; border: 3px solid rgba(255,255,255,0.3); box-shadow: 0 8px 32px rgba(0,0,0,0.2);\">';
                        html += '<div style=\"display: flex; align-items: center; gap: 15px; margin-bottom: 15px;\">';
                        html += '<div style=\"font-size: 48px;\">&#9889;</div>';
                        html += '<div>';
                        html += '<h2 style=\"color: white; margin: 0; font-size: 24px; text-shadow: 2px 2px 4px rgba(0,0,0,0.3);\">LOLLAPALOOZA EFFECT</h2>';
                        html += '<p style=\"margin: 5px 0 0 0; font-size: 14px; opacity: 0.9;\">Extreme Outcome Probability Detected</p>';
                        html += '</div></div>';
                        
                        html += '<div style=\"display: grid; grid-template-columns: repeat(3, 1fr); gap: 15px; margin: 20px 0;\">';
                        html += '<div style=\"text-align: center; padding: 15px; background: rgba(255,255,255,0.2); border-radius: 12px;\">';
                        html += '<div style=\"font-size: 36px; font-weight: bold;\">' + lollapalooza.convergence_count + '</div>';
                        html += '<div style=\"font-size: 11px; text-transform: uppercase; letter-spacing: 1px;\">Converging Models</div></div>';
                        html += '<div style=\"text-align: center; padding: 15px; background: rgba(255,255,255,0.2); border-radius: 12px;\">';
                        html += '<div style=\"font-size: 36px; font-weight: bold;\">' + (lollapalooza.convergence_score || 0) + '%</div>';
                        html += '<div style=\"font-size: 11px; text-transform: uppercase; letter-spacing: 1px;\">Convergence Score</div></div>';
                        html += '<div style=\"text-align: center; padding: 15px; background: rgba(255,255,255,0.2); border-radius: 12px;\">';
                        html += '<div style=\"font-size: 36px; font-weight: bold;\">' + (lollapalooza.categories_involved || []).length + '</div>';
                        html += '<div style=\"font-size: 11px; text-transform: uppercase; letter-spacing: 1px;\">Domains Involved</div></div>';
                        html += '</div>';
                        
                        html += '<div style=\"background: rgba(0,0,0,0.2); padding: 15px; border-radius: 12px; margin-bottom: 15px;\">';
                        html += '<h4 style=\"margin: 0 0 10px 0; font-size: 13px; text-transform: uppercase; letter-spacing: 1px;\">Converging Mental Models</h4>';
                        html += '<div style=\"display: flex; flex-wrap: wrap; gap: 8px;\">';
                        for (const model of (lollapalooza.converging_models || [])) {
                            html += '<span style=\"background: rgba(255,255,255,0.25); padding: 6px 12px; border-radius: 20px; font-size: 12px; font-weight: 500;\">' + model + '</span>';
                        }
                        html += '</div></div>';
                        
                        if (lollapalooza.cross_domain && (lollapalooza.categories_involved || []).length > 1) {
                            html += '<div style=\"background: rgba(255,215,0,0.3); padding: 12px; border-radius: 8px; margin-bottom: 15px; border: 1px solid rgba(255,215,0,0.5);\">';
                            html += '<strong>&#127942; Cross-Domain Confluence:</strong> Models span multiple disciplines: ' + (lollapalooza.categories_involved || []).join(' &#8594; ') + '';
                            html += '</div>';
                        }
                        
                        html += '<div style=\"background: rgba(255,255,255,0.1); padding: 15px; border-radius: 8px; border-left: 4px solid rgba(255,255,255,0.5);\">';
                        html += '<p style=\"margin: 0; font-style: italic; font-size: 13px;\">When several models combine, you get lollapalooza effects; this is when two, three, or four forces are all operating in the same direction. And, frequently, you do not get simple addition. It is often like a critical mass in physics where you get a nuclear explosion if you get to a certain point of mass.</p>';
                        html += '<p style=\"margin: 10px 0 0 0; font-size: 11px; opacity: 0.8; text-align: right;\">- Charlie Munger, Poor Charlies Almanack</p>';
                        html += '</div>';
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
    ">>.
