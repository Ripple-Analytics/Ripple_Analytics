%%%-------------------------------------------------------------------
%%% @doc AI Improver Handler Template - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(ai_improver_handler_template_part2).

-export([content/0]).

content() ->
    <<"
                    }
                } catch (e) {
                    alert('Failed to toggle improver: ' + e.message);
                }
            }
            
            async function forceImprovement() {
                try {
                    const res = await fetch(AI_IMPROVER_URL + '/api/improver/start', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ force: true })
                    });
                    const data = await res.json();
                    alert('Improvement cycle triggered');
                    loadImproverStatus();
                } catch (e) {
                    alert('Failed to trigger improvement: ' + e.message);
                }
            }
            
            async function testLmStudioConnection() {
                const url = document.getElementById('lm-studio-url').value;
                document.getElementById('lm-studio-status').innerHTML = '<p class=\"loading\">Testing connection...</p>';
                
                try {
                    const res = await fetch(AI_IMPROVER_URL + '/health');
                    const data = await res.json();
                    
                    if (data.lm_studio_connected) {
                        document.getElementById('lm-studio-status').innerHTML = 
                            '<p class=\"status-healthy\">Connected to LM Studio</p>';
                    } else {
                        document.getElementById('lm-studio-status').innerHTML = 
                            '<p class=\"status-unhealthy\">LM Studio not connected. Make sure it is running.</p>';
                    }
                } catch (e) {
                    document.getElementById('lm-studio-status').innerHTML = 
                        '<p class=\"status-unhealthy\">Could not check LM Studio status</p>';
                }
            }
            
            async function loadMetrics() {
                try {
                    const res = await fetch(AI_IMPROVER_URL + '/api/metrics');
                    const data = await res.json();
                    
                    let html = '<div style=\"display: grid; grid-template-columns: 1fr 1fr; gap: 10px;\">';
                    html += '<p><strong>Total Improvements:</strong> ' + data.total_improvements + '</p>';
                    html += '<p><strong>Total Failures:</strong> ' + data.total_failures + '</p>';
                    html += '<p><strong>Success Rate:</strong> ' + data.success_rate.toFixed(1) + '%</p>';
                    html += '<p><strong>Avg Cycle Time:</strong> ' + data.avg_cycle_time_ms + 'ms</p>';
                    html += '<p><strong>Uptime:</strong> ' + formatUptime(data.uptime_seconds) + '</p>';
                    html += '<p><strong>Last Improvement:</strong> ' + (data.last_improvement || 'never') + '</p>';
                    html += '</div>';
                    
                    document.getElementById('metrics-display').innerHTML = html;
                } catch (e) {
                    document.getElementById('metrics-display').innerHTML = 
                        '<p class=\"status-unknown\">Could not load metrics</p>';
                }
            }
            
            async function resetMetrics() {
                if (!confirm('Are you sure you want to reset all metrics?')) return;
                
                try {
                    await fetch(AI_IMPROVER_URL + '/api/metrics', { method: 'DELETE' });
                    loadMetrics();
                } catch (e) {
                    alert('Failed to reset metrics: ' + e.message);
                }
            }
            
            async function saveImproverConfig() {
                const config = {
                    improvement_interval: parseInt(document.getElementById('improvement-interval').value),
                    max_changes_per_cycle: parseInt(document.getElementById('max-changes').value),
                    auto_deploy: document.getElementById('auto-deploy').checked,
                    require_validation: document.getElementById('require-validation').checked
                };
                
                try {
                    const res = await fetch(AI_IMPROVER_URL + '/api/improver/config', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify(config)
                    });
                    
                    document.getElementById('config-save-status').innerHTML = 
                        '<span class=\"status-healthy\">Saved!</span>';
                    setTimeout(() => {
                        document.getElementById('config-save-status').innerHTML = '';
                    }, 3000);
                } catch (e) {
                    document.getElementById('config-save-status').innerHTML = 
                        '<span class=\"status-unhealthy\">Failed to save</span>';
                }
            }
            
            async function analyzeCode() {
                const path = document.getElementById('analyze-path').value;
                if (!path) {
                    alert('Please enter a path to analyze');
                    return;
                }
                
                document.getElementById('analysis-results').innerHTML = '<p class=\"loading\">Analyzing...</p>';
                
                try {
                    const res = await fetch(AI_IMPROVER_URL + '/api/improver/analyze', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ path: path })
                    });
                    const data = await res.json();
                    
                    let html = '<div style=\"background: #f8f9fa; padding: 15px; border-radius: 8px;\">';
                    html += '<h4>Analysis Results</h4>';
                    if (data.files && data.files.length > 0) {
                        html += '<ul>';
                        data.files.forEach(f => {
                            html += '<li><strong>' + f.file + '</strong>: ' + (f.issues || 0) + ' issues</li>';
                        });
                        html += '</ul>';
                    } else {
                        html += '<p>No issues found or no files analyzed.</p>';
                    }
                    html += '</div>';
                    
                    document.getElementById('analysis-results').innerHTML = html;
                } catch (e) {
                    document.getElementById('analysis-results').innerHTML = 
                        '<p class=\"status-unhealthy\">Analysis failed: ' + e.message + '</p>';
                }
            }
            
            async function loadGitStatus() {
                try {
                    const res = await fetch(AI_IMPROVER_URL + '/api/git/status');
                    const data = await res.json();
                    
                    let html = '<div style=\"display: grid; grid-template-columns: 1fr 1fr; gap: 10px;\">';
                    html += '<p><strong>Branch:</strong> ' + data.current_branch + '</p>';
                    html += '<p><strong>Auto Commit:</strong> ' + (data.auto_commit ? 'Yes' : 'No') + '</p>';
                    html += '<p><strong>Auto Push:</strong> ' + (data.auto_push ? 'Yes' : 'No') + '</p>';
                    html += '</div>';
                    if (data.changes) {
                        html += '<pre style=\"background: #f8f9fa; padding: 10px; border-radius: 4px; margin-top: 10px; max-height: 150px; overflow-y: auto;\">' + data.changes + '</pre>';
                    }
    ">>.
