%%%-------------------------------------------------------------------
%%% @doc AI Code Improver Handler - UI for autonomous code improvement
%%%-------------------------------------------------------------------
-module(ai_improver_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Content = [
        <<"
        <div class=\"card\" style=\"border-left: 4px solid #8b5cf6;\">
            <h2>AI Code Improver</h2>
            <p>Autonomous code improvement using LM Studio. The AI analyzes your codebase and suggests improvements 24/7.</p>
            <div id=\"improver-status\" style=\"margin: 15px 0;\">
                <p class=\"loading\">Loading AI improver status...</p>
            </div>
            <div style=\"display: flex; gap: 10px; flex-wrap: wrap; margin-top: 15px;\">
                <button class=\"btn\" id=\"improver-toggle-btn\" onclick=\"toggleImprover()\">Start Improver</button>
                <button class=\"btn btn-secondary\" onclick=\"forceImprovement()\">Force Improvement Cycle</button>
                <button class=\"btn btn-secondary\" onclick=\"loadImproverStatus()\">Refresh Status</button>
            </div>
        </div>
        
        <div class=\"grid\">
            <div class=\"card\">
                <h2>LM Studio Connection</h2>
                <div id=\"lm-studio-status\" style=\"margin: 15px 0;\">
                    <p class=\"loading\">Checking LM Studio connection...</p>
                </div>
                <div style=\"margin-top: 15px;\">
                    <label><strong>LM Studio URL</strong></label>
                    <input type=\"url\" id=\"lm-studio-url\" value=\"http://localhost:1234\" style=\"margin-top: 5px;\">
                    <small style=\"color: #666;\">URL where LM Studio is running</small>
                </div>
                <button class=\"btn btn-secondary\" onclick=\"testLmStudioConnection()\" style=\"margin-top: 10px;\">Test Connection</button>
            </div>
            
            <div class=\"card\">
                <h2>Improvement Metrics</h2>
                <div id=\"metrics-display\" style=\"margin: 15px 0;\">
                    <p class=\"loading\">Loading metrics...</p>
                </div>
                <button class=\"btn btn-secondary\" onclick=\"loadMetrics()\">Refresh Metrics</button>
                <button class=\"btn btn-secondary\" onclick=\"resetMetrics()\" style=\"margin-left: 10px;\">Reset Metrics</button>
            </div>
        </div>
        
        <div class=\"card\" style=\"border-left: 4px solid #10b981;\">
            <h2>Configuration</h2>
            <div style=\"display: grid; grid-template-columns: 1fr 1fr; gap: 15px;\">
                <div>
                    <label><strong>Improvement Interval (ms)</strong></label>
                    <input type=\"number\" id=\"improvement-interval\" value=\"300000\" min=\"60000\" max=\"3600000\" style=\"margin-top: 5px;\">
                    <small style=\"color: #666;\">Time between improvement cycles (60s - 1hr)</small>
                </div>
                <div>
                    <label><strong>Max Changes Per Cycle</strong></label>
                    <input type=\"number\" id=\"max-changes\" value=\"3\" min=\"1\" max=\"10\" style=\"margin-top: 5px;\">
                    <small style=\"color: #666;\">Maximum improvements per cycle</small>
                </div>
                <div>
                    <label style=\"display: flex; align-items: center; gap: 10px; cursor: pointer;\">
                        <input type=\"checkbox\" id=\"auto-deploy\" checked style=\"width: 20px; height: 20px;\">
                        <span><strong>Auto Deploy</strong></span>
                    </label>
                    <small style=\"color: #666;\">Automatically deploy validated improvements</small>
                </div>
                <div>
                    <label style=\"display: flex; align-items: center; gap: 10px; cursor: pointer;\">
                        <input type=\"checkbox\" id=\"require-validation\" checked style=\"width: 20px; height: 20px;\">
                        <span><strong>Require Validation</strong></span>
                    </label>
                    <small style=\"color: #666;\">Validate code before deployment</small>
                </div>
            </div>
            <button class=\"btn\" onclick=\"saveImproverConfig()\" style=\"margin-top: 15px;\">Save Configuration</button>
            <span id=\"config-save-status\" style=\"margin-left: 10px;\"></span>
        </div>
        
        <div class=\"card\" style=\"border-left: 4px solid #f59e0b;\">
            <h2>Code Analysis</h2>
            <p>Analyze specific files or directories for improvement opportunities.</p>
            <div style=\"margin-top: 15px;\">
                <label><strong>Path to Analyze</strong></label>
                <input type=\"text\" id=\"analyze-path\" placeholder=\"/app/services/analysis_service/src\" style=\"margin-top: 5px;\">
            </div>
            <button class=\"btn\" onclick=\"analyzeCode()\" style=\"margin-top: 10px;\">Analyze</button>
            <div id=\"analysis-results\" style=\"margin-top: 15px;\"></div>
        </div>
        
        <div class=\"card\" style=\"border-left: 4px solid #6366f1;\">
            <h2>Git Integration</h2>
            <div id=\"git-status-display\" style=\"margin: 15px 0;\">
                <p class=\"loading\">Loading git status...</p>
            </div>
            <div style=\"display: flex; gap: 10px; flex-wrap: wrap; margin-top: 15px;\">
                <button class=\"btn btn-secondary\" onclick=\"loadGitStatus()\">Refresh Status</button>
                <button class=\"btn btn-secondary\" onclick=\"loadRecentCommits()\">View Recent Commits</button>
                <button class=\"btn\" onclick=\"pushChanges()\">Push Changes</button>
            </div>
            <div id=\"git-commits-display\" style=\"margin-top: 15px;\"></div>
        </div>
        
        <div class=\"card\">
            <h2>Improvement History</h2>
            <div id=\"history-display\" style=\"margin: 15px 0; max-height: 400px; overflow-y: auto;\">
                <p class=\"loading\">Loading improvement history...</p>
            </div>
            <div style=\"display: flex; gap: 10px;\">
                <button class=\"btn btn-secondary\" onclick=\"loadHistory()\">Refresh History</button>
                <button class=\"btn btn-secondary\" onclick=\"clearHistory()\">Clear History</button>
            </div>
        </div>
        
        <div class=\"card\" style=\"border-left: 4px solid #ec4899;\">
            <h2>Design Philosophy</h2>
            <p>View the design principles that guide AI-generated improvements.</p>
            <button class=\"btn btn-secondary\" onclick=\"viewPhilosophy()\">View Philosophy</button>
            <button class=\"btn btn-secondary\" onclick=\"viewPromptTemplates()\" style=\"margin-left: 10px;\">View Prompt Templates</button>
            <div id=\"philosophy-display\" style=\"margin-top: 15px;\"></div>
        </div>
        
        <script>
            const AI_IMPROVER_URL = 'http://localhost:8025';
            
            async function loadImproverStatus() {
                try {
                    const res = await fetch(AI_IMPROVER_URL + '/api/improver/status');
                    const data = await res.json();
                    
                    let statusClass = data.running ? 'status-healthy' : 'status-unknown';
                    let statusText = data.running ? 'Running' : 'Stopped';
                    
                    let html = '<div style=\"display: grid; grid-template-columns: 1fr 1fr; gap: 10px;\">';
                    html += '<p><strong>Status:</strong> <span class=\"' + statusClass + '\">' + statusText + '</span></p>';
                    html += '<p><strong>Cycles Completed:</strong> ' + (data.cycles_completed || 0) + '</p>';
                    html += '<p><strong>Last Cycle:</strong> ' + (data.last_cycle || 'never') + '</p>';
                    html += '<p><strong>Next Cycle:</strong> ' + (data.next_cycle || 'N/A') + '</p>';
                    html += '</div>';
                    
                    document.getElementById('improver-status').innerHTML = html;
                    document.getElementById('improver-toggle-btn').textContent = data.running ? 'Stop Improver' : 'Start Improver';
                } catch (e) {
                    document.getElementById('improver-status').innerHTML = 
                        '<p class=\"status-unhealthy\">Could not connect to AI Improver service. Make sure it is running on port 8025.</p>';
                }
            }
            
            async function toggleImprover() {
                const btn = document.getElementById('improver-toggle-btn');
                const isRunning = btn.textContent === 'Stop Improver';
                
                try {
                    const endpoint = isRunning ? '/api/improver/stop' : '/api/improver/start';
                    const res = await fetch(AI_IMPROVER_URL + endpoint, { method: 'POST' });
                    const data = await res.json();
                    
                    if (data.status === 'started' || data.status === 'stopped') {
                        loadImproverStatus();
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
                    
                    document.getElementById('git-status-display').innerHTML = html;
                } catch (e) {
                    document.getElementById('git-status-display').innerHTML = 
                        '<p class=\"status-unknown\">Could not load git status</p>';
                }
            }
            
            async function loadRecentCommits() {
                try {
                    const res = await fetch(AI_IMPROVER_URL + '/api/git/commits?count=10');
                    const data = await res.json();
                    
                    let html = '<div style=\"background: #f8f9fa; padding: 15px; border-radius: 8px;\">';
                    html += '<h4>Recent Commits</h4>';
                    html += '<ul style=\"font-family: monospace; font-size: 12px;\">';
                    data.commits.forEach(c => {
                        html += '<li>' + c + '</li>';
                    });
                    html += '</ul></div>';
                    
                    document.getElementById('git-commits-display').innerHTML = html;
                } catch (e) {
                    document.getElementById('git-commits-display').innerHTML = 
                        '<p class=\"status-unknown\">Could not load commits</p>';
                }
            }
            
            async function pushChanges() {
                if (!confirm('Push all AI-generated changes to remote?')) return;
                
                try {
                    const res = await fetch(AI_IMPROVER_URL + '/api/git/push', { method: 'POST' });
                    const data = await res.json();
                    
                    if (data.status === 'pushed') {
                        alert('Changes pushed successfully!');
                    } else {
                        alert('Push failed: ' + (data.error || 'Unknown error'));
                    }
                    loadGitStatus();
                } catch (e) {
                    alert('Failed to push: ' + e.message);
                }
            }
            
            async function loadHistory() {
                try {
                    const res = await fetch(AI_IMPROVER_URL + '/api/improver/history');
                    const data = await res.json();
                    
                    if (!data.improvements || data.improvements.length === 0) {
                        document.getElementById('history-display').innerHTML = 
                            '<p style=\"color: #666;\">No improvements recorded yet.</p>';
                        return;
                    }
                    
                    let html = '<table style=\"width: 100%; border-collapse: collapse;\">';
                    html += '<tr style=\"background: #f0f0f0;\"><th style=\"padding: 8px; text-align: left;\">Time</th><th style=\"padding: 8px; text-align: left;\">File</th><th style=\"padding: 8px; text-align: left;\">Type</th><th style=\"padding: 8px; text-align: left;\">Status</th></tr>';
                    
                    data.improvements.slice(0, 50).forEach(imp => {
                        let statusClass = imp.status === 'deployed' ? 'status-healthy' : 
                                         imp.status === 'failed' ? 'status-unhealthy' : 'status-unknown';
                        html += '<tr style=\"border-bottom: 1px solid #e0e0e0;\">';
                        html += '<td style=\"padding: 8px;\">' + (imp.timestamp || 'N/A') + '</td>';
                        html += '<td style=\"padding: 8px; font-family: monospace; font-size: 12px;\">' + (imp.file || 'N/A') + '</td>';
                        html += '<td style=\"padding: 8px;\">' + (imp.type || 'N/A') + '</td>';
                        html += '<td style=\"padding: 8px;\"><span class=\"' + statusClass + '\">' + (imp.status || 'N/A') + '</span></td>';
                        html += '</tr>';
                    });
                    html += '</table>';
                    
                    document.getElementById('history-display').innerHTML = html;
                } catch (e) {
                    document.getElementById('history-display').innerHTML = 
                        '<p class=\"status-unknown\">Could not load history</p>';
                }
            }
            
            async function clearHistory() {
                if (!confirm('Are you sure you want to clear all improvement history?')) return;
                
                try {
                    await fetch(AI_IMPROVER_URL + '/api/improver/history', { method: 'DELETE' });
                    loadHistory();
                } catch (e) {
                    alert('Failed to clear history: ' + e.message);
                }
            }
            
            async function viewPhilosophy() {
                try {
                    const res = await fetch(AI_IMPROVER_URL + '/api/prompts/philosophy');
                    const data = await res.json();
                    
                    let html = '<div style=\"background: #f8f9fa; padding: 15px; border-radius: 8px; margin-top: 15px;\">';
                    html += '<h4>Design Philosophy</h4>';
                    html += '<pre style=\"white-space: pre-wrap; font-size: 12px; max-height: 300px; overflow-y: auto;\">' + 
                            JSON.stringify(data, null, 2) + '</pre>';
                    html += '</div>';
                    
                    document.getElementById('philosophy-display').innerHTML = html;
                } catch (e) {
                    document.getElementById('philosophy-display').innerHTML = 
                        '<p class=\"status-unknown\">Could not load philosophy</p>';
                }
            }
            
            async function viewPromptTemplates() {
                try {
                    const res = await fetch(AI_IMPROVER_URL + '/api/prompts/templates');
                    const data = await res.json();
                    
                    let html = '<div style=\"background: #f8f9fa; padding: 15px; border-radius: 8px; margin-top: 15px;\">';
                    html += '<h4>Prompt Templates</h4>';
                    html += '<pre style=\"white-space: pre-wrap; font-size: 12px; max-height: 300px; overflow-y: auto;\">' + 
                            JSON.stringify(data, null, 2) + '</pre>';
                    html += '</div>';
                    
                    document.getElementById('philosophy-display').innerHTML = html;
                } catch (e) {
                    document.getElementById('philosophy-display').innerHTML = 
                        '<p class=\"status-unknown\">Could not load templates</p>';
                }
            }
            
            function formatUptime(seconds) {
                const days = Math.floor(seconds / 86400);
                const hours = Math.floor((seconds % 86400) / 3600);
                const mins = Math.floor((seconds % 3600) / 60);
                
                if (days > 0) return days + 'd ' + hours + 'h ' + mins + 'm';
                if (hours > 0) return hours + 'h ' + mins + 'm';
                return mins + 'm';
            }
            
            // Load all data on page load
            document.addEventListener('DOMContentLoaded', function() {
                loadImproverStatus();
                testLmStudioConnection();
                loadMetrics();
                loadGitStatus();
                loadHistory();
            });
        </script>
        ">>
    ],
    
    Html = html_templates:page("AI Code Improver", Content),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/html">>},
        Html,
        Req0),
    {ok, Req, State}.
