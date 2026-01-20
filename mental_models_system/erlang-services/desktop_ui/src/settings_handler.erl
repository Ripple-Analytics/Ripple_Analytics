%%%-------------------------------------------------------------------
%%% @doc Settings Handler - System settings, updates, and chaos engineering
%%%-------------------------------------------------------------------
-module(settings_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Content = [
        <<"
        <div class=\"card\" style=\"border-left: 4px solid #4361ee;\">
            <h2>Software Updates</h2>
            <div id=\"update-status\">
                <p class=\"loading\">Loading update status...</p>
            </div>
            <div style=\"display: flex; gap: 10px; margin-top: 15px; flex-wrap: wrap;\">
                <button class=\"btn\" onclick=\"checkForUpdates()\">Check for Updates</button>
                <button class=\"btn btn-secondary\" onclick=\"restartUpdater()\">Restart Auto-Updater</button>
            </div>
        </div>
        
        <div class=\"card\">
            <h2>Update Configuration</h2>
            <p>Configure backup update sources. Primary source is GitHub, with Google Drive as fallback.</p>
            <br>
            <div style=\"margin-bottom: 15px;\">
                <label><strong>GitHub Token</strong> (for private repos and alerts)</label>
                <input type=\"password\" id=\"github-token\" placeholder=\"ghp_xxxxxxxxxxxx\" style=\"margin-top: 5px;\">
                <small style=\"color: #666;\">Used for authenticated access and creating alert issues when updates fail</small>
            </div>
            <div style=\"margin-bottom: 15px;\">
                <label><strong>Google Drive Backup URL</strong> (fallback source)</label>
                <input type=\"url\" id=\"gdrive-url\" placeholder=\"https://drive.google.com/file/d/xxx/view\" style=\"margin-top: 5px;\">
                <small style=\"color: #666;\">Backup source if GitHub is unavailable</small>
            </div>
            <div style=\"margin-bottom: 15px;\">
                <label><strong>Check Interval</strong> (seconds)</label>
                <input type=\"number\" id=\"check-interval\" value=\"300\" min=\"60\" max=\"3600\" style=\"margin-top: 5px; width: 150px;\">
                <small style=\"color: #666;\">How often to check for updates (60-3600 seconds)</small>
            </div>
            <button class=\"btn\" onclick=\"saveConfig()\">Save Configuration</button>
            <span id=\"config-status\" style=\"margin-left: 10px;\"></span>
        </div>
        
        <div class=\"grid\">
            <div class=\"card\">
                <h2>System Information</h2>
                <p><strong>Architecture:</strong> Erlang/OTP Microservices</p>
                <p><strong>Version:</strong> <span id=\"app-version\">1.1.0</span></p>
                <p><strong>Runtime:</strong> Erlang/OTP <span id=\"erlang-version\">26</span></p>
                <p><strong>UI Theme:</strong> Light Mode</p>
                <p><strong>Auto-Update:</strong> <span id=\"auto-update-status\">Enabled</span></p>
            </div>
            <div class=\"card\">
                <h2>Service Endpoints</h2>
                <p><strong>API Gateway:</strong> http://localhost:8000</p>
                <p><strong>Analysis Service:</strong> http://localhost:8001</p>
                <p><strong>Harvester Service:</strong> http://localhost:8002</p>
                <p><strong>Storage Service:</strong> http://localhost:8003</p>
                <p><strong>Chaos Engineering:</strong> http://localhost:8005</p>
                <p><strong>Desktop UI:</strong> http://localhost:3000</p>
            </div>
        </div>
        <div class=\"card\">
            <h2>Chaos Engineering</h2>
            <p>Test system resilience with controlled chaos experiments.</p>
            <br>
            <div style=\"display: flex; gap: 10px; flex-wrap: wrap;\">
                <button class=\"btn\" onclick=\"runLoadTest()\">Run Load Test</button>
                <button class=\"btn btn-secondary\" onclick=\"runCascadeTest()\">Cascade Failure Test</button>
                <button class=\"btn btn-secondary\" onclick=\"runRecoveryTest()\">Recovery Test</button>
                <button class=\"btn btn-secondary\" onclick=\"checkAllHealth()\">Check All Health</button>
            </div>
        </div>
        <div id=\"chaos-results\"></div>
        <script>
            // Update management functions
            async function loadUpdateStatus() {
                try {
                    const res = await fetch('/api/update/status');
                    const data = await res.json();
                    
                    let statusClass = 'status-healthy';
                    let statusText = 'OK';
                    if (data.status === 'error') {
                        statusClass = 'status-unhealthy';
                        statusText = 'Error';
                    } else if (data.status === 'fallback') {
                        statusClass = 'status-unknown';
                        statusText = 'Using Fallback';
                    }
                    
                    let html = '<div style=\"display: grid; grid-template-columns: 1fr 1fr; gap: 10px;\">';
                    html += '<p><strong>Status:</strong> <span class=\"' + statusClass + '\">' + statusText + '</span></p>';
                    html += '<p><strong>Update Source:</strong> ' + (data.update_source || 'unknown') + '</p>';
                    html += '<p><strong>Last Check:</strong> ' + (data.last_check || 'never') + '</p>';
                    html += '<p><strong>GitHub Available:</strong> ' + (data.github_available ? 'Yes' : 'No') + '</p>';
                    html += '</div>';
                    if (data.message) {
                        html += '<p style=\"margin-top: 10px; color: #666;\">' + data.message + '</p>';
                    }
                    
                    document.getElementById('update-status').innerHTML = html;
                    
                    if (data.version) {
                        document.getElementById('app-version').textContent = data.version;
                    }
                    if (data.erlang_version) {
                        document.getElementById('erlang-version').textContent = data.erlang_version;
                    }
                } catch (e) {
                    document.getElementById('update-status').innerHTML = 
                        '<p class=\"status-unknown\">Could not load update status</p>';
                }
            }
            
            async function loadConfig() {
                try {
                    const res = await fetch('/api/update/config');
                    const data = await res.json();
                    
                    if (data.check_interval) {
                        document.getElementById('check-interval').value = data.check_interval;
                    }
                    if (data.github_token) {
                        document.getElementById('github-token').placeholder = data.github_token || 'ghp_xxxxxxxxxxxx';
                    }
                    if (data.gdrive_url) {
                        document.getElementById('gdrive-url').placeholder = data.gdrive_url || 'https://drive.google.com/...';
                    }
                } catch (e) {
                    console.error('Failed to load config:', e);
                }
            }
            
            async function checkForUpdates() {
                document.getElementById('update-status').innerHTML = '<p class=\"loading\">Triggering update check...</p>';
                try {
                    const res = await fetch('/api/update/trigger', {method: 'POST'});
                    const data = await res.json();
                    
                    if (data.success) {
                        document.getElementById('update-status').innerHTML = 
                            '<div class=\"alert alert-success\">' + data.message + '</div>';
                        setTimeout(loadUpdateStatus, 5000);
                    } else {
                        document.getElementById('update-status').innerHTML = 
                            '<div class=\"alert alert-error\">Failed to trigger update</div>';
                    }
                } catch (e) {
                    document.getElementById('update-status').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function restartUpdater() {
                document.getElementById('update-status').innerHTML = '<p class=\"loading\">Restarting auto-updater...</p>';
                try {
                    const res = await fetch('/api/update/restart', {method: 'POST'});
                    const data = await res.json();
                    
                    if (data.success) {
                        document.getElementById('update-status').innerHTML = 
                            '<div class=\"alert alert-success\">' + data.message + '</div>';
                        setTimeout(loadUpdateStatus, 3000);
                    } else {
                        document.getElementById('update-status').innerHTML = 
                            '<div class=\"alert alert-error\">Failed to restart updater</div>';
                    }
                } catch (e) {
                    document.getElementById('update-status').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function saveConfig() {
                const statusEl = document.getElementById('config-status');
                statusEl.textContent = 'Saving...';
                statusEl.style.color = '#666';
                
                const config = {};
                
                const token = document.getElementById('github-token').value;
                if (token) config.github_token = token;
                
                const gdrive = document.getElementById('gdrive-url').value;
                if (gdrive) config.gdrive_url = gdrive;
                
                const interval = parseInt(document.getElementById('check-interval').value);
                if (interval >= 60 && interval <= 3600) config.check_interval = interval;
                
                try {
                    const res = await fetch('/api/update/config', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify(config)
                    });
                    const data = await res.json();
                    
                    if (data.success) {
                        statusEl.textContent = 'Saved! Restart updater to apply.';
                        statusEl.style.color = '#28a745';
                        document.getElementById('github-token').value = '';
                        loadConfig();
                    } else {
                        statusEl.textContent = 'Failed to save';
                        statusEl.style.color = '#dc3545';
                    }
                } catch (e) {
                    statusEl.textContent = 'Error: ' + e.message;
                    statusEl.style.color = '#dc3545';
                }
            }
            
            // Load status and config on page load
            loadUpdateStatus();
            loadConfig();
            setInterval(loadUpdateStatus, 30000);
            
            // Chaos engineering functions
            async function runLoadTest() {
                document.getElementById('chaos-results').innerHTML = '<div class=\"loading\">Running load test...</div>';
                try {
                    const res = await fetch('/api/chaos/test/load', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({target: 'api_gateway', requests: 50, concurrency: 5})
                    });
                    const data = await res.json();
                    
                    let html = '<div class=\"card\"><h2>Load Test Results</h2>';
                    html += '<p><strong>Target:</strong> ' + data.target + '</p>';
                    html += '<p><strong>Total Requests:</strong> ' + data.total_requests + '</p>';
                    html += '<p><strong>Successful:</strong> ' + data.successful + '</p>';
                    html += '<p><strong>Failed:</strong> ' + data.failed + '</p>';
                    html += '<p><strong>Duration:</strong> ' + data.duration_ms + 'ms</p>';
                    html += '<p><strong>Requests/sec:</strong> ' + (data.requests_per_second || 0).toFixed(2) + '</p>';
                    if (data.latency) {
                        html += '<h3 style=\"margin-top:15px;\">Latency Metrics</h3>';
                        html += '<p>Min: ' + data.latency.min + 'ms | ';
                        html += 'Avg: ' + (data.latency.avg || 0).toFixed(2) + 'ms | ';
                        html += 'Max: ' + data.latency.max + 'ms</p>';
                        html += '<p>p50: ' + data.latency.p50 + 'ms | ';
                        html += 'p95: ' + data.latency.p95 + 'ms | ';
                        html += 'p99: ' + data.latency.p99 + 'ms</p>';
                    }
                    html += '</div>';
                    document.getElementById('chaos-results').innerHTML = html;
                } catch (e) {
                    document.getElementById('chaos-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function runCascadeTest() {
                document.getElementById('chaos-results').innerHTML = '<div class=\"loading\">Running cascade failure test...</div>';
                try {
                    const res = await fetch('/api/chaos/scenario/cascade-failure', {method: 'POST'});
                    const data = await res.json();
                    
                    let html = '<div class=\"card\"><h2>Cascade Failure Test Results</h2>';
                    html += '<p><strong>Scenario:</strong> ' + data.scenario + '</p>';
                    html += '<p><strong>Completed:</strong> ' + (data.completed ? 'Yes' : 'No') + '</p>';
                    if (data.steps) {
                        html += '<h3 style=\"margin-top:15px;\">Steps</h3>';
                        for (const step of data.steps) {
                            html += '<p>Step ' + step.step + ': ' + step.action + ' → ' + JSON.stringify(step.result) + '</p>';
                        }
                    }
                    html += '</div>';
                    document.getElementById('chaos-results').innerHTML = html;
                } catch (e) {
                    document.getElementById('chaos-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function runRecoveryTest() {
                document.getElementById('chaos-results').innerHTML = '<div class=\"loading\">Running recovery test...</div>';
                try {
                    const res = await fetch('/api/chaos/scenario/recovery-test', {method: 'POST'});
                    const data = await res.json();
                    
                    let html = '<div class=\"card\"><h2>Recovery Test Results</h2>';
                    html += '<p><strong>Scenario:</strong> ' + data.scenario + '</p>';
                    html += '<p><strong>Recovery Successful:</strong> ' + (data.recovery_successful ? 'Yes' : 'No') + '</p>';
                    if (data.steps) {
                        html += '<h3 style=\"margin-top:15px;\">Steps</h3>';
                        for (const step of data.steps) {
                            html += '<p>Step ' + step.step + ': ' + step.action + ' → ' + JSON.stringify(step.result) + '</p>';
                        }
                    }
                    html += '</div>';
                    document.getElementById('chaos-results').innerHTML = html;
                } catch (e) {
                    document.getElementById('chaos-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function checkAllHealth() {
                document.getElementById('chaos-results').innerHTML = '<div class=\"loading\">Checking all services...</div>';
                try {
                    const res = await fetch('/api/chaos/services/health');
                    const data = await res.json();
                    
                    let html = '<div class=\"card\"><h2>Service Health Check</h2>';
                    if (data.services) {
                        for (const svc of data.services) {
                            const statusClass = svc.status === 'healthy' ? 'status-healthy' : 'status-unhealthy';
                            html += '<p><span class=\"' + statusClass + '\">●</span> ' + svc.name + ': ' + svc.status + '</p>';
                        }
                    }
                    html += '</div>';
                    document.getElementById('chaos-results').innerHTML = html;
                } catch (e) {
                    document.getElementById('chaos-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
        </script>">>
    ],
    Html = html_templates:base_layout(<<"Settings">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.
