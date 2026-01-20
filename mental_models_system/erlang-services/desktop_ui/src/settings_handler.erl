%%%-------------------------------------------------------------------
%%% @doc Settings Handler - System settings, chaos engineering, and auto-updater
%%%-------------------------------------------------------------------
-module(settings_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Content = [
        <<"<div class=\"grid\">
            <div class=\"card\">
                <h2>System Information</h2>
                <p><strong>Architecture:</strong> Erlang/OTP Microservices</p>
                <p><strong>Version:</strong> 1.0.0</p>
                <p><strong>Runtime:</strong> Erlang/OTP 26</p>
                <p><strong>UI Theme:</strong> Light Mode</p>
            </div>
            <div class=\"card\">
                <h2>Service Endpoints</h2>
                <p><strong>API Gateway:</strong> http://localhost:8000</p>
                <p><strong>Analysis Service:</strong> http://localhost:8001</p>
                <p><strong>Harvester Service:</strong> http://localhost:8002</p>
                <p><strong>Storage Service:</strong> http://localhost:8003</p>
                <p><strong>Chaos Engineering:</strong> http://localhost:8005</p>
                <p><strong>Auto-Updater:</strong> http://localhost:8006</p>
                <p><strong>Desktop UI:</strong> http://localhost:3000</p>
            </div>
        </div>
        
        <!-- Auto-Updater Section -->
        <div class=\"card\">
            <h2>🔄 Auto-Updater</h2>
            <p>Bulletproof update system with multiple fallback sources.</p>
            <div id=\"updater-status\" style=\"margin: 15px 0; padding: 15px; background: white; border-radius: 6px; border: 1px solid #e0e0e0;\">
                <p>Loading status...</p>
            </div>
            <div style=\"display: flex; gap: 10px; flex-wrap: wrap;\">
                <button class=\"btn\" onclick=\"checkForUpdates()\" id=\"check-btn\">Check for Updates</button>
                <button class=\"btn\" onclick=\"performUpdate()\" id=\"update-btn\" style=\"background: #28a745;\">Update Now</button>
                <button class=\"btn btn-secondary\" onclick=\"refreshUpdaterStatus()\">Refresh Status</button>
            </div>
            <div id=\"updater-results\" style=\"margin-top: 15px;\"></div>
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
            const UPDATER_URL = 'http://localhost:8006';
            
            // Auto-Updater Functions
            async function refreshUpdaterStatus() {
                try {
                    const res = await fetch(UPDATER_URL + '/api/updater/status');
                    const data = await res.json();
                    
                    let statusHtml = '';
                    const statusIcon = data.status === 'idle' ? '✅' : 
                                       data.status === 'checking' ? '🔍' :
                                       data.status === 'updating' ? '⏳' : '❌';
                    
                    statusHtml += '<p><strong>Status:</strong> ' + statusIcon + ' ' + data.status + '</p>';
                    
                    if (data.current_commit) {
                        statusHtml += '<p><strong>Current Version:</strong> ' + data.current_commit.substring(0, 8) + '</p>';
                    }
                    
                    if (data.last_check) {
                        statusHtml += '<p><strong>Last Check:</strong> ' + new Date(data.last_check).toLocaleString() + '</p>';
                    }
                    
                    if (data.last_update) {
                        statusHtml += '<p><strong>Last Update:</strong> ' + new Date(data.last_update).toLocaleString() + '</p>';
                    }
                    
                    if (data.update_source) {
                        statusHtml += '<p><strong>Update Source:</strong> ' + data.update_source + '</p>';
                    }
                    
                    if (data.update_available) {
                        statusHtml += '<p style=\"color: #28a745; font-weight: bold;\">🆕 Update available!</p>';
                        document.getElementById('update-btn').style.display = 'inline-block';
                    } else {
                        document.getElementById('update-btn').style.display = 'none';
                    }
                    
                    if (data.error_message) {
                        statusHtml += '<p style=\"color: #dc3545;\"><strong>Error:</strong> ' + data.error_message + '</p>';
                    }
                    
                    document.getElementById('updater-status').innerHTML = statusHtml;
                } catch (e) {
                    document.getElementById('updater-status').innerHTML = 
                        '<p style=\"color: #dc3545;\">❌ Auto-updater service not available. Make sure it is running on port 8006.</p>';
                }
            }
            
            async function checkForUpdates() {
                const btn = document.getElementById('check-btn');
                btn.disabled = true;
                btn.textContent = 'Checking...';
                document.getElementById('updater-results').innerHTML = '<div class=\"loading\">Checking for updates...</div>';
                
                try {
                    const res = await fetch(UPDATER_URL + '/api/updater/check', {method: 'POST'});
                    const data = await res.json();
                    
                    if (data.update_available) {
                        document.getElementById('updater-results').innerHTML = 
                            '<div class=\"alert alert-success\">🆕 Update available! Click \"Update Now\" to apply.</div>';
                    } else {
                        document.getElementById('updater-results').innerHTML = 
                            '<div class=\"alert alert-info\">✅ You are running the latest version.</div>';
                    }
                    
                    await refreshUpdaterStatus();
                } catch (e) {
                    document.getElementById('updater-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
                
                btn.disabled = false;
                btn.textContent = 'Check for Updates';
            }
            
            async function performUpdate() {
                if (!confirm('This will update the system and restart services. Continue?')) {
                    return;
                }
                
                const btn = document.getElementById('update-btn');
                btn.disabled = true;
                btn.textContent = 'Updating...';
                document.getElementById('updater-results').innerHTML = 
                    '<div class=\"loading\">Applying update... This may take a few minutes.</div>';
                
                try {
                    const res = await fetch(UPDATER_URL + '/api/updater/update', {method: 'POST'});
                    const data = await res.json();
                    
                    if (data.message) {
                        document.getElementById('updater-results').innerHTML = 
                            '<div class=\"alert alert-success\">✅ ' + data.message + ' (via ' + data.source + ')</div>';
                    } else if (data.error) {
                        document.getElementById('updater-results').innerHTML = 
                            '<div class=\"alert alert-error\">❌ Update failed: ' + JSON.stringify(data.error) + '</div>';
                    }
                    
                    await refreshUpdaterStatus();
                } catch (e) {
                    document.getElementById('updater-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
                
                btn.disabled = false;
                btn.textContent = 'Update Now';
            }
            
            // Load updater status on page load
            document.addEventListener('DOMContentLoaded', refreshUpdaterStatus);
            
            // Chaos Engineering Functions
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
