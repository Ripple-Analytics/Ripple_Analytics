%%%-------------------------------------------------------------------
%%% @doc Settings Handler - System settings, update sources, and chaos testing
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
                <p><strong>Branch:</strong> <span id=\"git-branch\">Loading...</span></p>
                <p><strong>Commit:</strong> <span id=\"git-commit\">Loading...</span></p>
            </div>
            <div class=\"card\">
                <h2>Service Endpoints</h2>
                <p><strong>API Gateway:</strong> http://localhost:8000</p>
                <p><strong>Auto-Updater:</strong> http://localhost:8006</p>
                <p><strong>GitHub Source:</strong> http://localhost:8010</p>
                <p><strong>GDrive Source:</strong> http://localhost:8011</p>
                <p><strong>Local Cache:</strong> http://localhost:8012</p>
                <p><strong>Chaos Monkey:</strong> http://localhost:8013</p>
                <p><strong>Desktop UI:</strong> http://localhost:3000</p>
            </div>
        </div>
        
        <!-- Update Sources Status -->
        <div class=\"card\">
            <h2>🔄 Update Sources</h2>
            <p>Bulletproof update system with multiple independent sources.</p>
            <div id=\"sources-status\" style=\"margin: 15px 0;\">
                <div class=\"grid\" style=\"grid-template-columns: repeat(3, 1fr);\">
                    <div id=\"github-source-card\" class=\"source-card\">
                        <h3>📦 GitHub</h3>
                        <p class=\"source-status\">Loading...</p>
                    </div>
                    <div id=\"gdrive-source-card\" class=\"source-card\">
                        <h3>☁️ Google Drive</h3>
                        <p class=\"source-status\">Loading...</p>
                    </div>
                    <div id=\"cache-source-card\" class=\"source-card\">
                        <h3>💾 Local Cache</h3>
                        <p class=\"source-status\">Loading...</p>
                    </div>
                </div>
            </div>
            <div style=\"display: flex; gap: 10px; flex-wrap: wrap; margin-top: 15px;\">
                <button class=\"btn\" onclick=\"checkForUpdates()\" id=\"check-btn\">Check for Updates</button>
                <button class=\"btn\" onclick=\"performUpdate()\" id=\"update-btn\" style=\"background: #28a745;\">Update Now</button>
                <button class=\"btn btn-secondary\" onclick=\"refreshAllSources()\">Refresh All Sources</button>
            </div>
            <div id=\"updater-results\" style=\"margin-top: 15px;\"></div>
        </div>
        
        <!-- Chaos Monkey Section -->
        <div class=\"card\">
            <h2>🐒 Update Chaos Monkey</h2>
            <p>Test update system resilience by simulating failures.</p>
            <div id=\"chaos-status\" style=\"margin: 15px 0; padding: 15px; background: white; border-radius: 6px; border: 1px solid #e0e0e0;\">
                <p>Loading chaos monkey status...</p>
            </div>
            <div style=\"display: flex; gap: 10px; flex-wrap: wrap;\">
                <button class=\"btn\" onclick=\"runChaosScenario('test_github_source')\">Test GitHub</button>
                <button class=\"btn\" onclick=\"runChaosScenario('test_gdrive_source')\">Test GDrive</button>
                <button class=\"btn\" onclick=\"runChaosScenario('test_local_cache')\">Test Cache</button>
                <button class=\"btn btn-secondary\" onclick=\"runChaosScenario('test_fallback_chain')\">Test Fallback Chain</button>
                <button class=\"btn btn-secondary\" onclick=\"runChaosScenario('stress_test')\">Stress Test</button>
            </div>
            <div id=\"chaos-results\" style=\"margin-top: 15px;\"></div>
        </div>
        
        <!-- Legacy Chaos Engineering -->
        <div class=\"card\">
            <h2>⚡ Service Chaos Engineering</h2>
            <p>Test core service resilience with controlled chaos experiments.</p>
            <br>
            <div style=\"display: flex; gap: 10px; flex-wrap: wrap;\">
                <button class=\"btn\" onclick=\"runLoadTest()\">Run Load Test</button>
                <button class=\"btn btn-secondary\" onclick=\"runCascadeTest()\">Cascade Failure Test</button>
                <button class=\"btn btn-secondary\" onclick=\"runRecoveryTest()\">Recovery Test</button>
                <button class=\"btn btn-secondary\" onclick=\"checkAllHealth()\">Check All Health</button>
            </div>
        </div>
        <div id=\"legacy-chaos-results\"></div>
        
        <style>
            .source-card {
                padding: 15px;
                background: white;
                border-radius: 8px;
                border: 2px solid #e0e0e0;
                text-align: center;
            }
            .source-card.available {
                border-color: #28a745;
                background: #f0fff0;
            }
            .source-card.unavailable {
                border-color: #dc3545;
                background: #fff0f0;
            }
            .source-card h3 {
                margin: 0 0 10px 0;
                font-size: 1.1em;
            }
            .source-status {
                margin: 0;
                font-size: 0.9em;
            }
            .chaos-result {
                padding: 10px;
                margin: 5px 0;
                border-radius: 4px;
                background: #f8f9fa;
            }
            .chaos-result.success {
                background: #d4edda;
                border-left: 4px solid #28a745;
            }
            .chaos-result.failure {
                background: #f8d7da;
                border-left: 4px solid #dc3545;
            }
        </style>
        
        <script>
            const UPDATER_URL = 'http://localhost:8006';
            const GITHUB_SOURCE_URL = 'http://localhost:8010';
            const GDRIVE_SOURCE_URL = 'http://localhost:8011';
            const LOCAL_CACHE_URL = 'http://localhost:8012';
            const CHAOS_MONKEY_URL = 'http://localhost:8013';
            
            // Refresh all source statuses
            async function refreshAllSources() {
                await Promise.all([
                    refreshGitHubSource(),
                    refreshGDriveSource(),
                    refreshLocalCache(),
                    refreshChaosStatus()
                ]);
            }
            
            async function refreshGitHubSource() {
                const card = document.getElementById('github-source-card');
                try {
                    const res = await fetch(GITHUB_SOURCE_URL + '/api/source/status');
                    const data = await res.json();
                    
                    card.className = 'source-card ' + (data.available ? 'available' : 'unavailable');
                    let html = '<h3>📦 GitHub</h3>';
                    html += '<p class=\"source-status\">' + (data.available ? '✅ Available' : '❌ Unavailable') + '</p>';
                    if (data.current_commit) {
                        html += '<p class=\"source-status\">Commit: ' + data.current_commit + '</p>';
                        document.getElementById('git-commit').textContent = data.current_commit;
                    }
                    if (data.auth_method) {
                        html += '<p class=\"source-status\">Auth: ' + data.auth_method + '</p>';
                    }
                    card.innerHTML = html;
                } catch (e) {
                    card.className = 'source-card unavailable';
                    card.innerHTML = '<h3>📦 GitHub</h3><p class=\"source-status\">❌ Service offline</p>';
                }
            }
            
            async function refreshGDriveSource() {
                const card = document.getElementById('gdrive-source-card');
                try {
                    const res = await fetch(GDRIVE_SOURCE_URL + '/api/source/status');
                    const data = await res.json();
                    
                    card.className = 'source-card ' + (data.available ? 'available' : 'unavailable');
                    let html = '<h3>☁️ Google Drive</h3>';
                    html += '<p class=\"source-status\">' + (data.available ? '✅ Configured' : '⚠️ Not configured') + '</p>';
                    if (data.last_download) {
                        html += '<p class=\"source-status\">Last sync: ' + new Date(data.last_download).toLocaleDateString() + '</p>';
                    }
                    card.innerHTML = html;
                } catch (e) {
                    card.className = 'source-card unavailable';
                    card.innerHTML = '<h3>☁️ Google Drive</h3><p class=\"source-status\">❌ Service offline</p>';
                }
            }
            
            async function refreshLocalCache() {
                const card = document.getElementById('cache-source-card');
                try {
                    const res = await fetch(LOCAL_CACHE_URL + '/api/source/status');
                    const data = await res.json();
                    
                    const hasVersions = data.versions_count > 0;
                    card.className = 'source-card ' + (hasVersions ? 'available' : 'unavailable');
                    let html = '<h3>💾 Local Cache</h3>';
                    html += '<p class=\"source-status\">' + (hasVersions ? '✅ ' + data.versions_count + ' versions' : '⚠️ No cached versions') + '</p>';
                    if (data.current_version) {
                        html += '<p class=\"source-status\">Current: ' + data.current_version.substring(0, 15) + '...</p>';
                    }
                    card.innerHTML = html;
                } catch (e) {
                    card.className = 'source-card unavailable';
                    card.innerHTML = '<h3>💾 Local Cache</h3><p class=\"source-status\">❌ Service offline</p>';
                }
            }
            
            async function refreshChaosStatus() {
                try {
                    const res = await fetch(CHAOS_MONKEY_URL + '/api/chaos/status');
                    const data = await res.json();
                    
                    let html = '<p><strong>Status:</strong> ' + data.status + '</p>';
                    html += '<p><strong>Available Tests:</strong> ' + data.total_tests + '</p>';
                    html += '<p><strong>Results Stored:</strong> ' + data.results_count + '</p>';
                    
                    document.getElementById('chaos-status').innerHTML = html;
                } catch (e) {
                    document.getElementById('chaos-status').innerHTML = 
                        '<p style=\"color: #dc3545;\">❌ Chaos Monkey service not available</p>';
                }
            }
            
            async function runChaosScenario(scenarioId) {
                document.getElementById('chaos-results').innerHTML = '<div class=\"loading\">Running ' + scenarioId + '...</div>';
                
                try {
                    const res = await fetch(CHAOS_MONKEY_URL + '/api/chaos/run', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({scenario: scenarioId})
                    });
                    const data = await res.json();
                    
                    let html = '<div class=\"chaos-result ' + (data.success ? 'success' : 'failure') + '\">';
                    html += '<strong>' + (data.success ? '✅ PASSED' : '❌ FAILED') + ':</strong> ' + scenarioId;
                    html += '<br><small>' + data.timestamp + '</small>';
                    
                    if (data.available_sources) {
                        html += '<br><strong>Available Sources:</strong> ' + data.available_sources.join(', ');
                    }
                    if (data.successful_requests !== undefined) {
                        html += '<br><strong>Success Rate:</strong> ' + data.successful_requests + '/' + data.total_requests;
                    }
                    if (data.error) {
                        html += '<br><strong>Error:</strong> ' + data.error;
                    }
                    
                    html += '</div>';
                    document.getElementById('chaos-results').innerHTML = html;
                } catch (e) {
                    document.getElementById('chaos-results').innerHTML = 
                        '<div class=\"chaos-result failure\">Error: ' + e.message + '</div>';
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
                    
                    await refreshAllSources();
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
                            '<div class=\"alert alert-success\">✅ ' + data.message + '</div>';
                    } else if (data.error) {
                        document.getElementById('updater-results').innerHTML = 
                            '<div class=\"alert alert-error\">❌ Update failed: ' + JSON.stringify(data.error) + '</div>';
                    }
                    
                    await refreshAllSources();
                } catch (e) {
                    document.getElementById('updater-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
                
                btn.disabled = false;
                btn.textContent = 'Update Now';
            }
            
            // Legacy Chaos Engineering Functions
            async function runLoadTest() {
                document.getElementById('legacy-chaos-results').innerHTML = '<div class=\"loading\">Running load test...</div>';
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
                    html += '</div>';
                    document.getElementById('legacy-chaos-results').innerHTML = html;
                } catch (e) {
                    document.getElementById('legacy-chaos-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function runCascadeTest() {
                document.getElementById('legacy-chaos-results').innerHTML = '<div class=\"loading\">Running cascade test...</div>';
                try {
                    const res = await fetch('/api/chaos/scenario/cascade-failure', {method: 'POST'});
                    const data = await res.json();
                    document.getElementById('legacy-chaos-results').innerHTML = 
                        '<div class=\"card\"><h2>Cascade Test</h2><pre>' + JSON.stringify(data, null, 2) + '</pre></div>';
                } catch (e) {
                    document.getElementById('legacy-chaos-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function runRecoveryTest() {
                document.getElementById('legacy-chaos-results').innerHTML = '<div class=\"loading\">Running recovery test...</div>';
                try {
                    const res = await fetch('/api/chaos/scenario/recovery-test', {method: 'POST'});
                    const data = await res.json();
                    document.getElementById('legacy-chaos-results').innerHTML = 
                        '<div class=\"card\"><h2>Recovery Test</h2><pre>' + JSON.stringify(data, null, 2) + '</pre></div>';
                } catch (e) {
                    document.getElementById('legacy-chaos-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function checkAllHealth() {
                document.getElementById('legacy-chaos-results').innerHTML = '<div class=\"loading\">Checking services...</div>';
                try {
                    const res = await fetch('/api/chaos/services/health');
                    const data = await res.json();
                    
                    let html = '<div class=\"card\"><h2>Service Health</h2>';
                    if (data.services) {
                        for (const svc of data.services) {
                            const icon = svc.status === 'healthy' ? '✅' : '❌';
                            html += '<p>' + icon + ' ' + svc.name + ': ' + svc.status + '</p>';
                        }
                    }
                    html += '</div>';
                    document.getElementById('legacy-chaos-results').innerHTML = html;
                } catch (e) {
                    document.getElementById('legacy-chaos-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            // Load all statuses on page load
            document.addEventListener('DOMContentLoaded', refreshAllSources);
            
            // Set branch info
            document.getElementById('git-branch').textContent = 'release';
        </script>">>
    ],
    Html = html_templates:base_layout(<<"Settings">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.
