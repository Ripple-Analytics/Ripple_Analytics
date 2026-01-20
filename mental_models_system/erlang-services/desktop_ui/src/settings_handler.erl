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
                <p><strong>Version:</strong> <span id=\"app-version\">1.2.0</span></p>
                <p><strong>Branch:</strong> <span id=\"git-branch\" style=\"font-family: monospace; background: #f0f0f0; padding: 2px 6px; border-radius: 3px;\">loading...</span></p>
                <p><strong>Commit:</strong> <span id=\"git-commit\" style=\"font-family: monospace; background: #f0f0f0; padding: 2px 6px; border-radius: 3px;\">loading...</span></p>
                <p><strong>Commit Date:</strong> <span id=\"commit-date\">loading...</span></p>
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
                <p><strong>GDrive Backup:</strong> http://localhost:8006</p>
                <p><strong>Chaos Monkey:</strong> http://localhost:8007</p>
                <p><strong>Desktop UI:</strong> http://localhost:3000</p>
            </div>
        </div>
        
        <div class=\"card\" style=\"border-left: 4px solid #28a745;\">
            <h2>System Health Check</h2>
            <p>Verify all services are running correctly.</p>
            <div id=\"health-status\" style=\"margin: 15px 0;\">
                <p class=\"loading\">Click button to check health...</p>
            </div>
            <button class=\"btn\" onclick=\"runHealthCheck()\">Run Health Check</button>
        </div>
        
        <div class=\"card\" style=\"border-left: 4px solid #f59e0b;\">
            <h2>Google Drive Backup</h2>
            <p>Manage backups and configure Google Drive as fallback source.</p>
            <div id=\"gdrive-status\" style=\"margin: 15px 0;\">
                <p class=\"loading\">Loading backup status...</p>
            </div>
            <div style=\"display: flex; gap: 10px; flex-wrap: wrap; margin-top: 15px;\">
                <button class=\"btn\" onclick=\"triggerBackup()\">Create Backup Now</button>
                <button class=\"btn btn-secondary\" onclick=\"listBackups()\">List Backups</button>
                <button class=\"btn btn-secondary\" onclick=\"loadGdriveStatus()\">Refresh Status</button>
            </div>
            <div id=\"backup-list\" style=\"margin-top: 15px;\"></div>
        </div>
        
        <div class=\"card\" style=\"border-left: 4px solid #dc3545;\">
            <h2>Chaos Monkey</h2>
            <p>Proactively test system resilience by randomly breaking things. <strong style=\"color: #dc3545;\">Use with caution!</strong></p>
            <div id=\"chaos-monkey-status\" style=\"margin: 15px 0;\">
                <p class=\"loading\">Loading chaos monkey status...</p>
            </div>
            <div style=\"display: flex; gap: 10px; flex-wrap: wrap; margin-top: 15px;\">
                <button class=\"btn\" id=\"chaos-toggle-btn\" onclick=\"toggleChaosMonkey()\">Enable Chaos Monkey</button>
                <button class=\"btn btn-secondary\" onclick=\"triggerRandomAttack()\">Trigger Random Attack</button>
                <button class=\"btn btn-secondary\" onclick=\"viewAttackHistory()\">View Attack History</button>
            </div>
            <div id=\"attack-types\" style=\"margin-top: 15px;\">
                <h4>Manual Attack Types:</h4>
                <div style=\"display: flex; gap: 10px; flex-wrap: wrap; margin-top: 10px;\">
                    <button class=\"btn btn-secondary\" onclick=\"triggerAttack('kill_container')\">Kill Container</button>
                    <button class=\"btn btn-secondary\" onclick=\"triggerAttack('network_latency')\">Network Latency</button>
                    <button class=\"btn btn-secondary\" onclick=\"triggerAttack('cpu_stress')\">CPU Stress</button>
                    <button class=\"btn btn-secondary\" onclick=\"triggerAttack('memory_pressure')\">Memory Pressure</button>
                    <button class=\"btn btn-secondary\" onclick=\"triggerAttack('service_restart')\">Service Restart</button>
                </div>
            </div>
            <div id=\"chaos-monkey-results\" style=\"margin-top: 15px;\"></div>
        </div>
        
        <div class=\"card\">
            <h2>Chaos Engineering (Legacy)</h2>
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
                    if (data.branch) {
                        document.getElementById('git-branch').textContent = data.branch;
                        document.getElementById('git-branch').style.color = data.branch === 'master' ? '#28a745' : '#f59e0b';
                    }
                    if (data.commit) {
                        document.getElementById('git-commit').textContent = data.commit;
                    }
                    if (data.commit_date) {
                        document.getElementById('commit-date').textContent = data.commit_date;
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
            loadGdriveStatus();
            loadChaosMonkeyStatus();
            setInterval(loadUpdateStatus, 30000);
            setInterval(loadGdriveStatus, 60000);
            setInterval(loadChaosMonkeyStatus, 30000);
            
            // Health check function
            async function runHealthCheck() {
                document.getElementById('health-status').innerHTML = '<p class=\"loading\">Checking all services...</p>';
                
                const services = [
                    {name: 'API Gateway', url: 'http://localhost:8000/health'},
                    {name: 'Analysis Service', url: 'http://localhost:8001/health'},
                    {name: 'Harvester Service', url: 'http://localhost:8002/health'},
                    {name: 'Storage Service', url: 'http://localhost:8003/health'},
                    {name: 'Chaos Engineering', url: 'http://localhost:8005/health'},
                    {name: 'GDrive Backup', url: 'http://localhost:8006/health'},
                    {name: 'Chaos Monkey', url: 'http://localhost:8007/health'},
                    {name: 'Desktop UI', url: '/health'}
                ];
                
                let html = '<div style=\"display: grid; grid-template-columns: repeat(auto-fill, minmax(200px, 1fr)); gap: 10px;\">';
                
                for (const svc of services) {
                    try {
                        const res = await fetch(svc.url, {timeout: 5000});
                        const data = await res.json();
                        const isHealthy = res.ok && (data.status === 'healthy' || data.status === 'ok');
                        const statusClass = isHealthy ? 'status-healthy' : 'status-unhealthy';
                        const statusText = isHealthy ? 'Healthy' : 'Unhealthy';
                        html += '<div style=\"padding: 10px; border-radius: 4px; background: #f8f9fa;\">';
                        html += '<span class=\"' + statusClass + '\">●</span> <strong>' + svc.name + '</strong><br>';
                        html += '<small>' + statusText + '</small></div>';
                    } catch (e) {
                        html += '<div style=\"padding: 10px; border-radius: 4px; background: #f8f9fa;\">';
                        html += '<span class=\"status-unhealthy\">●</span> <strong>' + svc.name + '</strong><br>';
                        html += '<small>Unreachable</small></div>';
                    }
                }
                
                html += '</div>';
                document.getElementById('health-status').innerHTML = html;
            }
            
            // Google Drive Backup functions
            async function loadGdriveStatus() {
                try {
                    const res = await fetch('http://localhost:8006/api/backup/status');
                    const data = await res.json();
                    
                    let html = '<div style=\"display: grid; grid-template-columns: 1fr 1fr; gap: 10px;\">';
                    html += '<p><strong>Status:</strong> ' + (data.gdrive?.status || 'unknown') + '</p>';
                    html += '<p><strong>Configured:</strong> ' + (data.gdrive?.gdrive_configured ? 'Yes' : 'No') + '</p>';
                    html += '<p><strong>Last Sync:</strong> ' + (data.gdrive?.last_sync || 'never') + '</p>';
                    html += '<p><strong>Backup Count:</strong> ' + (data.backup_count || 0) + '</p>';
                    html += '</div>';
                    
                    document.getElementById('gdrive-status').innerHTML = html;
                } catch (e) {
                    document.getElementById('gdrive-status').innerHTML = 
                        '<p class=\"status-unknown\">GDrive Backup service not available</p>';
                }
            }
            
            async function triggerBackup() {
                document.getElementById('backup-list').innerHTML = '<p class=\"loading\">Creating backup...</p>';
                try {
                    const res = await fetch('http://localhost:8006/api/backup/create', {method: 'POST'});
                    const data = await res.json();
                    
                    if (data.success) {
                        document.getElementById('backup-list').innerHTML = 
                            '<div class=\"alert alert-success\">Backup triggered successfully!</div>';
                        loadGdriveStatus();
                    } else {
                        document.getElementById('backup-list').innerHTML = 
                            '<div class=\"alert alert-error\">Failed to create backup</div>';
                    }
                } catch (e) {
                    document.getElementById('backup-list').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function listBackups() {
                document.getElementById('backup-list').innerHTML = '<p class=\"loading\">Loading backups...</p>';
                try {
                    const res = await fetch('http://localhost:8006/api/backup/list');
                    const data = await res.json();
                    
                    if (data.success && data.backups && data.backups.length > 0) {
                        let html = '<table style=\"width: 100%; border-collapse: collapse;\">';
                        html += '<tr style=\"background: #f8f9fa;\"><th style=\"padding: 8px; text-align: left;\">Name</th>';
                        html += '<th style=\"padding: 8px; text-align: left;\">Size</th>';
                        html += '<th style=\"padding: 8px; text-align: left;\">Modified</th></tr>';
                        
                        for (const backup of data.backups) {
                            html += '<tr style=\"border-bottom: 1px solid #eee;\">';
                            html += '<td style=\"padding: 8px;\">' + backup.name + '</td>';
                            html += '<td style=\"padding: 8px;\">' + formatBytes(backup.size) + '</td>';
                            html += '<td style=\"padding: 8px;\">' + backup.modified + '</td>';
                            html += '</tr>';
                        }
                        html += '</table>';
                        document.getElementById('backup-list').innerHTML = html;
                    } else {
                        document.getElementById('backup-list').innerHTML = 
                            '<p>No backups found. Click \"Create Backup Now\" to create one.</p>';
                    }
                } catch (e) {
                    document.getElementById('backup-list').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            function formatBytes(bytes) {
                if (bytes === 0) return '0 Bytes';
                const k = 1024;
                const sizes = ['Bytes', 'KB', 'MB', 'GB'];
                const i = Math.floor(Math.log(bytes) / Math.log(k));
                return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + sizes[i];
            }
            
            // Chaos Monkey functions
            let chaosMonkeyEnabled = false;
            
            async function loadChaosMonkeyStatus() {
                try {
                    const res = await fetch('http://localhost:8007/api/chaos/status');
                    const data = await res.json();
                    
                    chaosMonkeyEnabled = data.engine?.enabled || false;
                    
                    let html = '<div style=\"display: grid; grid-template-columns: 1fr 1fr; gap: 10px;\">';
                    html += '<p><strong>Status:</strong> <span class=\"' + (chaosMonkeyEnabled ? 'status-unhealthy' : 'status-healthy') + '\">' + 
                            (chaosMonkeyEnabled ? 'ENABLED (Active)' : 'Disabled (Safe)') + '</span></p>';
                    html += '<p><strong>Total Attacks:</strong> ' + (data.engine?.total_attacks || 0) + '</p>';
                    html += '<p><strong>Recovery Rate:</strong> ' + (data.engine?.recovery_rate?.toFixed(1) || 100) + '%</p>';
                    html += '<p><strong>Last Attack:</strong> ' + (data.engine?.last_attack || 'never') + '</p>';
                    html += '</div>';
                    
                    document.getElementById('chaos-monkey-status').innerHTML = html;
                    
                    // Update toggle button
                    const btn = document.getElementById('chaos-toggle-btn');
                    if (chaosMonkeyEnabled) {
                        btn.textContent = 'Disable Chaos Monkey';
                        btn.style.background = '#dc3545';
                    } else {
                        btn.textContent = 'Enable Chaos Monkey';
                        btn.style.background = '#4361ee';
                    }
                } catch (e) {
                    document.getElementById('chaos-monkey-status').innerHTML = 
                        '<p class=\"status-unknown\">Chaos Monkey service not available</p>';
                }
            }
            
            async function toggleChaosMonkey() {
                const endpoint = chaosMonkeyEnabled ? 
                    'http://localhost:8007/api/chaos/disable' : 
                    'http://localhost:8007/api/chaos/enable';
                
                try {
                    const res = await fetch(endpoint, {method: 'POST'});
                    const data = await res.json();
                    
                    if (data.success) {
                        loadChaosMonkeyStatus();
                        document.getElementById('chaos-monkey-results').innerHTML = 
                            '<div class=\"alert alert-success\">' + data.message + '</div>';
                    }
                } catch (e) {
                    document.getElementById('chaos-monkey-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function triggerRandomAttack() {
                if (!chaosMonkeyEnabled) {
                    document.getElementById('chaos-monkey-results').innerHTML = 
                        '<div class=\"alert alert-error\">Chaos Monkey is disabled. Enable it first.</div>';
                    return;
                }
                
                document.getElementById('chaos-monkey-results').innerHTML = '<p class=\"loading\">Executing random attack...</p>';
                try {
                    const res = await fetch('http://localhost:8007/api/chaos/attack', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({type: 'random'})
                    });
                    const data = await res.json();
                    
                    if (data.success) {
                        let html = '<div class=\"card\" style=\"margin-top: 10px;\">';
                        html += '<h4>Attack Result</h4>';
                        html += '<p><strong>Type:</strong> ' + (data.attack?.attack || 'unknown') + '</p>';
                        html += '<p><strong>Target:</strong> ' + (data.attack?.target || 'N/A') + '</p>';
                        html += '<p><strong>Recovered:</strong> ' + (data.attack?.recovered ? 'Yes' : 'No') + '</p>';
                        html += '</div>';
                        document.getElementById('chaos-monkey-results').innerHTML = html;
                        loadChaosMonkeyStatus();
                    } else {
                        document.getElementById('chaos-monkey-results').innerHTML = 
                            '<div class=\"alert alert-error\">' + (data.error || 'Attack failed') + '</div>';
                    }
                } catch (e) {
                    document.getElementById('chaos-monkey-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function triggerAttack(attackType) {
                if (!chaosMonkeyEnabled) {
                    document.getElementById('chaos-monkey-results').innerHTML = 
                        '<div class=\"alert alert-error\">Chaos Monkey is disabled. Enable it first.</div>';
                    return;
                }
                
                document.getElementById('chaos-monkey-results').innerHTML = '<p class=\"loading\">Executing ' + attackType + ' attack...</p>';
                try {
                    const res = await fetch('http://localhost:8007/api/chaos/attack', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({type: attackType})
                    });
                    const data = await res.json();
                    
                    if (data.success) {
                        let html = '<div class=\"card\" style=\"margin-top: 10px;\">';
                        html += '<h4>Attack Result: ' + attackType + '</h4>';
                        html += '<p><strong>Target:</strong> ' + (data.attack?.target || 'N/A') + '</p>';
                        html += '<p><strong>Recovered:</strong> ' + (data.attack?.recovered ? 'Yes' : 'No') + '</p>';
                        if (data.attack?.output) {
                            html += '<p><strong>Output:</strong> <code>' + data.attack.output.substring(0, 200) + '</code></p>';
                        }
                        html += '</div>';
                        document.getElementById('chaos-monkey-results').innerHTML = html;
                        loadChaosMonkeyStatus();
                    } else {
                        document.getElementById('chaos-monkey-results').innerHTML = 
                            '<div class=\"alert alert-error\">' + (data.error || 'Attack failed') + '</div>';
                    }
                } catch (e) {
                    document.getElementById('chaos-monkey-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function viewAttackHistory() {
                document.getElementById('chaos-monkey-results').innerHTML = '<p class=\"loading\">Loading attack history...</p>';
                try {
                    const res = await fetch('http://localhost:8007/api/chaos/history');
                    const data = await res.json();
                    
                    if (data.history && data.history.length > 0) {
                        let html = '<div class=\"card\" style=\"margin-top: 10px;\">';
                        html += '<h4>Attack History (Last ' + data.count + ')</h4>';
                        html += '<table style=\"width: 100%; border-collapse: collapse;\">';
                        html += '<tr style=\"background: #f8f9fa;\"><th style=\"padding: 8px; text-align: left;\">Type</th>';
                        html += '<th style=\"padding: 8px; text-align: left;\">Timestamp</th>';
                        html += '<th style=\"padding: 8px; text-align: left;\">Recovered</th></tr>';
                        
                        for (const attack of data.history.slice(0, 10)) {
                            html += '<tr style=\"border-bottom: 1px solid #eee;\">';
                            html += '<td style=\"padding: 8px;\">' + attack.type + '</td>';
                            html += '<td style=\"padding: 8px;\">' + attack.timestamp + '</td>';
                            html += '<td style=\"padding: 8px;\"><span class=\"' + (attack.recovered ? 'status-healthy' : 'status-unhealthy') + '\">●</span> ' + 
                                    (attack.recovered ? 'Yes' : 'No') + '</td>';
                            html += '</tr>';
                        }
                        html += '</table></div>';
                        document.getElementById('chaos-monkey-results').innerHTML = html;
                    } else {
                        document.getElementById('chaos-monkey-results').innerHTML = 
                            '<p>No attack history yet.</p>';
                    }
                } catch (e) {
                    document.getElementById('chaos-monkey-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            // Chaos engineering functions (legacy)
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
