%%%-------------------------------------------------------------------
%%% @doc Settings Handler Template - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(settings_handler_template_part2).

-export([content/0]).

content() ->
    <<"
            <div style=\"display: flex; gap: 10px; flex-wrap: wrap; margin-top: 15px;\">
                <button class=\"btn\" onclick=\"testHttpSource()\">Test Source</button>
                <button class=\"btn btn-secondary\" onclick=\"downloadFromHttp()\">Download Now</button>
                <button class=\"btn btn-secondary\" onclick=\"listHttpSources()\">List Sources</button>
            </div>
            <div id=\"http-results\" style=\"margin-top: 15px;\"></div>
        </div>
        
        <div class=\"card\" style=\"border-left: 4px solid #ec4899;\">
            <h2>LAN/Local Network Backup</h2>
            <p>Peer-to-peer sync with other instances on your local network.</p>
            <div id=\"lan-status\" style=\"margin: 15px 0;\">
                <p class=\"loading\">Loading LAN status...</p>
            </div>
            <div style=\"display: flex; gap: 10px; flex-wrap: wrap; margin-top: 15px;\">
                <button class=\"btn\" onclick=\"discoverPeers()\">Discover Peers</button>
                <button class=\"btn btn-secondary\" onclick=\"broadcastUpdate()\">Broadcast Update</button>
                <button class=\"btn btn-secondary\" onclick=\"listPeers()\">List Peers</button>
            </div>
            <div id=\"lan-results\" style=\"margin-top: 15px;\"></div>
        </div>
        
        <div class=\"card\" style=\"border-left: 4px solid #10b981;\">
            <h2>Scheduled Analysis</h2>
            <p>Set up recurring analyses to run automatically on a schedule.</p>
            <div id=\"scheduled-list\" style=\"margin: 15px 0;\">
                <p class=\"loading\">Loading scheduled tasks...</p>
            </div>
            <div style=\"background: #f8f9fa; padding: 15px; border-radius: 8px; margin-top: 15px;\">
                <h4 style=\"margin-bottom: 10px;\">Add New Scheduled Analysis</h4>
                <div style=\"display: grid; grid-template-columns: 1fr 1fr; gap: 10px;\">
                    <div>
                        <label><strong>Name</strong></label>
                        <input type=\"text\" id=\"schedule-name\" placeholder=\"Daily news analysis\" style=\"width: 100%; padding: 8px; border-radius: 4px; border: 1px solid #e0e0e0; margin-top: 5px;\">
                    </div>
                    <div>
                        <label><strong>Type</strong></label>
                        <select id=\"schedule-type\" style=\"width: 100%; padding: 8px; border-radius: 4px; border: 1px solid #e0e0e0; margin-top: 5px;\">
                            <option value=\"url\">URL Scrape + Analyze</option>
                            <option value=\"text\">Text Analysis</option>
                        </select>
                    </div>
                    <div>
                        <label><strong>URL or Text</strong></label>
                        <input type=\"text\" id=\"schedule-target\" placeholder=\"https://example.com or text to analyze\" style=\"width: 100%; padding: 8px; border-radius: 4px; border: 1px solid #e0e0e0; margin-top: 5px;\">
                    </div>
                    <div>
                        <label><strong>Interval</strong></label>
                        <select id=\"schedule-interval\" style=\"width: 100%; padding: 8px; border-radius: 4px; border: 1px solid #e0e0e0; margin-top: 5px;\">
                            <option value=\"3600\">Every hour</option>
                            <option value=\"21600\">Every 6 hours</option>
                            <option value=\"43200\">Every 12 hours</option>
                            <option value=\"86400\" selected>Daily</option>
                            <option value=\"604800\">Weekly</option>
                        </select>
                    </div>
                </div>
                <button class=\"btn\" onclick=\"addScheduledTask()\" style=\"margin-top: 15px;\">Add Scheduled Task</button>
            </div>
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
    ">>.
