%%%-------------------------------------------------------------------
%%% @doc Settings Handler Template - Part 1
%%% @end
%%%-------------------------------------------------------------------
-module(settings_handler_template_part1).

-export([content/0]).

content() ->
    <<"

        <div class=\"card\" style=\"border-left: 4px solid #6366f1;\">
            <h2>Appearance</h2>
            <p>Customize the look and feel of the application.</p>
            <div style=\"margin-top: 15px;\">
                <label style=\"display: flex; align-items: center; gap: 10px; cursor: pointer;\">
                    <input type=\"checkbox\" id=\"dark-mode-toggle\" onchange=\"toggleDarkMode()\" style=\"width: 20px; height: 20px;\">
                    <span><strong>Dark Mode</strong></span>
                </label>
                <small style=\"color: #666; display: block; margin-top: 5px;\">Enable dark theme for reduced eye strain in low-light environments</small>
            </div>
            <div style=\"margin-top: 15px;\">
                <label><strong>Font Size</strong></label>
                <select id=\"font-size-select\" onchange=\"changeFontSize()\" style=\"margin-left: 10px; padding: 8px; border-radius: 4px; border: 1px solid #e0e0e0;\">
                    <option value=\"small\">Small</option>
                    <option value=\"medium\" selected>Medium</option>
                    <option value=\"large\">Large</option>
                </select>
            </div>
        </div>
        
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
                <p><strong>Version:</strong> <span id=\"app-version\">1.3.0</span></p>
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
                <p><strong>S3/MinIO Backup:</strong> http://localhost:8008</p>
                <p><strong>HTTP Backup:</strong> http://localhost:8009</p>
                <p><strong>LAN Backup:</strong> http://localhost:8010</p>
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
        
        <div class=\"card\" style=\"border-left: 4px solid #6366f1;\">
            <h2>S3/MinIO Backup</h2>
            <p>Cloud storage backup using S3-compatible storage (AWS S3, MinIO, etc).</p>
            <div id=\"s3-status\" style=\"margin: 15px 0;\">
                <p class=\"loading\">Loading S3 status...</p>
            </div>
            <div style=\"display: flex; gap: 10px; flex-wrap: wrap; margin-top: 15px;\">
                <button class=\"btn\" onclick=\"testS3Connection()\">Test Connection</button>
                <button class=\"btn btn-secondary\" onclick=\"createS3Backup()\">Create Backup</button>
                <button class=\"btn btn-secondary\" onclick=\"listS3Backups()\">List Backups</button>
            </div>
            <div id=\"s3-results\" style=\"margin-top: 15px;\"></div>
        </div>
        
        <div class=\"card\" style=\"border-left: 4px solid #8b5cf6;\">
            <h2>HTTP/HTTPS Backup</h2>
            <p>Download updates from any HTTP/HTTPS URL source.</p>
            <div id=\"http-status\" style=\"margin: 15px 0;\">
                <p class=\"loading\">Loading HTTP backup status...</p>
            </div>
    ">>.
