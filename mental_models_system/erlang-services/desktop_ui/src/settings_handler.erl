%%%-------------------------------------------------------------------
%%% @doc Settings Handler - System settings and configuration
%%% JavaScript loaded from priv/static/js/settings.js
%%% @end
%%%-------------------------------------------------------------------
-module(settings_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Content = [appearance(), updates(), config(), system_info(), 
               health_check(), gdrive_backup(), chaos_monkey(),
               s3_backup(), http_backup(), lan_backup(), 
               scheduled_analysis(), chaos_legacy(), load_script()],
    Html = html_templates:base_layout(<<"Settings">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.

appearance() -> <<"
<div class=\"card\" style=\"border-left: 4px solid #6366f1;\">
  <h2>Appearance</h2>
  <div style=\"margin-top: 15px;\">
    <label style=\"display: flex; align-items: center; gap: 10px;\">
      <input type=\"checkbox\" id=\"dark-mode-toggle\" onchange=\"toggleDarkMode()\" style=\"width: 20px; height: 20px;\">
      <span><strong>Dark Mode</strong></span>
    </label>
  </div>
  <div style=\"margin-top: 15px;\">
    <label><strong>Font Size</strong></label>
    <select id=\"font-size-select\" onchange=\"changeFontSize()\" style=\"margin-left: 10px; padding: 8px;\">
      <option value=\"small\">Small</option><option value=\"medium\" selected>Medium</option><option value=\"large\">Large</option>
    </select>
  </div>
</div>">>.

updates() -> <<"
<div class=\"card\" style=\"border-left: 4px solid #4361ee;\">
  <h2>Software Updates</h2>
  <div id=\"update-status\"><p class=\"loading\">Loading...</p></div>
  <div style=\"display: flex; gap: 10px; margin-top: 15px;\">
    <button class=\"btn\" onclick=\"checkForUpdates()\">Check Updates</button>
    <button class=\"btn btn-secondary\" onclick=\"restartUpdater()\">Restart Updater</button>
  </div>
</div>">>.

config() -> <<"
<div class=\"card\">
  <h2>Update Configuration</h2>
  <div style=\"margin-bottom: 15px;\">
    <label><strong>GitHub Token</strong></label>
    <input type=\"password\" id=\"github-token\" placeholder=\"ghp_xxx\" style=\"margin-top: 5px;\">
  </div>
  <div style=\"margin-bottom: 15px;\">
    <label><strong>Check Interval (seconds)</strong></label>
    <input type=\"number\" id=\"check-interval\" value=\"300\" min=\"60\" max=\"3600\" style=\"margin-top: 5px; width: 150px;\">
  </div>
  <button class=\"btn\" onclick=\"saveConfig()\">Save</button>
  <span id=\"config-status\" style=\"margin-left: 10px;\"></span>
</div>">>.

system_info() -> <<"
<div class=\"card\">
  <h2>System Information</h2>
  <p><strong>Version:</strong> <span id=\"app-version\">1.3.0</span></p>
  <p><strong>Branch:</strong> <span id=\"git-branch\" style=\"font-family: monospace;\">loading...</span></p>
  <p><strong>Commit:</strong> <span id=\"git-commit\" style=\"font-family: monospace;\">loading...</span></p>
  <p><strong>Runtime:</strong> Erlang/OTP 26</p>
</div>">>.

health_check() -> <<"
<div class=\"card\" style=\"border-left: 4px solid #28a745;\">
  <h2>System Health</h2>
  <div id=\"health-status\" style=\"margin: 15px 0;\"><p>Click to check...</p></div>
  <button class=\"btn\" onclick=\"runHealthCheck()\">Run Health Check</button>
</div>">>.

gdrive_backup() -> <<"
<div class=\"card\" style=\"border-left: 4px solid #f59e0b;\">
  <h2>Google Drive Backup</h2>
  <div id=\"gdrive-status\" style=\"margin: 15px 0;\"><p class=\"loading\">Loading...</p></div>
  <div style=\"display: flex; gap: 10px; flex-wrap: wrap;\">
    <button class=\"btn\" onclick=\"triggerBackup()\">Create Backup</button>
    <button class=\"btn btn-secondary\" onclick=\"listBackups()\">List Backups</button>
  </div>
  <div id=\"backup-list\" style=\"margin-top: 15px;\"></div>
</div>">>.

chaos_monkey() -> <<"
<div class=\"card\" style=\"border-left: 4px solid #dc3545;\">
  <h2>Chaos Monkey</h2>
  <p style=\"color: #dc3545;\">Test system resilience. Use with caution!</p>
  <div id=\"chaos-monkey-status\" style=\"margin: 15px 0;\"><p class=\"loading\">Loading...</p></div>
  <div style=\"display: flex; gap: 10px; flex-wrap: wrap;\">
    <button class=\"btn\" id=\"chaos-toggle-btn\" onclick=\"toggleChaosMonkey()\">Enable</button>
    <button class=\"btn btn-secondary\" onclick=\"triggerRandomAttack()\">Random Attack</button>
    <button class=\"btn btn-secondary\" onclick=\"viewAttackHistory()\">History</button>
  </div>
  <div id=\"attack-types\" style=\"margin-top: 15px;\">
    <h4>Manual Attacks:</h4>
    <div style=\"display: flex; gap: 10px; flex-wrap: wrap; margin-top: 10px;\">
      <button class=\"btn btn-secondary\" onclick=\"triggerAttack('kill_container')\">Kill</button>
      <button class=\"btn btn-secondary\" onclick=\"triggerAttack('network_latency')\">Latency</button>
      <button class=\"btn btn-secondary\" onclick=\"triggerAttack('cpu_stress')\">CPU</button>
      <button class=\"btn btn-secondary\" onclick=\"triggerAttack('memory_pressure')\">Memory</button>
    </div>
  </div>
  <div id=\"chaos-monkey-results\" style=\"margin-top: 15px;\"></div>
</div>">>.

s3_backup() -> <<"
<div class=\"card\" style=\"border-left: 4px solid #6366f1;\">
  <h2>S3/MinIO Backup</h2>
  <div id=\"s3-status\" style=\"margin: 15px 0;\"><p class=\"loading\">Loading...</p></div>
  <div style=\"display: flex; gap: 10px; flex-wrap: wrap;\">
    <button class=\"btn\" onclick=\"testS3Connection()\">Test</button>
    <button class=\"btn btn-secondary\" onclick=\"createS3Backup()\">Backup</button>
    <button class=\"btn btn-secondary\" onclick=\"listS3Backups()\">List</button>
  </div>
  <div id=\"s3-results\" style=\"margin-top: 15px;\"></div>
</div>">>.

http_backup() -> <<"
<div class=\"card\" style=\"border-left: 4px solid #8b5cf6;\">
  <h2>HTTP Backup</h2>
  <div id=\"http-status\" style=\"margin: 15px 0;\"><p class=\"loading\">Loading...</p></div>
  <div style=\"display: flex; gap: 10px; flex-wrap: wrap;\">
    <button class=\"btn\" onclick=\"testHttpSource()\">Test</button>
    <button class=\"btn btn-secondary\" onclick=\"downloadFromHttp()\">Download</button>
  </div>
  <div id=\"http-results\" style=\"margin-top: 15px;\"></div>
</div>">>.

lan_backup() -> <<"
<div class=\"card\" style=\"border-left: 4px solid #ec4899;\">
  <h2>LAN Backup</h2>
  <div id=\"lan-status\" style=\"margin: 15px 0;\"><p class=\"loading\">Loading...</p></div>
  <div style=\"display: flex; gap: 10px; flex-wrap: wrap;\">
    <button class=\"btn\" onclick=\"discoverPeers()\">Discover</button>
    <button class=\"btn btn-secondary\" onclick=\"broadcastUpdate()\">Broadcast</button>
  </div>
  <div id=\"lan-results\" style=\"margin-top: 15px;\"></div>
</div>">>.

scheduled_analysis() -> <<"
<div class=\"card\" style=\"border-left: 4px solid #10b981;\">
  <h2>Scheduled Analysis</h2>
  <div id=\"scheduled-list\" style=\"margin: 15px 0;\"><p class=\"loading\">Loading...</p></div>
  <div style=\"background: #f8f9fa; padding: 15px; border-radius: 8px; margin-top: 15px;\">
    <h4>Add New Task</h4>
    <div style=\"display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin-top: 10px;\">
      <input type=\"text\" id=\"schedule-name\" placeholder=\"Name\" style=\"padding: 8px;\">
      <select id=\"schedule-type\" style=\"padding: 8px;\">
        <option value=\"url\">URL</option><option value=\"text\">Text</option>
      </select>
      <input type=\"text\" id=\"schedule-target\" placeholder=\"URL or text\" style=\"padding: 8px;\">
      <select id=\"schedule-interval\" style=\"padding: 8px;\">
        <option value=\"3600\">Hourly</option><option value=\"86400\" selected>Daily</option>
      </select>
    </div>
    <button class=\"btn\" onclick=\"addScheduledTask()\" style=\"margin-top: 15px;\">Add Task</button>
  </div>
</div>">>.

chaos_legacy() -> <<"
<div class=\"card\">
  <h2>Chaos Engineering</h2>
  <div style=\"display: flex; gap: 10px; flex-wrap: wrap;\">
    <button class=\"btn\" onclick=\"runLoadTest()\">Load Test</button>
    <button class=\"btn btn-secondary\" onclick=\"runCascadeTest()\">Cascade Test</button>
    <button class=\"btn btn-secondary\" onclick=\"runRecoveryTest()\">Recovery Test</button>
  </div>
</div>
<div id=\"chaos-results\"></div>">>.

load_script() -> <<"<script src=\"/static/js/settings.js\"></script>">>.
