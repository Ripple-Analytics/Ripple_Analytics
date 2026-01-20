%%%-------------------------------------------------------------------
%%% @doc AI Improver Handler Template - Part 1
%%% @end
%%%-------------------------------------------------------------------
-module(ai_improver_handler_template_part1).

-export([content/0]).

content() ->
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
    ">>.
