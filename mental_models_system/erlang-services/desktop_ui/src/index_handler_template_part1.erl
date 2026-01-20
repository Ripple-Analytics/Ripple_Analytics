%%%-------------------------------------------------------------------
%%% @doc Dashboard Handler Template - Part 1
%%% @end
%%%-------------------------------------------------------------------
-module(index_handler_template_part1).

-export([content/0]).

content() ->
    <<"
<script src=\"https://cdn.jsdelivr.net/npm/chart.js\"></script>
        <div class=\"stats-grid\">
            <div class=\"stat-card\">
                <div class=\"value\" id=\"models-count\">-</div>
                <div class=\"label\">Mental Models</div>
            </div>
            <div class=\"stat-card\">
                <div class=\"value\" id=\"categories-count\">-</div>
                <div class=\"label\">Categories</div>
            </div>
            <div class=\"stat-card\">
                <div class=\"value\" id=\"services-count\">-</div>
                <div class=\"label\">Services</div>
            </div>
            <div class=\"stat-card\">
                <div class=\"value status-healthy\" id=\"health-status\">-</div>
                <div class=\"label\">System Health</div>
            </div>
        </div>
        
        <div class=\"grid\">
            <div class=\"card\">
                <h2>Category Distribution</h2>
                <canvas id=\"categoryChart\" height=\"200\"></canvas>
            </div>
            <div class=\"card\">
                <h2>Analysis Activity</h2>
                <canvas id=\"activityChart\" height=\"200\"></canvas>
            </div>
        </div>
        
        <div class=\"grid\">
            <div class=\"card\">
                <h2>Quick Analysis</h2>
                <textarea id=\"quick-text\" placeholder=\"Paste text here for quick analysis...\" style=\"min-height: 80px;\"></textarea>
                <div style=\"display: flex; gap: 10px; margin-top: 10px;\">
                    <button class=\"btn\" onclick=\"quickAnalyze('full')\">Full Analysis</button>
                    <button class=\"btn btn-secondary\" onclick=\"quickAnalyze('models')\">Models Only</button>
                    <button class=\"btn btn-secondary\" onclick=\"quickAnalyze('biases')\">Biases Only</button>
                </div>
                <div id=\"quick-result\" style=\"margin-top: 15px; display: none;\"></div>
            </div>
            <div class=\"card\">
                <h2>Browse Models</h2>
                <p>Explore 174+ mental models across multiple categories.</p>
                <br>
                <a href=\"/models\" class=\"btn\">View Models</a>
            </div>
            <div class=\"card\">
                <h2>Data Harvester</h2>
                <p>Scrape URLs and process files for analysis.</p>
                <br>
                <a href=\"/harvester\" class=\"btn\">Open Harvester</a>
            </div>
            <div class=\"card\">
                <h2>Service Health</h2>
                <div id=\"service-health\">
                    <p class=\"loading\">Loading...</p>
                </div>
            </div>
        </div>
        
        <div class=\"grid\" style=\"margin-top: 20px;\">
            <div class=\"card\">
                <h2>Trending Models</h2>
                <p style=\"font-size: 13px; color: #666; margin-bottom: 15px;\">Most frequently detected in your analyses</p>
                <div id=\"trending-models\">
                    <p class=\"loading\">Loading...</p>
                </div>
            </div>
            <div class=\"card\">
                <h2>Top Biases Detected</h2>
                <p style=\"font-size: 13px; color: #666; margin-bottom: 15px;\">Common cognitive biases found in your content</p>
                <div id=\"trending-biases\">
                    <p class=\"loading\">Loading...</p>
                </div>
            </div>
        </div>
        
        <div class=\"card\" style=\"margin-top: 20px;\">
            <h2>Recent Analyses</h2>
            <div id=\"recent-analyses\">
                <p class=\"loading\">Loading recent activity...</p>
            </div>
        </div>
        
        <script>
            let categoryChart = null;
            let activityChart = null;
            
            // Quick analysis function
            async function quickAnalyze(type) {
                const text = document.getElementById('quick-text').value.trim();
                if (!text) {
                    alert('Please enter some text to analyze');
                    return;
                }
                
                const resultDiv = document.getElementById('quick-result');
                resultDiv.style.display = 'block';
                resultDiv.innerHTML = '<p class=\"loading\">Analyzing...</p>';
                
                try {
                    const res = await fetch('/api/analysis/analyze', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({text: text, type: type})
                    });
                    const data = await res.json();
                    
                    let html = '<div style=\"background: #e7f3ff; padding: 15px; border-radius: 8px;\">';
                    
                    if (type === 'full' || type === 'models') {
                        const models = data.models || [];
                        html += '<h4 style=\"margin-bottom: 10px;\">Mental Models (' + models.length + ')</h4>';
                        if (models.length > 0) {
                            html += '<div style=\"display: flex; flex-wrap: wrap; gap: 5px; margin-bottom: 10px;\">';
                            models.slice(0, 5).forEach(m => {
                                html += '<span style=\"background: #4361ee; color: white; padding: 3px 8px; border-radius: 4px; font-size: 12px;\">' + (m.name || m) + '</span>';
                            });
                            if (models.length > 5) html += '<span style=\"color: #666; font-size: 12px;\">+' + (models.length - 5) + ' more</span>';
                            html += '</div>';
                        } else {
                            html += '<p style=\"color: #666; font-size: 13px;\">No models detected</p>';
                        }
                    }
                    
                    if (type === 'full' || type === 'biases') {
                        const biases = data.biases || [];
                        html += '<h4 style=\"margin-bottom: 10px;\">Cognitive Biases (' + biases.length + ')</h4>';
                        if (biases.length > 0) {
                            html += '<div style=\"display: flex; flex-wrap: wrap; gap: 5px;\">';
                            biases.slice(0, 5).forEach(b => {
                                html += '<span style=\"background: #f72585; color: white; padding: 3px 8px; border-radius: 4px; font-size: 12px;\">' + (b.bias || b) + '</span>';
                            });
                            if (biases.length > 5) html += '<span style=\"color: #666; font-size: 12px;\">+' + (biases.length - 5) + ' more</span>';
                            html += '</div>';
                        } else {
                            html += '<p style=\"color: #666; font-size: 13px;\">No biases detected</p>';
                        }
                    }
                    
                    html += '<p style=\"margin-top: 15px;\"><a href=\"/analysis\" class=\"btn btn-secondary\" style=\"font-size: 12px;\">Full Analysis Page</a> <a href=\"/history\" class=\"btn btn-secondary\" style=\"font-size: 12px; margin-left: 5px;\">View History</a></p>';
                    html += '</div>';
                    
                    resultDiv.innerHTML = html;
                    
                    // Refresh dashboard to show new analysis in recent
                    setTimeout(loadDashboard, 1000);
                } catch (e) {
    ">>.
