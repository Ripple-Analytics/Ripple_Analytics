%%%-------------------------------------------------------------------
%%% @doc History Handler - Analysis history page
%%%-------------------------------------------------------------------
-module(history_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Content = [
        <<"<div class=\"card\">
            <h2>Analysis History</h2>
            <p>View your past analyses and track patterns over time.</p>
            <br>
            <div style=\"display: flex; gap: 10px; margin-bottom: 20px; flex-wrap: wrap;\">
                <select id=\"type-filter\" onchange=\"loadHistory()\" style=\"padding: 12px; border-radius: 6px; border: 1px solid #e0e0e0;\">
                    <option value=\"\">All Types</option>
                    <option value=\"models\">Model Analysis</option>
                    <option value=\"biases\">Bias Detection</option>
                    <option value=\"full\">Full Analysis</option>
                </select>
                <input type=\"number\" id=\"limit\" value=\"20\" min=\"1\" max=\"100\" style=\"width: 80px; padding: 12px; border-radius: 6px; border: 1px solid #e0e0e0;\">
                <button class=\"btn\" onclick=\"loadHistory()\">Refresh</button>
                <button class=\"btn btn-secondary\" onclick=\"loadStats()\">View Stats</button>
            </div>
        </div>
        
        <div id=\"stats-panel\" style=\"display: none;\"></div>
        
        <div id=\"history-list\">
            <div class=\"loading\">Loading history...</div>
        </div>
        
        <script>
            async function loadHistory() {
                const type = document.getElementById('type-filter').value;
                const limit = document.getElementById('limit').value || 20;
                
                document.getElementById('history-list').innerHTML = '<div class=\"loading\">Loading...</div>';
                
                try {
                    let url = '/api/storage/history?limit=' + limit;
                    if (type) url += '&type=' + type;
                    
                    const res = await fetch(url);
                    const data = await res.json();
                    
                    if (data.analyses && data.analyses.length > 0) {
                        renderHistory(data.analyses);
                    } else {
                        document.getElementById('history-list').innerHTML = 
                            '<div class=\"alert alert-info\">No analysis history found. Run some analyses to see them here.</div>';
                    }
                } catch (e) {
                    document.getElementById('history-list').innerHTML = 
                        '<div class=\"alert alert-error\">Error loading history: ' + e.message + '</div>';
                }
            }
            
            function renderHistory(analyses) {
                let html = '<div class=\"grid\">';
                for (const analysis of analyses) {
                    const date = new Date(analysis.timestamp * 1000).toLocaleString();
                    const typeLabel = analysis.type === 'models' ? 'Model Analysis' :
                                     analysis.type === 'biases' ? 'Bias Detection' : 'Full Analysis';
                    const typeClass = analysis.type === 'models' ? 'status-healthy' :
                                     analysis.type === 'biases' ? 'status-unknown' : 'status-healthy';
                    
                    html += '<div class=\"model-card\">';
                    html += '<div style=\"display: flex; justify-content: space-between; align-items: start;\">';
                    html += '<span class=\"' + typeClass + '\">' + typeLabel + '</span>';
                    html += '<small style=\"color: #666;\">' + date + '</small>';
                    html += '</div>';
                    html += '<p style=\"margin: 10px 0; font-style: italic; color: #555;\">' + 
                            (analysis.input_text || 'No text preview') + '</p>';
                    html += '<div style=\"display: flex; gap: 15px; margin-top: 10px;\">';
                    if (analysis.model_count > 0) {
                        html += '<span><strong>' + analysis.model_count + '</strong> models</span>';
                    }
                    if (analysis.bias_count > 0) {
                        html += '<span><strong>' + analysis.bias_count + '</strong> biases</span>';
                    }
                    html += '</div>';
                    html += '<div style=\"margin-top: 10px;\">';
                    html += '<button class=\"btn btn-secondary\" style=\"padding: 6px 12px; font-size: 12px;\" onclick=\"viewDetails(\\'' + analysis.id + '\\')\">View Details</button>';
                    html += '<button class=\"btn\" style=\"padding: 6px 12px; font-size: 12px; margin-left: 5px; background: #dc3545;\" onclick=\"deleteAnalysis(\\'' + analysis.id + '\\')\">Delete</button>';
                    html += '</div>';
                    html += '</div>';
                }
                html += '</div>';
                html += '<p style=\"margin-top: 20px; color: #666;\">Showing ' + analyses.length + ' analyses</p>';
                document.getElementById('history-list').innerHTML = html;
            }
            
            async function loadStats() {
                try {
                    const res = await fetch('/api/storage/history/stats');
                    const data = await res.json();
                    
                    if (data.stats) {
                        const stats = data.stats;
                        let html = '<div class=\"card\" style=\"margin-bottom: 20px;\">';
                        html += '<h3>Analysis Statistics</h3>';
                        html += '<div class=\"grid\" style=\"grid-template-columns: repeat(4, 1fr);\">';
                        html += '<div class=\"stat-card\"><div class=\"stat-value\">' + (stats.total || 0) + '</div><div class=\"stat-label\">Total Analyses</div></div>';
                        html += '<div class=\"stat-card\"><div class=\"stat-value\">' + (stats.total_models_detected || 0) + '</div><div class=\"stat-label\">Models Detected</div></div>';
                        html += '<div class=\"stat-card\"><div class=\"stat-value\">' + (stats.total_biases_detected || 0) + '</div><div class=\"stat-label\">Biases Detected</div></div>';
                        
                        const byType = stats.by_type || {};
                        const typeCount = Object.keys(byType).length;
                        html += '<div class=\"stat-card\"><div class=\"stat-value\">' + typeCount + '</div><div class=\"stat-label\">Analysis Types</div></div>';
                        html += '</div>';
                        
                        if (Object.keys(byType).length > 0) {
                            html += '<h4 style=\"margin-top: 20px;\">By Type</h4>';
                            html += '<ul>';
                            for (const [type, count] of Object.entries(byType)) {
                                html += '<li>' + type + ': ' + count + ' analyses</li>';
                            }
                            html += '</ul>';
                        }
                        
                        html += '<button class=\"btn btn-secondary\" onclick=\"document.getElementById(\\'stats-panel\\').style.display=\\'none\\'\">Hide Stats</button>';
                        html += '</div>';
                        document.getElementById('stats-panel').innerHTML = html;
                        document.getElementById('stats-panel').style.display = 'block';
                    }
                } catch (e) {
                    alert('Error loading stats: ' + e.message);
                }
            }
            
            async function viewDetails(id) {
                try {
                    const res = await fetch('/api/storage/history/' + id);
                    const data = await res.json();
                    
                    if (data.analysis) {
                        const a = data.analysis;
                        let details = 'Analysis ID: ' + a.id + '\\n';
                        details += 'Type: ' + a.type + '\\n';
                        details += 'Date: ' + new Date(a.timestamp * 1000).toLocaleString() + '\\n\\n';
                        details += 'Input Text:\\n' + (a.input_text || 'N/A') + '\\n\\n';
                        
                        if (a.models && a.models.length > 0) {
                            details += 'Models Detected:\\n';
                            for (const m of a.models) {
                                details += '- ' + (m.name || m) + '\\n';
                            }
                        }
                        
                        if (a.biases && a.biases.length > 0) {
                            details += '\\nBiases Detected:\\n';
                            for (const b of a.biases) {
                                details += '- ' + (b.bias || b) + '\\n';
                            }
                        }
                        
                        alert(details);
                    }
                } catch (e) {
                    alert('Error loading details: ' + e.message);
                }
            }
            
            async function deleteAnalysis(id) {
                if (!confirm('Are you sure you want to delete this analysis?')) return;
                
                try {
                    await fetch('/api/storage/history/' + id, {method: 'DELETE'});
                    loadHistory();
                } catch (e) {
                    alert('Error deleting: ' + e.message);
                }
            }
            
            // Load history on page load
            loadHistory();
        </script>">>
    ],
    Html = html_templates:base_layout(<<"History">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.
