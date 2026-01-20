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
                <button class=\"btn btn-secondary\" onclick=\"toggleCompareMode()\" id=\"compare-btn\">Compare Mode</button>
                <button class=\"btn\" onclick=\"compareSelected()\" id=\"compare-action-btn\" style=\"display: none;\">Compare Selected (<span id=\"compare-count\">0</span>)</button>
            </div>
        </div>
        
        <div id=\"stats-panel\" style=\"display: none;\"></div>
        
        <div id=\"compare-panel\" style=\"display: none;\"></div>
        
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
                allAnalysesData = analyses;
                let html = '<div class=\"grid\">';
                for (const analysis of analyses) {
                    const date = new Date(analysis.timestamp * 1000).toLocaleString();
                    const typeLabel = analysis.type === 'models' ? 'Model Analysis' :
                                     analysis.type === 'biases' ? 'Bias Detection' : 'Full Analysis';
                    const typeClass = analysis.type === 'models' ? 'status-healthy' :
                                     analysis.type === 'biases' ? 'status-unknown' : 'status-healthy';
                    
                    const isSelected = selectedForCompare.includes(analysis.id);
                    const cardStyle = isSelected ? 'border: 2px solid #007bff; background: #f0f7ff;' : '';
                    
                    html += '<div class=\"model-card\" style=\"' + cardStyle + '\">';
                    
                    // Compare mode checkbox
                    if (compareMode) {
                        html += '<div style=\"margin-bottom: 10px;\">';
                        html += '<label style=\"cursor: pointer; display: flex; align-items: center; gap: 8px;\">';
                        html += '<input type=\"checkbox\" ' + (isSelected ? 'checked' : '') + ' onchange=\"toggleSelectForCompare(\\'' + analysis.id + '\\')\" style=\"width: 18px; height: 18px;\">';
                        html += '<span style=\"font-weight: bold;\">' + (isSelected ? 'Selected for comparison' : 'Select to compare') + '</span>';
                        html += '</label></div>';
                    }
                    
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
            
            // Compare mode state
            let compareMode = false;
            let selectedForCompare = [];
            let allAnalysesData = [];
            
            function toggleCompareMode() {
                compareMode = !compareMode;
                const btn = document.getElementById('compare-btn');
                const actionBtn = document.getElementById('compare-action-btn');
                
                if (compareMode) {
                    btn.textContent = 'Exit Compare Mode';
                    btn.style.background = '#dc3545';
                    actionBtn.style.display = 'inline-block';
                    selectedForCompare = [];
                    updateCompareCount();
                } else {
                    btn.textContent = 'Compare Mode';
                    btn.style.background = '';
                    actionBtn.style.display = 'none';
                    selectedForCompare = [];
                    document.getElementById('compare-panel').style.display = 'none';
                }
                loadHistory();
            }
            
            function toggleSelectForCompare(id) {
                const idx = selectedForCompare.indexOf(id);
                if (idx > -1) {
                    selectedForCompare.splice(idx, 1);
                } else if (selectedForCompare.length < 4) {
                    selectedForCompare.push(id);
                } else {
                    alert('Maximum 4 analyses can be compared at once');
                    return;
                }
                updateCompareCount();
                loadHistory();
            }
            
            function updateCompareCount() {
                document.getElementById('compare-count').textContent = selectedForCompare.length;
            }
            
            async function compareSelected() {
                if (selectedForCompare.length < 2) {
                    alert('Select at least 2 analyses to compare');
                    return;
                }
                
                // Fetch full details for each selected analysis
                const analyses = [];
                for (const id of selectedForCompare) {
                    try {
                        const res = await fetch('/api/storage/history/' + id);
                        const data = await res.json();
                        if (data.analysis) analyses.push(data.analysis);
                    } catch (e) {
                        console.error('Error fetching analysis:', e);
                    }
                }
                
                if (analyses.length < 2) {
                    alert('Could not load analyses for comparison');
                    return;
                }
                
                renderComparison(analyses);
            }
            
            function renderComparison(analyses) {
                let html = '<div class=\"card\" style=\"margin-bottom: 20px;\">';
                html += '<div style=\"display: flex; justify-content: space-between; align-items: center;\">';
                html += '<h2>Analysis Comparison</h2>';
                html += '<button class=\"btn btn-secondary\" onclick=\"document.getElementById(\\'compare-panel\\').style.display=\\'none\\'\">Close</button>';
                html += '</div>';
                
                // Create comparison table
                html += '<div style=\"overflow-x: auto;\"><table style=\"width: 100%; border-collapse: collapse; margin-top: 15px;\">';
                
                // Header row with analysis dates
                html += '<tr style=\"background: #f8f9fa;\"><th style=\"padding: 10px; border: 1px solid #e0e0e0;\">Attribute</th>';
                for (const a of analyses) {
                    const date = new Date(a.timestamp * 1000).toLocaleDateString();
                    html += '<th style=\"padding: 10px; border: 1px solid #e0e0e0;\">' + date + '</th>';
                }
                html += '</tr>';
                
                // Type row
                html += '<tr><td style=\"padding: 10px; border: 1px solid #e0e0e0;\"><strong>Type</strong></td>';
                for (const a of analyses) {
                    html += '<td style=\"padding: 10px; border: 1px solid #e0e0e0;\">' + a.type + '</td>';
                }
                html += '</tr>';
                
                // Model count row
                html += '<tr><td style=\"padding: 10px; border: 1px solid #e0e0e0;\"><strong>Models Detected</strong></td>';
                for (const a of analyses) {
                    html += '<td style=\"padding: 10px; border: 1px solid #e0e0e0;\">' + (a.model_count || 0) + '</td>';
                }
                html += '</tr>';
                
                // Bias count row
                html += '<tr><td style=\"padding: 10px; border: 1px solid #e0e0e0;\"><strong>Biases Detected</strong></td>';
                for (const a of analyses) {
                    html += '<td style=\"padding: 10px; border: 1px solid #e0e0e0;\">' + (a.bias_count || 0) + '</td>';
                }
                html += '</tr>';
                
                // Models list row
                html += '<tr><td style=\"padding: 10px; border: 1px solid #e0e0e0; vertical-align: top;\"><strong>Models</strong></td>';
                for (const a of analyses) {
                    const models = (a.models || []).map(m => m.name || m).join(', ') || 'None';
                    html += '<td style=\"padding: 10px; border: 1px solid #e0e0e0; vertical-align: top; font-size: 12px;\">' + models + '</td>';
                }
                html += '</tr>';
                
                // Biases list row
                html += '<tr><td style=\"padding: 10px; border: 1px solid #e0e0e0; vertical-align: top;\"><strong>Biases</strong></td>';
                for (const a of analyses) {
                    const biases = (a.biases || []).map(b => b.bias || b).join(', ') || 'None';
                    html += '<td style=\"padding: 10px; border: 1px solid #e0e0e0; vertical-align: top; font-size: 12px;\">' + biases + '</td>';
                }
                html += '</tr>';
                
                // Input text row
                html += '<tr><td style=\"padding: 10px; border: 1px solid #e0e0e0; vertical-align: top;\"><strong>Input Preview</strong></td>';
                for (const a of analyses) {
                    html += '<td style=\"padding: 10px; border: 1px solid #e0e0e0; vertical-align: top; font-size: 11px; color: #666;\">' + (a.input_text || 'N/A') + '</td>';
                }
                html += '</tr>';
                
                html += '</table></div>';
                html += '</div>';
                
                document.getElementById('compare-panel').innerHTML = html;
                document.getElementById('compare-panel').style.display = 'block';
                document.getElementById('compare-panel').scrollIntoView({behavior: 'smooth'});
            }
            
            // Load history on page load
            loadHistory();
        </script>">>
    ],
    Html = html_templates:base_layout(<<"History">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.
