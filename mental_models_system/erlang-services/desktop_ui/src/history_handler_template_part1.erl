%%%-------------------------------------------------------------------
%%% @doc History Handler Template - Part 1
%%% @end
%%%-------------------------------------------------------------------
-module(history_handler_template_part1).

-export([content/0]).

content() ->
    <<"
<div class=\"card\">
            <h2>Analysis History</h2>
            <p>View your past analyses and track patterns over time.</p>
            <br>
            <div style=\"display: flex; gap: 10px; margin-bottom: 15px; flex-wrap: wrap;\">
                <input type=\"text\" id=\"search-input\" placeholder=\"Search analyses...\" onkeyup=\"filterAnalyses()\" style=\"flex: 1; min-width: 200px; padding: 12px; border-radius: 6px; border: 1px solid #e0e0e0;\">
                <select id=\"tag-filter\" onchange=\"filterAnalyses()\" style=\"padding: 12px; border-radius: 6px; border: 1px solid #e0e0e0;\">
                    <option value=\"\">All Tags</option>
                </select>
            </div>
            <div style=\"display: flex; gap: 10px; margin-bottom: 20px; flex-wrap: wrap;\">
                <select id=\"type-filter\" onchange=\"loadHistory()\" style=\"padding: 12px; border-radius: 6px; border: 1px solid #e0e0e0;\">
                    <option value=\"\">All Types</option>
                    <option value=\"models\">Model Analysis</option>
                    <option value=\"biases\">Bias Detection</option>
                    <option value=\"full\">Full Analysis</option>
                </select>
                <input type=\"number\" id=\"limit\" value=\"50\" min=\"1\" max=\"200\" style=\"width: 80px; padding: 12px; border-radius: 6px; border: 1px solid #e0e0e0;\">
                <button class=\"btn\" onclick=\"loadHistory()\">Refresh</button>
                <button class=\"btn btn-secondary\" onclick=\"loadStats()\">View Stats</button>
                <button class=\"btn btn-secondary\" onclick=\"toggleCompareMode()\" id=\"compare-btn\">Compare Mode</button>
                <button class=\"btn\" onclick=\"compareSelected()\" id=\"compare-action-btn\" style=\"display: none;\">Compare Selected (<span id=\"compare-count\">0</span>)</button>
                <button class=\"btn btn-secondary\" onclick=\"toggleBulkMode()\" id=\"bulk-btn\">Bulk Actions</button>
                <button class=\"btn\" onclick=\"exportAllHistory()\" style=\"background: #28a745;\">Export All</button>
                <button class=\"btn btn-secondary\" onclick=\"showImportDialog()\">Import</button>
                <button class=\"btn\" onclick=\"clearAllHistory()\" style=\"background: #dc3545;\">Clear All</button>
            </div>
            
            <div id=\"bulk-actions-bar\" style=\"display: none; background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px; border: 1px solid #e0e0e0;\">
                <div style=\"display: flex; gap: 10px; align-items: center; flex-wrap: wrap;\">
                    <span><strong id=\"bulk-count\">0</strong> selected</span>
                    <button class=\"btn btn-secondary\" onclick=\"selectAllVisible()\">Select All</button>
                    <button class=\"btn btn-secondary\" onclick=\"deselectAll()\">Deselect All</button>
                    <input type=\"text\" id=\"bulk-tag-input\" placeholder=\"Add tag to selected...\" style=\"padding: 8px; border-radius: 4px; border: 1px solid #e0e0e0; width: 150px;\">
                    <button class=\"btn\" onclick=\"bulkAddTag()\">Add Tag</button>
                    <button class=\"btn\" onclick=\"bulkDelete()\" style=\"background: #dc3545;\">Delete Selected</button>
                </div>
            </div>
        </div>
        
        <div id=\"stats-panel\" style=\"display: none;\"></div>
        
        <div id=\"compare-panel\" style=\"display: none;\"></div>
        
        <div id=\"history-list\">
            <div class=\"loading\">Loading history...</div>
        </div>
        
        <script>
            let allTags = [];
            let analysisTagsMap = {};
            
            async function loadTags() {
                try {
                    const res = await fetch('/api/storage/tags');
                    const data = await res.json();
                    allTags = data.tags || [];
                    
                    // Populate tag filter dropdown
                    const select = document.getElementById('tag-filter');
                    select.innerHTML = '<option value=\"\">All Tags</option>';
                    for (const tag of allTags) {
                        const opt = document.createElement('option');
                        opt.value = tag.name;
                        opt.textContent = tag.name + ' (' + tag.count + ')';
                        select.appendChild(opt);
                    }
                } catch (e) {
                    console.error('Error loading tags:', e);
                }
            }
            
            async function loadHistory() {
                const type = document.getElementById('type-filter').value;
                const limit = document.getElementById('limit').value || 50;
                
                document.getElementById('history-list').innerHTML = '<div class=\"loading\">Loading...</div>';
                
                try {
                    let url = '/api/storage/history?limit=' + limit;
                    if (type) url += '&type=' + type;
                    
                    const res = await fetch(url);
                    const data = await res.json();
                    
                    if (data.analyses && data.analyses.length > 0) {
                        // Load tags for each analysis
                        for (const analysis of data.analyses) {
                            try {
                                const tagsRes = await fetch('/api/storage/tags?analysis_id=' + analysis.id);
                                const tagsData = await tagsRes.json();
                                analysisTagsMap[analysis.id] = tagsData.tags || [];
                            } catch (e) {
                                analysisTagsMap[analysis.id] = [];
                            }
                        }
                        allAnalysesData = data.analyses;
                        filterAnalyses();
                    } else {
                        document.getElementById('history-list').innerHTML = 
                            '<div class=\"alert alert-info\">No analysis history found. Run some analyses to see them here.</div>';
                    }
                } catch (e) {
                    document.getElementById('history-list').innerHTML = 
                        '<div class=\"alert alert-error\">Error loading history: ' + e.message + '</div>';
                }
            }
            
            function filterAnalyses() {
                const searchTerm = document.getElementById('search-input').value.toLowerCase();
                const tagFilter = document.getElementById('tag-filter').value;
                
                let filtered = allAnalysesData.filter(analysis => {
                    // Search filter
                    const matchesSearch = !searchTerm || 
                        (analysis.input_text && analysis.input_text.toLowerCase().includes(searchTerm)) ||
                        (analysis.type && analysis.type.toLowerCase().includes(searchTerm));
                    
                    // Tag filter
                    const analysisTags = analysisTagsMap[analysis.id] || [];
                    const matchesTag = !tagFilter || analysisTags.includes(tagFilter);
                    
                    return matchesSearch && matchesTag;
                });
                
                renderHistory(filtered);
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
    ">>.
