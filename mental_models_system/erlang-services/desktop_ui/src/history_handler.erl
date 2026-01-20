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
                    
                    // Also fetch notes for this analysis
                    const notesRes = await fetch('/api/storage/notes?analysis_id=' + id);
                    const notesData = await notesRes.json();
                    const notes = notesData.notes || [];
                    
                    // Also fetch tags for this analysis
                    const tagsRes = await fetch('/api/storage/tags?analysis_id=' + id);
                    const tagsData = await tagsRes.json();
                    const tags = tagsData.tags || [];
                    
                    if (data.analysis) {
                        const a = data.analysis;
                        showDetailsModal(a, notes, tags);
                    }
                } catch (e) {
                    alert('Error loading details: ' + e.message);
                }
            }
            
            function showDetailsModal(a, notes, tags) {
                let html = '<div class=\"modal-overlay\" onclick=\"closeDetailsModal()\">';
                html += '<div class=\"modal-content\" onclick=\"event.stopPropagation()\" style=\"max-width: 700px; max-height: 80vh; overflow-y: auto;\">';
                html += '<button class=\"modal-close\" onclick=\"closeDetailsModal()\">&times;</button>';
                
                html += '<h2>Analysis Details</h2>';
                html += '<p><strong>ID:</strong> ' + a.id + '</p>';
                html += '<p><strong>Type:</strong> ' + a.type + '</p>';
                html += '<p><strong>Date:</strong> ' + new Date(a.timestamp * 1000).toLocaleString() + '</p>';
                
                // Tags section
                html += '<div style=\"margin-top: 15px;\" id=\"tags-section-' + a.id + '\">';
                html += '<strong>Tags:</strong> ';
                if (tags.length > 0) {
                    for (const tag of tags) {
                        html += '<span style=\"background: #e9ecef; padding: 3px 10px; border-radius: 12px; margin-right: 5px; font-size: 12px;\">' + tag + ' <button onclick=\"removeTag(\\'' + a.id + '\\', \\'' + tag + '\\')\" style=\"background: none; border: none; color: #dc3545; cursor: pointer; font-size: 12px; padding: 0 0 0 5px;\">&times;</button></span>';
                    }
                } else {
                    html += '<span style=\"color: #666; font-style: italic;\">No tags</span>';
                }
                html += '<div style=\"margin-top: 8px; display: flex; gap: 5px;\">';
                html += '<input type=\"text\" id=\"new-tag-' + a.id + '\" placeholder=\"Add tag...\" style=\"padding: 5px 10px; border-radius: 6px; border: 1px solid #e0e0e0; font-size: 12px; width: 120px;\">';
                html += '<button class=\"btn btn-secondary\" onclick=\"addTag(\\'' + a.id + '\\')\" style=\"padding: 5px 10px; font-size: 12px;\">Add</button>';
                html += '</div>';
                html += '</div>';
                
                html += '<div style=\"margin-top: 15px; padding: 15px; background: #f8f9fa; border-radius: 8px;\">';
                html += '<strong>Input Text:</strong><br><span style=\"font-style: italic; color: #666;\">' + (a.input_text || 'N/A') + '</span>';
                html += '</div>';
                
                if (a.models && a.models.length > 0) {
                    html += '<div style=\"margin-top: 15px;\"><strong>Models Detected (' + a.models.length + '):</strong><ul>';
                    for (const m of a.models) {
                        html += '<li>' + (m.name || m) + '</li>';
                    }
                    html += '</ul></div>';
                }
                
                if (a.biases && a.biases.length > 0) {
                    html += '<div style=\"margin-top: 15px;\"><strong>Biases Detected (' + a.biases.length + '):</strong><ul>';
                    for (const b of a.biases) {
                        html += '<li>' + (b.bias || b) + '</li>';
                    }
                    html += '</ul></div>';
                }
                
                // Notes section
                html += '<div style=\"margin-top: 20px; border-top: 1px solid #e0e0e0; padding-top: 15px;\">';
                html += '<h3>Notes</h3>';
                html += '<div id=\"notes-list-' + a.id + '\">';
                if (notes.length > 0) {
                    for (const note of notes) {
                        const noteDate = new Date(note.created_at * 1000).toLocaleString();
                        html += '<div style=\"padding: 10px; background: #fff3cd; border-radius: 6px; margin-bottom: 10px;\">';
                        html += '<div style=\"display: flex; justify-content: space-between; align-items: start;\">';
                        html += '<span style=\"font-size: 11px; color: #666;\">' + noteDate + '</span>';
                        html += '<button onclick=\"deleteNote(\\'' + note.id + '\\', \\'' + a.id + '\\')\" style=\"background: none; border: none; color: #dc3545; cursor: pointer; font-size: 14px;\">&times;</button>';
                        html += '</div>';
                        html += '<p style=\"margin: 5px 0 0 0;\">' + note.content + '</p>';
                        html += '</div>';
                    }
                } else {
                    html += '<p style=\"color: #666; font-style: italic;\">No notes yet.</p>';
                }
                html += '</div>';
                
                html += '<div style=\"margin-top: 10px;\">';
                html += '<textarea id=\"new-note-' + a.id + '\" placeholder=\"Add a note...\" style=\"width: 100%; padding: 10px; border-radius: 6px; border: 1px solid #e0e0e0; min-height: 60px;\"></textarea>';
                html += '<button class=\"btn\" onclick=\"addNote(\\'' + a.id + '\\')\" style=\"margin-top: 5px;\">Add Note</button>';
                html += '</div>';
                html += '</div>';
                
                html += '</div></div>';
                document.body.insertAdjacentHTML('beforeend', html);
            }
            
            function closeDetailsModal() {
                const modal = document.querySelector('.modal-overlay');
                if (modal) modal.remove();
            }
            
            async function addNote(analysisId) {
                const textarea = document.getElementById('new-note-' + analysisId);
                const content = textarea.value.trim();
                if (!content) {
                    alert('Please enter a note');
                    return;
                }
                
                try {
                    await fetch('/api/storage/notes', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({analysis_id: analysisId, content: content})
                    });
                    closeDetailsModal();
                    viewDetails(analysisId);
                } catch (e) {
                    alert('Error adding note: ' + e.message);
                }
            }
            
            async function deleteNote(noteId, analysisId) {
                if (!confirm('Delete this note?')) return;
                
                try {
                    await fetch('/api/storage/notes?id=' + noteId, {method: 'DELETE'});
                    closeDetailsModal();
                    viewDetails(analysisId);
                } catch (e) {
                    alert('Error deleting note: ' + e.message);
                }
            }
            
            async function addTag(analysisId) {
                const input = document.getElementById('new-tag-' + analysisId);
                const tag = input.value.trim().toLowerCase().replace(/[^a-z0-9-]/g, '-');
                if (!tag) {
                    alert('Please enter a tag');
                    return;
                }
                
                try {
                    await fetch('/api/storage/tags', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({analysis_id: analysisId, tag: tag})
                    });
                    closeDetailsModal();
                    viewDetails(analysisId);
                } catch (e) {
                    alert('Error adding tag: ' + e.message);
                }
            }
            
            async function removeTag(analysisId, tag) {
                try {
                    await fetch('/api/storage/tags?analysis_id=' + analysisId + '&tag=' + encodeURIComponent(tag), {method: 'DELETE'});
                    closeDetailsModal();
                    viewDetails(analysisId);
                } catch (e) {
                    alert('Error removing tag: ' + e.message);
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
            
            let currentComparisonData = [];
            
            function renderComparison(analyses) {
                currentComparisonData = analyses;
                let html = '<div class=\"card\" style=\"margin-bottom: 20px;\">';
                html += '<div style=\"display: flex; justify-content: space-between; align-items: center;\">';
                html += '<h2>Analysis Comparison</h2>';
                html += '<div style=\"display: flex; gap: 10px;\">';
                html += '<button class=\"btn\" onclick=\"exportComparisonCSV()\">Export CSV</button>';
                html += '<button class=\"btn btn-secondary\" onclick=\"exportComparisonJSON()\">Export JSON</button>';
                html += '<button class=\"btn btn-secondary\" onclick=\"document.getElementById(\\'compare-panel\\').style.display=\\'none\\'\">Close</button>';
                html += '</div>';
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
            
            function exportComparisonCSV() {
                if (currentComparisonData.length === 0) {
                    alert('No comparison data to export');
                    return;
                }
                
                let csv = 'Attribute';
                for (const a of currentComparisonData) {
                    csv += ',' + new Date(a.timestamp * 1000).toLocaleDateString();
                }
                csv += '\\n';
                
                csv += 'Type';
                for (const a of currentComparisonData) {
                    csv += ',' + a.type;
                }
                csv += '\\n';
                
                csv += 'Models Detected';
                for (const a of currentComparisonData) {
                    csv += ',' + (a.model_count || 0);
                }
                csv += '\\n';
                
                csv += 'Biases Detected';
                for (const a of currentComparisonData) {
                    csv += ',' + (a.bias_count || 0);
                }
                csv += '\\n';
                
                csv += 'Models';
                for (const a of currentComparisonData) {
                    const models = (a.models || []).map(m => m.name || m).join('; ') || 'None';
                    csv += ',\"' + models.replace(/\"/g, '\"\"') + '\"';
                }
                csv += '\\n';
                
                csv += 'Biases';
                for (const a of currentComparisonData) {
                    const biases = (a.biases || []).map(b => b.bias || b).join('; ') || 'None';
                    csv += ',\"' + biases.replace(/\"/g, '\"\"') + '\"';
                }
                csv += '\\n';
                
                csv += 'Input Text';
                for (const a of currentComparisonData) {
                    csv += ',\"' + (a.input_text || 'N/A').replace(/\"/g, '\"\"') + '\"';
                }
                csv += '\\n';
                
                downloadFile('comparison_' + Date.now() + '.csv', csv, 'text/csv');
            }
            
            function exportComparisonJSON() {
                if (currentComparisonData.length === 0) {
                    alert('No comparison data to export');
                    return;
                }
                
                const exportData = {
                    exported_at: new Date().toISOString(),
                    analyses: currentComparisonData.map(a => ({
                        id: a.id,
                        type: a.type,
                        timestamp: a.timestamp,
                        date: new Date(a.timestamp * 1000).toISOString(),
                        model_count: a.model_count || 0,
                        bias_count: a.bias_count || 0,
                        models: (a.models || []).map(m => m.name || m),
                        biases: (a.biases || []).map(b => b.bias || b),
                        input_text: a.input_text || ''
                    }))
                };
                
                downloadFile('comparison_' + Date.now() + '.json', JSON.stringify(exportData, null, 2), 'application/json');
            }
            
            function downloadFile(filename, content, mimeType) {
                const blob = new Blob([content], {type: mimeType});
                const url = URL.createObjectURL(blob);
                const a = document.createElement('a');
                a.href = url;
                a.download = filename;
                document.body.appendChild(a);
                a.click();
                document.body.removeChild(a);
                URL.revokeObjectURL(url);
            }
            
            // Load history on page load
            async function init() {
                await loadTags();
                await loadHistory();
            }
            init();
        </script>">>
    ],
    Html = html_templates:base_layout(<<"History">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.
