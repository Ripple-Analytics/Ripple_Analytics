%%%-------------------------------------------------------------------
%%% @doc History Handler Template - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(history_handler_template_part2).

-export([content/0]).

content() ->
    <<"
                    }
                    
                    // Bulk mode checkbox
                    if (bulkMode) {
                        const isBulkSelected = selectedForBulk.includes(analysis.id);
                        html += '<div style=\"margin-bottom: 10px;\">';
                        html += '<label style=\"cursor: pointer; display: flex; align-items: center; gap: 8px;\">';
                        html += '<input type=\"checkbox\" ' + (isBulkSelected ? 'checked' : '') + ' onchange=\"toggleSelectForBulk(\\'' + analysis.id + '\\')\" style=\"width: 18px; height: 18px;\">';
                        html += '<span style=\"font-weight: bold;\">' + (isBulkSelected ? 'Selected' : 'Select') + '</span>';
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
    ">>.
