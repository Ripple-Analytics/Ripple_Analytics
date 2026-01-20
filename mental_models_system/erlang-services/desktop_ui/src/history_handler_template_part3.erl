%%%-------------------------------------------------------------------
%%% @doc History Handler Template - Part 3
%%% @end
%%%-------------------------------------------------------------------
-module(history_handler_template_part3).

-export([content/0]).

content() ->
    <<"
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
    ">>.
