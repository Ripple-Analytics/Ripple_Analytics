%%%-------------------------------------------------------------------
%%% @doc History Handler Template - Part 5
%%% @end
%%%-------------------------------------------------------------------
-module(history_handler_template_part5).

-export([content/0]).

content() ->
    <<"
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
            
            // Bulk operations
            let bulkMode = false;
            let selectedForBulk = [];
            
            function toggleBulkMode() {
                bulkMode = !bulkMode;
                selectedForBulk = [];
                document.getElementById('bulk-btn').textContent = bulkMode ? 'Exit Bulk Mode' : 'Bulk Actions';
                document.getElementById('bulk-actions-bar').style.display = bulkMode ? 'block' : 'none';
                updateBulkCount();
                filterAnalyses();
            }
            
            function updateBulkCount() {
                document.getElementById('bulk-count').textContent = selectedForBulk.length;
            }
            
            function toggleSelectForBulk(id) {
                const idx = selectedForBulk.indexOf(id);
                if (idx > -1) {
                    selectedForBulk.splice(idx, 1);
                } else {
                    selectedForBulk.push(id);
                }
                updateBulkCount();
                filterAnalyses();
            }
            
            function selectAllVisible() {
                selectedForBulk = allAnalysesData.map(a => a.id);
                updateBulkCount();
                filterAnalyses();
            }
            
            function deselectAll() {
                selectedForBulk = [];
                updateBulkCount();
                filterAnalyses();
            }
            
            async function bulkAddTag() {
                const tag = document.getElementById('bulk-tag-input').value.trim();
                if (!tag) {
                    alert('Please enter a tag');
                    return;
                }
                if (selectedForBulk.length === 0) {
                    alert('No analyses selected');
                    return;
                }
                
                let successCount = 0;
                for (const id of selectedForBulk) {
                    try {
                        await fetch('/api/storage/tags', {
                            method: 'POST',
                            headers: {'Content-Type': 'application/json'},
                            body: JSON.stringify({analysis_id: id, tag: tag})
                        });
                        successCount++;
                    } catch (e) {
                        console.error('Error adding tag to ' + id, e);
                    }
                }
                
                alert('Added tag \"' + tag + '\" to ' + successCount + ' analyses');
                document.getElementById('bulk-tag-input').value = '';
                await loadTags();
                await loadHistory();
            }
            
            async function bulkDelete() {
                if (selectedForBulk.length === 0) {
                    alert('No analyses selected');
                    return;
                }
                
                if (!confirm('Are you sure you want to delete ' + selectedForBulk.length + ' analyses? This cannot be undone.')) {
                    return;
                }
                
                let successCount = 0;
                for (const id of selectedForBulk) {
                    try {
                        await fetch('/api/storage/history/' + id, {method: 'DELETE'});
                        successCount++;
                    } catch (e) {
                        console.error('Error deleting ' + id, e);
                    }
                }
                
                alert('Deleted ' + successCount + ' analyses');
                selectedForBulk = [];
                updateBulkCount();
                await loadHistory();
            }
            
            async function exportAllHistory() {
                try {
                    const res = await fetch('/api/storage/history?limit=1000');
                    const data = await res.json();
                    const analyses = data.analyses || [];
                    
                    if (analyses.length === 0) {
                        alert('No analyses to export');
                        return;
                    }
                    
                    const exportData = {
    ">>.
