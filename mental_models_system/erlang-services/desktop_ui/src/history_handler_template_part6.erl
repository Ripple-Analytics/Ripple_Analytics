%%%-------------------------------------------------------------------
%%% @doc History Handler Template - Part 6
%%% @end
%%%-------------------------------------------------------------------
-module(history_handler_template_part6).

-export([content/0]).

content() ->
    <<"
                        exported_at: new Date().toISOString(),
                        total_count: analyses.length,
                        analyses: analyses.map(a => ({
                            id: a.id,
                            type: a.type,
                            timestamp: a.timestamp,
                            date: new Date(a.timestamp * 1000).toISOString(),
                            model_count: a.model_count || 0,
                            bias_count: a.bias_count || 0,
                            models: (a.models || []).map(m => m.name || m),
                            biases: (a.biases || []).map(b => b.bias || b),
                            input_text: a.input_text || '',
                            tags: analysisTagsMap[a.id] || []
                        }))
                    };
                    
                    downloadFile('all_analyses_' + Date.now() + '.json', JSON.stringify(exportData, null, 2), 'application/json');
                } catch (e) {
                    alert('Export failed: ' + e.message);
                }
            }
            
            // Import history from JSON file
            function showImportDialog() {
                const input = document.createElement('input');
                input.type = 'file';
                input.accept = '.json';
                input.onchange = async (e) => {
                    const file = e.target.files[0];
                    if (!file) return;
                    
                    try {
                        const text = await file.text();
                        const data = JSON.parse(text);
                        
                        if (!data.analyses || !Array.isArray(data.analyses)) {
                            alert('Invalid file format. Expected JSON with \"analyses\" array.');
                            return;
                        }
                        
                        if (!confirm('Import ' + data.analyses.length + ' analyses? This will add them to your existing history.')) {
                            return;
                        }
                        
                        const res = await fetch('/api/storage/history/import', {
                            method: 'POST',
                            headers: {'Content-Type': 'application/json'},
                            body: JSON.stringify({analyses: data.analyses})
                        });
                        const result = await res.json();
                        
                        if (result.success) {
                            alert('Successfully imported ' + result.imported + ' of ' + result.total + ' analyses');
                            await loadHistory();
                        } else {
                            alert('Import failed: ' + (result.error || 'Unknown error'));
                        }
                    } catch (err) {
                        alert('Error reading file: ' + err.message);
                    }
                };
                input.click();
            }
            
            // Clear all history
            async function clearAllHistory() {
                if (!confirm('Are you sure you want to delete ALL analysis history? This cannot be undone!')) {
                    return;
                }
                
                if (!confirm('This will permanently delete all ' + allAnalysesData.length + ' analyses. Are you REALLY sure?')) {
                    return;
                }
                
                try {
                    const res = await fetch('/api/storage/history', {method: 'DELETE'});
                    const result = await res.json();
                    
                    if (result.success) {
                        alert('All history cleared successfully');
                        await loadHistory();
                    } else {
                        alert('Failed to clear history: ' + (result.error || 'Unknown error'));
                    }
                } catch (e) {
                    alert('Error clearing history: ' + e.message);
                }
            }
            
            // Load history on page load
            async function init() {
                await loadTags();
                await loadHistory();
            }
            init();
        </script>
    ">>.
