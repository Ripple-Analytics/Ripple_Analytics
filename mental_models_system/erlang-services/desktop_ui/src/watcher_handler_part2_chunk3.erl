%%%-------------------------------------------------------------------
%%% @doc watcher_handler_part2 Template Chunk 3
%%% @end
%%%-------------------------------------------------------------------
-module(watcher_handler_part2_chunk3).

-export([content/0]).

content() ->
    <<"
        }
        if (data.interval_ms) {
            document.getElementById('interval').value = data.interval_ms / 1000;
        }
        
        renderResults(data.recent_results || []);
        
        if (watching && !refreshTimer) {
            startAutoRefresh();
        } else if (!watching && refreshTimer) {
            stopAutoRefresh();
        }
    }
    
    function renderResults(results) {
        const container = document.getElementById('resultsList');
        
        if (!results || results.length === 0) {
            container.innerHTML = '<div class=\"empty-state\">No results yet. Start the watcher to begin automatic analysis.</div>';
            return;
        }
        
        let html = '';
        for (const result of results) {
            const fileName = result.file_name || result.file || 'Unknown';
            const analyzedAt = result.analyzed_at || '-';
            const isLollapalooza = result.lollapalooza_detected;
            const models = result.models || [];
            
            html += '<div class=\"result-item\">';
            html += '<div class=\"result-header\">';
            html += '<span class=\"result-file\">' + escapeHtml(fileName) + '</span>';
            html += '<span class=\"result-time\">' + analyzedAt + '</span>';
            html += '</div>';
            
            if (isLollapalooza) {
                html += '<span class=\"lollapalooza-badge\">LOLLAPALOOZA EFFECT</span>';
            }
            
            if (models.length > 0) {
                html += '<div class=\"models-preview\">';
                for (const model of models.slice(0, 5)) {
                    const name = model.name || model;
                    html += '<span class=\"model-tag\">' + escapeHtml(name) + '</span>';
                }
                if (models.length > 5) {
                    html += '<span class=\"model-tag\">+' + (models.length - 5) + ' more</span>';
                }
                html += '</div>';
            }
            
            html += '</div>';
        }
        
        container.innerHTML = html;
    }
    
    function escapeHtml(text) {
        const div = document.createElement('div');
        div.textContent = text;
        return div.innerHTML;
    }
    
    function startAutoRefresh() {
        if (refreshTimer) return;
        refreshTimer = setInterval(refreshStatus, 10000);
    }
    
    function stopAutoRefresh() {
        if (refreshTimer) {
            clearInterval(refreshTimer);
            refreshTimer = null;
        }
    }
    
    refreshStatus();
</script>

    ">>.
