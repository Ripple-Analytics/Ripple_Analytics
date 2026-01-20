%%%-------------------------------------------------------------------
%%% @doc Folder Watcher UI Handler
%%% 
%%% Web interface for controlling and monitoring the folder watcher.
%%% @end
%%%-------------------------------------------------------------------
-module(watcher_handler).

-export([init/2]).

init(Req0, State) ->
    Html = render_page(),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html">>
    }, Html, Req0),
    {ok, Req, State}.

render_page() ->
    [html_templates:base_html(<<"Folder Watcher">>, <<"Watcher">>, content())].

content() ->
    <<"
<style>
    .watcher-controls {
        display: flex;
        gap: 15px;
        align-items: center;
        margin-bottom: 20px;
        flex-wrap: wrap;
    }
    .watcher-controls input {
        padding: 10px;
        border: 1px solid #ddd;
        border-radius: 6px;
        font-size: 14px;
    }
    .watcher-controls input[type='text'] {
        flex: 1;
        min-width: 250px;
    }
    .watcher-controls input[type='number'] {
        width: 120px;
    }
    .btn {
        padding: 10px 20px;
        border: none;
        border-radius: 6px;
        cursor: pointer;
        font-size: 14px;
        font-weight: 500;
        transition: all 0.2s;
    }
    .btn-start {
        background: #10b981;
        color: white;
    }
    .btn-start:hover {
        background: #059669;
    }
    .btn-stop {
        background: #ef4444;
        color: white;
    }
    .btn-stop:hover {
        background: #dc2626;
    }
    .btn-clear {
        background: #6b7280;
        color: white;
    }
    .btn-clear:hover {
        background: #4b5563;
    }
    .status-panel {
        background: #f8fafc;
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 20px;
    }
    .status-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
        gap: 15px;
    }
    .status-item {
        text-align: center;
    }
    .status-value {
        font-size: 24px;
        font-weight: bold;
        color: #1e293b;
    }
    .status-label {
        font-size: 12px;
        color: #64748b;
        text-transform: uppercase;
    }
    .status-watching {
        color: #10b981;
    }
    .status-stopped {
        color: #ef4444;
    }
    .results-list {
        max-height: 500px;
        overflow-y: auto;
    }
    .result-item {
        background: white;
        border: 1px solid #e2e8f0;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 10px;
    }
    .result-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 10px;
    }
    .result-file {
        font-weight: 600;
        color: #1e293b;
    }
    .result-time {
        font-size: 12px;
        color: #64748b;
    }
    .lollapalooza-badge {
        background: linear-gradient(135deg, #f59e0b, #ef4444);
        color: white;
        padding: 4px 12px;
        border-radius: 20px;
        font-size: 12px;
        font-weight: 600;
    }
    .models-preview {
        display: flex;
        flex-wrap: wrap;
        gap: 5px;
        margin-top: 10px;
    }
    .model-tag {
        background: #e0e7ff;
        color: #3730a3;
        padding: 4px 8px;
        border-radius: 4px;
        font-size: 12px;
    }
    .empty-state {
        text-align: center;
        padding: 40px;
        color: #64748b;
    }
    .refresh-indicator {
        font-size: 12px;
        color: #64748b;
        margin-left: 10px;
    }
</style>

<div class='card'>
    <h2>Folder Watcher Configuration</h2>
    <div class='watcher-controls'>
        <input type='text' id='folderPath' placeholder='Folder path to watch...' />
        <input type='number' id='interval' placeholder='Interval (sec)' value='30' min='5' max='3600' />
        <button class='btn btn-start' onclick='startWatcher()'>Start Watching</button>
        <button class='btn btn-stop' onclick='stopWatcher()'>Stop Watching</button>
        <button class='btn btn-clear' onclick='clearResults()'>Clear Results</button>
        <button class='btn' onclick='useHostPath()' style='background: #6366f1; color: white;'>Use Host Path</button>
    </div>
</div>

<div class='card status-panel'>
    <h2>Watcher Status <span class='refresh-indicator' id='refreshIndicator'></span></h2>
    <div class='status-grid'>
        <div class='status-item'>
            <div class='status-value' id='statusWatching'>-</div>
            <div class='status-label'>Status</div>
        </div>
        <div class='status-item'>
            <div class='status-value' id='statusFolder'>-</div>
            <div class='status-label'>Folder</div>
        </div>
        <div class='status-item'>
            <div class='status-value' id='statusInterval'>-</div>
            <div class='status-label'>Interval</div>
        </div>
        <div class='status-item'>
            <div class='status-value' id='statusAnalyzed'>0</div>
            <div class='status-label'>Files Analyzed</div>
        </div>
        <div class='status-item'>
            <div class='status-value' id='statusLollapalooza'>0</div>
            <div class='status-label'>Lollapalooza Effects</div>
        </div>
        <div class='status-item'>
            <div class='status-value' id='statusLastScan'>-</div>
            <div class='status-label'>Last Scan</div>
        </div>
    </div>
</div>

<div class='card'>
    <h2>Recent Analysis Results</h2>
    <div class='results-list' id='resultsList'>
        <div class='empty-state'>No results yet. Start the watcher to begin automatic analysis.</div>
    </div>
</div>

<script>
    let refreshTimer = null;
    
    function useHostPath() {
        fetch('/api/config/host-path')
            .then(r => r.json())
            .then(data => {
                if (data.host_path) {
                    document.getElementById('folderPath').value = data.host_path;
                }
            })
            .catch(() => {
                alert('Could not fetch host path');
            });
    }
    
    function startWatcher() {
        const folderPath = document.getElementById('folderPath').value;
        const interval = parseInt(document.getElementById('interval').value) * 1000;
        
        if (!folderPath) {
            alert('Please enter a folder path');
            return;
        }
        
        fetch('/api/analysis/watcher', {
            method: 'POST',
            headers: {'Content-Type': 'application/json'},
            body: JSON.stringify({
                action: 'start',
                folder_path: folderPath,
                interval: interval
            })
        })
        .then(r => r.json())
        .then(data => {
            if (data.success) {
                refreshStatus();
                startAutoRefresh();
            } else {
                alert('Failed to start watcher: ' + (data.error || 'Unknown error'));
            }
        })
        .catch(err => alert('Error: ' + err.message));
    }
    
    function stopWatcher() {
        fetch('/api/analysis/watcher', {
            method: 'POST',
            headers: {'Content-Type': 'application/json'},
            body: JSON.stringify({action: 'stop'})
        })
        .then(r => r.json())
        .then(data => {
            if (data.success) {
                refreshStatus();
                stopAutoRefresh();
            } else {
                alert('Failed to stop watcher: ' + (data.error || 'Unknown error'));
            }
        })
        .catch(err => alert('Error: ' + err.message));
    }
    
    function clearResults() {
        if (!confirm('Clear all results?')) return;
        
        fetch('/api/analysis/watcher', {
            method: 'POST',
            headers: {'Content-Type': 'application/json'},
            body: JSON.stringify({action: 'clear'})
        })
        .then(r => r.json())
        .then(data => {
            if (data.success) {
                refreshStatus();
            }
        })
        .catch(err => alert('Error: ' + err.message));
    }
    
    function refreshStatus() {
        document.getElementById('refreshIndicator').textContent = 'Refreshing...';
        
        fetch('/api/analysis/watcher')
            .then(r => r.json())
            .then(data => {
                updateStatusDisplay(data);
                document.getElementById('refreshIndicator').textContent = '';
            })
            .catch(err => {
                document.getElementById('refreshIndicator').textContent = 'Error';
            });
    }
    
    function updateStatusDisplay(data) {
        const watching = data.watching;
        document.getElementById('statusWatching').textContent = watching ? 'WATCHING' : 'STOPPED';
        document.getElementById('statusWatching').className = 'status-value ' + (watching ? 'status-watching' : 'status-stopped');
        
        const folder = data.folder_path || '-';
        document.getElementById('statusFolder').textContent = folder.length > 20 ? '...' + folder.slice(-20) : folder;
        document.getElementById('statusFolder').title = folder;
        
        document.getElementById('statusInterval').textContent = (data.interval_ms / 1000) + 's';
        document.getElementById('statusAnalyzed').textContent = data.files_analyzed || 0;
        document.getElementById('statusLollapalooza').textContent = data.lollapalooza_count || 0;
        document.getElementById('statusLastScan').textContent = data.last_scan || '-';
        
        if (data.folder_path) {
            document.getElementById('folderPath').value = data.folder_path;
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
