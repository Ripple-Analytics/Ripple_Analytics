%%%-------------------------------------------------------------------
%%% @doc watcher_handler_part2 Template Chunk 2
%%% @end
%%%-------------------------------------------------------------------
-module(watcher_handler_part2_chunk2).

-export([content/0]).

content() ->
    <<"
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
    ">>.
