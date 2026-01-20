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
    
