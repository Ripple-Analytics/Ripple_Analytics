%%%-------------------------------------------------------------------
%%% @doc Settings Handler Template - Part 4
%%% @end
%%%-------------------------------------------------------------------
-module(settings_handler_template_part4).

-export([content/0]).

content() ->
    <<"
                const fontSize = localStorage.getItem('fontSize') || 'medium';
                
                document.getElementById('dark-mode-toggle').checked = isDark;
                document.getElementById('font-size-select').value = fontSize;
                
                applyTheme(isDark);
                applyFontSize(fontSize);
            }
            
            // Load status and config on page load
            loadAppearanceSettings();
            loadUpdateStatus();
            loadConfig();
            loadGdriveStatus();
            loadChaosMonkeyStatus();
            setInterval(loadUpdateStatus, 30000);
            setInterval(loadGdriveStatus, 60000);
            setInterval(loadChaosMonkeyStatus, 30000);
            
            // Health check function
            async function runHealthCheck() {
                document.getElementById('health-status').innerHTML = '<p class=\"loading\">Checking all services...</p>';
                
                const services = [
                    {name: 'API Gateway', url: 'http://localhost:8000/health'},
                    {name: 'Analysis Service', url: 'http://localhost:8001/health'},
                    {name: 'Harvester Service', url: 'http://localhost:8002/health'},
                    {name: 'Storage Service', url: 'http://localhost:8003/health'},
                    {name: 'Chaos Engineering', url: 'http://localhost:8005/health'},
                    {name: 'GDrive Backup', url: 'http://localhost:8006/health'},
                    {name: 'Chaos Monkey', url: 'http://localhost:8007/health'},
                    {name: 'S3/MinIO Backup', url: 'http://localhost:8008/health'},
                    {name: 'HTTP Backup', url: 'http://localhost:8009/health'},
                    {name: 'LAN Backup', url: 'http://localhost:8010/health'},
                    {name: 'Desktop UI', url: '/health'}
                ];
                
                let html = '<div style=\"display: grid; grid-template-columns: repeat(auto-fill, minmax(200px, 1fr)); gap: 10px;\">';
                
                for (const svc of services) {
                    try {
                        const res = await fetch(svc.url, {timeout: 5000});
                        const data = await res.json();
                        const isHealthy = res.ok && (data.status === 'healthy' || data.status === 'ok');
                        const statusClass = isHealthy ? 'status-healthy' : 'status-unhealthy';
                        const statusText = isHealthy ? 'Healthy' : 'Unhealthy';
                        html += '<div style=\"padding: 10px; border-radius: 4px; background: #f8f9fa;\">';
                        html += '<span class=\"' + statusClass + '\">●</span> <strong>' + svc.name + '</strong><br>';
                        html += '<small>' + statusText + '</small></div>';
                    } catch (e) {
                        html += '<div style=\"padding: 10px; border-radius: 4px; background: #f8f9fa;\">';
                        html += '<span class=\"status-unhealthy\">●</span> <strong>' + svc.name + '</strong><br>';
                        html += '<small>Unreachable</small></div>';
                    }
                }
                
                html += '</div>';
                document.getElementById('health-status').innerHTML = html;
            }
            
            // Google Drive Backup functions
            async function loadGdriveStatus() {
                try {
                    const res = await fetch('http://localhost:8006/api/backup/status');
                    const data = await res.json();
                    
                    let html = '<div style=\"display: grid; grid-template-columns: 1fr 1fr; gap: 10px;\">';
                    html += '<p><strong>Status:</strong> ' + (data.gdrive?.status || 'unknown') + '</p>';
                    html += '<p><strong>Configured:</strong> ' + (data.gdrive?.gdrive_configured ? 'Yes' : 'No') + '</p>';
                    html += '<p><strong>Last Sync:</strong> ' + (data.gdrive?.last_sync || 'never') + '</p>';
                    html += '<p><strong>Backup Count:</strong> ' + (data.backup_count || 0) + '</p>';
                    html += '</div>';
                    
                    document.getElementById('gdrive-status').innerHTML = html;
                } catch (e) {
                    document.getElementById('gdrive-status').innerHTML = 
                        '<p class=\"status-unknown\">GDrive Backup service not available</p>';
                }
            }
            
            async function triggerBackup() {
                document.getElementById('backup-list').innerHTML = '<p class=\"loading\">Creating backup...</p>';
                try {
                    const res = await fetch('http://localhost:8006/api/backup/create', {method: 'POST'});
                    const data = await res.json();
                    
                    if (data.success) {
                        document.getElementById('backup-list').innerHTML = 
                            '<div class=\"alert alert-success\">Backup triggered successfully!</div>';
                        loadGdriveStatus();
                    } else {
                        document.getElementById('backup-list').innerHTML = 
                            '<div class=\"alert alert-error\">Failed to create backup</div>';
                    }
                } catch (e) {
                    document.getElementById('backup-list').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function listBackups() {
                document.getElementById('backup-list').innerHTML = '<p class=\"loading\">Loading backups...</p>';
                try {
                    const res = await fetch('http://localhost:8006/api/backup/list');
                    const data = await res.json();
                    
                    if (data.success && data.backups && data.backups.length > 0) {
                        let html = '<table style=\"width: 100%; border-collapse: collapse;\">';
                        html += '<tr style=\"background: #f8f9fa;\"><th style=\"padding: 8px; text-align: left;\">Name</th>';
                        html += '<th style=\"padding: 8px; text-align: left;\">Size</th>';
                        html += '<th style=\"padding: 8px; text-align: left;\">Modified</th></tr>';
                        
                        for (const backup of data.backups) {
                            html += '<tr style=\"border-bottom: 1px solid #eee;\">';
                            html += '<td style=\"padding: 8px;\">' + backup.name + '</td>';
                            html += '<td style=\"padding: 8px;\">' + formatBytes(backup.size) + '</td>';
                            html += '<td style=\"padding: 8px;\">' + backup.modified + '</td>';
                            html += '</tr>';
                        }
                        html += '</table>';
                        document.getElementById('backup-list').innerHTML = html;
                    } else {
                        document.getElementById('backup-list').innerHTML = 
                            '<p>No backups found. Click \"Create Backup Now\" to create one.</p>';
                    }
                } catch (e) {
                    document.getElementById('backup-list').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            function formatBytes(bytes) {
                if (bytes === 0) return '0 Bytes';
                const k = 1024;
                const sizes = ['Bytes', 'KB', 'MB', 'GB'];
                const i = Math.floor(Math.log(bytes) / Math.log(k));
                return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + sizes[i];
            }
            
            // Chaos Monkey functions
            let chaosMonkeyEnabled = false;
            
            async function loadChaosMonkeyStatus() {
                try {
                    const res = await fetch('http://localhost:8007/api/chaos/status');
                    const data = await res.json();
                    
                    chaosMonkeyEnabled = data.engine?.enabled || false;
                    
                    let html = '<div style=\"display: grid; grid-template-columns: 1fr 1fr; gap: 10px;\">';
    ">>.
