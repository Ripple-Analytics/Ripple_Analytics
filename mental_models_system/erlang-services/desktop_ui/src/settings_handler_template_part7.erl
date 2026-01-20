%%%-------------------------------------------------------------------
%%% @doc Settings Handler Template - Part 7
%%% @end
%%%-------------------------------------------------------------------
-module(settings_handler_template_part7).

-export([content/0]).

content() ->
    <<"
                            '<div class=\"alert alert-success\">Backup created: ' + data.backup?.name + '</div>';
                    } else {
                        document.getElementById('s3-results').innerHTML = 
                            '<div class=\"alert alert-error\">' + (data.error || 'Backup failed') + '</div>';
                    }
                } catch (e) {
                    document.getElementById('s3-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function listS3Backups() {
                document.getElementById('s3-results').innerHTML = '<p class=\"loading\">Loading S3 backups...</p>';
                try {
                    const res = await fetch('http://localhost:8008/api/s3/backup/list');
                    const data = await res.json();
                    
                    if (data.success && data.backups?.length > 0) {
                        let html = '<table style=\"width: 100%; border-collapse: collapse;\">';
                        html += '<tr style=\"background: #f8f9fa;\"><th style=\"padding: 8px;\">Name</th><th style=\"padding: 8px;\">Size</th></tr>';
                        for (const b of data.backups) {
                            html += '<tr><td style=\"padding: 8px;\">' + b.name + '</td><td style=\"padding: 8px;\">' + formatBytes(b.size) + '</td></tr>';
                        }
                        html += '</table>';
                        document.getElementById('s3-results').innerHTML = html;
                    } else {
                        document.getElementById('s3-results').innerHTML = '<p>No backups found.</p>';
                    }
                } catch (e) {
                    document.getElementById('s3-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            // HTTP/HTTPS Backup functions
            async function loadHttpStatus() {
                try {
                    const res = await fetch('http://localhost:8009/api/http/status');
                    const data = await res.json();
                    
                    let html = '<div style=\"display: grid; grid-template-columns: 1fr 1fr; gap: 10px;\">';
                    html += '<p><strong>Sources:</strong> ' + (data.http?.source_count || 0) + '</p>';
                    html += '<p><strong>Primary URL:</strong> ' + (data.http?.primary_url || 'Not set') + '</p>';
                    html += '<p><strong>Last Download:</strong> ' + (data.http?.last_download || 'never') + '</p>';
                    html += '<p><strong>Downloads:</strong> ' + (data.http?.download_count || 0) + '</p>';
                    html += '</div>';
                    
                    document.getElementById('http-status').innerHTML = html;
                } catch (e) {
                    document.getElementById('http-status').innerHTML = 
                        '<p class=\"status-unknown\">HTTP Backup service not available</p>';
                }
            }
            
            async function testHttpSource() {
                document.getElementById('http-results').innerHTML = '<p class=\"loading\">Testing HTTP source...</p>';
                try {
                    const res = await fetch('http://localhost:8009/api/http/test', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({source: 'primary'})
                    });
                    const data = await res.json();
                    
                    if (data.success) {
                        document.getElementById('http-results').innerHTML = 
                            '<div class=\"alert alert-success\">Source reachable! Latency: ' + data.test?.latency_ms + 'ms</div>';
                    } else {
                        document.getElementById('http-results').innerHTML = 
                            '<div class=\"alert alert-error\">' + (data.error || 'Test failed') + '</div>';
                    }
                } catch (e) {
                    document.getElementById('http-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function downloadFromHttp() {
                document.getElementById('http-results').innerHTML = '<p class=\"loading\">Downloading from HTTP source...</p>';
                try {
                    const res = await fetch('http://localhost:8009/api/http/download', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({source: 'primary'})
                    });
                    const data = await res.json();
                    
                    if (data.success) {
                        document.getElementById('http-results').innerHTML = 
                            '<div class=\"alert alert-success\">Download complete!</div>';
                        loadHttpStatus();
                    } else {
                        document.getElementById('http-results').innerHTML = 
                            '<div class=\"alert alert-error\">' + (data.error || 'Download failed') + '</div>';
                    }
                } catch (e) {
                    document.getElementById('http-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function listHttpSources() {
                document.getElementById('http-results').innerHTML = '<p class=\"loading\">Loading HTTP sources...</p>';
                try {
                    const res = await fetch('http://localhost:8009/api/http/sources');
                    const data = await res.json();
                    
                    if (data.success && data.sources?.length > 0) {
                        let html = '<table style=\"width: 100%; border-collapse: collapse;\">';
                        html += '<tr style=\"background: #f8f9fa;\"><th style=\"padding: 8px;\">Name</th><th style=\"padding: 8px;\">URL</th></tr>';
                        for (const s of data.sources) {
                            html += '<tr><td style=\"padding: 8px;\">' + s.name + '</td><td style=\"padding: 8px;\">' + s.url + '</td></tr>';
                        }
                        html += '</table>';
                        document.getElementById('http-results').innerHTML = html;
                    } else {
                        document.getElementById('http-results').innerHTML = '<p>No sources configured.</p>';
                    }
                } catch (e) {
                    document.getElementById('http-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            // LAN/Local Network Backup functions
            async function loadLanStatus() {
                try {
                    const res = await fetch('http://localhost:8010/api/lan/status');
                    const data = await res.json();
                    
                    let html = '<div style=\"display: grid; grid-template-columns: 1fr 1fr; gap: 10px;\">';
                    html += '<p><strong>Peers:</strong> ' + (data.lan?.peer_count || 0) + '</p>';
                    html += '<p><strong>Discovery:</strong> ' + (data.lan?.discovery_enabled ? 'Enabled' : 'Disabled') + '</p>';
                    html += '<p><strong>Last Sync:</strong> ' + (data.lan?.last_sync || 'never') + '</p>';
                    html += '<p><strong>Syncs:</strong> ' + (data.lan?.sync_count || 0) + '</p>';
                    html += '</div>';
                    
                    document.getElementById('lan-status').innerHTML = html;
                } catch (e) {
                    document.getElementById('lan-status').innerHTML = 
                        '<p class=\"status-unknown\">LAN Backup service not available</p>';
                }
            }
            
            async function discoverPeers() {
                document.getElementById('lan-results').innerHTML = '<p class=\"loading\">Discovering peers...</p>';
                try {
                    const res = await fetch('http://localhost:8010/api/lan/discover', {method: 'POST'});
                    const data = await res.json();
                    
    ">>.
