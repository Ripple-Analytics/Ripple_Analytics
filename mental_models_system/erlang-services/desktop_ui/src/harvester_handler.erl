%%%-------------------------------------------------------------------
%%% @doc Harvester Handler - URL scraping and file processing
%%%-------------------------------------------------------------------
-module(harvester_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Content = [
        <<"<div class=\"grid\">
            <div class=\"card\">
                <h2>Scrape Single URL</h2>
                <p>Enter a URL to scrape and extract content.</p>
                <br>
                <input type=\"url\" id=\"scrape-url\" placeholder=\"https://example.com/article\">
                <button class=\"btn\" onclick=\"scrapeUrl()\" style=\"margin-top:10px;\">Scrape URL</button>
            </div>
            <div class=\"card\">
                <h2>Harvester Stats</h2>
                <div id=\"harvester-stats\">
                    <p class=\"loading\">Loading stats...</p>
                </div>
                <button class=\"btn btn-secondary\" onclick=\"loadStats()\" style=\"margin-top:10px;\">Refresh</button>
            </div>
        </div>
        
        <div class=\"card\" style=\"margin-top: 20px;\">
            <h2>Batch URL Scraping</h2>
            <p>Enter multiple URLs (one per line) to scrape in parallel. Maximum 50 URLs per batch.</p>
            <br>
            <textarea id=\"batch-urls\" placeholder=\"https://example.com/article1&#10;https://example.com/article2&#10;https://example.com/article3\" style=\"min-height: 120px;\"></textarea>
            <div style=\"display: flex; gap: 10px; margin-top: 10px;\">
                <button class=\"btn\" onclick=\"batchScrape()\">Scrape All URLs</button>
                <button class=\"btn btn-secondary\" onclick=\"clearBatchUrls()\">Clear</button>
                <span id=\"url-count\" style=\"align-self: center; color: #666;\">0 URLs</span>
            </div>
        </div>
        
        <div id=\"results\"></div>
        <script>
            async function scrapeUrl() {
                const url = document.getElementById('scrape-url').value;
                if (!url.trim()) {
                    alert('Please enter a URL');
                    return;
                }
                
                document.getElementById('results').innerHTML = '<div class=\"loading\">Scraping URL...</div>';
                
                try {
                    const res = await fetch('/api/harvester/scrape', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({url: url})
                    });
                    const data = await res.json();
                    
                    let html = '<div class=\"card\"><h2>Scrape Results</h2>';
                    if (data.success) {
                        html += '<p><strong>URL:</strong> ' + data.url + '</p>';
                        html += '<p><strong>Content Type:</strong> ' + (data.content_type || 'text/html') + '</p>';
                        html += '<p><strong>Size:</strong> ' + (data.size || 0) + ' bytes</p>';
                        if (data.content) {
                            const preview = data.content.substring(0, 500);
                            html += '<p style=\"margin-top:15px;\"><strong>Content Preview:</strong></p>';
                            html += '<pre style=\"background:#f5f5f5;padding:10px;border-radius:4px;overflow:auto;max-height:200px;font-size:12px;\">' + 
                                    preview.replace(/</g, '&lt;').replace(/>/g, '&gt;') + '...</pre>';
                        }
                    } else {
                        html += '<div class=\"alert alert-error\">Scraping failed: ' + (data.reason || 'Unknown error') + '</div>';
                    }
                    html += '</div>';
                    document.getElementById('results').innerHTML = html;
                    loadStats();
                } catch (e) {
                    document.getElementById('results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function loadStats() {
                try {
                    const res = await fetch('/api/harvester/stats');
                    const data = await res.json();
                    
                    let html = '';
                    html += '<p><strong>URLs Scraped:</strong> ' + (data.urls_scraped || 0) + '</p>';
                    html += '<p><strong>Files Processed:</strong> ' + (data.files_processed || 0) + '</p>';
                    html += '<p><strong>Bytes Downloaded:</strong> ' + formatBytes(data.bytes_downloaded || 0) + '</p>';
                    html += '<p><strong>Batch Scrapes:</strong> ' + (data.batch_scrapes || 0) + '</p>';
                    html += '<p><strong>Batch URLs:</strong> ' + (data.batch_urls_successful || 0) + '/' + (data.batch_urls_total || 0) + ' successful</p>';
                    html += '<p><strong>Errors:</strong> ' + (data.errors || 0) + '</p>';
                    html += '<p><strong>Uptime:</strong> ' + formatUptime(data.uptime_seconds || 0) + '</p>';
                    document.getElementById('harvester-stats').innerHTML = html;
                } catch (e) {
                    document.getElementById('harvester-stats').innerHTML = 
                        '<p class=\"status-unhealthy\">Unable to load stats</p>';
                }
            }
            
            function formatBytes(bytes) {
                if (bytes === 0) return '0 B';
                const k = 1024;
                const sizes = ['B', 'KB', 'MB', 'GB'];
                const i = Math.floor(Math.log(bytes) / Math.log(k));
                return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + sizes[i];
            }
            
            function formatUptime(seconds) {
                const h = Math.floor(seconds / 3600);
                const m = Math.floor((seconds % 3600) / 60);
                const s = seconds % 60;
                return h + 'h ' + m + 'm ' + s + 's';
            }
            
            // Update URL count when textarea changes
            document.getElementById('batch-urls').addEventListener('input', function() {
                const urls = this.value.split('\\n').filter(u => u.trim());
                document.getElementById('url-count').textContent = urls.length + ' URLs';
            });
            
            function clearBatchUrls() {
                document.getElementById('batch-urls').value = '';
                document.getElementById('url-count').textContent = '0 URLs';
            }
            
            async function batchScrape() {
                const urlText = document.getElementById('batch-urls').value;
                const urls = urlText.split('\\n').map(u => u.trim()).filter(u => u);
                
                if (urls.length === 0) {
                    alert('Please enter at least one URL');
                    return;
                }
                
                if (urls.length > 50) {
                    alert('Maximum 50 URLs per batch. You have ' + urls.length + ' URLs.');
                    return;
                }
                
                document.getElementById('results').innerHTML = '<div class=\"loading\">Scraping ' + urls.length + ' URLs in parallel...</div>';
                
                try {
                    const res = await fetch('/api/harvester/batch-scrape', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({urls: urls})
                    });
                    const data = await res.json();
                    
                    let html = '<div class=\"card\"><h2>Batch Scrape Results</h2>';
                    html += '<div class=\"stats-grid\" style=\"grid-template-columns: repeat(3, 1fr); margin-bottom: 20px;\">';
                    html += '<div class=\"stat-card\"><div class=\"value\">' + data.total + '</div><div class=\"label\">Total URLs</div></div>';
                    html += '<div class=\"stat-card\"><div class=\"value\" style=\"color: #28a745;\">' + data.successful + '</div><div class=\"label\">Successful</div></div>';
                    html += '<div class=\"stat-card\"><div class=\"value\" style=\"color: #dc3545;\">' + data.failed + '</div><div class=\"label\">Failed</div></div>';
                    html += '</div>';
                    
                    if (data.results && data.results.length > 0) {
                        html += '<h3>Results</h3>';
                        for (const result of data.results) {
                            const statusClass = result.success ? 'status-healthy' : 'status-unhealthy';
                            html += '<div class=\"model-card\">';
                            html += '<div style=\"display: flex; justify-content: space-between;\">';
                            html += '<strong style=\"word-break: break-all;\">' + result.url + '</strong>';
                            html += '<span class=\"' + statusClass + '\">' + (result.success ? 'Success' : 'Failed') + '</span>';
                            html += '</div>';
                            if (result.success) {
                                html += '<p style=\"margin-top: 5px; font-size: 12px; color: #666;\">Type: ' + result.content_type + ' | Size: ' + formatBytes(result.size) + '</p>';
                            } else {
                                html += '<p style=\"margin-top: 5px; font-size: 12px; color: #dc3545;\">' + result.error + '</p>';
                            }
                            html += '</div>';
                        }
                    }
                    html += '</div>';
                    document.getElementById('results').innerHTML = html;
                    loadStats();
                } catch (e) {
                    document.getElementById('results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            loadStats();
        </script>">>
    ],
    Html = html_templates:base_layout(<<"Harvester">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.
