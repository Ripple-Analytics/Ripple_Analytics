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
            <h2>Scrape &amp; Analyze</h2>
            <p>Scrape a URL and immediately analyze its content for mental models and cognitive biases.</p>
            <br>
            <input type=\"url\" id=\"analyze-url\" placeholder=\"https://example.com/article\">
            <div style=\"display: flex; gap: 10px; margin-top: 10px; flex-wrap: wrap;\">
                <button class=\"btn\" onclick=\"scrapeAndAnalyze()\">Scrape &amp; Analyze</button>
                <label style=\"display: flex; align-items: center; gap: 5px;\">
                    <input type=\"checkbox\" id=\"detect-biases\" checked> Detect Biases
                </label>
                <label style=\"display: flex; align-items: center; gap: 5px;\">
                    Top N Models: <input type=\"number\" id=\"top-n\" value=\"5\" min=\"1\" max=\"20\" style=\"width: 60px;\">
                </label>
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
            
            async function scrapeAndAnalyze() {
                const url = document.getElementById('analyze-url').value;
                if (!url.trim()) {
                    alert('Please enter a URL');
                    return;
                }
                
                const detectBiases = document.getElementById('detect-biases').checked;
                const topN = parseInt(document.getElementById('top-n').value) || 5;
                
                document.getElementById('results').innerHTML = '<div class=\"loading\">Scraping and analyzing URL...</div>';
                
                try {
                    const res = await fetch('/api/harvester/scrape-analyze', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({url: url, top_n: topN, detect_biases: detectBiases})
                    });
                    const data = await res.json();
                    
                    let html = '<div class=\"card\"><h2>Scrape &amp; Analyze Results</h2>';
                    
                    if (data.success) {
                        html += '<p><strong>URL:</strong> ' + data.url + '</p>';
                        html += '<p><strong>Content Type:</strong> ' + (data.scrape?.content_type || 'text/html') + '</p>';
                        html += '<p><strong>Text Length:</strong> ' + (data.scrape?.text_length || 0) + ' characters</p>';
                        
                        // Mental Models
                        html += '<h3 style=\"margin-top: 20px;\">Mental Models Detected</h3>';
                        const models = data.analysis?.models || [];
                        if (models.length > 0) {
                            for (const model of models) {
                                html += '<div class=\"model-card\">';
                                html += '<h4>' + model.name + '</h4>';
                                html += '<span class=\"category\">' + model.category + '</span>';
                                html += '<p>' + model.description + '</p>';
                                html += '</div>';
                            }
                        } else {
                            html += '<p style=\"color: #666;\">No mental models detected in this content.</p>';
                        }
                        
                        // Biases
                        if (detectBiases) {
                            html += '<h3 style=\"margin-top: 20px;\">Cognitive Biases Detected</h3>';
                            const biases = data.analysis?.biases || [];
                            if (biases.length > 0) {
                                for (const bias of biases) {
                                    const severityClass = bias.severity === 'high' ? 'status-unhealthy' : 
                                                          bias.severity === 'medium' ? 'status-unknown' : 'status-healthy';
                                    html += '<div class=\"model-card\">';
                                    html += '<h4>' + bias.bias.replace(/_/g, ' ').replace(/\\b\\w/g, l => l.toUpperCase()) + '</h4>';
                                    html += '<span class=\"' + severityClass + '\">Severity: ' + bias.severity + '</span>';
                                    html += '</div>';
                                }
                            } else {
                                html += '<p style=\"color: #666;\">No cognitive biases detected in this content.</p>';
                            }
                        }
                        
                        html += '<p style=\"margin-top: 15px; font-size: 12px; color: #666;\">Analysis method: ' + (data.analysis?.method || 'keyword_matching') + '</p>';
                    } else {
                        html += '<div class=\"alert alert-error\">Failed to scrape URL: ' + (data.error || 'Unknown error') + '</div>';
                    }
                    html += '</div>';
                    document.getElementById('results').innerHTML = html;
                    loadStats();
                } catch (e) {
                    document.getElementById('results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
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
