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
                <h2>Scrape URL</h2>
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
            
            loadStats();
        </script>">>
    ],
    Html = html_templates:base_layout(<<"Harvester">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.
