%%%-------------------------------------------------------------------
%%% @doc Harvester Handler Template - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(harvester_handler_template_part2).

-export([content/0]).

content() ->
    <<"
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
        </script>
    ">>.
