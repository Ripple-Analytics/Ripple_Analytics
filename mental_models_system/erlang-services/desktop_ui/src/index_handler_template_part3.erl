%%%-------------------------------------------------------------------
%%% @doc Dashboard Handler Template - Part 3
%%% @end
%%%-------------------------------------------------------------------
-module(index_handler_template_part3).

-export([content/0]).

content() ->
    <<"
                        scales: {
                            y: { beginAtZero: true, ticks: { stepSize: 1 } }
                        }
                    }
                });
            }
            
            async function loadDashboard() {
                try {
                    // Load models count and categories
                    const models = await apiCall('/analysis/models');
                    const modelCount = models.count || 174;
                    document.getElementById('models-count').textContent = modelCount;
                    
                    // Calculate category distribution
                    let categories = [];
                    if (models.models && models.models.length > 0) {
                        const catMap = {};
                        models.models.forEach(m => {
                            catMap[m.category] = (catMap[m.category] || 0) + 1;
                        });
                        categories = Object.entries(catMap)
                            .map(([name, count]) => ({name, count}))
                            .sort((a, b) => b.count - a.count)
                            .slice(0, 8);
                    } else {
                        // Default categories if API not available
                        categories = [
                            {name: 'Decision Making', count: 28},
                            {name: 'Problem Solving', count: 24},
                            {name: 'Systems Thinking', count: 22},
                            {name: 'Cognitive Biases', count: 35},
                            {name: 'Economics', count: 18},
                            {name: 'Psychology', count: 20},
                            {name: 'Strategy', count: 15},
                            {name: 'Other', count: 12}
                        ];
                    }
                    
                    document.getElementById('categories-count').textContent = categories.length + '+';
                    initCategoryChart(categories);
                    
                    // Load history for activity chart and recent analyses
                    let historyData = [];
                    try {
                        const historyRes = await fetch('/api/storage/history?limit=100');
                        const historyJson = await historyRes.json();
                        historyData = historyJson.analyses || [];
                    } catch (e) {
                        console.log('Could not load history:', e);
                    }
                    
                    initActivityChart(historyData);
                    
                    // Load trending models and biases from history
                    loadTrendingData(historyData);
                    
                    // Load service health
                    const health = await fetch('/api/gateway/health').then(r => r.json()).catch(() => ({status: 'unknown'}));
                    document.getElementById('health-status').textContent = health.status === 'healthy' ? 'OK' : 'Check';
                    document.getElementById('services-count').textContent = '6';
                    
                    // Load detailed health
                    const services = [
                        {name: 'API Gateway', url: '/api/gateway/health'},
                        {name: 'Analysis', url: '/api/analysis/health'},
                        {name: 'Harvester', url: '/api/harvester/health'},
                        {name: 'Storage', url: '/api/storage/health'},
                        {name: 'Chaos Engineering', url: '/api/chaos/health'}
                    ];
                    
                    let healthHtml = '';
                    for (const svc of services) {
                        try {
                            const res = await fetch(svc.url);
                            const data = await res.json();
                            const status = data.status === 'healthy' ? 'healthy' : 'unhealthy';
                            healthHtml += '<p><span class=\"status-' + status + '\">●</span> ' + svc.name + '</p>';
                        } catch (e) {
                            healthHtml += '<p><span class=\"status-unknown\">●</span> ' + svc.name + ' (unreachable)</p>';
                        }
                    }
                    document.getElementById('service-health').innerHTML = healthHtml;
                    
                    // Display recent analyses from history
                    if (historyData.length > 0) {
                        const recent = historyData.slice(0, 5);
                        let recentHtml = '<table style=\"width:100%;border-collapse:collapse;\">';
                        recentHtml += '<tr style=\"border-bottom:1px solid #e0e0e0;\"><th style=\"text-align:left;padding:8px;\">Type</th><th style=\"text-align:left;padding:8px;\">Input</th><th style=\"text-align:left;padding:8px;\">Models</th><th style=\"text-align:left;padding:8px;\">Biases</th><th style=\"text-align:left;padding:8px;\">Date</th><th style=\"padding:8px;\"></th></tr>';
                        for (const analysis of recent) {
                            const typeLabel = analysis.type === 'full' ? 'Full' : analysis.type === 'models' ? 'Models' : 'Biases';
                            const inputPreview = (analysis.input_text || '').substring(0, 50) + ((analysis.input_text || '').length > 50 ? '...' : '');
                            const date = new Date(analysis.timestamp).toLocaleDateString();
                            recentHtml += '<tr style=\"border-bottom:1px solid #f0f0f0;\">';
                            recentHtml += '<td style=\"padding:8px;\"><span class=\"category\">' + typeLabel + '</span></td>';
                            recentHtml += '<td style=\"padding:8px;color:#666;\">' + inputPreview + '</td>';
                            recentHtml += '<td style=\"padding:8px;\">' + (analysis.model_count || 0) + '</td>';
                            recentHtml += '<td style=\"padding:8px;\">' + (analysis.bias_count || 0) + '</td>';
                            recentHtml += '<td style=\"padding:8px;color:#666;\">' + date + '</td>';
                            recentHtml += '<td style=\"padding:8px;\"><a href=\"/history\" class=\"btn btn-secondary\" style=\"font-size:11px;padding:4px 8px;\">View</a></td>';
                            recentHtml += '</tr>';
                        }
                        recentHtml += '</table>';
                        if (historyData.length > 5) {
                            recentHtml += '<p style=\"margin-top:10px;\"><a href=\"/history\">View all ' + historyData.length + ' analyses</a></p>';
                        }
                        document.getElementById('recent-analyses').innerHTML = recentHtml;
                    } else {
                        document.getElementById('recent-analyses').innerHTML = 
                            '<p style=\"color: #666;\">No recent analyses. <a href=\"/analysis\">Start your first analysis</a></p>';
                    }
                    
                } catch (e) {
                    console.error('Dashboard load error:', e);
                }
            }
            loadDashboard();
            setInterval(loadDashboard, 30000);
        </script>
    ">>.
