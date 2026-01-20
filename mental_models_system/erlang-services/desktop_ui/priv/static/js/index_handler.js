<script>
    let categoryChart = null;
    let activityChart = null;
    
    // Quick analysis function
    async function quickAnalyze(type) {
        const text = document.getElementById('quick-text').value.trim();
        if (!text) {
            alert('Please enter some text to analyze');
            return;
        }
        
        const resultDiv = document.getElementById('quick-result');
        resultDiv.style.display = 'block';
        resultDiv.innerHTML = '<p class=\"loading\">Analyzing...</p>';
        
        try {
            const res = await fetch('/api/analysis/analyze', {
                method: 'POST',
                headers: {'Content-Type': 'application/json'},
                body: JSON.stringify({text: text, type: type})
            });
            const data = await res.json();
            
            let html = '<div style=\"background: #e7f3ff; padding: 15px; border-radius: 8px;\">';
            
            if (type === 'full' || type === 'models') {
                const models = data.models || [];
                html += '<h4 style=\"margin-bottom: 10px;\">Mental Models (' + models.length + ')</h4>';
                if (models.length > 0) {
                    html += '<div style=\"display: flex; flex-wrap: wrap; gap: 5px; margin-bottom: 10px;\">';
                    models.slice(0, 5).forEach(m => {
                        html += '<span style=\"background: #4361ee; color: white; padding: 3px 8px; border-radius: 4px; font-size: 12px;\">' + (m.name || m) + '</span>';
                    });
                    if (models.length > 5) html += '<span style=\"color: #666; font-size: 12px;\">+' + (models.length - 5) + ' more</span>';
                    html += '</div>';
                } else {
                    html += '<p style=\"color: #666; font-size: 13px;\">No models detected</p>';
                }
            }
            
            if (type === 'full' || type === 'biases') {
                const biases = data.biases || [];
                html += '<h4 style=\"margin-bottom: 10px;\">Cognitive Biases (' + biases.length + ')</h4>';
                if (biases.length > 0) {
                    html += '<div style=\"display: flex; flex-wrap: wrap; gap: 5px;\">';
                    biases.slice(0, 5).forEach(b => {
                        html += '<span style=\"background: #f72585; color: white; padding: 3px 8px; border-radius: 4px; font-size: 12px;\">' + (b.bias || b) + '</span>';
                    });
                    if (biases.length > 5) html += '<span style=\"color: #666; font-size: 12px;\">+' + (biases.length - 5) + ' more</span>';
                    html += '</div>';
                } else {
                    html += '<p style=\"color: #666; font-size: 13px;\">No biases detected</p>';
                }
            }
            
            html += '<p style=\"margin-top: 15px;\"><a href=\"/analysis\" class=\"btn btn-secondary\" style=\"font-size: 12px;\">Full Analysis Page</a> <a href=\"/history\" class=\"btn btn-secondary\" style=\"font-size: 12px; margin-left: 5px;\">View History</a></p>';
            html += '</div>';
            
            resultDiv.innerHTML = html;
            
            // Refresh dashboard to show new analysis in recent
            setTimeout(loadDashboard, 1000);
        } catch (e) {
            resultDiv.innerHTML = '<p style=\"color: #dc3545;\">Analysis failed: ' + e.message + '</p>';
        }
    }
    
    // Category distribution chart
    function initCategoryChart(categories) {
        const ctx = document.getElementById('categoryChart').getContext('2d');
        const colors = [
            '#4361ee', '#3f37c9', '#4895ef', '#4cc9f0', '#7209b7',
            '#f72585', '#b5179e', '#560bad', '#480ca8', '#3a0ca3'
        ];
        
        if (categoryChart) categoryChart.destroy();
        categoryChart = new Chart(ctx, {
            type: 'doughnut',
            data: {
                labels: categories.map(c => c.name),
                datasets: [{
                    data: categories.map(c => c.count),
                    backgroundColor: colors.slice(0, categories.length),
                    borderWidth: 2,
                    borderColor: '#ffffff'
                }]
            },
            options: {
                responsive: true,
                plugins: {
                    legend: {
                        position: 'right',
                        labels: { boxWidth: 12, padding: 8 }
                    }
                }
            }
        });
    }
    
    // Load trending models and biases
    function loadTrendingData(historyData) {
        // Count model occurrences
        const modelCounts = {};
        const biasCounts = {};
        
        for (const analysis of historyData) {
            const models = analysis.models || [];
            for (const model of models) {
                const name = model.name || model;
                modelCounts[name] = (modelCounts[name] || 0) + 1;
            }
            
            const biases = analysis.biases || [];
            for (const bias of biases) {
                const name = bias.bias || bias;
                biasCounts[name] = (biasCounts[name] || 0) + 1;
            }
        }
        
        // Sort and get top 5 models
        const topModels = Object.entries(modelCounts)
            .sort((a, b) => b[1] - a[1])
            .slice(0, 5);
        
        // Sort and get top 5 biases
        const topBiases = Object.entries(biasCounts)
            .sort((a, b) => b[1] - a[1])
            .slice(0, 5);
        
        // Render trending models
        if (topModels.length > 0) {
            let html = '';
            for (const [name, count] of topModels) {
                const barWidth = Math.min(100, (count / topModels[0][1]) * 100);
                html += '<div style=\"margin-bottom: 10px;\">';
                html += '<div style=\"display: flex; justify-content: space-between; margin-bottom: 3px;\">';
                html += '<span style=\"font-size: 13px;\">' + name + '</span>';
                html += '<span style=\"font-size: 12px; color: #666;\">' + count + 'x</span>';
                html += '</div>';
                html += '<div style=\"background: #e9ecef; border-radius: 4px; height: 8px;\">';
                html += '<div style=\"background: #4361ee; border-radius: 4px; height: 8px; width: ' + barWidth + '%;\"></div>';
                html += '</div></div>';
            }
            html += '<a href=\"/models\" style=\"font-size: 12px;\">View all models</a>';
            document.getElementById('trending-models').innerHTML = html;
        } else {
            document.getElementById('trending-models').innerHTML = '<p style=\"color: #666; font-size: 13px;\">No data yet. Run some analyses to see trends.</p>';
        }
        
        // Render trending biases
        if (topBiases.length > 0) {
            let html = '';
            for (const [name, count] of topBiases) {
                const barWidth = Math.min(100, (count / topBiases[0][1]) * 100);
                const displayName = name.replace(/_/g, ' ').replace(/\\b\\w/g, l => l.toUpperCase());
                html += '<div style=\"margin-bottom: 10px;\">';
                html += '<div style=\"display: flex; justify-content: space-between; margin-bottom: 3px;\">';
                html += '<span style=\"font-size: 13px;\">' + displayName + '</span>';
                html += '<span style=\"font-size: 12px; color: #666;\">' + count + 'x</span>';
                html += '</div>';
                html += '<div style=\"background: #e9ecef; border-radius: 4px; height: 8px;\">';
                html += '<div style=\"background: #f72585; border-radius: 4px; height: 8px; width: ' + barWidth + '%;\"></div>';
                html += '</div></div>';
            }
            document.getElementById('trending-biases').innerHTML = html;
        } else {
            document.getElementById('trending-biases').innerHTML = '<p style=\"color: #666; font-size: 13px;\">No biases detected yet. Run bias detection to see trends.</p>';
        }
    }
    
    // Activity chart with real data from history
    function initActivityChart(historyData) {
        const ctx = document.getElementById('activityChart').getContext('2d');
        
        // Group analyses by day (last 7 days)
        const days = [];
        const counts = [];
        const now = new Date();
        
        for (let i = 6; i >= 0; i--) {
            const date = new Date(now);
            date.setDate(date.getDate() - i);
            const dayName = date.toLocaleDateString('en-US', { weekday: 'short' });
            const dateStr = date.toISOString().split('T')[0];
            days.push(dayName);
            
            // Count analyses for this day
            const count = historyData.filter(h => {
                const hDate = new Date(h.timestamp).toISOString().split('T')[0];
                return hDate === dateStr;
            }).length;
            counts.push(count);
        }
        
        if (activityChart) activityChart.destroy();
        activityChart = new Chart(ctx, {
            type: 'line',
            data: {
                labels: days,
                datasets: [{
                    label: 'Analyses',
                    data: counts,
                    borderColor: '#4361ee',
                    backgroundColor: 'rgba(67, 97, 238, 0.1)',
                    fill: true,
                    tension: 0.4
                }]
            },
            options: {
                responsive: true,
                plugins: {
                    legend: { display: false }
                },
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
</script>">>
    ],
