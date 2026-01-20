%%%-------------------------------------------------------------------
%%% @doc Dashboard Handler Template - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(index_handler_template_part2).

-export([content/0]).

content() ->
    <<"
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
    ">>.
