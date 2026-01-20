%%%-------------------------------------------------------------------
%%% @doc Settings Handler Template - Part 5
%%% @end
%%%-------------------------------------------------------------------
-module(settings_handler_template_part5).

-export([content/0]).

content() ->
    <<"
                    html += '<p><strong>Status:</strong> <span class=\"' + (chaosMonkeyEnabled ? 'status-unhealthy' : 'status-healthy') + '\">' + 
                            (chaosMonkeyEnabled ? 'ENABLED (Active)' : 'Disabled (Safe)') + '</span></p>';
                    html += '<p><strong>Total Attacks:</strong> ' + (data.engine?.total_attacks || 0) + '</p>';
                    html += '<p><strong>Recovery Rate:</strong> ' + (data.engine?.recovery_rate?.toFixed(1) || 100) + '%</p>';
                    html += '<p><strong>Last Attack:</strong> ' + (data.engine?.last_attack || 'never') + '</p>';
                    html += '</div>';
                    
                    document.getElementById('chaos-monkey-status').innerHTML = html;
                    
                    // Update toggle button
                    const btn = document.getElementById('chaos-toggle-btn');
                    if (chaosMonkeyEnabled) {
                        btn.textContent = 'Disable Chaos Monkey';
                        btn.style.background = '#dc3545';
                    } else {
                        btn.textContent = 'Enable Chaos Monkey';
                        btn.style.background = '#4361ee';
                    }
                } catch (e) {
                    document.getElementById('chaos-monkey-status').innerHTML = 
                        '<p class=\"status-unknown\">Chaos Monkey service not available</p>';
                }
            }
            
            async function toggleChaosMonkey() {
                const endpoint = chaosMonkeyEnabled ? 
                    'http://localhost:8007/api/chaos/disable' : 
                    'http://localhost:8007/api/chaos/enable';
                
                try {
                    const res = await fetch(endpoint, {method: 'POST'});
                    const data = await res.json();
                    
                    if (data.success) {
                        loadChaosMonkeyStatus();
                        document.getElementById('chaos-monkey-results').innerHTML = 
                            '<div class=\"alert alert-success\">' + data.message + '</div>';
                    }
                } catch (e) {
                    document.getElementById('chaos-monkey-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function triggerRandomAttack() {
                if (!chaosMonkeyEnabled) {
                    document.getElementById('chaos-monkey-results').innerHTML = 
                        '<div class=\"alert alert-error\">Chaos Monkey is disabled. Enable it first.</div>';
                    return;
                }
                
                document.getElementById('chaos-monkey-results').innerHTML = '<p class=\"loading\">Executing random attack...</p>';
                try {
                    const res = await fetch('http://localhost:8007/api/chaos/attack', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({type: 'random'})
                    });
                    const data = await res.json();
                    
                    if (data.success) {
                        let html = '<div class=\"card\" style=\"margin-top: 10px;\">';
                        html += '<h4>Attack Result</h4>';
                        html += '<p><strong>Type:</strong> ' + (data.attack?.attack || 'unknown') + '</p>';
                        html += '<p><strong>Target:</strong> ' + (data.attack?.target || 'N/A') + '</p>';
                        html += '<p><strong>Recovered:</strong> ' + (data.attack?.recovered ? 'Yes' : 'No') + '</p>';
                        html += '</div>';
                        document.getElementById('chaos-monkey-results').innerHTML = html;
                        loadChaosMonkeyStatus();
                    } else {
                        document.getElementById('chaos-monkey-results').innerHTML = 
                            '<div class=\"alert alert-error\">' + (data.error || 'Attack failed') + '</div>';
                    }
                } catch (e) {
                    document.getElementById('chaos-monkey-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function triggerAttack(attackType) {
                if (!chaosMonkeyEnabled) {
                    document.getElementById('chaos-monkey-results').innerHTML = 
                        '<div class=\"alert alert-error\">Chaos Monkey is disabled. Enable it first.</div>';
                    return;
                }
                
                document.getElementById('chaos-monkey-results').innerHTML = '<p class=\"loading\">Executing ' + attackType + ' attack...</p>';
                try {
                    const res = await fetch('http://localhost:8007/api/chaos/attack', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({type: attackType})
                    });
                    const data = await res.json();
                    
                    if (data.success) {
                        let html = '<div class=\"card\" style=\"margin-top: 10px;\">';
                        html += '<h4>Attack Result: ' + attackType + '</h4>';
                        html += '<p><strong>Target:</strong> ' + (data.attack?.target || 'N/A') + '</p>';
                        html += '<p><strong>Recovered:</strong> ' + (data.attack?.recovered ? 'Yes' : 'No') + '</p>';
                        if (data.attack?.output) {
                            html += '<p><strong>Output:</strong> <code>' + data.attack.output.substring(0, 200) + '</code></p>';
                        }
                        html += '</div>';
                        document.getElementById('chaos-monkey-results').innerHTML = html;
                        loadChaosMonkeyStatus();
                    } else {
                        document.getElementById('chaos-monkey-results').innerHTML = 
                            '<div class=\"alert alert-error\">' + (data.error || 'Attack failed') + '</div>';
                    }
                } catch (e) {
                    document.getElementById('chaos-monkey-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function viewAttackHistory() {
                document.getElementById('chaos-monkey-results').innerHTML = '<p class=\"loading\">Loading attack history...</p>';
                try {
                    const res = await fetch('http://localhost:8007/api/chaos/history');
                    const data = await res.json();
                    
                    if (data.history && data.history.length > 0) {
                        let html = '<div class=\"card\" style=\"margin-top: 10px;\">';
                        html += '<h4>Attack History (Last ' + data.count + ')</h4>';
                        html += '<table style=\"width: 100%; border-collapse: collapse;\">';
                        html += '<tr style=\"background: #f8f9fa;\"><th style=\"padding: 8px; text-align: left;\">Type</th>';
                        html += '<th style=\"padding: 8px; text-align: left;\">Timestamp</th>';
                        html += '<th style=\"padding: 8px; text-align: left;\">Recovered</th></tr>';
                        
                        for (const attack of data.history.slice(0, 10)) {
                            html += '<tr style=\"border-bottom: 1px solid #eee;\">';
                            html += '<td style=\"padding: 8px;\">' + attack.type + '</td>';
                            html += '<td style=\"padding: 8px;\">' + attack.timestamp + '</td>';
                            html += '<td style=\"padding: 8px;\"><span class=\"' + (attack.recovered ? 'status-healthy' : 'status-unhealthy') + '\">●</span> ' + 
                                    (attack.recovered ? 'Yes' : 'No') + '</td>';
                            html += '</tr>';
                        }
                        html += '</table></div>';
                        document.getElementById('chaos-monkey-results').innerHTML = html;
                    } else {
                        document.getElementById('chaos-monkey-results').innerHTML = 
                            '<p>No attack history yet.</p>';
                    }
                } catch (e) {
                    document.getElementById('chaos-monkey-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
    ">>.
