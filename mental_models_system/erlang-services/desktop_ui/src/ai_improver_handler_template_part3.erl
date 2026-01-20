%%%-------------------------------------------------------------------
%%% @doc AI Improver Handler Template - Part 3
%%% @end
%%%-------------------------------------------------------------------
-module(ai_improver_handler_template_part3).

-export([content/0]).

content() ->
    <<"
                    
                    document.getElementById('git-status-display').innerHTML = html;
                } catch (e) {
                    document.getElementById('git-status-display').innerHTML = 
                        '<p class=\"status-unknown\">Could not load git status</p>';
                }
            }
            
            async function loadRecentCommits() {
                try {
                    const res = await fetch(AI_IMPROVER_URL + '/api/git/commits?count=10');
                    const data = await res.json();
                    
                    let html = '<div style=\"background: #f8f9fa; padding: 15px; border-radius: 8px;\">';
                    html += '<h4>Recent Commits</h4>';
                    html += '<ul style=\"font-family: monospace; font-size: 12px;\">';
                    data.commits.forEach(c => {
                        html += '<li>' + c + '</li>';
                    });
                    html += '</ul></div>';
                    
                    document.getElementById('git-commits-display').innerHTML = html;
                } catch (e) {
                    document.getElementById('git-commits-display').innerHTML = 
                        '<p class=\"status-unknown\">Could not load commits</p>';
                }
            }
            
            async function pushChanges() {
                if (!confirm('Push all AI-generated changes to remote?')) return;
                
                try {
                    const res = await fetch(AI_IMPROVER_URL + '/api/git/push', { method: 'POST' });
                    const data = await res.json();
                    
                    if (data.status === 'pushed') {
                        alert('Changes pushed successfully!');
                    } else {
                        alert('Push failed: ' + (data.error || 'Unknown error'));
                    }
                    loadGitStatus();
                } catch (e) {
                    alert('Failed to push: ' + e.message);
                }
            }
            
            async function loadHistory() {
                try {
                    const res = await fetch(AI_IMPROVER_URL + '/api/improver/history');
                    const data = await res.json();
                    
                    if (!data.improvements || data.improvements.length === 0) {
                        document.getElementById('history-display').innerHTML = 
                            '<p style=\"color: #666;\">No improvements recorded yet.</p>';
                        return;
                    }
                    
                    let html = '<table style=\"width: 100%; border-collapse: collapse;\">';
                    html += '<tr style=\"background: #f0f0f0;\"><th style=\"padding: 8px; text-align: left;\">Time</th><th style=\"padding: 8px; text-align: left;\">File</th><th style=\"padding: 8px; text-align: left;\">Type</th><th style=\"padding: 8px; text-align: left;\">Status</th></tr>';
                    
                    data.improvements.slice(0, 50).forEach(imp => {
                        let statusClass = imp.status === 'deployed' ? 'status-healthy' : 
                                         imp.status === 'failed' ? 'status-unhealthy' : 'status-unknown';
                        html += '<tr style=\"border-bottom: 1px solid #e0e0e0;\">';
                        html += '<td style=\"padding: 8px;\">' + (imp.timestamp || 'N/A') + '</td>';
                        html += '<td style=\"padding: 8px; font-family: monospace; font-size: 12px;\">' + (imp.file || 'N/A') + '</td>';
                        html += '<td style=\"padding: 8px;\">' + (imp.type || 'N/A') + '</td>';
                        html += '<td style=\"padding: 8px;\"><span class=\"' + statusClass + '\">' + (imp.status || 'N/A') + '</span></td>';
                        html += '</tr>';
                    });
                    html += '</table>';
                    
                    document.getElementById('history-display').innerHTML = html;
                } catch (e) {
                    document.getElementById('history-display').innerHTML = 
                        '<p class=\"status-unknown\">Could not load history</p>';
                }
            }
            
            async function clearHistory() {
                if (!confirm('Are you sure you want to clear all improvement history?')) return;
                
                try {
                    await fetch(AI_IMPROVER_URL + '/api/improver/history', { method: 'DELETE' });
                    loadHistory();
                } catch (e) {
                    alert('Failed to clear history: ' + e.message);
                }
            }
            
            async function viewPhilosophy() {
                try {
                    const res = await fetch(AI_IMPROVER_URL + '/api/prompts/philosophy');
                    const data = await res.json();
                    
                    let html = '<div style=\"background: #f8f9fa; padding: 15px; border-radius: 8px; margin-top: 15px;\">';
                    html += '<h4>Design Philosophy</h4>';
                    html += '<pre style=\"white-space: pre-wrap; font-size: 12px; max-height: 300px; overflow-y: auto;\">' + 
                            JSON.stringify(data, null, 2) + '</pre>';
                    html += '</div>';
                    
                    document.getElementById('philosophy-display').innerHTML = html;
                } catch (e) {
                    document.getElementById('philosophy-display').innerHTML = 
                        '<p class=\"status-unknown\">Could not load philosophy</p>';
                }
            }
            
            async function viewPromptTemplates() {
                try {
                    const res = await fetch(AI_IMPROVER_URL + '/api/prompts/templates');
                    const data = await res.json();
                    
                    let html = '<div style=\"background: #f8f9fa; padding: 15px; border-radius: 8px; margin-top: 15px;\">';
                    html += '<h4>Prompt Templates</h4>';
                    html += '<pre style=\"white-space: pre-wrap; font-size: 12px; max-height: 300px; overflow-y: auto;\">' + 
                            JSON.stringify(data, null, 2) + '</pre>';
                    html += '</div>';
                    
                    document.getElementById('philosophy-display').innerHTML = html;
                } catch (e) {
                    document.getElementById('philosophy-display').innerHTML = 
                        '<p class=\"status-unknown\">Could not load templates</p>';
                }
            }
            
            function formatUptime(seconds) {
                const days = Math.floor(seconds / 86400);
                const hours = Math.floor((seconds % 86400) / 3600);
                const mins = Math.floor((seconds % 3600) / 60);
                
                if (days > 0) return days + 'd ' + hours + 'h ' + mins + 'm';
                if (hours > 0) return hours + 'h ' + mins + 'm';
                return mins + 'm';
            }
            
            // Load all data on page load
            document.addEventListener('DOMContentLoaded', function() {
                loadImproverStatus();
                testLmStudioConnection();
                loadMetrics();
                loadGitStatus();
                loadHistory();
            });
        </script>
        
    ">>.
