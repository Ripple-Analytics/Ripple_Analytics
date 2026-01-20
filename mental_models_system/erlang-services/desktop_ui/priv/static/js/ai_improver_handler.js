<script>
    const AI_IMPROVER_URL = 'http://localhost:8025';
    
    async function loadImproverStatus() {
        try {
            const res = await fetch(AI_IMPROVER_URL + '/api/improver/status');
            const data = await res.json();
            
            let statusClass = data.running ? 'status-healthy' : 'status-unknown';
            let statusText = data.running ? 'Running' : 'Stopped';
            
            let html = '<div style=\"display: grid; grid-template-columns: 1fr 1fr; gap: 10px;\">';
            html += '<p><strong>Status:</strong> <span class=\"' + statusClass + '\">' + statusText + '</span></p>';
            html += '<p><strong>Cycles Completed:</strong> ' + (data.cycles_completed || 0) + '</p>';
            html += '<p><strong>Last Cycle:</strong> ' + (data.last_cycle || 'never') + '</p>';
            html += '<p><strong>Next Cycle:</strong> ' + (data.next_cycle || 'N/A') + '</p>';
            html += '</div>';
            
            document.getElementById('improver-status').innerHTML = html;
            document.getElementById('improver-toggle-btn').textContent = data.running ? 'Stop Improver' : 'Start Improver';
        } catch (e) {
            document.getElementById('improver-status').innerHTML = 
                '<p class=\"status-unhealthy\">Could not connect to AI Improver service. Make sure it is running on port 8025.</p>';
        }
    }
    
    async function toggleImprover() {
        const btn = document.getElementById('improver-toggle-btn');
        const isRunning = btn.textContent === 'Stop Improver';
        
        try {
            const endpoint = isRunning ? '/api/improver/stop' : '/api/improver/start';
            const res = await fetch(AI_IMPROVER_URL + endpoint, { method: 'POST' });
            const data = await res.json();
            
            if (data.status === 'started' || data.status === 'stopped') {
                loadImproverStatus();
            }
        } catch (e) {
            alert('Failed to toggle improver: ' + e.message);
        }
    }
    
    async function forceImprovement() {
        try {
            const res = await fetch(AI_IMPROVER_URL + '/api/improver/start', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ force: true })
            });
            const data = await res.json();
            alert('Improvement cycle triggered');
            loadImproverStatus();
        } catch (e) {
            alert('Failed to trigger improvement: ' + e.message);
        }
    }
    
    async function testLmStudioConnection() {
        const url = document.getElementById('lm-studio-url').value;
        document.getElementById('lm-studio-status').innerHTML = '<p class=\"loading\">Testing connection...</p>';
        
        try {
            const res = await fetch(AI_IMPROVER_URL + '/health');
            const data = await res.json();
            
            if (data.lm_studio_connected) {
                document.getElementById('lm-studio-status').innerHTML = 
                    '<p class=\"status-healthy\">Connected to LM Studio</p>';
            } else {
                document.getElementById('lm-studio-status').innerHTML = 
                    '<p class=\"status-unhealthy\">LM Studio not connected. Make sure it is running.</p>';
            }
        } catch (e) {
            document.getElementById('lm-studio-status').innerHTML = 
                '<p class=\"status-unhealthy\">Could not check LM Studio status</p>';
        }
    }
    
    async function loadMetrics() {
        try {
            const res = await fetch(AI_IMPROVER_URL + '/api/metrics');
            const data = await res.json();
            
            let html = '<div style=\"display: grid; grid-template-columns: 1fr 1fr; gap: 10px;\">';
            html += '<p><strong>Total Improvements:</strong> ' + data.total_improvements + '</p>';
            html += '<p><strong>Total Failures:</strong> ' + data.total_failures + '</p>';
            html += '<p><strong>Success Rate:</strong> ' + data.success_rate.toFixed(1) + '%</p>';
            html += '<p><strong>Avg Cycle Time:</strong> ' + data.avg_cycle_time_ms + 'ms</p>';
            html += '<p><strong>Uptime:</strong> ' + formatUptime(data.uptime_seconds) + '</p>';
            html += '<p><strong>Last Improvement:</strong> ' + (data.last_improvement || 'never') + '</p>';
            html += '</div>';
            
            document.getElementById('metrics-display').innerHTML = html;
        } catch (e) {
            document.getElementById('metrics-display').innerHTML = 
                '<p class=\"status-unknown\">Could not load metrics</p>';
        }
    }
    
    async function resetMetrics() {
        if (!confirm('Are you sure you want to reset all metrics?')) return;
        
        try {
            await fetch(AI_IMPROVER_URL + '/api/metrics', { method: 'DELETE' });
            loadMetrics();
        } catch (e) {
            alert('Failed to reset metrics: ' + e.message);
        }
    }
    
    async function saveImproverConfig() {
        const config = {
            improvement_interval: parseInt(document.getElementById('improvement-interval').value),
            max_changes_per_cycle: parseInt(document.getElementById('max-changes').value),
            auto_deploy: document.getElementById('auto-deploy').checked,
            require_validation: document.getElementById('require-validation').checked
        };
        
        try {
            const res = await fetch(AI_IMPROVER_URL + '/api/improver/config', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(config)
            });
            
            document.getElementById('config-save-status').innerHTML = 
                '<span class=\"status-healthy\">Saved!</span>';
            setTimeout(() => {
                document.getElementById('config-save-status').innerHTML = '';
            }, 3000);
        } catch (e) {
            document.getElementById('config-save-status').innerHTML = 
                '<span class=\"status-unhealthy\">Failed to save</span>';
        }
    }
    
    async function analyzeCode() {
        const path = document.getElementById('analyze-path').value;
        if (!path) {
            alert('Please enter a path to analyze');
            return;
        }
        
        document.getElementById('analysis-results').innerHTML = '<p class=\"loading\">Analyzing...</p>';
        
        try {
            const res = await fetch(AI_IMPROVER_URL + '/api/improver/analyze', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ path: path })
            });
            const data = await res.json();
            
            let html = '<div style=\"background: #f8f9fa; padding: 15px; border-radius: 8px;\">';
            html += '<h4>Analysis Results</h4>';
            if (data.files && data.files.length > 0) {
                html += '<ul>';
                data.files.forEach(f => {
                    html += '<li><strong>' + f.file + '</strong>: ' + (f.issues || 0) + ' issues</li>';
                });
                html += '</ul>';
            } else {
                html += '<p>No issues found or no files analyzed.</p>';
            }
            html += '</div>';
            
            document.getElementById('analysis-results').innerHTML = html;
        } catch (e) {
            document.getElementById('analysis-results').innerHTML = 
                '<p class=\"status-unhealthy\">Analysis failed: ' + e.message + '</p>';
        }
    }
    
    async function loadGitStatus() {
        try {
            const res = await fetch(AI_IMPROVER_URL + '/api/git/status');
            const data = await res.json();
            
            let html = '<div style=\"display: grid; grid-template-columns: 1fr 1fr; gap: 10px;\">';
            html += '<p><strong>Branch:</strong> ' + data.current_branch + '</p>';
            html += '<p><strong>Auto Commit:</strong> ' + (data.auto_commit ? 'Yes' : 'No') + '</p>';
            html += '<p><strong>Auto Push:</strong> ' + (data.auto_push ? 'Yes' : 'No') + '</p>';
            html += '</div>';
            if (data.changes) {
                html += '<pre style=\"background: #f8f9fa; padding: 10px; border-radius: 4px; margin-top: 10px; max-height: 150px; overflow-y: auto;\">' + data.changes + '</pre>';
            }
            
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
">>
    ],
    
    Html = html_templates:page("AI Code Improver", Content),
    Req = cowboy_req:reply(200,
#{<<"content-type">> => <<"text/html">>},
