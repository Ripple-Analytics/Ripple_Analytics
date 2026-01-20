%%%-------------------------------------------------------------------
%%% @doc Settings Handler Template - Part 6
%%% @end
%%%-------------------------------------------------------------------
-module(settings_handler_template_part6).

-export([content/0]).

content() ->
    <<"
            // Chaos engineering functions (legacy)
            async function runLoadTest() {
                document.getElementById('chaos-results').innerHTML = '<div class=\"loading\">Running load test...</div>';
                try {
                    const res = await fetch('/api/chaos/test/load', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({target: 'api_gateway', requests: 50, concurrency: 5})
                    });
                    const data = await res.json();
                    
                    let html = '<div class=\"card\"><h2>Load Test Results</h2>';
                    html += '<p><strong>Target:</strong> ' + data.target + '</p>';
                    html += '<p><strong>Total Requests:</strong> ' + data.total_requests + '</p>';
                    html += '<p><strong>Successful:</strong> ' + data.successful + '</p>';
                    html += '<p><strong>Failed:</strong> ' + data.failed + '</p>';
                    html += '<p><strong>Duration:</strong> ' + data.duration_ms + 'ms</p>';
                    html += '<p><strong>Requests/sec:</strong> ' + (data.requests_per_second || 0).toFixed(2) + '</p>';
                    if (data.latency) {
                        html += '<h3 style=\"margin-top:15px;\">Latency Metrics</h3>';
                        html += '<p>Min: ' + data.latency.min + 'ms | ';
                        html += 'Avg: ' + (data.latency.avg || 0).toFixed(2) + 'ms | ';
                        html += 'Max: ' + data.latency.max + 'ms</p>';
                        html += '<p>p50: ' + data.latency.p50 + 'ms | ';
                        html += 'p95: ' + data.latency.p95 + 'ms | ';
                        html += 'p99: ' + data.latency.p99 + 'ms</p>';
                    }
                    html += '</div>';
                    document.getElementById('chaos-results').innerHTML = html;
                } catch (e) {
                    document.getElementById('chaos-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function runCascadeTest() {
                document.getElementById('chaos-results').innerHTML = '<div class=\"loading\">Running cascade failure test...</div>';
                try {
                    const res = await fetch('/api/chaos/scenario/cascade-failure', {method: 'POST'});
                    const data = await res.json();
                    
                    let html = '<div class=\"card\"><h2>Cascade Failure Test Results</h2>';
                    html += '<p><strong>Scenario:</strong> ' + data.scenario + '</p>';
                    html += '<p><strong>Completed:</strong> ' + (data.completed ? 'Yes' : 'No') + '</p>';
                    if (data.steps) {
                        html += '<h3 style=\"margin-top:15px;\">Steps</h3>';
                        for (const step of data.steps) {
                            html += '<p>Step ' + step.step + ': ' + step.action + ' → ' + JSON.stringify(step.result) + '</p>';
                        }
                    }
                    html += '</div>';
                    document.getElementById('chaos-results').innerHTML = html;
                } catch (e) {
                    document.getElementById('chaos-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function runRecoveryTest() {
                document.getElementById('chaos-results').innerHTML = '<div class=\"loading\">Running recovery test...</div>';
                try {
                    const res = await fetch('/api/chaos/scenario/recovery-test', {method: 'POST'});
                    const data = await res.json();
                    
                    let html = '<div class=\"card\"><h2>Recovery Test Results</h2>';
                    html += '<p><strong>Scenario:</strong> ' + data.scenario + '</p>';
                    html += '<p><strong>Recovery Successful:</strong> ' + (data.recovery_successful ? 'Yes' : 'No') + '</p>';
                    if (data.steps) {
                        html += '<h3 style=\"margin-top:15px;\">Steps</h3>';
                        for (const step of data.steps) {
                            html += '<p>Step ' + step.step + ': ' + step.action + ' → ' + JSON.stringify(step.result) + '</p>';
                        }
                    }
                    html += '</div>';
                    document.getElementById('chaos-results').innerHTML = html;
                } catch (e) {
                    document.getElementById('chaos-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function checkAllHealth() {
                document.getElementById('chaos-results').innerHTML = '<div class=\"loading\">Checking all services...</div>';
                try {
                    const res = await fetch('/api/chaos/services/health');
                    const data = await res.json();
                    
                    let html = '<div class=\"card\"><h2>Service Health Check</h2>';
                    if (data.services) {
                        for (const svc of data.services) {
                            const statusClass = svc.status === 'healthy' ? 'status-healthy' : 'status-unhealthy';
                            html += '<p><span class=\"' + statusClass + '\">●</span> ' + svc.name + ': ' + svc.status + '</p>';
                        }
                    }
                    html += '</div>';
                    document.getElementById('chaos-results').innerHTML = html;
                } catch (e) {
                    document.getElementById('chaos-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            // S3/MinIO Backup functions
            async function loadS3Status() {
                try {
                    const res = await fetch('http://localhost:8008/api/s3/status');
                    const data = await res.json();
                    
                    let html = '<div style=\"display: grid; grid-template-columns: 1fr 1fr; gap: 10px;\">';
                    html += '<p><strong>Configured:</strong> ' + (data.s3?.configured ? 'Yes' : 'No') + '</p>';
                    html += '<p><strong>Endpoint:</strong> ' + (data.s3?.endpoint || 'Not set') + '</p>';
                    html += '<p><strong>Bucket:</strong> ' + (data.s3?.bucket || 'Not set') + '</p>';
                    html += '<p><strong>Connection:</strong> ' + (data.s3?.connection_status || 'unknown') + '</p>';
                    html += '</div>';
                    
                    document.getElementById('s3-status').innerHTML = html;
                } catch (e) {
                    document.getElementById('s3-status').innerHTML = 
                        '<p class=\"status-unknown\">S3 Backup service not available</p>';
                }
            }
            
            async function testS3Connection() {
                document.getElementById('s3-results').innerHTML = '<p class=\"loading\">Testing S3 connection...</p>';
                try {
                    const res = await fetch('http://localhost:8008/api/s3/test', {method: 'POST'});
                    const data = await res.json();
                    
                    if (data.success) {
                        document.getElementById('s3-results').innerHTML = 
                            '<div class=\"alert alert-success\">Connection successful!</div>';
                        loadS3Status();
                    } else {
                        document.getElementById('s3-results').innerHTML = 
                            '<div class=\"alert alert-error\">' + (data.error || 'Connection failed') + '</div>';
                    }
                } catch (e) {
                    document.getElementById('s3-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function createS3Backup() {
                document.getElementById('s3-results').innerHTML = '<p class=\"loading\">Creating S3 backup...</p>';
                try {
                    const res = await fetch('http://localhost:8008/api/s3/backup/create', {method: 'POST'});
                    const data = await res.json();
                    
                    if (data.success) {
                        document.getElementById('s3-results').innerHTML = 
    ">>.
