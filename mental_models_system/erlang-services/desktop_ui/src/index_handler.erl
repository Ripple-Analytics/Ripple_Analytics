%%%-------------------------------------------------------------------
%%% @doc Index Handler - Dashboard page
%%%-------------------------------------------------------------------
-module(index_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Content = [
        <<"<div class=\"stats-grid\">
            <div class=\"stat-card\">
                <div class=\"value\" id=\"models-count\">-</div>
                <div class=\"label\">Mental Models</div>
            </div>
            <div class=\"stat-card\">
                <div class=\"value\" id=\"categories-count\">-</div>
                <div class=\"label\">Categories</div>
            </div>
            <div class=\"stat-card\">
                <div class=\"value\" id=\"services-count\">-</div>
                <div class=\"label\">Services</div>
            </div>
            <div class=\"stat-card\">
                <div class=\"value status-healthy\" id=\"health-status\">-</div>
                <div class=\"label\">System Health</div>
            </div>
        </div>
        <div class=\"grid\">
            <div class=\"card\">
                <h2>Quick Analysis</h2>
                <p>Analyze text for mental model patterns and cognitive biases.</p>
                <br>
                <a href=\"/analysis\" class=\"btn\">Start Analysis</a>
            </div>
            <div class=\"card\">
                <h2>Browse Models</h2>
                <p>Explore 174+ mental models across multiple categories.</p>
                <br>
                <a href=\"/models\" class=\"btn\">View Models</a>
            </div>
            <div class=\"card\">
                <h2>Data Harvester</h2>
                <p>Scrape URLs and process files for analysis.</p>
                <br>
                <a href=\"/harvester\" class=\"btn\">Open Harvester</a>
            </div>
            <div class=\"card\">
                <h2>Service Health</h2>
                <div id=\"service-health\">
                    <p class=\"loading\">Loading...</p>
                </div>
            </div>
        </div>
        <script>
            async function loadDashboard() {
                try {
                    // Load models count
                    const models = await apiCall('/analysis/models');
                    document.getElementById('models-count').textContent = models.count || '174+';
                    document.getElementById('categories-count').textContent = 
                        (models.models ? [...new Set(models.models.map(m => m.category))].length : '44+');
                    
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
                } catch (e) {
                    console.error('Dashboard load error:', e);
                }
            }
            loadDashboard();
            setInterval(loadDashboard, 30000);
        </script>">>
    ],
    Html = html_templates:base_layout(<<"Dashboard">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.
