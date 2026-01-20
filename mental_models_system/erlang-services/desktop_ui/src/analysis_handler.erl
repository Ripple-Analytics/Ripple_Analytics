%%%-------------------------------------------------------------------
%%% @doc Analysis Handler - Mental model analysis page
%%%-------------------------------------------------------------------
-module(analysis_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Content = [
        <<"<div class=\"card\">
            <h2>Analyze Text</h2>
            <p>Enter text to identify relevant mental models and detect cognitive biases.</p>
            <br>
            <textarea id=\"analysis-text\" placeholder=\"Enter text to analyze...\"></textarea>
            <div style=\"display: flex; gap: 10px; margin-top: 10px;\">
                <button class=\"btn\" onclick=\"analyzeText()\">Analyze for Models</button>
                <button class=\"btn btn-secondary\" onclick=\"detectBiases()\">Detect Biases</button>
            </div>
        </div>
        <div id=\"results\"></div>
        <script>
            async function analyzeText() {
                const text = document.getElementById('analysis-text').value;
                if (!text.trim()) {
                    alert('Please enter some text to analyze');
                    return;
                }
                
                document.getElementById('results').innerHTML = '<div class=\"loading\">Analyzing...</div>';
                
                try {
                    const res = await fetch('/api/analysis/analyze', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({text: text, top_n: 5})
                    });
                    const data = await res.json();
                    
                    let html = '<div class=\"card\"><h2>Analysis Results</h2>';
                    if (data.models && data.models.length > 0) {
                        html += '<p>Found ' + data.models.length + ' relevant mental models:</p><br>';
                        for (const model of data.models) {
                            html += '<div class=\"model-card\">';
                            html += '<h4>' + model.name + '</h4>';
                            html += '<span class=\"category\">' + model.category + '</span>';
                            html += '<p>' + model.description + '</p>';
                            if (model.relevance) {
                                html += '<p><strong>Relevance:</strong> ' + model.relevance + '</p>';
                            }
                            html += '</div>';
                        }
                    } else {
                        html += '<p>No specific mental models detected. Try providing more context.</p>';
                    }
                    html += '<p style=\"margin-top:15px;font-size:12px;color:#666;\">Method: ' + (data.method || 'keyword_matching') + '</p>';
                    html += '</div>';
                    document.getElementById('results').innerHTML = html;
                } catch (e) {
                    document.getElementById('results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function detectBiases() {
                const text = document.getElementById('analysis-text').value;
                if (!text.trim()) {
                    alert('Please enter some text to analyze');
                    return;
                }
                
                document.getElementById('results').innerHTML = '<div class=\"loading\">Detecting biases...</div>';
                
                try {
                    const res = await fetch('/api/analysis/detect-biases', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({text: text})
                    });
                    const data = await res.json();
                    
                    let html = '<div class=\"card\"><h2>Bias Detection Results</h2>';
                    if (data.biases && data.biases.length > 0) {
                        html += '<p>Found ' + data.biases.length + ' potential cognitive biases:</p><br>';
                        for (const bias of data.biases) {
                            const severityClass = bias.severity === 'high' ? 'status-unhealthy' : 
                                                  bias.severity === 'medium' ? 'status-unknown' : 'status-healthy';
                            html += '<div class=\"model-card\">';
                            html += '<h4>' + bias.bias.replace(/_/g, ' ').replace(/\\b\\w/g, l => l.toUpperCase()) + '</h4>';
                            html += '<span class=\"' + severityClass + '\">Severity: ' + bias.severity + '</span>';
                            if (bias.evidence) {
                                html += '<p><strong>Evidence:</strong> ' + bias.evidence.join(', ') + '</p>';
                            }
                            html += '</div>';
                        }
                    } else {
                        html += '<p>No obvious cognitive biases detected.</p>';
                    }
                    html += '</div>';
                    document.getElementById('results').innerHTML = html;
                } catch (e) {
                    document.getElementById('results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
        </script>">>
    ],
    Html = html_templates:base_layout(<<"Analysis">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.
