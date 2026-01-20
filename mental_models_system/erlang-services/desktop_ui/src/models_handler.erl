%%%-------------------------------------------------------------------
%%% @doc Models Handler - Browse mental models
%%%-------------------------------------------------------------------
-module(models_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Content = [
        <<"<div class=\"card\">
            <h2>Mental Models Library</h2>
            <p>Browse and search through 174+ mental models across multiple categories.</p>
            <br>
            <input type=\"text\" id=\"search\" placeholder=\"Search models...\" onkeyup=\"filterModels()\">
            <select id=\"category-filter\" onchange=\"filterModels()\" style=\"padding: 12px; border-radius: 6px; border: 1px solid #e0e0e0; margin-left: 10px;\">
                <option value=\"\">All Categories</option>
            </select>
        </div>
        <div id=\"models-list\">
            <div class=\"loading\">Loading models...</div>
        </div>
        <script>
            let allModels = [];
            
            async function loadModels() {
                try {
                    const res = await fetch('/api/analysis/models');
                    const data = await res.json();
                    allModels = data.models || [];
                    
                    // Populate categories
                    const categories = [...new Set(allModels.map(m => m.category))].sort();
                    const select = document.getElementById('category-filter');
                    for (const cat of categories) {
                        const opt = document.createElement('option');
                        opt.value = cat;
                        opt.textContent = cat;
                        select.appendChild(opt);
                    }
                    
                    renderModels(allModels);
                } catch (e) {
                    document.getElementById('models-list').innerHTML = 
                        '<div class=\"alert alert-error\">Error loading models: ' + e.message + '</div>';
                }
            }
            
            function filterModels() {
                const search = document.getElementById('search').value.toLowerCase();
                const category = document.getElementById('category-filter').value;
                
                const filtered = allModels.filter(m => {
                    const matchesSearch = !search || 
                        m.name.toLowerCase().includes(search) || 
                        m.description.toLowerCase().includes(search);
                    const matchesCategory = !category || m.category === category;
                    return matchesSearch && matchesCategory;
                });
                
                renderModels(filtered);
            }
            
            function renderModels(models) {
                if (models.length === 0) {
                    document.getElementById('models-list').innerHTML = 
                        '<div class=\"alert alert-info\">No models found matching your criteria.</div>';
                    return;
                }
                
                let html = '<div class=\"grid\">';
                for (const model of models) {
                    html += '<div class=\"model-card\" onclick=\"showModelDetails(\\'' + encodeURIComponent(JSON.stringify(model)) + '\\')\" style=\"cursor:pointer;\">';
                    html += '<h4>' + model.name + '</h4>';
                    html += '<span class=\"category\">' + model.category + '</span>';
                    html += '<p>' + model.description + '</p>';
                    if (model.key_insight) {
                        html += '<p style=\"margin-top:10px;font-style:italic;color:#666;\"><strong>Key Insight:</strong> ' + model.key_insight + '</p>';
                    }
                    if (model.failure_modes && model.failure_modes.length > 0) {
                        html += '<p style=\"margin-top:10px;font-size:12px;\"><strong>Failure Modes:</strong> ' + model.failure_modes.join(', ') + '</p>';
                    }
                    html += '<div style=\"margin-top:10px;\">';
                    html += '<button class=\"btn btn-secondary\" onclick=\"event.stopPropagation(); analyzeWithModel(\\'' + model.name + '\\')\" style=\"font-size:12px;padding:5px 10px;\">Use in Analysis</button>';
                    html += '</div>';
                    html += '</div>';
                }
                html += '</div>';
                html += '<p style=\"margin-top:20px;color:#666;\">Showing ' + models.length + ' of ' + allModels.length + ' models</p>';
                document.getElementById('models-list').innerHTML = html;
            }
            
            function showModelDetails(encodedModel) {
                const model = JSON.parse(decodeURIComponent(encodedModel));
                let html = '<div class=\"modal-overlay\" onclick=\"closeModal()\">';
                html += '<div class=\"modal-content\" onclick=\"event.stopPropagation()\">';
                html += '<button class=\"modal-close\" onclick=\"closeModal()\">&times;</button>';
                html += '<h2>' + model.name + '</h2>';
                html += '<span class=\"category\">' + model.category + '</span>';
                html += '<p style=\"margin-top:15px;\">' + model.description + '</p>';
                if (model.key_insight) {
                    html += '<div style=\"margin-top:15px;padding:15px;background:#f8f9fa;border-radius:8px;\">';
                    html += '<strong>Key Insight:</strong><br>' + model.key_insight;
                    html += '</div>';
                }
                if (model.keywords && model.keywords.length > 0) {
                    html += '<div style=\"margin-top:15px;\"><strong>Keywords:</strong> ';
                    html += model.keywords.map(k => '<span style=\"background:#e9ecef;padding:2px 8px;border-radius:4px;margin-right:5px;\">' + k + '</span>').join('');
                    html += '</div>';
                }
                if (model.failure_modes && model.failure_modes.length > 0) {
                    html += '<div style=\"margin-top:15px;\"><strong>Failure Modes:</strong><ul>';
                    for (const fm of model.failure_modes) {
                        html += '<li>' + fm + '</li>';
                    }
                    html += '</ul></div>';
                }
                html += '<div style=\"margin-top:20px;\">';
                html += '<button class=\"btn\" onclick=\"analyzeWithModel(\\'' + model.name + '\\')\">Use in Analysis</button>';
                html += '</div>';
                html += '</div></div>';
                document.body.insertAdjacentHTML('beforeend', html);
            }
            
            function closeModal() {
                const modal = document.querySelector('.modal-overlay');
                if (modal) modal.remove();
            }
            
            function analyzeWithModel(modelName) {
                // Store the model name and redirect to analysis page
                sessionStorage.setItem('selectedModel', modelName);
                window.location.href = '/analysis';
            }
            
            loadModels();
        </script>">>
    ],
    Html = html_templates:base_layout(<<"Models">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.
