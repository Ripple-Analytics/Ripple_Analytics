%%%-------------------------------------------------------------------
%%% @doc Models Handler - Browse mental models
%%%-------------------------------------------------------------------
-module(models_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Content = [
        <<"<div id=\"recommendations-section\" style=\"display: none; margin-bottom: 20px;\">
            <div class=\"card\" style=\"border-left: 4px solid #28a745;\">
                <h2>Recommended for You</h2>
                <p>Based on your analysis history, you might find these models useful:</p>
                <div id=\"recommendations-list\" style=\"margin-top: 15px; display: flex; gap: 10px; flex-wrap: wrap;\"></div>
            </div>
        </div>
        
        <div id=\"trending-section\" style=\"display: none; margin-bottom: 20px;\">
            <div class=\"card\" style=\"border-left: 4px solid #f59e0b;\">
                <h2>Trending Models</h2>
                <p>Most frequently detected models in your recent analyses:</p>
                <div id=\"trending-list\" style=\"margin-top: 15px; display: flex; gap: 10px; flex-wrap: wrap;\"></div>
            </div>
        </div>
        
        <div class=\"card\">
            <h2>Mental Models Library</h2>
            <p>Browse and search through 174+ mental models across multiple categories.</p>
            <br>
            <div style=\"display: flex; gap: 10px; margin-bottom: 15px;\">
                <button class=\"btn\" id=\"all-tab\" onclick=\"showAllModels()\" style=\"flex: 1;\">All Models</button>
                <button class=\"btn btn-secondary\" id=\"favorites-tab\" onclick=\"showFavorites()\" style=\"flex: 1;\">Favorites (<span id=\"fav-count\">0</span>)</button>
                <button class=\"btn btn-secondary\" id=\"recommended-tab\" onclick=\"showRecommended()\" style=\"flex: 1;\">Recommended</button>
            </div>
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
            let favorites = [];
            let showingFavorites = false;
            let recommendedModels = [];
            let trendingModels = [];
            
            async function loadFavorites() {
                try {
                    const res = await fetch('/api/storage/favorites');
                    const data = await res.json();
                    favorites = data.favorites || [];
                    document.getElementById('fav-count').textContent = favorites.length;
                } catch (e) {
                    console.error('Error loading favorites:', e);
                    favorites = [];
                }
            }
            
            function isFavorite(modelName) {
                return favorites.some(f => f.name === modelName || f.id === modelName);
            }
            
            async function toggleFavorite(modelName, category, event) {
                if (event) event.stopPropagation();
                
                if (isFavorite(modelName)) {
                    // Remove from favorites
                    try {
                        await fetch('/api/storage/favorites?model_id=' + encodeURIComponent(modelName), {method: 'DELETE'});
                        favorites = favorites.filter(f => f.name !== modelName && f.id !== modelName);
                    } catch (e) {
                        console.error('Error removing favorite:', e);
                    }
                } else {
                    // Add to favorites
                    try {
                        await fetch('/api/storage/favorites', {
                            method: 'POST',
                            headers: {'Content-Type': 'application/json'},
                            body: JSON.stringify({model_id: modelName, model_name: modelName, category: category})
                        });
                        favorites.push({id: modelName, name: modelName, category: category});
                    } catch (e) {
                        console.error('Error adding favorite:', e);
                    }
                }
                
                document.getElementById('fav-count').textContent = favorites.length;
                if (showingFavorites) {
                    showFavorites();
                } else {
                    filterModels();
                }
            }
            
            function showAllModels() {
                showingFavorites = false;
                document.getElementById('all-tab').className = 'btn';
                document.getElementById('favorites-tab').className = 'btn btn-secondary';
                filterModels();
            }
            
            function showFavorites() {
                showingFavorites = true;
                document.getElementById('all-tab').className = 'btn btn-secondary';
                document.getElementById('favorites-tab').className = 'btn';
                document.getElementById('recommended-tab').className = 'btn btn-secondary';
                
                const favoriteModels = allModels.filter(m => isFavorite(m.name));
                renderModels(favoriteModels);
            }
            
            function showRecommended() {
                showingFavorites = false;
                document.getElementById('all-tab').className = 'btn btn-secondary';
                document.getElementById('favorites-tab').className = 'btn btn-secondary';
                document.getElementById('recommended-tab').className = 'btn';
                
                // Show models that appear frequently in history but aren't favorites yet
                const recommendedNotFavorite = recommendedModels.filter(name => !isFavorite(name));
                const models = allModels.filter(m => recommendedNotFavorite.includes(m.name));
                renderModels(models);
            }
            
            async function loadRecommendations() {
                try {
                    const res = await fetch('/api/storage/history?limit=100');
                    const data = await res.json();
                    const analyses = data.analyses || [];
                    
                    // Count model occurrences
                    const modelCounts = {};
                    for (const analysis of analyses) {
                        const models = analysis.models || [];
                        for (const model of models) {
                            const name = model.name || model;
                            modelCounts[name] = (modelCounts[name] || 0) + 1;
                        }
                    }
                    
                    // Sort by count and get top 10
                    const sorted = Object.entries(modelCounts)
                        .sort((a, b) => b[1] - a[1])
                        .slice(0, 10);
                    
                    trendingModels = sorted.map(([name, count]) => ({name, count}));
                    recommendedModels = sorted.map(([name]) => name);
                    
                    // Show trending section if we have data
                    if (trendingModels.length > 0) {
                        document.getElementById('trending-section').style.display = 'block';
                        let html = '';
                        for (const {name, count} of trendingModels.slice(0, 5)) {
                            html += '<div style=\"background: #fff3cd; padding: 8px 15px; border-radius: 20px; display: flex; align-items: center; gap: 8px;\">';
                            html += '<span style=\"font-weight: bold;\">' + name + '</span>';
                            html += '<span style=\"background: #f59e0b; color: white; padding: 2px 8px; border-radius: 10px; font-size: 11px;\">' + count + 'x</span>';
                            html += '</div>';
                        }
                        document.getElementById('trending-list').innerHTML = html;
                    }
                    
                    // Show recommendations section if we have models not in favorites
                    const recommendedNotFav = recommendedModels.filter(name => !isFavorite(name));
                    if (recommendedNotFav.length > 0) {
                        document.getElementById('recommendations-section').style.display = 'block';
                        let html = '';
                        for (const name of recommendedNotFav.slice(0, 5)) {
                            html += '<div style=\"background: #d1e7dd; padding: 8px 15px; border-radius: 20px; cursor: pointer;\" onclick=\"searchForModel(\\'' + name.replace(/'/g, "\\\\'") + '\\')\">';
                            html += '<span>' + name + '</span>';
                            html += '</div>';
                        }
                        document.getElementById('recommendations-list').innerHTML = html;
                    }
                } catch (e) {
                    console.error('Error loading recommendations:', e);
                }
            }
            
            function searchForModel(name) {
                document.getElementById('search').value = name;
                showAllModels();
                filterModels();
            }
            
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
                    const msg = showingFavorites ? 'No favorites yet. Click the star icon on any model to add it to your favorites.' : 'No models found matching your criteria.';
                    document.getElementById('models-list').innerHTML = 
                        '<div class=\"alert alert-info\">' + msg + '</div>';
                    return;
                }
                
                let html = '<div class=\"grid\">';
                for (const model of models) {
                    const isFav = isFavorite(model.name);
                    const starStyle = isFav ? 'color: #ffc107; font-size: 20px;' : 'color: #ccc; font-size: 20px;';
                    const starChar = isFav ? '&#9733;' : '&#9734;';
                    
                    html += '<div class=\"model-card\" onclick=\"showModelDetails(\\'' + encodeURIComponent(JSON.stringify(model)) + '\\')\" style=\"cursor:pointer;\">';
                    html += '<div style=\"display: flex; justify-content: space-between; align-items: start;\">';
                    html += '<h4 style=\"margin: 0;\">' + model.name + '</h4>';
                    html += '<button onclick=\"toggleFavorite(\\'' + model.name.replace(/'/g, "\\\\'") + '\\', \\'' + model.category + '\\', event)\" style=\"background: none; border: none; cursor: pointer; ' + starStyle + '\" title=\"' + (isFav ? 'Remove from favorites' : 'Add to favorites') + '\">' + starChar + '</button>';
                    html += '</div>';
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
            
            // Initialize: load favorites first, then models, then recommendations
            async function init() {
                await loadFavorites();
                await loadModels();
                await loadRecommendations();
            }
            init();
        </script>">>
    ],
    Html = html_templates:base_layout(<<"Models">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.
