%%%-------------------------------------------------------------------
%%% @doc Models Handler Template - Part 1
%%% @end
%%%-------------------------------------------------------------------
-module(models_handler_template_part1).

-export([content/0]).

content() ->
    <<"
<div id=\"recommendations-section\" style=\"display: none; margin-bottom: 20px;\">
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
            
            async function toggleFavorite(encodedModelName, encodedCategory, event) {
                if (event) event.stopPropagation();
                const modelName = decodeURIComponent(encodedModelName);
                const category = decodeURIComponent(encodedCategory);
                
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
    ">>.
