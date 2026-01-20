%%%-------------------------------------------------------------------
%%% @doc Models Handler Template - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(models_handler_template_part2).

-export([content/0]).

content() ->
    <<"
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
                            html += '<div style=\"background: #d1e7dd; padding: 8px 15px; border-radius: 20px; cursor: pointer;\" onclick=\"searchForModel(\\'' + encodeURIComponent(name) + '\\')\">';
                            html += '<span>' + name + '</span>';
                            html += '</div>';
                        }
                        document.getElementById('recommendations-list').innerHTML = html;
                    }
                } catch (e) {
                    console.error('Error loading recommendations:', e);
                }
            }
            
            function searchForModel(encodedName) {
                document.getElementById('search').value = decodeURIComponent(encodedName);
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
                    var msg;
                    if (showingFavorites) {
                        msg = 'No favorites yet. Click the star icon on any model to add it to your favorites.';
                    } else {
                        msg = 'No models found matching your criteria.';
                    }
                    document.getElementById('models-list').innerHTML = 
                        '<div class=\"alert alert-info\">' + msg + '</div>';
                    return;
                }
                
                let html = '<div class=\"grid\">';
                for (const model of models) {
                    const isFav = isFavorite(model.name);
                    var starStyle, starChar, starTitle;
                    if (isFav) {
                        starStyle = 'color: #ffc107; font-size: 20px;';
                        starChar = '&#9733;';
                        starTitle = 'Remove from favorites';
                    } else {
                        starStyle = 'color: #ccc; font-size: 20px;';
                        starChar = '&#9734;';
                        starTitle = 'Add to favorites';
                    }
                    
                    html += '<div class=\"model-card\" onclick=\"showModelDetails(\\'' + encodeURIComponent(JSON.stringify(model)) + '\\')\" style=\"cursor:pointer;\">';
                    html += '<div style=\"display: flex; justify-content: space-between; align-items: start;\">';
                    html += '<h4 style=\"margin: 0;\">' + model.name + '</h4>';
                    html += '<button onclick=\"toggleFavorite(\\'' + encodeURIComponent(model.name) + '\\', \\'' + encodeURIComponent(model.category) + '\\', event)\" style=\"background: none; border: none; cursor: pointer; ' + starStyle + '\" title=\"' + starTitle + '\">' + starChar + '</button>';
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
            
    ">>.
