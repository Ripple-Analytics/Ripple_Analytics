%%%-------------------------------------------------------------------
%%% @doc Models Handler Template - Part 3
%%% @end
%%%-------------------------------------------------------------------
-module(models_handler_template_part3).

-export([content/0]).

content() ->
    <<"
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
        </script>
    ">>.
