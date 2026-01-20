%%%-------------------------------------------------------------------
%%% @doc Folder Handler Template - Part 3
%%% @end
%%%-------------------------------------------------------------------
-module(folder_handler_template_part3).

-export([content/0]).

content() ->
    <<"
                                if (insight.action) {
                                    html += '<p style=\"margin-top: 10px;\"><strong>Action:</strong> ' + insight.action + '</p>';
                                }
                                html += '</div>';
                            }
                        }
                        
                        html += '</div>';
                        document.getElementById('analysis-results').innerHTML = html;
                    } else {
                        document.getElementById('analysis-results').innerHTML = '<div class=\"alert alert-error\">Error: ' + (data.error || 'Unknown error') + '</div>';
                    }
                } catch (e) {
                    document.getElementById('analysis-results').innerHTML = '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            function toggleFileDetails(element) {
                const details = element.querySelector('.file-details');
                if (details) {
                    details.style.display = details.style.display === 'none' ? 'block' : 'none';
                }
            }
            
            function formatSize(bytes) {
                if (!bytes) return '0 B';
                const sizes = ['B', 'KB', 'MB', 'GB'];
                const i = Math.floor(Math.log(bytes) / Math.log(1024));
                return (bytes / Math.pow(1024, i)).toFixed(1) + ' ' + sizes[i];
            }
            
            function escapeQuotes(str) {
                return str.replace(/'/g, \"\\\\'\").replace(/\"/g, '\\\\\"');
            }
            
            // Load host path on page load
            loadHostPath();
        </script>
    ">>.
