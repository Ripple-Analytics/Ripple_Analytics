%%%-------------------------------------------------------------------
%%% @doc Analysis Handler Template - Part 9
%%% @end
%%%-------------------------------------------------------------------
-module(analysis_handler_template_part9).

-export([content/0]).

content() ->
    <<"
                    alert('No analysis results to export');
                    return;
                }
                
                setStatus('Generating report...', 'yellow');
                
                try {
                    const reportData = {
                        action: 'generate',
                        result: lastAnalysisResult,
                        title: 'Mental Models Analysis Report',
                        format: 'html'
                    };
                    
                    const response = await fetch('/api/analysis/report', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify(reportData)
                    });
                    
                    if (!response.ok) {
                        throw new Error('Failed to generate report');
                    }
                    
                    const blob = await response.blob();
                    const url = URL.createObjectURL(blob);
                    const a = document.createElement('a');
                    a.href = url;
                    a.download = 'mental-models-report-' + new Date().toISOString().split('T')[0] + '.html';
                    a.click();
                    URL.revokeObjectURL(url);
                    
                    setStatus('Report downloaded', 'green');
                } catch (e) {
                    console.error('Report generation error:', e);
                    alert('Failed to generate HTML report: ' + e.message);
                    setStatus('Error', 'red');
                }
            }
        </script>
    ">>.
