%%%-------------------------------------------------------------------
%%% @doc History Handler Template - Part 4
%%% @end
%%%-------------------------------------------------------------------
-module(history_handler_template_part4).

-export([content/0]).

content() ->
    <<"
                if (selectedForCompare.length < 2) {
                    alert('Select at least 2 analyses to compare');
                    return;
                }
                
                // Fetch full details for each selected analysis
                const analyses = [];
                for (const id of selectedForCompare) {
                    try {
                        const res = await fetch('/api/storage/history/' + id);
                        const data = await res.json();
                        if (data.analysis) analyses.push(data.analysis);
                    } catch (e) {
                        console.error('Error fetching analysis:', e);
                    }
                }
                
                if (analyses.length < 2) {
                    alert('Could not load analyses for comparison');
                    return;
                }
                
                renderComparison(analyses);
            }
            
            let currentComparisonData = [];
            
            function renderComparison(analyses) {
                currentComparisonData = analyses;
                let html = '<div class=\"card\" style=\"margin-bottom: 20px;\">';
                html += '<div style=\"display: flex; justify-content: space-between; align-items: center;\">';
                html += '<h2>Analysis Comparison</h2>';
                html += '<div style=\"display: flex; gap: 10px;\">';
                html += '<button class=\"btn\" onclick=\"exportComparisonCSV()\">Export CSV</button>';
                html += '<button class=\"btn btn-secondary\" onclick=\"exportComparisonJSON()\">Export JSON</button>';
                html += '<button class=\"btn btn-secondary\" onclick=\"document.getElementById(\\'compare-panel\\').style.display=\\'none\\'\">Close</button>';
                html += '</div>';
                html += '</div>';
                
                // Create comparison table
                html += '<div style=\"overflow-x: auto;\"><table style=\"width: 100%; border-collapse: collapse; margin-top: 15px;\">';
                
                // Header row with analysis dates
                html += '<tr style=\"background: #f8f9fa;\"><th style=\"padding: 10px; border: 1px solid #e0e0e0;\">Attribute</th>';
                for (const a of analyses) {
                    const date = new Date(a.timestamp * 1000).toLocaleDateString();
                    html += '<th style=\"padding: 10px; border: 1px solid #e0e0e0;\">' + date + '</th>';
                }
                html += '</tr>';
                
                // Type row
                html += '<tr><td style=\"padding: 10px; border: 1px solid #e0e0e0;\"><strong>Type</strong></td>';
                for (const a of analyses) {
                    html += '<td style=\"padding: 10px; border: 1px solid #e0e0e0;\">' + a.type + '</td>';
                }
                html += '</tr>';
                
                // Model count row
                html += '<tr><td style=\"padding: 10px; border: 1px solid #e0e0e0;\"><strong>Models Detected</strong></td>';
                for (const a of analyses) {
                    html += '<td style=\"padding: 10px; border: 1px solid #e0e0e0;\">' + (a.model_count || 0) + '</td>';
                }
                html += '</tr>';
                
                // Bias count row
                html += '<tr><td style=\"padding: 10px; border: 1px solid #e0e0e0;\"><strong>Biases Detected</strong></td>';
                for (const a of analyses) {
                    html += '<td style=\"padding: 10px; border: 1px solid #e0e0e0;\">' + (a.bias_count || 0) + '</td>';
                }
                html += '</tr>';
                
                // Models list row
                html += '<tr><td style=\"padding: 10px; border: 1px solid #e0e0e0; vertical-align: top;\"><strong>Models</strong></td>';
                for (const a of analyses) {
                    const models = (a.models || []).map(m => m.name || m).join(', ') || 'None';
                    html += '<td style=\"padding: 10px; border: 1px solid #e0e0e0; vertical-align: top; font-size: 12px;\">' + models + '</td>';
                }
                html += '</tr>';
                
                // Biases list row
                html += '<tr><td style=\"padding: 10px; border: 1px solid #e0e0e0; vertical-align: top;\"><strong>Biases</strong></td>';
                for (const a of analyses) {
                    const biases = (a.biases || []).map(b => b.bias || b).join(', ') || 'None';
                    html += '<td style=\"padding: 10px; border: 1px solid #e0e0e0; vertical-align: top; font-size: 12px;\">' + biases + '</td>';
                }
                html += '</tr>';
                
                // Input text row
                html += '<tr><td style=\"padding: 10px; border: 1px solid #e0e0e0; vertical-align: top;\"><strong>Input Preview</strong></td>';
                for (const a of analyses) {
                    html += '<td style=\"padding: 10px; border: 1px solid #e0e0e0; vertical-align: top; font-size: 11px; color: #666;\">' + (a.input_text || 'N/A') + '</td>';
                }
                html += '</tr>';
                
                html += '</table></div>';
                html += '</div>';
                
                document.getElementById('compare-panel').innerHTML = html;
                document.getElementById('compare-panel').style.display = 'block';
                document.getElementById('compare-panel').scrollIntoView({behavior: 'smooth'});
            }
            
            function exportComparisonCSV() {
                if (currentComparisonData.length === 0) {
                    alert('No comparison data to export');
                    return;
                }
                
                let csv = 'Attribute';
                for (const a of currentComparisonData) {
                    csv += ',' + new Date(a.timestamp * 1000).toLocaleDateString();
                }
                csv += '\\n';
                
                csv += 'Type';
                for (const a of currentComparisonData) {
                    csv += ',' + a.type;
                }
                csv += '\\n';
                
                csv += 'Models Detected';
                for (const a of currentComparisonData) {
                    csv += ',' + (a.model_count || 0);
                }
                csv += '\\n';
                
                csv += 'Biases Detected';
                for (const a of currentComparisonData) {
                    csv += ',' + (a.bias_count || 0);
                }
                csv += '\\n';
                
                csv += 'Models';
                for (const a of currentComparisonData) {
                    const models = (a.models || []).map(m => m.name || m).join('; ') || 'None';
                    csv += ',\"' + models.replace(/\"/g, '\"\"') + '\"';
                }
                csv += '\\n';
                
                csv += 'Biases';
                for (const a of currentComparisonData) {
                    const biases = (a.biases || []).map(b => b.bias || b).join('; ') || 'None';
                    csv += ',\"' + biases.replace(/\"/g, '\"\"') + '\"';
                }
                csv += '\\n';
                
                csv += 'Input Text';
                for (const a of currentComparisonData) {
                    csv += ',\"' + (a.input_text || 'N/A').replace(/\"/g, '\"\"') + '\"';
                }
    ">>.
