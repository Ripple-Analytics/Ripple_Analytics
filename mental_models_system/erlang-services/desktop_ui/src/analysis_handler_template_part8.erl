%%%-------------------------------------------------------------------
%%% @doc Analysis Handler Template - Part 8
%%% @end
%%%-------------------------------------------------------------------
-module(analysis_handler_template_part8).

-export([content/0]).

content() ->
    <<"
                if (lastAnalysisType === 'full') {
                    const comp = lastAnalysisResult.comprehensive || {};
                    const lolla = comp.lollapalooza || {};
                    
                    if (lolla.detected) {
                        md += '## LOLLAPALOOZA EFFECT DETECTED\\n\\n';
                        md += '**Converging Models:** ' + (lolla.converging_models || []).join(', ') + '\\n';
                        md += '**Strength:** ' + lolla.strength + '\\n';
                        md += '**Convergence Score:** ' + lolla.convergence_score + '%\\n\\n';
                    }
                    
                    md += '## Mental Models\\n\\n';
                    for (const model of (comp.analysis || [])) {
                        md += '### ' + model.name + ' (' + model.relevance + '%)\\n';
                        md += '**Category:** ' + model.category + '\\n\\n';
                        md += model.description + '\\n\\n';
                    }
                    
                    const biases = lastAnalysisResult.biases?.biases || [];
                    if (biases.length > 0) {
                        md += '## Cognitive Biases\\n\\n';
                        for (const bias of biases) {
                            md += '- **' + bias.bias.replace(/_/g, ' ') + '** (Severity: ' + bias.severity + ')\\n';
                        }
                        md += '\\n';
                    }
                }
                
                const blob = new Blob([md], {type: 'text/markdown'});
                const url = URL.createObjectURL(blob);
                const a = document.createElement('a');
                a.href = url;
                a.download = 'mental-models-analysis-' + new Date().toISOString().split('T')[0] + '.md';
                a.click();
                URL.revokeObjectURL(url);
            }
            
            function exportPDF() {
                if (!lastAnalysisResult) {
                    alert('No analysis results to export');
                    return;
                }
                
                let content = '<html><head><title>Mental Models Analysis Report</title>';
                content += '<style>';
                content += 'body{font-family:Arial,sans-serif;padding:40px;max-width:800px;margin:0 auto;font-size:12px;}';
                content += 'h1{color:#4361ee;font-size:24px;}h2{color:#3f37c9;margin-top:25px;font-size:18px;}';
                content += '.model{border:1px solid #e0e0e0;padding:12px;margin:8px 0;border-radius:6px;}';
                content += '.category{background:#4895ef;color:white;padding:2px 8px;border-radius:4px;font-size:11px;}';
                content += '.lolla{background:linear-gradient(135deg,#667eea,#764ba2);color:white;padding:20px;border-radius:8px;margin-bottom:20px;}';
                content += '</style></head><body>';
                content += '<h1>Mental Models Analysis Report</h1>';
                content += '<p>Generated: ' + new Date().toLocaleString() + '</p>';
                
                if (lastAnalysisType === 'full') {
                    const comp = lastAnalysisResult.comprehensive || {};
                    const lolla = comp.lollapalooza || {};
                    
                    if (lolla.detected) {
                        content += '<div class=\"lolla\">';
                        content += '<h2 style=\"color:white;margin-top:0;\">LOLLAPALOOZA EFFECT DETECTED</h2>';
                        content += '<p><strong>Converging Models:</strong> ' + (lolla.converging_models || []).join(', ') + '</p>';
                        content += '<p><strong>Strength:</strong> ' + lolla.strength + ' | <strong>Score:</strong> ' + lolla.convergence_score + '%</p>';
                        content += '</div>';
                    }
                    
                    content += '<h2>Mental Models Detected</h2>';
                    for (const model of (comp.analysis || [])) {
                        content += '<div class=\"model\">';
                        content += '<h3 style=\"margin:0 0 5px 0;\">' + model.name + ' <span style=\"color:#28a745;\">(' + model.relevance + '%)</span></h3>';
                        content += '<span class=\"category\">' + model.category + '</span>';
                        content += '<p style=\"margin-top:10px;\">' + model.description + '</p>';
                        content += '</div>';
                    }
                    
                    const biases = lastAnalysisResult.biases?.biases || [];
                    if (biases.length > 0) {
                        content += '<h2>Cognitive Biases Detected</h2>';
                        for (const bias of biases) {
                            content += '<div class=\"model\">';
                            content += '<strong>' + bias.bias.replace(/_/g, ' ') + '</strong> - Severity: ' + bias.severity;
                            content += '</div>';
                        }
                    }
                }
                
                content += '</body></html>';
                
                const printWindow = window.open('', '_blank');
                printWindow.document.write(content);
                printWindow.document.close();
                printWindow.print();
            }
            
            function copyToClipboard() {
                if (!lastAnalysisResult) {
                    alert('No analysis results to copy');
                    return;
                }
                
                let text = 'MENTAL MODELS ANALYSIS REPORT\\n';
                text += '============================\\n';
                text += 'Generated: ' + new Date().toLocaleString() + '\\n\\n';
                
                if (lastAnalysisType === 'full') {
                    const comp = lastAnalysisResult.comprehensive || {};
                    const lolla = comp.lollapalooza || {};
                    
                    if (lolla.detected) {
                        text += '*** LOLLAPALOOZA EFFECT DETECTED ***\\n';
                        text += 'Converging Models: ' + (lolla.converging_models || []).join(', ') + '\\n';
                        text += 'Strength: ' + lolla.strength + '\\n\\n';
                    }
                    
                    text += 'MENTAL MODELS:\\n';
                    for (const model of (comp.analysis || [])) {
                        text += '- ' + model.name + ' (' + model.relevance + '%) - ' + model.category + '\\n';
                        text += '  ' + model.description + '\\n\\n';
                    }
                    
                    const biases = lastAnalysisResult.biases?.biases || [];
                    if (biases.length > 0) {
                        text += 'COGNITIVE BIASES:\\n';
                        for (const bias of biases) {
                            text += '- ' + bias.bias.replace(/_/g, ' ') + ' (Severity: ' + bias.severity + ')\\n';
                        }
                    }
                } else if (lastAnalysisType === 'models') {
                    text += 'MENTAL MODELS:\\n';
                    for (const model of (lastAnalysisResult.models || [])) {
                        text += '- ' + model.name + ' (' + model.category + ')\\n';
                        text += '  ' + model.description + '\\n\\n';
                    }
                } else if (lastAnalysisType === 'biases') {
                    text += 'COGNITIVE BIASES:\\n';
                    for (const bias of (lastAnalysisResult.biases || [])) {
                        text += '- ' + bias.bias.replace(/_/g, ' ') + ' (Severity: ' + bias.severity + ')\\n';
                    }
                }
                
                navigator.clipboard.writeText(text).then(() => {
                    alert('Results copied to clipboard!');
                }).catch(err => {
                    console.error('Failed to copy:', err);
                    alert('Failed to copy to clipboard');
                });
            }
            
            async function exportHTMLReport() {
                if (!lastAnalysisResult) {
    ">>.
