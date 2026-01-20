%%%-------------------------------------------------------------------
%%% @doc Analysis Handler Template - Part 2
%%% @end
%%%-------------------------------------------------------------------
-module(analysis_handler_template_part2).

-export([content/0]).

content() ->
    <<"
            function handleFileUpload(event) {
                const files = Array.from(event.target.files);
                files.forEach(file => {
                    const reader = new FileReader();
                    reader.onload = (e) => {
                        importedFiles.push({
                            name: file.name,
                            type: file.type || getFileType(file.name),
                            content: e.target.result,
                            size: file.size
                        });
                        updateImportPreview();
                    };
                    reader.readAsText(file);
                });
            }
            
            function getFileType(filename) {
                const ext = filename.split('.').pop().toLowerCase();
                const types = {txt: 'text/plain', csv: 'text/csv', json: 'application/json', md: 'text/markdown'};
                return types[ext] || 'text/plain';
            }
            
            function updateImportPreview() {
                const preview = document.getElementById('import-preview');
                const list = document.getElementById('import-files-list');
                const count = document.getElementById('file-count');
                
                if (importedFiles.length > 0) {
                    preview.style.display = 'block';
                    count.textContent = importedFiles.length + ' file(s)';
                    list.innerHTML = importedFiles.map((f, i) => 
                        '<div style=\"display: flex; justify-content: space-between; align-items: center; padding: 3px 0;\">' +
                        '<span>' + f.name + ' (' + formatSize(f.size) + ')</span>' +
                        '<button onclick=\"removeFile(' + i + ')\" style=\"background: none; border: none; color: white; cursor: pointer; font-size: 14px;\">x</button>' +
                        '</div>'
                    ).join('');
                } else {
                    preview.style.display = 'none';
                    count.textContent = '';
                }
            }
            
            function formatSize(bytes) {
                if (bytes < 1024) return bytes + ' B';
                if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(1) + ' KB';
                return (bytes / (1024 * 1024)).toFixed(1) + ' MB';
            }
            
            function removeFile(index) {
                importedFiles.splice(index, 1);
                updateImportPreview();
            }
            
            function clearImportedFiles() {
                importedFiles = [];
                document.getElementById('file-upload').value = '';
                updateImportPreview();
            }
            
            function showPasteModal() {
                document.getElementById('paste-modal').style.display = 'flex';
            }
            
            function closePasteModal() {
                document.getElementById('paste-modal').style.display = 'none';
            }
            
            function importPastedData() {
                const data = document.getElementById('bulk-paste-area').value.trim();
                if (!data) { alert('Please paste some data first'); return; }
                
                try {
                    const jsonData = JSON.parse(data);
                    if (Array.isArray(jsonData)) {
                        jsonData.forEach((item, i) => {
                            const text = typeof item === 'string' ? item : (item.text || item.content || JSON.stringify(item));
                            importedFiles.push({ name: 'item-' + (i + 1) + '.txt', type: 'text/plain', content: text, size: text.length });
                        });
                    }
                } catch (e) {
                    if (data.includes(',') && data.includes('\\n')) {
                        const lines = data.split('\\n');
                        const headers = lines[0].split(',').map(h => h.trim());
                        const textCol = headers.findIndex(h => h.toLowerCase().includes('text') || h.toLowerCase().includes('content'));
                        if (textCol >= 0) {
                            lines.slice(1).forEach((line, i) => {
                                const cols = line.split(',');
                                if (cols[textCol]) {
                                    importedFiles.push({ name: 'row-' + (i + 1) + '.txt', type: 'text/plain', content: cols[textCol].replace(/^\"|\"$/g, ''), size: cols[textCol].length });
                                }
                            });
                        } else {
                            lines.forEach((line, i) => {
                                if (line.trim()) importedFiles.push({ name: 'line-' + (i + 1) + '.txt', type: 'text/plain', content: line.trim(), size: line.length });
                            });
                        }
                    } else {
                        const entries = data.split(/\\n\\s*\\n/).filter(e => e.trim());
                        entries.forEach((entry, i) => {
                            importedFiles.push({ name: 'entry-' + (i + 1) + '.txt', type: 'text/plain', content: entry.trim(), size: entry.length });
                        });
                    }
                }
                updateImportPreview();
                closePasteModal();
                document.getElementById('bulk-paste-area').value = '';
            }
            
            async function processBatchData() {
                if (importedFiles.length === 0) { alert('No files to process'); return; }
                
                setStatus('Processing batch...', 'yellow');
                document.getElementById('results').innerHTML = '<div class=\"loading\">Processing ' + importedFiles.length + ' items...</div>';
                batchResults = [];
                
                for (let i = 0; i < importedFiles.length; i++) {
                    const file = importedFiles[i];
                    document.getElementById('results').innerHTML = '<div class=\"loading\">Processing item ' + (i + 1) + ' of ' + importedFiles.length + '...</div>';
                    
                    try {
                        const res = await fetch('/api/analysis/comprehensive', {
                            method: 'POST',
                            headers: {'Content-Type': 'application/json'},
                            body: JSON.stringify({text: file.content, top_n: 5})
                        });
                        const data = await res.json();
                        batchResults.push({ file: file.name, success: true, result: data });
                    } catch (e) {
                        batchResults.push({ file: file.name, success: false, error: e.message });
                    }
                }
                
                displayBatchResults();
                setStatus('Batch complete', 'green');
            }
            
            function displayBatchResults() {
                let html = '<div class=\"card\">';
                html += '<h2>Batch Analysis Results</h2>';
                html += '<p style=\"font-size: 12px; color: #666;\">Processed ' + batchResults.length + ' items</p>';
                html += '<div style=\"margin-top: 10px; display: flex; gap: 8px;\">';
                html += '<button class=\"btn\" onclick=\"exportBatchJSON()\" style=\"font-size: 11px;\">Export All as JSON</button>';
                html += '<button class=\"btn btn-secondary\" onclick=\"exportBatchCSV()\" style=\"font-size: 11px;\">Export Summary CSV</button>';
                html += '</div></div>';
                
                batchResults.forEach((item) => {
                    html += '<div class=\"card\" style=\"margin-top: 10px;\">';
                    html += '<div style=\"display: flex; justify-content: space-between; align-items: center;\">';
                    html += '<h4 style=\"margin: 0;\">' + item.file + '</h4>';
    ">>.
