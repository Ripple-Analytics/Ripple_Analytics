%%%-------------------------------------------------------------------
%%% @doc Folder Handler Template - Part 1
%%% @end
%%%-------------------------------------------------------------------
-module(folder_handler_template_part1).

-export([content/0]).

content() ->
    <<"
<div class=\"card\">
            <h2>Folder Analysis</h2>
            <p>Automatically scan and analyze all text files in a folder for mental models, patterns, and biases.</p>
            <br>
            
            <div style=\"margin-bottom: 15px;\">
                <label style=\"font-weight: bold; display: block; margin-bottom: 8px;\">Folder Path:</label>
                <div style=\"display: flex; gap: 10px;\">
                    <input type=\"text\" id=\"folder-path\" placeholder=\"Enter folder path or leave empty for default\" style=\"flex: 1; padding: 10px; border: 1px solid #ddd; border-radius: 6px;\">
                    <button class=\"btn btn-secondary\" onclick=\"useHostPath()\">Use Host Path</button>
                </div>
                <p style=\"font-size: 12px; color: #666; margin-top: 5px;\">Host path: <span id=\"host-path-display\">Loading...</span></p>
            </div>
            
            <div style=\"margin-bottom: 15px;\">
                <label style=\"font-weight: bold; display: block; margin-bottom: 8px;\">Options:</label>
                <div style=\"display: flex; gap: 20px; flex-wrap: wrap;\">
                    <label style=\"display: flex; align-items: center; gap: 5px;\">
                        <input type=\"checkbox\" id=\"recursive-scan\"> Scan subfolders recursively
                    </label>
                    <select id=\"analysis-type\" style=\"padding: 8px; border: 1px solid #ddd; border-radius: 6px;\">
                        <option value=\"full\">Full Analysis</option>
                        <option value=\"models\">Models Only</option>
                        <option value=\"patterns\">Patterns Only</option>
                        <option value=\"quick\">Quick Scan</option>
                    </select>
                </div>
            </div>
            
            <div style=\"display: flex; gap: 10px; margin-top: 15px;\">
                <button class=\"btn\" onclick=\"scanFolder()\">Scan Folder</button>
                <button class=\"btn btn-secondary\" onclick=\"analyzeFolder()\">Analyze All Files</button>
            </div>
        </div>
        
        <div id=\"scan-results\" style=\"display: none;\">
            <div class=\"card\">
                <h2>Files Found</h2>
                <div id=\"file-list\"></div>
            </div>
        </div>
        
        <div id=\"analysis-results\"></div>
        
        <script>
            let hostPath = '';
            
            async function loadHostPath() {
                try {
                    const res = await fetch('/api/system/info');
                    const data = await res.json();
                    hostPath = data.host_path || '';
                    document.getElementById('host-path-display').textContent = hostPath || 'Not set';
                } catch (e) {
                    document.getElementById('host-path-display').textContent = 'Error loading';
                }
            }
            
            function useHostPath() {
                if (hostPath) {
                    document.getElementById('folder-path').value = hostPath;
                } else {
                    alert('Host path not available');
                }
            }
            
            async function scanFolder() {
                const folder = document.getElementById('folder-path').value || hostPath;
                const recursive = document.getElementById('recursive-scan').checked;
                
                if (!folder) {
                    alert('Please enter a folder path or set HOST_PATH');
                    return;
                }
                
                document.getElementById('scan-results').style.display = 'none';
                document.getElementById('analysis-results').innerHTML = '<div class=\"loading\">Scanning folder...</div>';
                
                try {
                    const res = await fetch('/api/analysis/folder', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({
                            action: 'scan',
                            folder: folder,
                            recursive: recursive
                        })
                    });
                    const data = await res.json();
                    
                    if (data.success) {
                        let html = '<p>Found <strong>' + data.file_count + '</strong> files in ' + data.folder + '</p>';
                        html += '<p style=\"font-size: 12px; color: #666;\">Supported extensions: ' + data.extensions.join(', ') + '</p><br>';
                        
                        if (data.files && data.files.length > 0) {
                            html += '<table style=\"width: 100%; border-collapse: collapse;\">';
                            html += '<tr style=\"background: #f8f9fa;\"><th style=\"padding: 10px; text-align: left;\">File</th><th style=\"padding: 10px; text-align: right;\">Size</th><th style=\"padding: 10px; text-align: center;\">Action</th></tr>';
                            for (const file of data.files) {
                                html += '<tr style=\"border-bottom: 1px solid #eee;\">';
                                html += '<td style=\"padding: 10px;\">' + file.name + '</td>';
                                html += '<td style=\"padding: 10px; text-align: right;\">' + formatSize(file.size) + '</td>';
                                html += '<td style=\"padding: 10px; text-align: center;\"><button class=\"btn btn-secondary\" style=\"padding: 4px 12px; font-size: 12px;\" onclick=\"analyzeFile(\\'' + escapeQuotes(file.path) + '\\')\">Analyze</button></td>';
                                html += '</tr>';
                            }
                            html += '</table>';
                        } else {
                            html += '<p>No supported files found in this folder.</p>';
                        }
                        
                        document.getElementById('file-list').innerHTML = html;
                        document.getElementById('scan-results').style.display = 'block';
                        document.getElementById('analysis-results').innerHTML = '';
                    } else {
                        document.getElementById('analysis-results').innerHTML = '<div class=\"alert alert-error\">Error: ' + (data.error || 'Unknown error') + '</div>';
                    }
                } catch (e) {
                    document.getElementById('analysis-results').innerHTML = '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function analyzeFolder() {
                const folder = document.getElementById('folder-path').value || hostPath;
                const recursive = document.getElementById('recursive-scan').checked;
                const analysisType = document.getElementById('analysis-type').value;
                
                if (!folder) {
                    alert('Please enter a folder path or set HOST_PATH');
                    return;
                }
                
                document.getElementById('scan-results').style.display = 'none';
                document.getElementById('analysis-results').innerHTML = '<div class=\"loading\">Analyzing all files in folder... This may take a moment.</div>';
                
                try {
                    const res = await fetch('/api/analysis/folder', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({
                            action: 'analyze',
                            folder: folder,
                            recursive: recursive,
                            analysis_type: analysisType
                        })
                    });
                    const data = await res.json();
                    
                    if (data.success) {
                        let html = '';
                        
                        // Summary card
    ">>.
