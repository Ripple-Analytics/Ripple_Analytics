%%%-------------------------------------------------------------------
%%% @doc Settings Handler Template - Part 3
%%% @end
%%%-------------------------------------------------------------------
-module(settings_handler_template_part3).

-export([content/0]).

content() ->
    <<"
                        document.getElementById('update-status').innerHTML = 
                            '<div class=\"alert alert-success\">' + data.message + '</div>';
                        setTimeout(loadUpdateStatus, 5000);
                    } else {
                        document.getElementById('update-status').innerHTML = 
                            '<div class=\"alert alert-error\">Failed to trigger update</div>';
                    }
                } catch (e) {
                    document.getElementById('update-status').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function restartUpdater() {
                document.getElementById('update-status').innerHTML = '<p class=\"loading\">Restarting auto-updater...</p>';
                try {
                    const res = await fetch('/api/update/restart', {method: 'POST'});
                    const data = await res.json();
                    
                    if (data.success) {
                        document.getElementById('update-status').innerHTML = 
                            '<div class=\"alert alert-success\">' + data.message + '</div>';
                        setTimeout(loadUpdateStatus, 3000);
                    } else {
                        document.getElementById('update-status').innerHTML = 
                            '<div class=\"alert alert-error\">Failed to restart updater</div>';
                    }
                } catch (e) {
                    document.getElementById('update-status').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function saveConfig() {
                const statusEl = document.getElementById('config-status');
                statusEl.textContent = 'Saving...';
                statusEl.style.color = '#666';
                
                const config = {};
                
                const token = document.getElementById('github-token').value;
                if (token) config.github_token = token;
                
                const gdrive = document.getElementById('gdrive-url').value;
                if (gdrive) config.gdrive_url = gdrive;
                
                const interval = parseInt(document.getElementById('check-interval').value);
                if (interval >= 60 && interval <= 3600) config.check_interval = interval;
                
                try {
                    const res = await fetch('/api/update/config', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify(config)
                    });
                    const data = await res.json();
                    
                    if (data.success) {
                        statusEl.textContent = 'Saved! Restart updater to apply.';
                        statusEl.style.color = '#28a745';
                        document.getElementById('github-token').value = '';
                        loadConfig();
                    } else {
                        statusEl.textContent = 'Failed to save';
                        statusEl.style.color = '#dc3545';
                    }
                } catch (e) {
                    statusEl.textContent = 'Error: ' + e.message;
                    statusEl.style.color = '#dc3545';
                }
            }
            
            // Dark mode and appearance functions
            function toggleDarkMode() {
                const isDark = document.getElementById('dark-mode-toggle').checked;
                localStorage.setItem('darkMode', isDark);
                applyTheme(isDark);
            }
            
            function applyTheme(isDark) {
                if (isDark) {
                    document.body.style.backgroundColor = '#1a1a2e';
                    document.body.style.color = '#e0e0e0';
                    document.querySelectorAll('.card').forEach(el => {
                        el.style.backgroundColor = '#16213e';
                        el.style.borderColor = '#0f3460';
                    });
                    document.querySelectorAll('.model-card').forEach(el => {
                        el.style.backgroundColor = '#16213e';
                        el.style.borderColor = '#0f3460';
                    });
                    document.querySelectorAll('.stat-card').forEach(el => {
                        el.style.backgroundColor = '#16213e';
                        el.style.borderColor = '#0f3460';
                    });
                    document.querySelectorAll('input, textarea, select').forEach(el => {
                        el.style.backgroundColor = '#0f3460';
                        el.style.color = '#e0e0e0';
                        el.style.borderColor = '#1a1a2e';
                    });
                    document.querySelectorAll('.nav a').forEach(el => {
                        if (!el.classList.contains('active')) {
                            el.style.backgroundColor = '#16213e';
                            el.style.color = '#e0e0e0';
                            el.style.borderColor = '#0f3460';
                        }
                    });
                } else {
                    document.body.style.backgroundColor = '#ffffff';
                    document.body.style.color = '#1a1a2e';
                    document.querySelectorAll('.card').forEach(el => {
                        el.style.backgroundColor = '#f8f9fa';
                        el.style.borderColor = '#e0e0e0';
                    });
                    document.querySelectorAll('.model-card').forEach(el => {
                        el.style.backgroundColor = 'white';
                        el.style.borderColor = '#e0e0e0';
                    });
                    document.querySelectorAll('.stat-card').forEach(el => {
                        el.style.backgroundColor = 'white';
                        el.style.borderColor = '#e0e0e0';
                    });
                    document.querySelectorAll('input, textarea, select').forEach(el => {
                        el.style.backgroundColor = 'white';
                        el.style.color = '#1a1a2e';
                        el.style.borderColor = '#e0e0e0';
                    });
                    document.querySelectorAll('.nav a').forEach(el => {
                        if (!el.classList.contains('active')) {
                            el.style.backgroundColor = '#f8f9fa';
                            el.style.color = '#1a1a2e';
                            el.style.borderColor = '#e0e0e0';
                        }
                    });
                }
            }
            
            function changeFontSize() {
                const size = document.getElementById('font-size-select').value;
                localStorage.setItem('fontSize', size);
                applyFontSize(size);
            }
            
            function applyFontSize(size) {
                const sizes = {small: '13px', medium: '14px', large: '16px'};
                document.body.style.fontSize = sizes[size] || '14px';
            }
            
            function loadAppearanceSettings() {
                const isDark = localStorage.getItem('darkMode') === 'true';
    ">>.
