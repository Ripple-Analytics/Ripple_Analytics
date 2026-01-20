%%%-------------------------------------------------------------------
%%% @doc HTML Templates Chunk 3
%%% @end
%%%-------------------------------------------------------------------
-module(html_templates_chunk3).

-export([styles/0]).

styles() ->
    <<"
            }
            
            // Single key shortcuts (when not in input)
            switch(e.key) {
                case '?': // Show help
                    showKeyboardShortcuts();
                    break;
                case 'Escape': // Close modals
                    const modal = document.querySelector('.modal-overlay');
                    if (modal) modal.remove();
                    const shortcuts = document.getElementById('shortcuts-modal');
                    if (shortcuts) shortcuts.remove();
                    break;
            }
        });
        
        // System notifications
        let notificationQueue = [];
        let notificationContainer = null;
        
        function initNotifications() {
            if (!notificationContainer) {
                notificationContainer = document.createElement('div');
                notificationContainer.id = 'notification-container';
                notificationContainer.style.cssText = 'position: fixed; top: 20px; right: 20px; z-index: 9999; display: flex; flex-direction: column; gap: 10px;';
                document.body.appendChild(notificationContainer);
            }
        }
        
        function showNotification(message, type = 'info', duration = 5000) {
            initNotifications();
            
            const colors = {
                info: {bg: '#e7f3ff', border: '#4361ee', text: '#084298'},
                success: {bg: '#d1e7dd', border: '#28a745', text: '#0f5132'},
                warning: {bg: '#fff3cd', border: '#ffc107', text: '#664d03'},
                error: {bg: '#f8d7da', border: '#dc3545', text: '#842029'}
            };
            const c = colors[type] || colors.info;
            
            const notification = document.createElement('div');
            notification.style.cssText = 'padding: 15px 20px; border-radius: 8px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); max-width: 350px; animation: slideIn 0.3s ease;';
            notification.style.backgroundColor = c.bg;
            notification.style.borderLeft = '4px solid ' + c.border;
            notification.style.color = c.text;
            notification.innerHTML = '<div style=\"display: flex; justify-content: space-between; align-items: start;\"><span>' + message + '</span><button onclick=\"this.parentElement.parentElement.remove()\" style=\"background: none; border: none; cursor: pointer; font-size: 18px; margin-left: 10px;\">&times;</button></div>';
            
            notificationContainer.appendChild(notification);
            
            if (duration > 0) {
                setTimeout(() => {
                    notification.style.animation = 'slideOut 0.3s ease';
                    setTimeout(() => notification.remove(), 300);
                }, duration);
            }
        }
        
        // Add CSS animation for notifications
        const notifStyle = document.createElement('style');
        notifStyle.textContent = '@keyframes slideIn { from { transform: translateX(100%); opacity: 0; } to { transform: translateX(0); opacity: 1; } } @keyframes slideOut { from { transform: translateX(0); opacity: 1; } to { transform: translateX(100%); opacity: 0; } }';
        document.head.appendChild(notifStyle);
        
        // Check for updates and show notification
        let lastUpdateCheck = null;
        async function checkForUpdatesNotification() {
            try {
                const res = await fetch('http://localhost:8006/api/updater/status');
                const data = await res.json();
                
                if (data.update_available && lastUpdateCheck !== data.current_commit) {
                    showNotification('New update available! The system will auto-update shortly.', 'info', 10000);
                    lastUpdateCheck = data.current_commit;
                }
            } catch (e) {
                // Silently fail
            }
        }
        
        // Check for updates every 60 seconds
        setInterval(checkForUpdatesNotification, 60000);
        
        // Real-time notifications via Server-Sent Events
        let notificationEventSource = null;
        
        function connectToNotifications() {
            if (notificationEventSource) {
                notificationEventSource.close();
            }
            
            try {
                notificationEventSource = new EventSource('/api/analysis/notifications');
                
                notificationEventSource.addEventListener('connected', function(e) {
                    console.log('Connected to notification service');
                });
                
                notificationEventSource.addEventListener('lollapalooza', function(e) {
                    const data = JSON.parse(e.data);
                    const msg = '<strong>LOLLAPALOOZA DETECTED!</strong><br>' + 
                                data.data.file + '<br>' +
                                '<small>' + (data.data.models || []).join(', ') + '</small>';
                    showNotification(msg, 'warning', 15000);
                });
                
                notificationEventSource.addEventListener('analysis_complete', function(e) {
                    const data = JSON.parse(e.data);
                    const msg = '<strong>Analysis Complete</strong><br>' + 
                                data.data.file + '<br>' +
                                '<small>' + (data.data.models_found || 0) + ' models found</small>';
                    showNotification(msg, 'success', 8000);
                });
                
                notificationEventSource.onerror = function(e) {
                    console.log('Notification connection error, will retry...');
                    notificationEventSource.close();
                    setTimeout(connectToNotifications, 5000);
                };
            } catch (e) {
                console.log('Could not connect to notifications:', e);
                setTimeout(connectToNotifications, 10000);
            }
        }
        
        // Connect to notifications on page load
        connectToNotifications();
        
        function showKeyboardShortcuts() {
            // Remove existing modal if present
            const existing = document.getElementById('shortcuts-modal');
            if (existing) { existing.remove(); return; }
            
            let html = '<div id=\"shortcuts-modal\" style=\"position: fixed; top: 0; left: 0; right: 0; bottom: 0; background: rgba(0,0,0,0.5); display: flex; align-items: center; justify-content: center; z-index: 1000;\" onclick=\"this.remove()\">';
            html += '<div style=\"background: white; padding: 30px; border-radius: 12px; max-width: 400px; box-shadow: 0 10px 40px rgba(0,0,0,0.2);\" onclick=\"event.stopPropagation()\">';
            html += '<h2 style=\"margin-bottom: 20px; color: #4361ee;\">Keyboard Shortcuts</h2>';
            html += '<table style=\"width: 100%;\">';
            html += '<tr><td style=\"padding: 8px 0;\"><kbd style=\"background: #e9ecef; padding: 4px 8px; border-radius: 4px;\">Ctrl+D</kbd></td><td>Dashboard</td></tr>';
            html += '<tr><td style=\"padding: 8px 0;\"><kbd style=\"background: #e9ecef; padding: 4px 8px; border-radius: 4px;\">Ctrl+A</kbd></td><td>Analysis</td></tr>';
            html += '<tr><td style=\"padding: 8px 0;\"><kbd style=\"background: #e9ecef; padding: 4px 8px; border-radius: 4px;\">Ctrl+M</kbd></td><td>Models</td></tr>';
            html += '<tr><td style=\"padding: 8px 0;\"><kbd style=\"background: #e9ecef; padding: 4px 8px; border-radius: 4px;\">Ctrl+H</kbd></td><td>History</td></tr>';
            html += '<tr><td style=\"padding: 8px 0;\"><kbd style=\"background: #e9ecef; padding: 4px 8px; border-radius: 4px;\">Ctrl+S</kbd></td><td>Settings</td></tr>';
            html += '<tr><td style=\"padding: 8px 0;\"><kbd style=\"background: #e9ecef; padding: 4px 8px; border-radius: 4px;\">Ctrl+K</kbd></td><td>Show shortcuts</td></tr>';
            html += '<tr><td style=\"padding: 8px 0;\"><kbd style=\"background: #e9ecef; padding: 4px 8px; border-radius: 4px;\">?</kbd></td><td>Show shortcuts</td></tr>';
            html += '<tr><td style=\"padding: 8px 0;\"><kbd style=\"background: #e9ecef; padding: 4px 8px; border-radius: 4px;\">Esc</kbd></td><td>Close modals</td></tr>';
            html += '</table>';
            html += '<button onclick=\"this.parentElement.parentElement.remove()\" style=\"margin-top: 20px; padding: 10px 20px; background: #4361ee; color: white; border: none; border-radius: 6px; cursor: pointer;\">Close</button>';
            html += '</div></div>';
            document.body.insertAdjacentHTML('beforeend', html);
        }
    </script>
</body>
    ">>.
