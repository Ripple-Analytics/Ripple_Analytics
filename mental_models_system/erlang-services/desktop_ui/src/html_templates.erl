%%%-------------------------------------------------------------------
%%% @doc HTML Templates - Light mode UI templates
%%%-------------------------------------------------------------------
-module(html_templates).

-export([base_layout/2, nav_html/1, card/3, alert/2]).

%% Light mode color scheme
-define(BG_COLOR, "#ffffff").
-define(TEXT_COLOR, "#1a1a2e").
-define(PRIMARY_COLOR, "#4361ee").
-define(SECONDARY_COLOR, "#3f37c9").
-define(ACCENT_COLOR, "#4895ef").
-define(BORDER_COLOR, "#e0e0e0").
-define(CARD_BG, "#f8f9fa").

base_layout(Title, Content) ->
    iolist_to_binary([
        <<"<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
    <title>">>, Title, <<" - Mental Models System</title>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif;
            background-color: ">>, ?BG_COLOR, <<";
            color: ">>, ?TEXT_COLOR, <<";
            line-height: 1.6;
        }
        .container { max-width: 1200px; margin: 0 auto; padding: 20px; }
        .header {
            background: linear-gradient(135deg, ">>, ?PRIMARY_COLOR, <<", ">>, ?SECONDARY_COLOR, <<");
            color: white;
            padding: 20px;
            margin-bottom: 20px;
            border-radius: 8px;
        }
        .header h1 { font-size: 24px; font-weight: 600; }
        .nav {
            display: flex;
            gap: 10px;
            margin-bottom: 20px;
            flex-wrap: wrap;
        }
        .nav a {
            padding: 10px 20px;
            background: ">>, ?CARD_BG, <<";
            color: ">>, ?TEXT_COLOR, <<";
            text-decoration: none;
            border-radius: 6px;
            border: 1px solid ">>, ?BORDER_COLOR, <<";
            transition: all 0.2s;
        }
        .nav a:hover, .nav a.active {
            background: ">>, ?PRIMARY_COLOR, <<";
            color: white;
            border-color: ">>, ?PRIMARY_COLOR, <<";
        }
        .card {
            background: ">>, ?CARD_BG, <<";
            border: 1px solid ">>, ?BORDER_COLOR, <<";
            border-radius: 8px;
            padding: 20px;
            margin-bottom: 20px;
        }
        .card h2 {
            color: ">>, ?PRIMARY_COLOR, <<";
            margin-bottom: 15px;
            font-size: 18px;
        }
        .card h3 { margin-bottom: 10px; font-size: 16px; }
        .btn {
            display: inline-block;
            padding: 10px 20px;
            background: ">>, ?PRIMARY_COLOR, <<";
            color: white;
            border: none;
            border-radius: 6px;
            cursor: pointer;
            text-decoration: none;
            font-size: 14px;
            transition: background 0.2s;
        }
        .btn:hover { background: ">>, ?SECONDARY_COLOR, <<"; }
        .btn-secondary {
            background: ">>, ?CARD_BG, <<";
            color: ">>, ?TEXT_COLOR, <<";
            border: 1px solid ">>, ?BORDER_COLOR, <<";
        }
        .btn-secondary:hover { background: ">>, ?BORDER_COLOR, <<"; }
        .grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 20px; }
        .status-healthy { color: #28a745; }
        .status-unhealthy { color: #dc3545; }
        .status-unknown { color: #ffc107; }
        textarea, input[type=\"text\"], input[type=\"url\"] {
            width: 100%;
            padding: 12px;
            border: 1px solid ">>, ?BORDER_COLOR, <<";
            border-radius: 6px;
            font-size: 14px;
            margin-bottom: 10px;
        }
        textarea { min-height: 150px; resize: vertical; }
        .alert {
            padding: 15px;
            border-radius: 6px;
            margin-bottom: 20px;
        }
        .alert-info { background: #e7f3ff; border: 1px solid #b6d4fe; color: #084298; }
        .alert-success { background: #d1e7dd; border: 1px solid #badbcc; color: #0f5132; }
        .alert-warning { background: #fff3cd; border: 1px solid #ffecb5; color: #664d03; }
        .alert-error { background: #f8d7da; border: 1px solid #f5c2c7; color: #842029; }
        .model-card {
            background: white;
            border: 1px solid ">>, ?BORDER_COLOR, <<";
            border-radius: 8px;
            padding: 15px;
            margin-bottom: 10px;
        }
        .model-card h4 { color: ">>, ?PRIMARY_COLOR, <<"; margin-bottom: 8px; }
        .model-card .category {
            display: inline-block;
            padding: 2px 8px;
            background: ">>, ?ACCENT_COLOR, <<";
            color: white;
            border-radius: 4px;
            font-size: 12px;
            margin-bottom: 8px;
        }
        .model-card p { font-size: 14px; color: #666; }
        .stats-grid { display: grid; grid-template-columns: repeat(4, 1fr); gap: 15px; margin-bottom: 20px; }
        .stat-card {
            background: white;
            border: 1px solid ">>, ?BORDER_COLOR, <<";
            border-radius: 8px;
            padding: 20px;
            text-align: center;
        }
        .stat-card .value { font-size: 32px; font-weight: bold; color: ">>, ?PRIMARY_COLOR, <<"; }
        .stat-card .label { font-size: 14px; color: #666; }
        #results { margin-top: 20px; }
        .loading { text-align: center; padding: 40px; color: #666; }
        @media (max-width: 768px) {
            .stats-grid { grid-template-columns: repeat(2, 1fr); }
            .nav { flex-direction: column; }
        }
    </style>
</head>
<body>
    <div class=\"container\">
        <div class=\"header\" style=\"display: flex; justify-content: space-between; align-items: center;\">
            <div>
                <h1>Mental Models System</h1>
                <p>Erlang/OTP Microservices Architecture</p>
            </div>
            <div id=\"branch-indicator\" style=\"text-align: right; font-size: 12px;\">
                <span style=\"background: rgba(255,255,255,0.2); padding: 4px 10px; border-radius: 4px;\">
                    <span id=\"git-branch-nav\">loading...</span>
                </span>
                <div style=\"margin-top: 4px; opacity: 0.8;\">
                    <span id=\"git-commit-nav\"></span>
                </div>
            </div>
        </div>
        <script>
            (async function loadBranchInfo() {
                try {
                    const res = await fetch('http://localhost:8006/api/updater/status');
                    const data = await res.json();
                    if (data.current_commit) {
                        const branchEl = document.getElementById('git-branch-nav');
                        const commitEl = document.getElementById('git-commit-nav');
                        // Try to get branch from config
                        const configRes = await fetch('http://localhost:8006/api/updater/config');
                        const config = await configRes.json();
                        branchEl.textContent = config.github_branch || 'release';
                        commitEl.textContent = data.current_commit.substring(0, 8);
                    }
                } catch (e) {
                    document.getElementById('git-branch-nav').textContent = 'offline';
                }
            })();
        </script>
        ">>, nav_html(Title), <<"
        ">>, Content, <<"
    </div>
    <script>
        async function apiCall(endpoint, method = 'GET', body = null) {
            const opts = { method, headers: { 'Content-Type': 'application/json' } };
            if (body) opts.body = JSON.stringify(body);
            const res = await fetch('/api' + endpoint, opts);
            return res.json();
        }
        
        // Global keyboard shortcuts
        document.addEventListener('keydown', function(e) {
            // Don't trigger shortcuts when typing in inputs
            if (e.target.tagName === 'INPUT' || e.target.tagName === 'TEXTAREA') return;
            
            // Ctrl/Cmd + key shortcuts
            if (e.ctrlKey || e.metaKey) {
                switch(e.key.toLowerCase()) {
                    case 'h': // Go to History
                        e.preventDefault();
                        window.location.href = '/history';
                        break;
                    case 'm': // Go to Models
                        e.preventDefault();
                        window.location.href = '/models';
                        break;
                    case 'a': // Go to Analysis
                        e.preventDefault();
                        window.location.href = '/analysis';
                        break;
                    case 'd': // Go to Dashboard
                        e.preventDefault();
                        window.location.href = '/';
                        break;
                    case 's': // Go to Settings
                        e.preventDefault();
                        window.location.href = '/settings';
                        break;
                    case 'k': // Show keyboard shortcuts help
                        e.preventDefault();
                        showKeyboardShortcuts();
                        break;
                }
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
</html>">>
    ]).

nav_html(ActivePage) ->
    Pages = [
        {<<"Dashboard">>, <<"/">>},
        {<<"Analysis">>, <<"/analysis">>},
        {<<"Models">>, <<"/models">>},
        {<<"History">>, <<"/history">>},
        {<<"Harvester">>, <<"/harvester">>},
        {<<"Settings">>, <<"/settings">>}
    ],
    Links = [begin
        Active = case ActivePage of
            P -> <<" class=\"active\"">>;
            _ -> <<>>
        end,
        [<<"<a href=\"">>, Url, <<"\"">>, Active, <<">">>, P, <<"</a>">>]
    end || {P, Url} <- Pages],
    [<<"<nav class=\"nav\">">>, Links, <<"</nav>">>].

card(Title, Content, Extra) ->
    [<<"<div class=\"card\"><h2>">>, Title, <<"</h2>">>, Content, Extra, <<"</div>">>].

alert(Type, Message) ->
    [<<"<div class=\"alert alert-">>, Type, <<"\">">>, Message, <<"</div>">>].
