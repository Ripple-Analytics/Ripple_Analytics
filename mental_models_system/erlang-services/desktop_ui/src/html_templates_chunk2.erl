%%%-------------------------------------------------------------------
%%% @doc HTML Templates Chunk 2
%%% @end
%%%-------------------------------------------------------------------
-module(html_templates_chunk2).

-export([styles/0]).

styles() ->
    <<"
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
                <div id=\"host-path-indicator\" style=\"margin-top: 4px; opacity: 0.7; font-size: 10px; max-width: 300px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;\" title=\"Loading path...\">
                    Loading...
                </div>
            </div>
        </div>
        <script>
            // Smart auto-refresh - only updates when user is idle (not typing or interacting)
            let lastInteraction = 0;
            const IDLE_THRESHOLD = 2000; // Wait 2 seconds after last interaction
            
            // Track user interactions
            document.addEventListener('keydown', () => { lastInteraction = Date.now(); });
            document.addEventListener('mousedown', () => { lastInteraction = Date.now(); });
            document.addEventListener('input', () => { lastInteraction = Date.now(); });
            
            function isUserIdle() {
                // Don't refresh if user is typing in an input
                const active = document.activeElement;
                if (active && (active.tagName === 'INPUT' || active.tagName === 'TEXTAREA')) {
                    return false;
                }
                // Don't refresh if user interacted recently
                return (Date.now() - lastInteraction) > IDLE_THRESHOLD;
            }
            
            async function refreshStatusBar() {
                // Only refresh when user is idle to avoid interrupting tasks
                if (!isUserIdle()) return;
                
                // Store current focus to restore after refresh (prevents tab switching)
                const currentFocus = document.activeElement;
                const currentScrollY = window.scrollY;
                const currentScrollX = window.scrollX;
                
                try {
                    const res = await fetch('http://localhost:8006/api/updater/status');
                    const data = await res.json();
                    if (data.current_commit) {
                        const branchEl = document.getElementById('git-branch-nav');
                        const commitEl = document.getElementById('git-commit-nav');
                        // Try to get branch from config
                        const configRes = await fetch('http://localhost:8006/api/updater/config');
                        const config = await configRes.json();
                        if (branchEl) branchEl.textContent = config.github_branch || 'release';
                        if (commitEl) commitEl.textContent = data.current_commit.substring(0, 8);
                    }
                } catch (e) {
                    const branchEl = document.getElementById('git-branch-nav');
                    if (branchEl) branchEl.textContent = 'offline';
                }
                
                // Load host path from settings API
                try {
                    const pathRes = await fetch('/api/system/info');
                    const pathData = await pathRes.json();
                    const hostPathEl = document.getElementById('host-path-indicator');
                    if (hostPathEl) {
                        if (pathData.host_path) {
                            hostPathEl.textContent = pathData.host_path;
                            hostPathEl.title = pathData.host_path;
                        } else {
                            hostPathEl.textContent = 'Path not available';
                            hostPathEl.title = 'HOST_PATH environment variable not set';
                        }
                    }
                } catch (e) {
                    const hostPathEl = document.getElementById('host-path-indicator');
                    if (hostPathEl) {
                        hostPathEl.textContent = 'Path unavailable';
                        hostPathEl.title = 'Could not fetch system info: ' + e.message;
                    }
                }
                
                // Restore focus and scroll position to prevent any UI disruption
                if (currentFocus && currentFocus !== document.body) {
                    try { currentFocus.focus({ preventScroll: true }); } catch (e) {}
                }
                window.scrollTo(currentScrollX, currentScrollY);
            }
            
            // Initial load (always runs)
            (async function() { 
                lastInteraction = 0; // Force initial load
                await refreshStatusBar();
                lastInteraction = Date.now();
            })();
            
            // Auto-refresh every 1 second, but only when user is idle
            setInterval(refreshStatusBar, 1000);
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
    ">>.
