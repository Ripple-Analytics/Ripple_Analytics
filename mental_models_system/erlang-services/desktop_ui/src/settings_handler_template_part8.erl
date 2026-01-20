%%%-------------------------------------------------------------------
%%% @doc Settings Handler Template - Part 8
%%% @end
%%%-------------------------------------------------------------------
-module(settings_handler_template_part8).

-export([content/0]).

content() ->
    <<"
                    if (data.success) {
                        const count = data.peers?.length || 0;
                        document.getElementById('lan-results').innerHTML = 
                            '<div class=\"alert alert-success\">Found ' + count + ' peer(s) on local network</div>';
                        loadLanStatus();
                    } else {
                        document.getElementById('lan-results').innerHTML = 
                            '<div class=\"alert alert-error\">' + (data.error || 'Discovery failed') + '</div>';
                    }
                } catch (e) {
                    document.getElementById('lan-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function broadcastUpdate() {
                document.getElementById('lan-results').innerHTML = '<p class=\"loading\">Broadcasting update...</p>';
                try {
                    const res = await fetch('http://localhost:8010/api/lan/broadcast', {method: 'POST'});
                    const data = await res.json();
                    
                    if (data.success) {
                        document.getElementById('lan-results').innerHTML = 
                            '<div class=\"alert alert-success\">Broadcast sent to ' + (data.broadcast?.peers_notified || 0) + ' peer(s)</div>';
                    } else {
                        document.getElementById('lan-results').innerHTML = 
                            '<div class=\"alert alert-error\">' + (data.error || 'Broadcast failed') + '</div>';
                    }
                } catch (e) {
                    document.getElementById('lan-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            async function listPeers() {
                document.getElementById('lan-results').innerHTML = '<p class=\"loading\">Loading peers...</p>';
                try {
                    const res = await fetch('http://localhost:8010/api/lan/peers');
                    const data = await res.json();
                    
                    if (data.success && data.peers?.length > 0) {
                        let html = '<table style=\"width: 100%; border-collapse: collapse;\">';
                        html += '<tr style=\"background: #f8f9fa;\"><th style=\"padding: 8px;\">ID</th><th style=\"padding: 8px;\">Address</th></tr>';
                        for (const p of data.peers) {
                            html += '<tr><td style=\"padding: 8px;\">' + p.id + '</td><td style=\"padding: 8px;\">' + p.address + '</td></tr>';
                        }
                        html += '</table>';
                        document.getElementById('lan-results').innerHTML = html;
                    } else {
                        document.getElementById('lan-results').innerHTML = '<p>No peers found. Click \"Discover Peers\" to scan.</p>';
                    }
                } catch (e) {
                    document.getElementById('lan-results').innerHTML = 
                        '<div class=\"alert alert-error\">Error: ' + e.message + '</div>';
                }
            }
            
            // Load all backup statuses on page load
            loadS3Status();
            loadHttpStatus();
            loadLanStatus();
            setInterval(loadS3Status, 60000);
            setInterval(loadHttpStatus, 60000);
            setInterval(loadLanStatus, 60000);
        </script>
    ">>.
