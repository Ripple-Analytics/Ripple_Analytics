%%%-------------------------------------------------------------------
%%% @doc HTML Templates - Light mode UI templates
%%%-------------------------------------------------------------------
-module(html_templates).
%% Helper modules: html_templates_part2

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