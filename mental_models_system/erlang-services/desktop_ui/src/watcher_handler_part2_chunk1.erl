%%%-------------------------------------------------------------------
%%% @doc watcher_handler_part2 Template Chunk 1
%%% @end
%%%-------------------------------------------------------------------
-module(watcher_handler_part2_chunk1).

-export([content/0]).

content() ->
    <<"

<style>
    .watcher-controls {
        display: flex;
        gap: 15px;
        align-items: center;
        margin-bottom: 20px;
        flex-wrap: wrap;
    }
    .watcher-controls input {
        padding: 10px;
        border: 1px solid #ddd;
        border-radius: 6px;
        font-size: 14px;
    }
    .watcher-controls input[type='text'] {
        flex: 1;
        min-width: 250px;
    }
    .watcher-controls input[type='number'] {
        width: 120px;
    }
    .btn {
        padding: 10px 20px;
        border: none;
        border-radius: 6px;
        cursor: pointer;
        font-size: 14px;
        font-weight: 500;
        transition: all 0.2s;
    }
    .btn-start {
        background: #10b981;
        color: white;
    }
    .btn-start:hover {
        background: #059669;
    }
    .btn-stop {
        background: #ef4444;
        color: white;
    }
    .btn-stop:hover {
        background: #dc2626;
    }
    .btn-clear {
        background: #6b7280;
        color: white;
    }
    .btn-clear:hover {
        background: #4b5563;
    }
    .status-panel {
        background: #f8fafc;
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 20px;
    }
    .status-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
        gap: 15px;
    }
    .status-item {
        text-align: center;
    }
    .status-value {
        font-size: 24px;
        font-weight: bold;
        color: #1e293b;
    }
    .status-label {
        font-size: 12px;
        color: #64748b;
        text-transform: uppercase;
    }
    .status-watching {
        color: #10b981;
    }
    .status-stopped {
        color: #ef4444;
    }
    .results-list {
        max-height: 500px;
        overflow-y: auto;
    }
    .result-item {
        background: white;
        border: 1px solid #e2e8f0;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 10px;
    }
    .result-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 10px;
    }
    .result-file {
        font-weight: 600;
        color: #1e293b;
    }
    .result-time {
        font-size: 12px;
        color: #64748b;
    }
    .lollapalooza-badge {
        background: linear-gradient(135deg, #f59e0b, #ef4444);
        color: white;
        padding: 4px 12px;
        border-radius: 20px;
        font-size: 12px;
        font-weight: 600;
    }
    .models-preview {
        display: flex;
        flex-wrap: wrap;
        gap: 5px;
        margin-top: 10px;
    }
    .model-tag {
        background: #e0e7ff;
        color: #3730a3;
        padding: 4px 8px;
        border-radius: 4px;
        font-size: 12px;
    }
    .empty-state {
        text-align: center;
        padding: 40px;
        color: #64748b;
    }
    .refresh-indicator {
        font-size: 12px;
        color: #64748b;
        margin-left: 10px;
    }
</style>

<div class='card'>
    <h2>Folder Watcher Configuration</h2>
    <div class='watcher-controls'>
        <input type='text' id='folderPath' placeholder='Folder path to watch...' />
        <input type='number' id='interval' placeholder='Interval (sec)' value='30' min='5' max='3600' />
        <button class='btn btn-start' onclick='startWatcher()'>Start Watching</button>
        <button class='btn btn-stop' onclick='stopWatcher()'>Stop Watching</button>
        <button class='btn btn-clear' onclick='clearResults()'>Clear Results</button>
        <button class='btn' onclick='useHostPath()' style='background: #6366f1; color: white;'>Use Host Path</button>
    </div>
    ">>.
