%%%-------------------------------------------------------------------
%%% @doc Analysis Handler - Mental model analysis page
%%% JavaScript loaded from priv/static/js/analysis.js
%%% @end
%%%-------------------------------------------------------------------
-module(analysis_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Content = [status_bar(), import_section(), paste_modal(), 
               main_card(), results_section(), export_section(), 
               load_script()],
    Html = html_templates:base_layout(<<"Analysis">>, Content),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req, State}.

status_bar() -> <<"
<div class=\"card\" style=\"padding: 10px 15px; margin-bottom: 10px;\">
  <div style=\"display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 10px;\">
    <div style=\"display: flex; align-items: center; gap: 15px;\">
      <span style=\"font-weight: bold; color: #4361ee;\">Mental Models Analysis</span>
      <span id=\"analysis-status\" class=\"status-healthy\" style=\"font-size: 11px;\">Ready</span>
    </div>
    <div style=\"display: flex; align-items: center; gap: 15px; font-size: 11px; color: #666;\">
      <span>Last: <span id=\"last-analysis-time\">Never</span></span>
      <span>Models: <span id=\"model-count\">...</span></span>
      <button class=\"btn btn-secondary\" style=\"font-size: 10px; padding: 4px 8px;\" onclick=\"refreshSystemStatus()\">Refresh</button>
    </div>
  </div>
</div>">>.

import_section() -> <<"
<div class=\"card\" style=\"background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; margin-bottom: 10px;\">
  <div style=\"display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 10px;\">
    <div>
      <h3 style=\"margin: 0; color: white; font-size: 14px;\">Batch Data Import</h3>
      <p style=\"margin: 5px 0 0 0; font-size: 11px; opacity: 0.9;\">Upload files or paste data</p>
    </div>
    <div style=\"display: flex; gap: 8px; align-items: center;\">
      <input type=\"file\" id=\"file-upload\" accept=\".txt,.csv,.json,.md\" multiple style=\"display: none;\" onchange=\"handleFileUpload(event)\">
      <button class=\"btn\" style=\"background: white; color: #667eea; font-size: 11px;\" onclick=\"document.getElementById('file-upload').click()\">Upload</button>
      <button class=\"btn\" style=\"background: rgba(255,255,255,0.2); border: 1px solid white; font-size: 11px;\" onclick=\"showPasteModal()\">Paste</button>
      <span id=\"file-count\" style=\"font-size: 11px;\"></span>
    </div>
  </div>
  <div id=\"import-preview\" style=\"margin-top: 10px; display: none;\">
    <div style=\"background: rgba(255,255,255,0.1); padding: 8px; border-radius: 6px; max-height: 100px; overflow-y: auto;\">
      <div id=\"import-files-list\" style=\"font-size: 11px;\"></div>
    </div>
    <div style=\"margin-top: 8px; display: flex; gap: 8px;\">
      <button class=\"btn\" style=\"background: #28a745; font-size: 11px;\" onclick=\"processBatchData()\">Process All</button>
      <button class=\"btn\" style=\"background: rgba(255,255,255,0.2); font-size: 11px;\" onclick=\"clearImportedFiles()\">Clear</button>
    </div>
  </div>
</div>">>.

paste_modal() -> <<"
<div id=\"paste-modal\" style=\"display: none; position: fixed; top: 0; left: 0; right: 0; bottom: 0; background: rgba(0,0,0,0.5); z-index: 1000; align-items: center; justify-content: center;\">
  <div style=\"background: white; padding: 20px; border-radius: 12px; max-width: 600px; width: 90%;\">
    <h3 style=\"margin: 0 0 10px 0;\">Paste Bulk Data</h3>
    <textarea id=\"bulk-paste-area\" style=\"width: 100%; height: 200px; margin: 10px 0; padding: 10px; border: 1px solid #ddd; border-radius: 6px; font-family: monospace;\" placeholder=\"Paste data here...\"></textarea>
    <div style=\"display: flex; gap: 8px; justify-content: flex-end;\">
      <button class=\"btn btn-secondary\" onclick=\"closePasteModal()\">Cancel</button>
      <button class=\"btn\" onclick=\"importPastedData()\">Import</button>
    </div>
  </div>
</div>">>.

main_card() -> <<"
<div class=\"card\">
  <h2>Text Analysis Input</h2>
  <p style=\"font-size: 12px; color: #666; margin-bottom: 10px;\">Enter text to identify mental models and detect cognitive biases.</p>
  
  <div style=\"margin-bottom: 12px; padding: 10px; background: #f8f9fa; border-radius: 6px;\">
    <label style=\"font-weight: bold; display: block; margin-bottom: 6px; font-size: 12px;\">Templates:</label>
    <div style=\"display: flex; gap: 6px; flex-wrap: wrap;\">
      <button class=\"btn btn-secondary\" style=\"font-size: 11px; padding: 4px 10px;\" onclick=\"loadTemplate('decision')\">Decision</button>
      <button class=\"btn btn-secondary\" style=\"font-size: 11px; padding: 4px 10px;\" onclick=\"loadTemplate('problem')\">Problem</button>
      <button class=\"btn btn-secondary\" style=\"font-size: 11px; padding: 4px 10px;\" onclick=\"loadTemplate('strategy')\">Strategy</button>
      <button class=\"btn btn-secondary\" style=\"font-size: 11px; padding: 4px 10px;\" onclick=\"loadTemplate('investment')\">Investment</button>
      <button class=\"btn btn-secondary\" style=\"font-size: 11px; padding: 4px 10px;\" onclick=\"loadTemplate('munger')\">Munger</button>
    </div>
  </div>
  
  <textarea id=\"analysis-text\" placeholder=\"Enter text to analyze...\" style=\"min-height: 150px;\"></textarea>
  
  <div style=\"margin-top: 10px; padding: 10px; background: #f8f9fa; border-radius: 6px;\">
    <div style=\"display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 10px;\">
      <div style=\"display: flex; align-items: center; gap: 10px;\">
        <label style=\"font-size: 12px;\">Top:</label>
        <select id=\"top-n-select\" style=\"padding: 4px 8px; border-radius: 4px; border: 1px solid #ddd;\">
          <option value=\"5\">5</option><option value=\"10\" selected>10</option><option value=\"15\">15</option>
        </select>
      </div>
      <div style=\"display: flex; align-items: center; gap: 8px;\">
        <label style=\"font-size: 12px;\"><input type=\"checkbox\" id=\"auto-save-history\" checked> Auto-save</label>
        <label style=\"font-size: 12px;\"><input type=\"checkbox\" id=\"show-explanations\" checked> Explanations</label>
      </div>
    </div>
  </div>
  
  <div style=\"margin-top: 15px;\">
    <div style=\"display: flex; gap: 8px; flex-wrap: wrap; margin-bottom: 10px;\">
      <button class=\"btn\" onclick=\"runFullAnalysis()\" style=\"background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);\">Comprehensive Analysis</button>
    </div>
    <div style=\"display: flex; gap: 8px; flex-wrap: wrap;\">
      <button class=\"btn btn-secondary\" onclick=\"analyzeText()\">Models</button>
      <button class=\"btn btn-secondary\" onclick=\"detectBiases()\">Biases</button>
      <button class=\"btn btn-secondary\" onclick=\"runBayesianAnalysis()\">Bayesian</button>
      <button class=\"btn btn-secondary\" onclick=\"runPatternAnalysis()\">Patterns</button>
      <button class=\"btn btn-secondary\" onclick=\"clearInput()\" style=\"background: #6c757d;\">Clear</button>
    </div>
  </div>
</div>">>.

results_section() -> <<"<div id=\"results\"></div>">>.

export_section() -> <<"
<div id=\"export-buttons\" style=\"display: none; margin-top: 15px;\">
  <div class=\"card\">
    <div style=\"display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 10px;\">
      <div>
        <h3 style=\"margin: 0; font-size: 14px;\">Export Results</h3>
      </div>
      <div style=\"display: flex; gap: 8px; flex-wrap: wrap;\">
        <button class=\"btn\" onclick=\"exportHTMLReport()\" style=\"font-size: 12px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);\">HTML</button>
        <button class=\"btn btn-secondary\" onclick=\"exportJSON()\" style=\"font-size: 12px;\">JSON</button>
        <button class=\"btn btn-secondary\" onclick=\"exportMarkdown()\" style=\"font-size: 12px;\">Markdown</button>
        <button class=\"btn btn-secondary\" onclick=\"copyToClipboard()\" style=\"font-size: 12px;\">Copy</button>
      </div>
    </div>
  </div>
</div>">>.

load_script() -> <<"<script src=\"/static/js/analysis.js\"></script>">>.
