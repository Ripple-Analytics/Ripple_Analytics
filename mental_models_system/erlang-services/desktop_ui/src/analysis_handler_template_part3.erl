%%%-------------------------------------------------------------------
%%% @doc Analysis Handler Template - Part 3
%%% @end
%%%-------------------------------------------------------------------
-module(analysis_handler_template_part3).

-export([content/0]).

content() ->
    <<"
                    html += '<span style=\"font-size: 11px; color: ' + (item.success ? '#28a745' : '#dc3545') + ';\">' + (item.success ? 'Success' : 'Failed') + '</span>';
                    html += '</div>';
                    
                    if (item.success && item.result) {
                        const lolla = item.result.lollapalooza || {};
                        if (lolla.detected) {
                            html += '<div style=\"background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); color: white; padding: 8px; border-radius: 6px; margin: 8px 0; font-size: 12px;\">';
                            html += '<strong>Lollapalooza!</strong> ' + lolla.convergence_count + ' models (' + lolla.strength + ')';
                            html += '</div>';
                        }
                        const models = item.result.analysis || [];
                        if (models.length > 0) {
                            html += '<div style=\"display: flex; flex-wrap: wrap; gap: 4px; margin-top: 8px;\">';
                            models.slice(0, 5).forEach(m => {
                                html += '<span style=\"background: #e9ecef; padding: 3px 8px; border-radius: 4px; font-size: 11px;\">' + m.name + '</span>';
                            });
                            html += '</div>';
                        }
                    }
                    html += '</div>';
                });
                
                document.getElementById('results').innerHTML = html;
                document.getElementById('export-buttons').style.display = 'none';
                lastAnalysisResult = batchResults;
                lastAnalysisType = 'batch';
            }
            
            function exportBatchJSON() {
                const blob = new Blob([JSON.stringify(batchResults, null, 2)], {type: 'application/json'});
                const url = URL.createObjectURL(blob);
                const a = document.createElement('a'); a.href = url;
                a.download = 'batch-analysis-' + new Date().toISOString().slice(0,10) + '.json';
                a.click();
            }
            
            function exportBatchCSV() {
                let csv = 'File,Success,Lollapalooza,Convergence,TopModel,TopScore\\n';
                batchResults.forEach(item => {
                    const lolla = item.result?.lollapalooza || {};
                    const top = item.result?.analysis?.[0] || {};
                    csv += [item.file, item.success, lolla.detected || false, lolla.convergence_count || 0, top.name || '', top.relevance || 0].join(',') + '\\n';
                });
                const blob = new Blob([csv], {type: 'text/csv'});
                const url = URL.createObjectURL(blob);
                const a = document.createElement('a'); a.href = url;
                a.download = 'batch-summary-' + new Date().toISOString().slice(0,10) + '.csv';
                a.click();
            }
            
            // ========== Core Functions ==========
            
            // Initialize on page load
            document.addEventListener('DOMContentLoaded', function() {
                refreshSystemStatus();
                loadLastAnalysisTime();
            });
            
            async function refreshSystemStatus() {
                try {
                    const res = await fetch('/api/analysis/models');
                    const data = await res.json();
                    document.getElementById('model-count').textContent = (data.models || []).length;
                } catch (e) {
                    document.getElementById('model-count').textContent = 'Error';
                }
            }
            
            function loadLastAnalysisTime() {
                const lastTime = localStorage.getItem('lastAnalysisTime');
                if (lastTime) {
                    document.getElementById('last-analysis-time').textContent = new Date(lastTime).toLocaleString();
                }
            }
            
            function updateLastAnalysisTime() {
                const now = new Date().toISOString();
                localStorage.setItem('lastAnalysisTime', now);
                document.getElementById('last-analysis-time').textContent = new Date(now).toLocaleString();
            }
            
            function setStatus(status, color) {
                const statusEl = document.getElementById('analysis-status');
                statusEl.textContent = status;
                statusEl.className = color === 'green' ? 'status-healthy' : 
                                     color === 'yellow' ? 'status-unknown' : 
                                     color === 'red' ? 'status-unhealthy' : 'status-healthy';
            }
            
            function clearInput() {
                document.getElementById('analysis-text').value = '';
                document.getElementById('results').innerHTML = '';
                document.getElementById('export-buttons').style.display = 'none';
                setStatus('Ready', 'green');
            }
            
            function getTopN() {
                return parseInt(document.getElementById('top-n-select').value) || 10;
            }
            
            const templates = {
                decision: 'DECISION ANALYSIS\\n\\nDecision to make: [Describe the decision]\\n\\nOptions:\\n1. [Option A]\\n2. [Option B]\\n3. [Option C]\\n\\nPros and Cons:\\n- Option A: [pros/cons]\\n- Option B: [pros/cons]\\n- Option C: [pros/cons]\\n\\nKey factors to consider:\\n- [Factor 1]\\n- [Factor 2]\\n\\nTimeline: [When decision needs to be made]\\n\\nStakeholders affected: [Who is impacted]',
                problem: 'PROBLEM SOLVING\\n\\nProblem statement: [Describe the problem clearly]\\n\\nCurrent situation: [What is happening now]\\n\\nDesired outcome: [What should be happening]\\n\\nRoot causes identified:\\n1. [Cause 1]\\n2. [Cause 2]\\n\\nPotential solutions:\\n1. [Solution 1]\\n2. [Solution 2]\\n\\nConstraints: [Time, budget, resources]\\n\\nSuccess metrics: [How will we know it is solved]',
                strategy: 'STRATEGY REVIEW\\n\\nObjective: [What are we trying to achieve]\\n\\nCurrent strategy: [Describe current approach]\\n\\nMarket conditions: [External factors]\\n\\nCompetitive landscape: [Key competitors and their moves]\\n\\nStrengths to leverage:\\n- [Strength 1]\\n- [Strength 2]\\n\\nWeaknesses to address:\\n- [Weakness 1]\\n- [Weakness 2]\\n\\nOpportunities identified:\\n- [Opportunity 1]\\n\\nThreats to mitigate:\\n- [Threat 1]\\n\\nProposed changes: [What should we do differently]',
                negotiation: 'NEGOTIATION PREPARATION\\n\\nNegotiation context: [What is being negotiated]\\n\\nOur position: [What we want]\\n\\nTheir likely position: [What they want]\\n\\nOur BATNA (Best Alternative): [What we do if no deal]\\n\\nTheir likely BATNA: [What they do if no deal]\\n\\nKey interests (ours):\\n- [Interest 1]\\n- [Interest 2]\\n\\nKey interests (theirs):\\n- [Interest 1]\\n- [Interest 2]\\n\\nPotential trade-offs: [What can we give up]\\n\\nDeal breakers: [What we cannot accept]\\n\\nOpening offer: [Where to start]\\n\\nTarget outcome: [Ideal result]',
                investment: 'INVESTMENT ANALYSIS\\n\\nInvestment opportunity: [Describe the investment]\\n\\nAmount: [How much]\\n\\nExpected return: [ROI expectations]\\n\\nTime horizon: [Investment period]\\n\\nRisk factors:\\n1. [Risk 1]\\n2. [Risk 2]\\n3. [Risk 3]\\n\\nMitigation strategies: [How to reduce risks]\\n\\nMarket analysis: [Industry trends]\\n\\nCompetitive moat: [What protects this investment]\\n\\nExit strategy: [How and when to exit]\\n\\nAlternative investments considered: [Other options]',
                meeting: 'MEETING NOTES\\n\\nDate: [Date]\\nAttendees: [Who was present]\\nPurpose: [Why we met]\\n\\nKey discussion points:\\n1. [Topic 1]: [Summary]\\n2. [Topic 2]: [Summary]\\n3. [Topic 3]: [Summary]\\n\\nDecisions made:\\n- [Decision 1]\\n- [Decision 2]\\n\\nAction items:\\n- [Action 1] - Owner: [Name] - Due: [Date]\\n- [Action 2] - Owner: [Name] - Due: [Date]\\n\\nOpen questions:\\n- [Question 1]\\n\\nNext steps: [What happens next]\\n\\nFollow-up meeting: [If scheduled]',
                munger: 'MUNGER INVESTMENT CHECKLIST\\n\\nCompany/Opportunity: [Name]\\n\\n1. UNDERSTANDING THE BUSINESS\\n- Can I explain this business to a child? [Yes/No]\\n- What is the core value proposition?\\n- How does it make money?\\n- What are the unit economics?\\n\\n2. COMPETITIVE ADVANTAGE (MOAT)\\n- Does it have pricing power?\\n- Network effects?\\n- Switching costs?\\n- Cost advantages?\\n- Intangible assets (brand, patents)?\\n\\n3. MANAGEMENT QUALITY\\n- Are they owner-operators?\\n- Capital allocation track record?\\n- Insider ownership?\\n- Compensation aligned with shareholders?\\n\\n4. FINANCIAL STRENGTH\\n- Debt levels reasonable?\\n- Free cash flow positive?\\n- Return on invested capital?\\n- Margin trends?\\n\\n5. VALUATION\\n- What is it worth?\\n- Margin of safety?\\n- What could go wrong?\\n\\n6. INVERSION - What would make this fail?\\n- [Failure mode 1]\\n- [Failure mode 2]\\n- [Failure mode 3]'
            };
            
            function loadTemplate(type) {
                if (templates[type]) {
                    document.getElementById('analysis-text').value = templates[type];
                }
            }
            
            async function saveToHistory(type, inputText, models, biases) {
                if (!document.getElementById('auto-save-history').checked) return;
                try {
                    await fetch('/api/storage/history', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({
                            type: type,
                            input_text: inputText,
                            models: models || [],
                            biases: biases || []
                        })
                    });
                } catch (e) {
                    console.log('Failed to save to history:', e);
                }
            }
            
            async function analyzeText() {
                const text = document.getElementById('analysis-text').value;
                if (!text.trim()) {
                    alert('Please enter some text to analyze');
                    return;
                }
                
                setStatus('Analyzing...', 'yellow');
                analysisStartTime = Date.now();
                document.getElementById('results').innerHTML = '<div class=\"loading\">Detecting mental models...</div>';
                document.getElementById('export-buttons').style.display = 'none';
                
                try {
                    const res = await fetch('/api/analysis/analyze', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
    ">>.
