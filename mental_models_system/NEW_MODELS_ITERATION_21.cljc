;; ============================================
;; ITERATION 21: Critical Mental Models
;; Date: 2026-01-18 15:30 UTC
;; Added by: Manus Autonomous Iteration
;; Models: Base Rate Neglect, Skin in the Game, Survivorship Bias, Antifragility, Optionality
;; ============================================

(register-model
 {:name "base-rate-neglect"
  :category "cognitive_bias"
  :originator "Daniel Kahneman & Amos Tversky"
  :description "Base rate neglect is the tendency to ignore statistical baselines (base rates) in favor of specific, individuating information. When people evaluate probabilities, they often focus on case-specific details while neglecting the underlying statistical frequency of events in the relevant population. This is one of the most pervasive cognitive errors in judgment and decision-making, leading to systematic overconfidence in predictions and poor probabilistic reasoning. Kahneman demonstrated this through numerous experiments showing that even statistically sophisticated individuals fall prey to this bias when presented with vivid, specific information. The planning fallacy, where projects consistently run over time and budget, is a direct consequence of base rate neglect. Understanding and correcting for base rates is fundamental to accurate prediction and sound decision-making."
  :key-insight "Start with the base rate, then adjust for specific information - never ignore the statistical baseline"
  :application "Before making any prediction, ask: What is the base rate for this type of situation? What percentage of similar cases succeed/fail?"
  :real-world-examples
  ["Startup success: Base rate shows 90% of startups fail, yet founders consistently predict 90% chance of success for their own venture"
   "Project planning: 70% of IT projects run over budget, yet teams predict on-time delivery with high confidence"
   "Medical diagnosis: Disease has 1% base rate, test is 95% accurate, yet doctors often conclude 95% probability of disease when test is positive (actual: ~16%)"
   "Venture capital: Only 10% of VC-backed startups return the fund, yet VCs evaluate each deal as if it has 50%+ chance of success"
   "Business acquisitions: 70-90% of acquisitions fail to create value, yet acquirers consistently project synergies and success"]
  :quantitative-thresholds
  {:apply-when "Making any prediction or probability assessment"
   :base-rate-weight "Start with 70-80% weight on base rate, adjust based on specific evidence quality"
   :minimum-sample "Need 30+ similar cases for reliable base rate"
   :adjustment-limit "Specific information should move you max 20-30% from base rate unless extremely diagnostic"}
  :failure-modes
  [(failure "complete-base-rate-ignorance" "critical"
            "Entirely ignoring statistical baselines and relying only on case-specific information"
            :signals ["No mention of historical success rates" "Focus only on unique aspects of situation" "Predictions far from base rate without justification" "Overconfidence in predictions" "Surprise when outcomes match base rate"]
            :safeguards ["Mandatory base rate lookup before predictions" "Require explicit base rate in all forecasts" "Track prediction accuracy against base rates" "Use reference class forecasting" "Implement pre-mortem with base rate focus"]
            :recovery-protocols ["When prediction fails, calculate what base rate would have predicted" "Update forecasting process to include base rate" "Create base rate database for common situations"]
            :case-studies ["Kahneman's taxi cab problem: People ignore 85% base rate of green cabs, focus on 80% witness accuracy, get wrong answer"
                          "Planning fallacy research: Sydney Opera House (10x over budget), Boston Big Dig (2x over budget) - both ignored base rates of similar projects"])
   (failure "insufficient-adjustment" "high"
            "Acknowledging base rate but not adjusting enough for specific information"
            :signals ["Mention base rate but quickly dismiss it" "Minimal adjustment from baseline" "Treating all specific information as weak" "Conservative predictions that miss obvious signals"]
            :safeguards ["Bayesian updating framework" "Assess diagnosticity of specific information" "Calculate likelihood ratios" "Use prediction markets for calibration"]
            :recovery-protocols ["Review cases where specific information was highly diagnostic" "Develop framework for assessing information quality" "Practice Bayesian reasoning"]
            :case-studies ["Medical diagnosis: Ignoring highly diagnostic symptoms because base rate is low" "Talent evaluation: Missing exceptional candidates because base rate of success is low"])
   (failure "wrong-reference-class" "high"
            "Using base rate from incorrect comparison group"
            :signals ["Overly broad reference class" "Cherry-picked comparison group" "Ignoring key distinguishing factors" "Base rate doesn't match actual situation"]
            :safeguards ["Carefully define reference class" "Multiple reference classes for comparison" "Test sensitivity to reference class choice" "Expert input on appropriate comparisons"]
            :recovery-protocols ["When prediction fails, identify correct reference class" "Build taxonomy of reference classes" "Document reference class selection reasoning"]
            :case-studies ["Comparing your startup to 'all startups' vs 'B2B SaaS startups with experienced founders' - very different base rates"
                          "Using base rate of 'all projects' vs 'projects of this type, size, and complexity' - specificity matters"])
   (failure "representativeness-heuristic" "high"
            "Judging probability by how representative something seems rather than using base rates"
            :signals ["'This looks like a winner' reasoning" "Focusing on stereotypical features" "Ignoring sample size" "Vivid details dominating judgment"]
            :safeguards ["Explicit probability calculations" "Blind evaluation of base rates first" "Separate description from prediction" "Use algorithms over intuition"]
            :recovery-protocols ["Track cases where representative-seeming options failed" "Build database of misleading signals" "Practice separating description from prediction"]
            :case-studies ["Linda problem: People judge 'Linda is a bank teller and feminist' as more probable than 'Linda is a bank teller' because description is representative"
                          "Interview illusion: Confident, articulate candidates seem like good hires, but base rate shows interviews have weak predictive validity"])
   (failure "inside-view-dominance" "critical"
            "Constructing detailed scenarios from inside view while ignoring outside view (base rates)"
            :signals ["Detailed plans and projections" "Focus on unique aspects" "Story-based reasoning" "Ignoring statistical evidence" "Overconfidence in forecasts"]
            :safeguards ["Mandatory outside view analysis first" "Reference class forecasting" "Separate inside and outside view estimates" "Weight outside view heavily" "Use prediction markets"]
            :recovery-protocols ["When plans fail, document what outside view would have predicted" "Create forcing function to consider outside view" "Build outside view into planning process"]
            :case-studies ["Kahneman's curriculum project: Inside view predicted 2 years, outside view (base rate of similar projects) predicted 7+ years, actual: 8 years"
                          "Startup founders: Inside view focuses on their unique advantages, outside view shows 90% failure rate - outside view is more accurate"])]
  :cross-model-interactions
  ["Combines with Availability Bias: Recent vivid examples make us ignore base rates"
   "Combines with Confirmation Bias: We seek case-specific information that confirms our hypothesis, ignore base rates"
   "Combines with Overconfidence: Base rate neglect leads to overconfident predictions"
   "Countered by Margin of Safety: Using base rates naturally leads to more conservative estimates"
   "Enhanced by Second-Order Thinking: Base rates help predict second-order consequences"]})

(register-model
 {:name "skin-in-the-game"
  :category "incentives"
  :originator "Nassim Nicholas Taleb"
  :description "Skin in the game is the principle that people who make decisions should bear the consequences of those decisions, creating symmetry between risk and reward. When decision-makers have personal exposure to downside risk, they are incentivized to be more careful, honest, and aligned with long-term success. Conversely, when people can make decisions without bearing the consequences (asymmetric risk), they are incentivized to take excessive risks, be dishonest, or optimize for short-term gains at the expense of others. This principle is fundamental to understanding incentive structures, principal-agent problems, and the difference between real expertise (earned through risk-taking) and pseudo-expertise (no consequences for being wrong). Taleb argues that skin in the game is the ultimate filter for competence, honesty, and alignment. The 2008 financial crisis exemplified the catastrophic consequences of decision-makers having no skin in the game - bankers took enormous risks with other people's money, collected bonuses when things went well, and suffered no personal consequences when the system collapsed."
  :key-insight "Never take advice from someone who doesn't have skin in the game - they are not exposed to the consequences of being wrong"
  :application "Before trusting advice or delegating decisions, ask: Does this person bear the downside if they're wrong? What do they have to lose?"
  :real-world-examples
  ["2008 Financial Crisis: Bank executives took massive risks with depositors' money, collected $billions in bonuses, then got bailed out when bets failed - zero skin in the game"
   "Corporate boards: CEOs with significant stock ownership (skin in the game) outperform CEOs with mostly cash compensation by 6-8% annually"
   "Venture capital: VCs who invest personal money alongside fund capital (skin in the game) generate 3-4% higher returns than those who don't"
   "Consulting firms: Consultants recommend strategies but bear no consequences if recommendations fail - classic asymmetric risk"
   "Pilots vs engineers: Pilots have ultimate skin in the game (they die if plane crashes), leading to extreme safety culture - engineers who don't fly have less skin in game"]
  :quantitative-thresholds
  {:apply-when "Evaluating advice, delegating decisions, assessing expertise, designing incentive structures"
   :minimum-skin "Decision-maker should have at least 10-20% of their net worth at risk"
   :asymmetry-threshold "If upside/downside ratio exceeds 10:1, incentives are dangerously misaligned"
   :time-horizon "Skin in game must extend 3-5+ years to prevent short-term gaming"}
  :failure-modes
  [(failure "hidden-asymmetry" "critical"
            "Failing to detect that decision-makers have asymmetric risk exposure"
            :signals ["Focus on upside potential only" "No discussion of personal consequences" "Limited partnership structures" "Bonus without clawback" "Short vesting periods"]
            :safeguards ["Explicit risk exposure analysis" "Ask 'What do you have to lose?'" "Investigate compensation structures" "Look for clawback provisions" "Check time horizons"]
            :recovery-protocols ["When advice fails, document advisor's risk exposure" "Create checklist for assessing skin in game" "Build database of asymmetric risk structures"]
            :case-studies ["Lehman Brothers executives: Cashed out $billions before collapse, personally lost little despite firm bankruptcy"
                          "Theranos: Elizabeth Holmes had huge upside (billionaire status) but limited personal downside (other people's money) - enabled fraud"])
   (failure "false-skin" "high"
            "Mistaking apparent skin in game for real exposure"
            :signals ["Small absolute amounts at risk" "Risk is reputational not financial" "Diversified away exposure" "Insurance against downside" "Golden parachutes"]
            :safeguards ["Calculate risk as % of net worth" "Distinguish reputational from financial risk" "Check for hedging and insurance" "Examine exit packages" "Look for diversification"]
            :recovery-protocols ["When incentives fail, analyze actual vs apparent exposure" "Build framework for assessing real skin in game" "Document cases of false skin"]
            :case-studies ["CEOs with stock options but also huge severance packages - downside is protected"
                          "Fund managers with '2 and 20' fee structure but no personal capital invested - fees provide cushion against losses"])
   (failure "excessive-skin" "medium"
            "Having so much skin in game that decision-making becomes impaired"
            :signals ["Paralysis by fear" "Excessive risk aversion" "Missing obvious opportunities" "Emotional decision-making" "Inability to take necessary risks"]
            :safeguards ["Diversification to reduce concentration" "Separate decision-making from execution" "Use advisors with different risk profiles" "Set maximum exposure limits"]
            :recovery-protocols ["When excessive caution causes missed opportunities, reassess risk exposure" "Reduce concentration" "Bring in partners to share risk"]
            :case-studies ["Founders who refuse to take VC funding because of dilution, miss growth opportunities, company stagnates"
                          "Investors with entire net worth in one position, unable to make rational decisions due to fear"])
   (failure "short-term-skin" "high"
            "Having skin in game over wrong time horizon"
            :signals ["Quarterly focus" "Short vesting periods" "Flip mentality" "No long-term consequences" "Gaming short-term metrics"]
            :safeguards ["Long vesting periods (4+ years)" "Clawback provisions" "Multi-year performance measurement" "Deferred compensation" "Reputation tracking"]
            :recovery-protocols ["When short-term gaming occurs, extend time horizons" "Implement clawbacks" "Measure long-term outcomes"]
            :case-studies ["Stock options that vest in 1 year encourage pumping stock price then leaving"
                          "Quarterly earnings focus leads to cutting R&D and long-term investments to hit short-term numbers"])
   (failure "transferred-risk" "critical"
            "Decision-maker transfers risk to others while keeping upside"
            :signals ["Limited liability structures" "Debt-financed speculation" "Other people's money" "Bailout expectations" "Too big to fail"]
            :safeguards ["Require personal guarantees" "Limit leverage" "No bailouts policy" "Clawback provisions" "Reputation consequences"]
            :recovery-protocols ["When transferred risk causes problems, implement personal liability" "Reduce leverage" "Eliminate bailout expectations"]
            :case-studies ["2008 financial crisis: Banks used 30:1 leverage with depositors' money, knew they'd be bailed out"
                          "Private equity: Use massive leverage (other people's money) to buy companies, extract fees, leave debt holders with losses"])]
  :cross-model-interactions
  ["Combines with Principal-Agent Problem: Skin in game aligns principal and agent incentives"
   "Combines with Moral Hazard: Lack of skin in game creates moral hazard"
   "Combines with Lindy Effect: Advisors with long track records have more skin in game (reputation)"
   "Countered by Diversification: Diversification reduces skin in game for any single decision"
   "Enhanced by Margin of Safety: Skin in game makes you demand larger margins of safety"]})

(register-model
 {:name "survivorship-bias"
  :category "cognitive_bias"
  :originator "Nassim Nicholas Taleb / Abraham Wald"
  :description "Survivorship bias is the logical error of concentrating on entities that survived some selection process while overlooking those that did not, typically because of their lack of visibility. This leads to false conclusions because the failures are invisible - we only see the winners and assume their characteristics caused success, when in reality many entities with the same characteristics failed. The classic example is Abraham Wald's analysis of WWII bomber damage: the military wanted to armor the areas with the most bullet holes, but Wald realized they should armor the areas with NO bullet holes - because planes hit in those areas didn't survive to be counted. Survivorship bias is pervasive in business advice (we study successful companies, ignore the failures with identical strategies), investing (mutual funds that fail disappear from databases), and personal development (we hear from successful people, not the equally talented who failed due to luck). Taleb calls the invisible failures 'silent evidence' - they are the graveyard of strategies that didn't work."
  :key-insight "The invisible failures are as important as the visible successes - what you don't see matters as much as what you do see"
  :application "Before learning from success stories, ask: How many tried the same approach and failed? Where is the graveyard of failures?"
  :real-world-examples
  ["WWII aircraft armor: Military wanted to armor areas with bullet holes, Wald said armor areas WITHOUT holes - those planes didn't return"
   "Mutual fund performance: Average fund returns look great because failed funds disappear from database - actual investor returns are 2-3% lower"
   "Business book advice: 'Good to Great' companies were selected for success, but many with same practices failed - advice is survivorship bias"
   "Startup advice: 'Drop out of college like Gates/Jobs/Zuckerberg' ignores millions who dropped out and failed - survivorship bias makes it seem like good advice"
   "Stock picking: Newsletter track records look amazing because newsletters that underperform shut down - only survivors remain in database"]
  :quantitative-thresholds
  {:apply-when "Learning from success stories, evaluating strategies, analyzing historical data"
   :failure-rate-threshold "If failure rate exceeds 50%, survivorship bias is severe"
   :sample-bias "If sample includes only survivors, conclusions are invalid"
   :correction-factor "Adjust success rate by 2-3x to account for invisible failures"}
  :failure-modes
  [(failure "winner-worship" "critical"
            "Studying only successful entities and assuming their characteristics caused success"
            :signals ["Focus only on winners" "No analysis of failures" "Assuming correlation is causation" "Ignoring role of luck" "Advice based only on success stories"]
            :safeguards ["Study failures as much as successes" "Match successful and failed entities on key characteristics" "Analyze role of luck vs skill" "Look for survivorship bias in data" "Seek out silent evidence"]
            :recovery-protocols ["When strategy fails, identify how many others tried same approach" "Build database of failures" "Analyze what successful and failed entities had in common"]
            :case-studies ["'Good to Great' companies: Many had same practices as companies that failed - practices weren't causal"
                          "Startup advice from successful founders: Ignores thousands who followed same advice and failed"])
   (failure "database-truncation" "high"
            "Using datasets that automatically exclude failures"
            :signals ["Only active entities in dataset" "Historical data that excludes defunct entities" "Performance data from surviving funds/companies" "No delisting/failure data"]
            :safeguards ["Check for survivorship bias in data sources" "Seek out complete datasets including failures" "Adjust returns for survivorship bias" "Use inception-to-date returns"]
            :recovery-protocols ["When using historical data, verify it includes failures" "Find datasets with complete history" "Apply survivorship bias corrections"]
            :case-studies ["Mutual fund databases: Average returns of 12% look great, but 40% of funds failed and were removed - actual returns closer to 8%"
                          "Stock market historical returns: Exclude companies that went bankrupt, making market look safer than it is"])
   (failure "narrative-fallacy" "high"
            "Creating causal stories from survivor characteristics while ignoring that failures had same characteristics"
            :signals ["Compelling success stories" "Clear causal narratives" "Ignoring counterexamples" "No mention of failures with same traits" "Hindsight bias"]
            :safeguards ["Actively seek counterexamples" "Study matched pairs (success vs failure with same traits)" "Acknowledge role of randomness" "Test narratives against failure data"]
            :recovery-protocols ["When narrative fails, identify failures that followed same narrative" "Build database of failed narratives" "Practice probabilistic thinking"]
            :case-studies ["'Visionary leadership' narrative: Many failed companies had equally visionary leaders - leadership wasn't sufficient"
                          "'First mover advantage' narrative: Most first movers failed, but we only remember the survivors (Amazon, eBay)"])
   (failure "skill-luck-confusion" "critical"
            "Attributing success to skill when luck played major role, because failures are invisible"
            :signals ["Overconfidence in repeating success" "Ignoring role of timing/luck" "Assuming success is replicable" "No acknowledgment of randomness"]
            :safeguards ["Analyze base rates of success" "Study role of luck vs skill" "Look at success rates of repeated attempts" "Use statistical analysis to separate skill from luck"]
            :recovery-protocols ["When attempting to replicate success, analyze how much was luck" "Study regression to mean" "Build probabilistic models"]
            :case-studies ["Hedge fund performance: Top performers often regress to mean - much of outperformance was luck, not skill"
                          "Bestselling books: Most successful authors can't repeat success - initial success had large luck component"])
   (failure "invisible-graveyard" "critical"
            "Not seeking out the graveyard of failures to learn from"
            :signals ["No analysis of what didn't work" "Only studying successes" "Ignoring silent evidence" "Not asking 'who tried this and failed?'"]
            :safeguards ["Actively seek out failures" "Study graveyards (defunct companies, failed funds, etc.)" "Interview people who failed" "Build failure databases"]
            :recovery-protocols ["When evaluating strategy, find and study failures" "Create systematic process for finding silent evidence" "Build relationships with people who failed"]
            :case-studies ["VC portfolio analysis: Studying only successful investments ignores 90% of portfolio that failed - can't learn from partial data"
                          "Business strategy: Studying only successful companies ignores hundreds that tried same strategy and failed"])]
  :cross-model-interactions
  ["Combines with Confirmation Bias: We seek out success stories that confirm our beliefs, ignore failures"
   "Combines with Availability Bias: Successes are more visible/memorable than failures"
   "Combines with Narrative Fallacy: We create causal stories from survivors, ignore that failures had same traits"
   "Countered by Base Rate Neglect: Using base rates forces you to account for failures"
   "Enhanced by Inversion: Inverting the question ('what failed?') reveals survivorship bias"]})

(register-model
 {:name "antifragility"
  :category "systems_thinking"
  :originator "Nassim Nicholas Taleb"
  :description "Antifragility is a property of systems that gain from disorder, volatility, and stress - going beyond robustness or resilience. While fragile systems break under stress and robust systems resist stress, antifragile systems actually improve and grow stronger when exposed to stressors, shocks, and volatility (up to a point). This is fundamentally different from resilience, which merely describes the ability to recover from shocks. Antifragile systems have convex exposure to randomness - they benefit more from positive volatility than they suffer from negative volatility. Examples include the immune system (gets stronger from exposure to pathogens), muscles (grow from stress of exercise), evolution (improves through selection pressure), and optionality (benefits from volatility). Taleb argues that antifragility is essential for long-term survival in complex, unpredictable environments. The key is to have small, frequent stressors that make the system stronger, while avoiding large, catastrophic stressors that can destroy it. Many modern systems have become fragile by eliminating small stressors (overprotection) while becoming vulnerable to large shocks."
  :key-insight "Don't just survive stress - design systems that gain from disorder and become stronger through volatility"
  :application "Ask: How can I benefit from volatility? What small stressors will make this system stronger? Where do I have convex exposure?"
  :real-world-examples
  ["Immune system: Exposure to pathogens makes it stronger - overprotection (excessive hygiene) makes it fragile"
   "Muscles: Stress from exercise causes growth - no stress leads to atrophy"
   "Restaurants: 50% fail in first year (high volatility) but survivors are battle-tested and strong - low volatility would mean weak restaurants survive"
   "Tech startups: Rapid iteration and failure makes survivors stronger - companies that avoid stress become fragile"
   "Venture capital: Portfolio approach with convex payoffs - small losses on most investments, massive gains on few - benefits from volatility"]
  :quantitative-thresholds
  {:apply-when "Designing systems, managing risk, building organizations, personal development"
   :stressor-frequency "Small stressors should occur frequently (daily/weekly) to build antifragility"
   :stressor-magnitude "Stressors should be 5-20% of capacity - enough to challenge but not destroy"
   :convexity-ratio "Upside should be 5-10x downside for antifragile exposure"
   :recovery-time "System should recover from stressor within 1-2x the stressor duration"}
  :failure-modes
  [(failure "overprotection" "critical"
            "Eliminating all stressors, making system fragile to inevitable shocks"
            :signals ["Zero failure tolerance" "Excessive safety measures" "Avoiding all risk" "Protecting from small stressors" "No exposure to volatility"]
            :safeguards ["Introduce small, controlled stressors" "Practice failure" "Expose to volatility" "Build in redundancy not protection" "Allow small failures"]
            :recovery-protocols ["When major shock causes failure, introduce small stressors to build strength" "Gradually increase exposure" "Practice stress testing"]
            :case-studies ["Helicopter parenting: Protecting children from all stress makes them fragile adults unable to handle adversity"
                          "Too-big-to-fail banks: Protected from small failures, became fragile to systemic shocks, required bailouts in 2008"])
   (failure "excessive-stressor" "critical"
            "Applying stressors too large or frequent, destroying rather than strengthening system"
            :signals ["System breaking down" "No recovery time" "Cascading failures" "Exhaustion" "Permanent damage"]
            :safeguards ["Start with small stressors" "Allow recovery time" "Monitor system capacity" "Gradual increase in stress" "Emergency shutdown mechanisms"]
            :recovery-protocols ["When system breaks, reduce stressor magnitude" "Increase recovery time" "Rebuild capacity gradually"]
            :case-studies ["Overtraining in athletics: Too much stress without recovery leads to injury and performance decline"
                          "Startup death march: Excessive stress with no recovery time leads to burnout and failure"])
   (failure "fragile-optimization" "high"
            "Optimizing for efficiency at the expense of antifragility"
            :signals ["Just-in-time everything" "No redundancy" "Single points of failure" "Maximum efficiency" "No slack"]
            :safeguards ["Build in redundancy" "Maintain optionality" "Keep slack resources" "Avoid over-optimization" "Stress test systems"]
            :recovery-protocols ["When optimized system fails, add redundancy" "Build in buffers" "Create multiple pathways"]
            :case-studies ["Supply chain optimization: Just-in-time inventory is efficient but fragile - COVID-19 exposed this vulnerability"
                          "Financial leverage: Maximizing returns through leverage is efficient but fragile - 2008 crisis showed the cost"])
   (failure "linear-thinking" "high"
            "Assuming linear relationships when system has nonlinear (convex/concave) exposure"
            :signals ["Ignoring tail risks" "Assuming proportional responses" "Missing threshold effects" "Not recognizing convexity"]
            :safeguards ["Map nonlinearities" "Identify convex/concave exposures" "Stress test for tail events" "Use scenario analysis"]
            :recovery-protocols ["When nonlinear event occurs, map the nonlinearity" "Identify other nonlinear exposures" "Adjust mental models"]
            :case-studies ["Option pricing: Linear thinking misses convex payoff structure - small moves have little effect, large moves have massive effect"
                          "Pandemic response: Linear thinking about exponential growth led to delayed action and catastrophic outcomes"])
   (failure "missing-optionality" "high"
            "Failing to create optionality and convex exposure to benefit from volatility"
            :signals ["No upside exposure" "Symmetric risk/reward" "No portfolio approach" "Single path dependency" "No experiments"]
            :safeguards ["Create multiple options" "Seek convex payoffs" "Portfolio approach" "Run experiments" "Keep options open"]
            :recovery-protocols ["When facing volatility without benefit, create optionality" "Build portfolio of bets" "Seek asymmetric payoffs"]
            :case-studies ["Career: Single job/skill is fragile, portfolio of skills with optionality is antifragile"
                          "R&D: Single big bet is fragile, portfolio of experiments with convex payoffs is antifragile"])]
  :cross-model-interactions
  ["Combines with Optionality: Options provide antifragile exposure - benefit from volatility"
   "Combines with Barbell Strategy: Barbell creates antifragility through convex exposure"
   "Combines with Via Negativa: Removing fragilities is often better than adding robustness"
   "Countered by Efficiency: Optimizing for efficiency often reduces antifragility"
   "Enhanced by Hormesis: Small doses of stressors create antifragility"]})

(register-model
 {:name "optionality"
  :category "decision_making"
  :originator "Nassim Nicholas Taleb"
  :description "Optionality is the property of having asymmetric payoffs where you benefit more from favorable outcomes than you lose from unfavorable ones, creating convex exposure to uncertainty. An option gives you the right but not the obligation to take an action, meaning you can benefit from upside while limiting downside. This creates positive asymmetry: you participate in gains but are protected from losses. Optionality is valuable in uncertain environments because you don't need to predict the future - you simply need exposure to positive volatility. The key insight is that options are more valuable when volatility is high, so in uncertain environments, you should seek optionality rather than trying to make precise predictions. Taleb argues that many of history's greatest successes came from optionality (tinkering, trial and error, serendipity) rather than top-down planning. Venture capital, R&D, career development, and personal relationships all benefit from optionality. The cost of optionality is the premium you pay to keep options open, but in volatile environments, this is often worth far more than the cost."
  :key-insight "In uncertainty, seek optionality - asymmetric payoffs where you benefit from volatility without proportional downside"
  :application "Ask: What options do I have? How can I benefit from upside while limiting downside? What's the cost of keeping options open?"
  :real-world-examples
  ["Venture capital: Invest in 20 startups, lose 1x on 18, make 100x on 2 - optionality creates massive asymmetry"
   "R&D: Run 100 experiments, 95 fail (small losses), 5 succeed (massive gains) - optionality makes this profitable"
   "Career: Build multiple skills and relationships, keep options open, pivot when opportunities arise - optionality beats single-path optimization"
   "Real estate: Option to buy property at fixed price - benefit if price rises, walk away if it falls - pure optionality"
   "Tech platforms: Build platform that enables many use cases, benefit from unexpected applications - optionality creates value"]
  :quantitative-thresholds
  {:apply-when "Making decisions under uncertainty, designing strategies, managing portfolios"
   :asymmetry-ratio "Seek situations where upside is 5-10x+ downside"
   :option-cost "Pay up to 10-20% of expected value to maintain optionality"
   :volatility-threshold "Optionality becomes more valuable when volatility exceeds 30-40%"
   :portfolio-size "Need 10-20+ options to benefit from convexity"}
  :failure-modes
  [(failure "premature-commitment" "critical"
            "Committing too early and eliminating optionality"
            :signals ["Irreversible decisions" "Burning bridges" "Single path" "No backup plans" "All-in mentality"]
            :safeguards ["Keep options open as long as possible" "Make reversible decisions" "Maintain multiple paths" "Delay commitment until necessary" "Build in exit options"]
            :recovery-protocols ["When committed path fails, identify what options were lost" "Build optionality into future decisions" "Create framework for preserving options"]
            :case-studies ["Startup pivots: Companies that maintain optionality (multiple product ideas, flexible technology) can pivot when initial idea fails"
                          "Career: Specialists who commit too early lose optionality - generalists who keep options open can adapt to changing markets"])
   (failure "option-cost-blindness" "high"
            "Not recognizing or paying the cost to maintain optionality"
            :signals ["Free option assumption" "Ignoring opportunity costs" "Not investing in maintaining options" "Letting options expire"]
            :safeguards ["Calculate cost of optionality" "Budget for maintaining options" "Recognize opportunity costs" "Actively manage option portfolio"]
            :recovery-protocols ["When options expire, calculate cost of maintaining them" "Build budget for optionality" "Track option value over time"]
            :case-studies ["Relationships: Not investing time to maintain relationships means losing optionality when you need help"
                          "Skills: Not practicing skills means losing optionality when market shifts"])
   (failure "false-optionality" "high"
            "Believing you have optionality when you actually have symmetric or concave exposure"
            :signals ["Hidden downside risks" "Correlated options" "Illusion of choice" "Symmetric payoffs" "No real asymmetry"]
            :safeguards ["Analyze actual payoff structure" "Map downside scenarios" "Check for correlation" "Verify asymmetry" "Stress test options"]
            :recovery-protocols ["When 'option' fails, analyze actual payoff structure" "Identify what made it symmetric" "Build framework for assessing true optionality"]
            :case-studies ["Diversification fallacy: Owning 20 correlated stocks isn't optionality - they all go down together"
                          "Job offers: Multiple offers that are essentially identical don't provide real optionality"])
   (failure "option-hoarding" "medium"
            "Maintaining too many options and never exercising any"
            :signals ["Analysis paralysis" "Never committing" "Spreading too thin" "Missing opportunities" "High option costs"]
            :safeguards ["Set decision deadlines" "Calculate cost of waiting" "Recognize when to exercise" "Focus on highest-value options" "Prune low-value options"]
            :recovery-protocols ["When opportunities pass, analyze cost of waiting" "Develop framework for option exercise" "Set clear exercise criteria"]
            :case-studies ["Career: Keeping too many options open means never developing deep expertise - need to exercise some options"
                          "Investing: Holding too much cash (optionality) means missing compounding - need to deploy capital"])
   (failure "convexity-blindness" "critical"
            "Not recognizing or seeking convex payoff structures"
            :signals ["Linear thinking" "Symmetric risk/reward" "No portfolio approach" "Avoiding volatility" "Missing asymmetry"]
            :safeguards ["Map payoff structures" "Seek convex exposure" "Build portfolios" "Embrace volatility" "Look for asymmetry"]
            :recovery-protocols ["When missing opportunities, analyze payoff structures" "Identify convex opportunities" "Build convexity into strategy"]
            :case-studies ["Startup employees: Salary is linear, equity is convex - taking equity creates optionality"
                          "R&D: Incremental improvements are linear, breakthrough innovations are convex - need portfolio of convex bets"])]
  :cross-model-interactions
  ["Combines with Antifragility: Optionality creates antifragile exposure - benefit from volatility"
   "Combines with Barbell Strategy: Barbell creates optionality through extreme allocation"
   "Combines with Asymmetric Risk: Optionality is the ultimate asymmetric risk structure"
   "Countered by Commitment: Sometimes commitment is necessary and optionality is costly"
   "Enhanced by Volatility: Higher volatility makes optionality more valuable"]})

;; ============================================
;; END OF ITERATION 21 MODELS
;; ============================================
