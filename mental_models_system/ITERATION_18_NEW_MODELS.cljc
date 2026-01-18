;; ============================================
;; ITERATION 18: Enhanced Mental Models
;; Date: 2026-01-18
;; Focus: Taleb, Kahneman, Ole Peters
;; ============================================

;; New models to add to models.cljc

(register-model
 {:name "skin-in-the-game"
  :category "decision_making"
  :originator "Nassim Nicholas Taleb"
  :description "Exposure to the real-world consequences of one's decisions and advice. When you have skin in the game, you share the risk and reward of your actions, creating proper incentive alignment. Asymmetric risk-reward structures, where some benefit from decisions while others bear the costs, are fundamentally unethical and lead to systemic fragility. The concept applies across domains: business leaders should own significant equity, advisors should invest alongside clients, policymakers should live under the laws they create, and experts should face consequences for bad predictions. Historical examples include Hammurabi's Code (builders who constructed unsafe buildings were executed if they collapsed), Roman engineers standing under bridges they built during opening ceremonies, and ship captains being last to leave sinking vessels. Modern violations include: 2008 financial crisis where bankers took bonuses while taxpayers absorbed losses, consultants who profit regardless of advice quality, and academics who theorize without practical experience. The principle is not just about fairness but about information flow—those with skin in the game have better information and stronger incentives to be right. As Taleb notes, 'Never trust anyone who doesn't have skin in the game. Without it, fools and crooks will benefit, and their mistakes will never come back to haunt them.'"
  :key-insight "Symmetry of risk and reward is essential for ethical decision-making and system stability"
  :application "Before accepting advice or making decisions, ask: Does this person share in the downside? Will they suffer if they're wrong?"
  :quantitative-thresholds
  {:executive-equity "CEOs should own 5-10x annual salary in company stock"
   :advisor-alignment "Financial advisors should invest minimum 50% of personal wealth in recommended strategies"
   :consultant-fees "At least 30% of consulting fees should be performance-based"
   :prediction-accuracy "Public forecasters should track and publish prediction accuracy (aim for >70%)"
   :regulatory-compliance "Regulators should have worked minimum 5 years in industries they regulate"}
  :failure-modes
  [(failure "asymmetric-risk-transfer" "critical"
            "Benefiting from upside while transferring downside to others"
            :signals ["Bonus structures disconnected from long-term outcomes"
                     "Limited liability with unlimited upside"
                     "Advice given without personal investment"
                     "Decisions made by those who won't bear consequences"
                     "Golden parachutes and bailout expectations"]
            :safeguards ["Mandatory equity ownership for executives (5-10x salary)"
                        "Clawback provisions for bonuses (3-5 year vesting)"
                        "Personal liability for gross negligence"
                        "Require advisors to invest alongside clients"
                        "Skin-in-game verification before accepting advice"]
            :case-studies
            [{:event "2008 Financial Crisis"
              :date "2007-2009"
              :description "Investment bankers created and sold toxic mortgage-backed securities while taking massive bonuses. When securities collapsed, bankers kept bonuses while taxpayers absorbed $700B+ in losses."
              :quantitative-impact "Banks lost $2.8T in market value, executives kept $140B in bonuses (2000-2008), taxpayers paid $700B bailout"
              :skin-in-game-violation "Executives had <2% personal wealth in firm equity, bonus structures rewarded short-term volume over long-term quality"
              :outcome "Lehman Brothers bankruptcy, Bear Stearns collapse, AIG bailout, Great Recession"}
             {:event "Long-Term Capital Management Collapse"
              :date "1998"
              :description "Nobel Prize-winning economists created hedge fund using complex models, took massive leverage (25:1), then required $3.6B Fed-orchestrated bailout when models failed."
              :quantitative-impact "$4.6B fund collapsed, $1T+ in derivatives exposure, systemic risk to global financial system"
              :skin-in-game-violation "Partners had significant personal wealth invested BUT used extreme leverage that transferred tail risk to counterparties and financial system"
              :outcome "Partners lost personal wealth but systemic risk was socialized, demonstrating that even personal investment insufficient if leverage transfers tail risk"}])
   (failure "false-skin" "high"
            "Appearing to have skin in the game while actually protected from downside"
            :signals ["Stock options without holding requirements"
                     "Insurance or hedges that eliminate personal risk"
                     "Reputational risk only (no financial consequences)"
                     "Short time horizons that avoid long-term consequences"
                     "Ability to exit before consequences manifest"]
            :safeguards ["Minimum holding periods for equity (3-5 years post-vesting)"
                        "Prohibition on hedging company stock"
                        "Deferred compensation tied to long-term outcomes"
                        "Public track records that follow individuals"
                        "Verification of actual exposure, not just nominal ownership"]
            :case-studies
            [{:event "Enron Executive Stock Sales"
              :date "2000-2001"
              :description "Executives sold $1.1B in stock while publicly promoting company, employees lost $1.2B in retirement savings when company collapsed."
              :quantitative-impact "Executives cashed out $1.1B, 20,000 employees lost jobs and $1.2B in 401(k) savings, shareholders lost $74B"
              :skin-in-game-violation "Executives had nominal stock ownership but sold shares while promoting stock to employees, eliminating their downside exposure"
              :outcome "Criminal convictions for fraud, but executives had already extracted wealth"}])
   (failure "expert-without-consequences" "high"
            "Giving advice or making predictions without bearing cost of being wrong"
            :signals ["Pundits who are never held accountable for predictions"
                     "Consultants paid regardless of outcomes"
                     "Academics theorizing without practical experience"
                     "Regulators who never worked in regulated industry"
                     "Forecasters who don't track accuracy"]
            :safeguards ["Track and publish prediction accuracy publicly"
                        "Performance-based compensation (minimum 30% of fees)"
                        "Require practical experience before theorizing"
                        "Mandatory disclosure of conflicts and track record"
                        "Reputation systems that follow individuals across roles"]
            :case-studies
            [{:event "Economic Forecaster Accuracy Study"
              :date "2000-2020"
              :description "Analysis of 6,000+ economic forecasts showed professional economists predicted only 2 of 150 recessions one year in advance, yet face no consequences for inaccuracy."
              :quantitative-impact "Prediction accuracy: 1.3% for recessions, yet forecasters maintain credibility and compensation"
              :skin-in-game-violation "Forecasters paid for forecasts regardless of accuracy, no tracking or accountability for predictions"
              :outcome "Systematic overconfidence in economic predictions, misallocation of capital based on flawed forecasts"}])
   (failure "principal-agent-misalignment" "critical"
            "Agents acting in their interest rather than principals' due to misaligned incentives"
            :signals ["Short-term incentives for agents, long-term consequences for principals"
                     "Agents can exit before consequences manifest"
                     "Compensation based on activity/volume rather than outcomes"
                     "Information asymmetry favoring agents"
                     "Agents diversified across many principals, principals concentrated"]
            :safeguards ["Long-term incentive alignment (3-5 year vesting)"
                        "Significant personal investment required from agents"
                        "Transparency and information symmetry"
                        "Reputation systems and public track records"
                        "Fiduciary standards with legal liability"]
            :case-studies
            [{:event "Mutual Fund Manager Underperformance"
              :date "1990-2020"
              :description "85% of actively managed mutual funds underperform index funds over 15 years, yet managers collect 1-2% annual fees regardless of performance."
              :quantitative-impact "Investors paid $100B+ annually in fees for underperformance, managers collected fees regardless of returns"
              :skin-in-game-violation "Managers compensated on assets under management (AUM) not performance, can collect fees while destroying value"
              :outcome "Massive wealth transfer from investors to managers, systematic underperformance persists"}])
   (failure "regulatory-capture" "critical"
            "Regulators serving industry interests rather than public due to lack of skin in game"
            :signals ["Revolving door between regulators and industry"
                     "Regulators who never worked in industry they regulate"
                     "Rules that favor incumbents over competition"
                     "Enforcement that targets small players, not systemically important firms"
                     "Regulatory complexity that only large firms can navigate"]
            :safeguards ["Cooling-off periods (5+ years) before regulators can join industry"
                        "Require regulators to have industry experience before regulating"
                        "Personal liability for regulatory failures"
                        "Transparent decision-making and public accountability"
                        "Skin-in-game through living under regulations created"]
            :case-studies
            [{:event "SEC and 2008 Financial Crisis"
              :date "2000-2008"
              :description "SEC allowed investment banks to increase leverage from 12:1 to 30:1 in 2004, failed to detect Madoff fraud despite warnings, many SEC officials later joined firms they regulated."
              :quantitative-impact "Leverage increase contributed to $2.8T in bank losses, Madoff fraud: $65B, 33% of SEC senior officials joined financial industry within 2 years of leaving"
              :skin-in-game-violation "Regulators had no personal exposure to consequences of lax regulation, incentivized to be friendly to potential future employers"
              :outcome "Regulatory failure contributed to systemic crisis, revolving door continues"}])]
  :cross-model-interactions
  [{:model "circle-of-competence"
    :interaction "Skin in the game forces you to stay within your circle—you can't afford to be wrong outside it"
    :lollapalooza-potential "high"
    :example "Warren Buffett's rule: only invest in businesses you understand AND have significant personal wealth invested"}
   {:model "margin-of-safety"
    :interaction "When you have skin in the game, you naturally demand larger margins of safety"
    :lollapalooza-potential "high"
    :example "Builders who must stand under their bridges demand higher safety margins"}
   {:model "inversion"
    :interaction "Invert: What happens if I'm wrong? Skin in the game makes this question visceral, not theoretical"
    :lollapalooza-potential "high"
    :example "Taleb's barbell strategy: maximum skin in game on safe side, limited skin in game on risky side"}
   {:model "incentive-bias"
    :interaction "Skin in the game aligns incentives, reducing bias from misaligned incentives"
    :lollapalooza-potential "critical"
    :example "Executives with significant equity ownership make different decisions than those with only salary"}]
  :historical-wisdom
  [{:source "Hammurabi's Code (1750 BC)"
    :principle "If a builder builds a house and it collapses, killing the owner, the builder shall be put to death"
    :skin-in-game "Ultimate skin in the game—life for life—ensured quality construction"}
   {:source "Roman Engineering"
    :principle "Engineers stood under bridges and aqueducts during opening ceremonies"
    :skin-in-game "Personal safety dependent on quality of work, no ability to transfer risk"}
   {:source "Ship Captains"
    :principle "Captain is last to leave sinking ship, goes down with ship if necessary"
    :skin-in-game "Personal survival tied to vessel safety, creates maximum incentive for caution"}]
  :modern-applications
  [{:domain "Executive Compensation"
    :recommendation "CEO equity ownership should be 5-10x annual salary, with 5-year holding requirements and clawback provisions"
    :rationale "Aligns executive incentives with long-term shareholder value, prevents short-term extraction"}
   {:domain "Financial Advice"
    :recommendation "Advisors must invest minimum 50% of personal wealth in strategies they recommend, with public disclosure"
    :rationale "Ensures advisors share client risk, eliminates conflicts from commission-based incentives"}
   {:domain "Consulting"
    :recommendation "Minimum 30% of fees should be performance-based, paid only if recommendations succeed"
    :rationale "Aligns consultant incentives with client outcomes, eliminates incentive to recommend unnecessary work"}
   {:domain "Public Policy"
    :recommendation "Policymakers should live under laws they create, with no exemptions for government officials"
    :rationale "Ensures laws are practical and fair, prevents creation of rules that benefit elites at expense of public"}
   {:domain "Forecasting"
    :recommendation "All public forecasts must be tracked and accuracy published, with compensation tied to accuracy"
    :rationale "Creates accountability for predictions, reduces overconfident forecasting"}]})

;; ============================================
;; Model 2: Base Rate Neglect
;; ============================================

(register-model
 {:name "base-rate-neglect"
  :category "psychology"
  :originator "Daniel Kahneman & Amos Tversky"
  :description "The systematic tendency to ignore or underweight statistical base rates (prior probabilities) in favor of specific case information. When evaluating probability, people focus on individuating information about the specific case while neglecting the underlying statistical frequency in the reference class. This is a fundamental violation of Bayesian reasoning. For example, if a test is 95% accurate and a disease has 1% prevalence, a positive test result means only 16% chance of having the disease—but most people (including doctors) estimate 95%. The error stems from representativeness heuristic: we judge probability by how well something matches our mental prototype rather than by actual statistical frequency. Base rate neglect is pervasive in: startup success predictions (ignoring 90% failure rate), project planning (ignoring that 70% of projects exceed budget), medical diagnosis (ignoring disease prevalence), hiring (ignoring that most candidates fail), and investment (ignoring that most active managers underperform). Kahneman's research showed even statisticians and trained professionals fall prey to this bias. The planning fallacy—systematic underestimation of time and cost—is a special case of base rate neglect. The outside view (reference class forecasting) is the antidote: identify a reference class, determine base rate in that class, adjust for specific case details. As Kahneman notes, 'The most useful thing I've learned from decades of research is the outside view.'"
  :key-insight "Statistical base rates are more predictive than case-specific details, but our minds systematically ignore them"
  :application "Before making predictions, ask: What is the base rate for this type of situation? Start with the base rate, then adjust."
  :quantitative-thresholds
  {:startup-success "Base rate: 10% of startups succeed, 90% fail within 5 years"
   :project-planning "Base rate: 70% of projects exceed budget, 60% exceed timeline"
   :active-management "Base rate: 85% of active fund managers underperform index over 15 years"
   :medical-diagnosis "Must consider disease prevalence (base rate) before interpreting test results"
   :hiring-success "Base rate: 50-70% of hires work out, depending on industry and role"}
  :failure-modes
  [(failure "ignoring-reference-class" "critical"
            "Failing to identify and use relevant statistical base rates"
            :signals ["Making predictions without checking historical frequency"
                     "Focusing entirely on case-specific details"
                     "Believing 'this time is different' without evidence"
                     "Not asking 'what usually happens in situations like this?'"
                     "Overconfidence in predictions despite poor track record"]
            :safeguards ["Mandatory reference class forecasting for all predictions"
                        "Identify relevant reference class before analyzing case"
                        "Look up base rate statistics before making estimates"
                        "Start with base rate, then adjust for case specifics"
                        "Track prediction accuracy against base rates"]
            :case-studies
            [{:event "Sydney Opera House Project"
              :date "1957-1973"
              :description "Estimated 4 years and $7M, actually took 16 years and $102M (14.5x over budget). Classic planning fallacy from ignoring base rate of large construction projects."
              :quantitative-impact "Budget: $7M → $102M (1,357% overrun), Timeline: 4 years → 16 years (300% overrun)"
              :base-rate "Large construction projects: 70% exceed budget by average of 50-100%"
              :outcome "If base rate had been used (assume 2x budget, 2x time), estimate would have been $14M/8 years—still wrong but much closer"}])
   (failure "representativeness-override" "high"
            "Letting case-specific details override statistical base rates"
            :signals ["Vivid anecdotes overriding statistics"
                     "Believing detailed stories over numerical data"
                     "Thinking specific case is 'special' or 'different'"
                     "Overweighting personal experience vs. statistical evidence"
                     "Narrative fallacy: creating coherent story that ignores base rates"]
            :safeguards ["Explicitly state base rate before considering case details"
                        "Use Bayesian updating: start with prior, adjust with evidence"
                        "Require quantitative justification for deviating from base rate"
                        "Separate base rate analysis from case-specific analysis"
                        "Use reference class forecasting methodology"]
            :case-studies
            [{:event "Startup Founder Overconfidence"
              :date "Ongoing"
              :description "Study of 3,000 startup founders: 81% believed they had 70%+ chance of success, actual success rate was 10%. Founders focused on their specific advantages while ignoring base rate."
              :quantitative-impact "Predicted success: 70%+, Actual success: 10%, Overconfidence factor: 7x"
              :base-rate "90% of startups fail within 5 years"
              :outcome "Systematic overconfidence leads to insufficient risk management, undercapitalization, and preventable failures"}])
   (failure "planning-fallacy" "critical"
            "Systematic underestimation of time and cost due to ignoring base rates"
            :signals ["Optimistic project timelines without historical reference"
                     "Best-case scenario planning"
                     "Ignoring that past projects ran over budget/schedule"
                     "Inside view dominance: focusing on project specifics not reference class"
                     "Surprise when projects exceed estimates"]
            :safeguards ["Reference class forecasting: find similar past projects"
                        "Use base rate: 70% of projects exceed budget by 50%+"
                        "Multiply initial estimates by 2-3x for realistic planning"
                        "Track historical accuracy of estimates"
                        "Use outside view: what happened to similar projects?"]
            :case-studies
            [{:event "Big Dig Boston"
              :date "1991-2007"
              :description "Highway project estimated at $2.6B and 10 years, actually cost $14.6B and took 16 years. Classic planning fallacy."
              :quantitative-impact "Budget: $2.6B → $14.6B (462% overrun), Timeline: 10 years → 16 years (60% overrun)"
              :base-rate "Large infrastructure projects: average 45% cost overrun, 20% time overrun"
              :outcome "If base rate had been applied (1.5x budget, 1.2x time), estimate would have been $3.9B/12 years—still underestimated but closer"}])
   (failure "medical-diagnosis-error" "critical"
            "Misinterpreting test results by ignoring disease prevalence (base rate)"
            :signals ["Assuming positive test means high probability of disease"
                     "Not considering disease prevalence before interpreting test"
                     "Overweighting test accuracy, underweighting base rate"
                     "Doctors and patients both make this error"
                     "False positive problem in low-prevalence screening"]
            :safeguards ["Always calculate using Bayes' theorem"
                        "Consider disease prevalence (base rate) first"
                        "Use natural frequencies instead of percentages"
                        "Require Bayesian reasoning in medical training"
                        "Provide decision support tools for probability calculations"]
            :case-studies
            [{:event "Mammography False Positives"
              :date "Ongoing"
              :description "Mammography is 90% sensitive, 93% specific. Breast cancer prevalence is 1%. Positive test → only 11% chance of cancer, but most doctors estimate 70-90%."
              :quantitative-impact "Doctor estimates: 70-90% probability, Actual probability: 11% (Bayes' theorem), Error factor: 6-8x"
              :base-rate "Breast cancer prevalence: 1% in screening population"
              :outcome "Overestimation of cancer probability leads to unnecessary biopsies, anxiety, and overtreatment"}])
   (failure "investment-overconfidence" "high"
            "Believing you can beat the market despite base rate of underperformance"
            :signals ["Picking individual stocks despite evidence of underperformance"
                     "Hiring active managers despite 85% underperformance rate"
                     "Believing you're in the 15% who outperform"
                     "Ignoring base rate of active management failure"
                     "Overweighting recent performance, underweighting long-term base rate"]
            :safeguards ["Start with base rate: 85% of active managers underperform over 15 years"
                        "Require extraordinary evidence to deviate from index investing"
                        "Track personal investment performance vs. index"
                        "Use Bayesian updating: start with base rate, adjust with evidence"
                        "Acknowledge that you're probably not in the 15%"]
            :case-studies
            [{:event "Active Management Underperformance"
              :date "2005-2020"
              :description "SPIVA study: 85% of active large-cap managers underperformed S&P 500 over 15 years, yet investors continue paying higher fees for active management."
              :quantitative-impact "Underperformance rate: 85% over 15 years, Average underperformance: 1-2% annually, Cumulative cost: 20-30% of wealth over 15 years"
              :base-rate "85% of active managers underperform index over 15 years"
              :outcome "Investors systematically destroy wealth by ignoring base rate and believing they can identify the 15% who outperform"}])]
  :cross-model-interactions
  [{:model "margin-of-safety"
    :interaction "Base rates tell you what margin of safety you need—if 70% of projects exceed budget, you need 2x margin"
    :lollapalooza-potential "high"
    :example "Munger: 'We don't try to jump over 7-foot bars; we look for 1-foot bars we can step over.' Base rates tell you bar height."}
   {:model "circle-of-competence"
    :interaction "Base rates help define your circle—if you can't beat base rate, you're outside your circle"
    :lollapalooza-potential "high"
    :example "If you can't beat index fund returns, stock picking is outside your circle of competence"}
   {:model "inversion"
    :interaction "Invert: What's the base rate of failure? Start there, then explain why you'll succeed"
    :lollapalooza-potential "critical"
    :example "90% of startups fail. Why will yours be in the 10%? Requires extraordinary evidence."}]})

;; ============================================
;; Model 3: Survivorship Bias
;; ============================================

(register-model
 {:name "survivorship-bias"
  :category "psychology"
  :originator "Nassim Nicholas Taleb"
  :description "The logical error of concentrating on entities that survived a selection process while overlooking those that did not, leading to false conclusions. We see the winners and forget the losers, creating a distorted view of reality. The classic example: during WWII, the military wanted to armor planes where they saw bullet holes. Statistician Abraham Wald pointed out the error—planes with holes in those areas survived; armor should go where there were NO holes, because planes hit there didn't return. This insight saved countless lives. Survivorship bias is everywhere: we study successful companies and miss the lessons from failures (for every Apple, 1,000 failed), we read biographies of billionaires and ignore the identical strategies that led to bankruptcy, we see mutual funds with good track records and miss that poor performers were closed and erased from history, we hear startup success stories and ignore the 90% that failed. The bias is compounded by narrative fallacy—we create coherent stories about why survivors succeeded, ignoring that failed entities often did the same things. As Taleb emphasizes, the 'silent evidence' of failures is often more informative than the loud evidence of success. The cemetery of failed strategies is larger and more instructive than the museum of successful ones."
  :key-insight "The dead don't talk—absence of evidence is not evidence of absence, and survivors are not representative"
  :application "Before drawing lessons from success, ask: How many tried the same thing and failed? What am I not seeing?"
  :quantitative-thresholds
  {:mutual-fund-survival "50% of mutual funds close within 10 years, their records disappear from databases"
   :startup-survival "90% of startups fail within 5 years, we only hear about the 10%"
   :business-book-bias "For every successful company profiled, 100+ tried same strategies and failed"
   :investment-strategy "Backtest survivorship: include delisted/bankrupt companies, not just survivors"
   :career-advice "For every billionaire dropout, 10,000+ dropouts are working minimum wage"}
  :failure-modes
  [(failure "studying-only-winners" "critical"
            "Drawing conclusions from successful entities while ignoring failures"
            :signals ["Reading only success stories and biographies of winners"
                     "Business books profiling successful companies only"
                     "Studying 'best practices' without studying failures"
                     "Assuming successful strategies are good because survivors used them"
                     "Ignoring that failed entities often used same strategies"]
            :safeguards ["Deliberately study failures, not just successes"
                        "For every success story, find 10 failure stories"
                        "Ask: How many tried this and failed?"
                        "Include failed entities in analysis"
                        "Study the cemetery, not just the museum"]
            :case-studies
            [{:event "In Search of Excellence Failure"
              :date "1982-1990"
              :description "Peters & Waterman's 1982 book profiled 43 'excellent' companies. Within 5 years, 1/3 were in financial trouble. They studied survivors, ignored that many failed companies had same practices."
              :quantitative-impact "43 companies profiled as 'excellent', 14 (33%) in financial trouble within 5 years, practices weren't causal—survivorship bias"
              :survivorship-bias "Authors studied successful companies at a point in time, didn't account for regression to mean or that failed companies had similar practices"
              :outcome "Book sold millions, but advice was flawed due to survivorship bias. Demonstrates danger of studying only winners."}])
   (failure "mutual-fund-performance-illusion" "critical"
            "Evaluating fund performance without accounting for funds that closed"
            :signals ["Comparing current fund performance to historical averages"
                     "Not including closed/merged funds in analysis"
                     "Database showing only surviving funds"
                     "Performance statistics that exclude failures"
                     "Fund families closing poor performers to improve average"]
            :safeguards ["Include all funds that existed at start of period, including closed ones"
                        "Adjust performance statistics for survivorship bias (typically -1-2% annually)"
                        "Track fund closures and mergers"
                        "Use survivorship-bias-free databases"
                        "Assume published performance overstates reality by 1-2% annually"]
            :case-studies
            [{:event "Mutual Fund Survivorship Bias Study"
              :date "1990-2020"
              :description "Studies show mutual fund databases overstate returns by 1-2% annually because poorly performing funds are closed and removed from databases."
              :quantitative-impact "Survivorship bias: 1-2% annual return overstatement, 50% of funds close within 10 years, cumulative effect: 10-20% over 10 years"
              :survivorship-bias "Only surviving funds appear in performance databases, creating illusion of better performance than reality"
              :outcome "Investors systematically overestimate active management performance, leading to poor investment decisions"}])
   (failure "strategy-attribution-error" "high"
            "Attributing success to strategies used by winners, ignoring losers used same strategies"
            :signals ["'Successful companies do X' without checking if failed companies also did X"
                     "Assuming correlation implies causation"
                     "Narrative fallacy: creating story of why winners won"
                     "Ignoring base rate: most companies that did X failed"
                     "Selection bias in case studies"]
            :safeguards ["For every successful case, find failed cases that used same strategy"
                        "Check base rate: what % of entities using strategy succeeded?"
                        "Distinguish correlation from causation"
                        "Study matched pairs: winners and losers with same strategy"
                        "Use control groups in analysis"]
            :case-studies
            [{:event "Startup Pivot Mythology"
              :date "Ongoing"
              :description "Many successful startups (Instagram, Twitter, Slack) pivoted from original idea. But 1,000+ failed startups also pivoted. Pivot is not causal to success."
              :quantitative-impact "Successful pivots highlighted: 10-20 famous cases, Failed pivots ignored: 1,000+, Pivot success rate: ~2%"
              :survivorship-bias "We hear about successful pivots (Instagram, Slack) but not the 98% of pivots that failed"
              :outcome "Entrepreneurs overestimate value of pivoting, may pivot when they should persist or vice versa"}])
   (failure "wwii-airplane-armor-error" "critical"
            "The original survivorship bias example: armoring planes where bullet holes were, not where they weren't"
            :signals ["Focusing on visible damage/problems in survivors"
                     "Ignoring that absence of damage in survivors means that's where fatal damage occurs"
                     "Not considering entities that didn't survive"
                     "Assuming survivors show you what to protect"
                     "Missing that survivors survived DESPITE damage in those areas"]
            :safeguards ["Invert: what's NOT damaged in survivors? That's what killed non-survivors"
                        "Study failures, not just successes"
                        "Consider selection process: what determines survival?"
                        "Ask: what would we see if we could examine non-survivors?"
                        "Use Wald's logic: absence of evidence IS evidence"]
            :case-studies
            [{:event "WWII Airplane Armor"
              :date "1943"
              :description "Military wanted to armor planes where bullet holes were. Abraham Wald said armor where there WEREN'T holes—those planes didn't return."
              :quantitative-impact "Wald's insight saved thousands of lives by armoring cockpit and engines (no holes in survivors) instead of wings and fuselage (many holes in survivors)"
              :survivorship-bias "Only seeing planes that survived created inverse conclusion—armor should go where survivors WEREN'T hit"
              :outcome "Classic example of survivorship bias and how inverting the question reveals truth"}])
   (failure "career-advice-from-outliers" "high"
            "Taking career advice from extreme outliers without accounting for survivorship bias"
            :signals ["'Bill Gates dropped out, so dropping out is good'"
                     "'Follow your passion' from people whose passion happened to be lucrative"
                     "Advice from billionaires without considering failed billionaire-wannabes"
                     "Assuming outlier strategies work for normal people"
                     "Ignoring base rate of strategy success"]
            :safeguards ["Check base rate: what % of people following this advice succeeded?"
                        "For every successful outlier, find 1,000 failed outliers"
                        "Distinguish luck from skill"
                        "Consider selection effects and survivorship bias"
                        "Take advice from representative sample, not just winners"]
            :case-studies
            [{:event "College Dropout Mythology"
              :date "Ongoing"
              :description "Gates, Jobs, Zuckerberg dropped out and succeeded. But 10,000+ dropouts failed for every one who succeeded. Dropout success rate: <0.01%."
              :quantitative-impact "Famous dropout billionaires: ~10, Failed dropouts: 100,000+, Dropout success rate: <0.01%, College graduate success rate: 10-20x higher"
              :survivorship-bias "We hear about Gates and Jobs, not the 99.99% of dropouts who failed"
              :outcome "Survivorship bias makes dropping out seem viable when base rate shows it's terrible strategy for 99.99%"}])]
  :cross-model-interactions
  [{:model "base-rate-neglect"
    :interaction "Survivorship bias makes us ignore base rates—we see winners and forget 90% failed"
    :lollapalooza-potential "critical"
    :example "Startup success stories make us ignore 90% failure rate (base rate)"}
   {:model "inversion"
    :interaction "Invert survivorship bias: study the dead, not just the living"
    :lollapalooza-potential "high"
    :example "Wald's airplane armor insight came from inverting: what would dead planes show us?"}
   {:model "margin-of-safety"
    :interaction "Survivorship bias makes risks look smaller than they are—we don't see the dead"
    :lollapalooza-potential "high"
    :example "Leverage looks safe when you only study surviving firms, not the bankrupt ones"}]})

;; ============================================
;; TO BE CONTINUED: Models 4-10
;; ============================================
;; Next models to add:
;; 4. Antifragility (Taleb)
;; 5. Optionality (Taleb)
;; 6. Via Negativa (Taleb/Munger)
;; 7. Barbell Strategy (Taleb)
;; 8. Narrative Fallacy (Taleb/Kahneman)
;; 9. Availability Cascade (Kahneman)
;; 10. Ergodicity (Ole Peters)
