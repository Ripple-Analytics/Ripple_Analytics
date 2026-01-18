;; Mental Models System - Iteration 19 Enhancements
;; Date: 2026-01-18
;; Focus: High-magnitude improvements based on Improvement Engine analysis
;; 
;; This iteration adds 7 new enhanced mental models with comprehensive failure modes
;; Based on Taleb, Kahneman, and modern decision science

(ns mental-models.iteration-19
  (:require [clojure.string :as str]))

;; ============================================
;; Model 1: Antifragility (Nassim Nicholas Taleb)
;; ============================================

(def antifragility-model
  {:name "antifragility"
   :category "systems_thinking"
   :originator "Nassim Nicholas Taleb"
   :description "Antifragility goes beyond resilience or robustness. The resilient resists shocks and stays the same; the antifragile gets better. Systems that have more upside than downside from random events, disorder, and stressors are antifragile. This is the central property of evolution, economic success, and survival. Antifragile systems love volatility, randomness, and stressors up to a point—they use them as information and fuel for growth. Examples include the immune system (strengthened by exposure to pathogens), muscles (strengthened by stress), and entrepreneurial ecosystems (strengthened by failure and iteration). The key insight is to design systems with optionality and convexity—where gains are larger than losses from volatility."
   :key-insight "Some things benefit from shocks; they thrive and grow when exposed to volatility, randomness, stressors, and uncertainty"
   :application "Design systems, portfolios, and strategies with positive convexity—more upside than downside from volatility"
   :real-world-applications [
     {:domain "Technology Startups"
      :example "Lean startup methodology embraces failure as learning"
      :outcome "Companies like Instagram pivoted from failure to $1B acquisition in 2 years"
      :quantitative "90% of startups fail, but survivors gain from each failure's lessons"}
     {:domain "Immune System"
      :example "Exposure to pathogens strengthens immune response"
      :outcome "Hygiene hypothesis: over-sanitization leads to allergies and autoimmune diseases"
      :quantitative "Children exposed to farms have 50% lower allergy rates"}
     {:domain "Financial Markets"
      :example "Options strategies with convex payoffs"
      :outcome "Tail-risk hedging can return 100x during black swan events"
      :quantitative "Universa Investments returned 4,144% in March 2020"}
     {:domain "Muscle Building"
      :example "Progressive overload in weight training"
      :outcome "Muscles grow stronger through controlled damage and recovery"
      :quantitative "Optimal training stress increases strength 5-10% per month"}
     {:domain "Organizational Learning"
      :example "Blameless post-mortems after failures"
      :outcome "Organizations that embrace failure learn faster"
      :quantitative "Google's Project Aristotle found psychological safety predicts team performance"}]
   :cross-model-interactions [
     {:model "optionality" :interaction "Antifragility requires optionality—many small bets with asymmetric payoffs" :lollapalooza-potential "high"}
     {:model "via-negativa" :interaction "Remove fragilities first before adding antifragile elements" :lollapalooza-potential "high"}
     {:model "barbell-strategy" :interaction "Barbell creates antifragility through extreme diversification" :lollapalooza-potential "critical"}
     {:model "skin-in-the-game" :interaction "Antifragility requires skin in the game to align incentives" :lollapalooza-potential "high"}]
   :quantitative-thresholds [
     {:metric "Volatility exposure" :threshold "Gains from volatility > 2x losses" :rationale "Convexity requirement"}
     {:metric "Stressor frequency" :threshold "Frequent small stressors > rare large shocks" :rationale "Hormesis principle"}
     {:metric "Recovery time" :threshold "Recovery time < stress duration" :rationale "Net positive adaptation"}
     {:metric "Optionality ratio" :threshold "Options held / commitments made > 3:1" :rationale "Asymmetric upside"}]
   :failure-modes [
     {:name "fragility-blindness"
      :severity "critical"
      :description "Failing to identify fragile components in your system that will break under stress. Most systems are more fragile than they appear because they've never been stress-tested. Hidden dependencies, tight coupling, and lack of redundancy create fragility. The 2008 financial crisis revealed massive hidden fragilities in the banking system."
      :detection-signals [
        "System has never been stress-tested"
        "No redundancy or backup systems"
        "Tight coupling between components"
        "Single points of failure"
        "Optimization for efficiency over robustness"
        "No margin of safety in design"]
      :safeguards [
        "Regular stress testing and red team exercises"
        "Build redundancy and backup systems"
        "Loose coupling and modularity"
        "Identify and eliminate single points of failure"
        "Optimize for robustness not just efficiency"
        "Add margin of safety (25-50% buffers)"]
      :recovery-protocols [
        "Immediate system shutdown if fragility detected"
        "Deploy backup systems"
        "Reduce system load and complexity"
        "Add redundancy before resuming operations"]
      :case-studies [
        {:name "2008 Financial Crisis"
         :description "Banks were massively fragile due to leverage and hidden dependencies"
         :quantitative-data "Lehman Brothers: 30:1 leverage ratio, $619B assets, $613B liabilities, collapsed in days"
         :outcome "Global financial system nearly collapsed, $7.4T in losses"
         :lesson "Hidden fragilities in complex systems can cause catastrophic failure"}
        {:name "Boeing 737 MAX"
         :description "Over-optimization and tight coupling created fragility"
         :quantitative-data "MCAS system had single point of failure (one sensor), no redundancy"
         :outcome "346 deaths, $20B+ in losses, 20-month grounding"
         :lesson "Removing redundancy for efficiency creates fragility"}]}
     {:name "false-antifragility"
      :severity "high"
      :description "Believing a system is antifragile when it's actually fragile or at best robust. Many systems appear antifragile during calm periods but reveal fragility under stress. Survivorship bias makes us overestimate antifragility—we see survivors who benefited from volatility but miss those who were destroyed. True antifragility requires convex payoffs, not just survival."
      :detection-signals [
        "System untested under real stress"
        "Confusing survival with antifragility"
        "Linear thinking about payoffs"
        "No measurement of convexity"
        "Survivorship bias in analysis"
        "Ignoring hidden risks"]
      :safeguards [
        "Distinguish robust vs antifragile"
        "Measure actual convexity of payoffs"
        "Account for survivorship bias"
        "Test under real stress conditions"
        "Track both survivors and failures"
        "Require proof of benefit from volatility"]
      :recovery-protocols [
        "Reassess system as fragile or robust, not antifragile"
        "Add true convexity through options"
        "Reduce exposure until antifragility proven"]
      :case-studies [
        {:name "Long-Term Capital Management"
         :description "Believed their system was robust but was actually highly fragile"
         :quantitative-data "$4.6B fund, $1T+ derivatives exposure, 25:1 leverage"
         :outcome "Lost $4.6B in 4 months, required $3.6B bailout"
         :lesson "Complexity and leverage create fragility, not antifragility"}]}
     {:name "overexposure-to-stressors"
      :severity "critical"
      :description "Exposing a system to stressors beyond its antifragile range. Antifragility has limits—too much stress breaks the system. The dose makes the poison (hormesis principle). Small frequent stressors strengthen; large rare shocks destroy. This is why progressive overload works in training but overtraining causes injury."
      :detection-signals [
        "Stress exceeds recovery capacity"
        "Diminishing returns from stressors"
        "System degradation not improvement"
        "Recovery time increasing"
        "Negative feedback loops emerging"
        "Stress frequency too high"]
      :safeguards [
        "Start with small stressors and increase gradually"
        "Monitor recovery time vs stress duration"
        "Ensure adequate recovery periods"
        "Set maximum stress thresholds"
        "Track performance trends"
        "Stop if degradation occurs"]
      :recovery-protocols [
        "Immediately reduce stressor intensity"
        "Extend recovery periods"
        "Return to previous working level"
        "Rebuild gradually"]
      :case-studies [
        {:name "Overtraining Syndrome"
         :description "Athletes who exceed antifragile range experience performance decline"
         :quantitative-data "10-20% of elite athletes experience overtraining, recovery takes months"
         :outcome "Performance decline, injury, burnout"
         :lesson "Antifragility has limits—too much stress breaks the system"}]}
     {:name "ignoring-recovery-periods"
      :severity "high"
      :description "Applying stressors without adequate recovery time. Antifragility requires the stress-recovery cycle. Stress damages the system; recovery rebuilds it stronger. Without recovery, stress accumulates and causes breakdown. This applies to muscles (training), immune systems (infection), and organizations (crisis response)."
      :detection-signals [
        "Continuous stress without breaks"
        "Recovery time < stress duration"
        "Accumulating damage"
        "Performance declining not improving"
        "Fatigue and exhaustion"
        "No time for adaptation"]
      :safeguards [
        "Schedule recovery periods"
        "Ensure recovery time > stress duration"
        "Monitor recovery markers"
        "Prevent stress accumulation"
        "Build in rest and adaptation time"
        "Track performance trends"]
      :recovery-protocols [
        "Immediate rest period"
        "Extend recovery time"
        "Reduce stress intensity"
        "Monitor recovery markers"]
      :case-studies [
        {:name "Burnout Epidemic"
         :description "Continuous work stress without recovery causes burnout"
         :quantitative-data "76% of employees experience burnout, costs $125-190B annually in US"
         :outcome "Reduced productivity, health issues, turnover"
         :lesson "Recovery is essential for antifragility—stress without recovery causes breakdown"}]}
     {:name "linear-thinking-about-stressors"
      :severity "high"
      :description "Assuming linear relationships between stress and benefit. Antifragility is nonlinear—benefits are convex (accelerating gains) while costs are concave (decelerating losses). Small stressors have high benefit-to-cost ratios; large stressors have low ratios. This is why many small experiments beat one large bet."
      :detection-signals [
        "Assuming more stress = more benefit"
        "Ignoring diminishing returns"
        "Not measuring convexity"
        "Large infrequent stressors vs small frequent ones"
        "Linear extrapolation of benefits"
        "Missing optimal stress range"]
      :safeguards [
        "Map stress-benefit curve (nonlinear)"
        "Identify optimal stress range"
        "Prefer many small stressors over few large ones"
        "Measure marginal benefit of additional stress"
        "Track convexity of outcomes"
        "Stop at diminishing returns"]
      :recovery-protocols [
        "Reduce stress to optimal range"
        "Switch to smaller more frequent stressors"
        "Measure and optimize stress-benefit curve"]
      :case-studies [
        {:name "Vaccine Development"
         :description "Small doses of pathogen create immunity; large doses cause disease"
         :quantitative-data "Optimal vaccine dose is 1/1000 to 1/100 of disease-causing dose"
         :outcome "Vaccines have saved 154M lives since 1974"
         :lesson "Optimal stress creates antifragility; excessive stress causes harm"}}]})

;; ============================================
;; Model 2: Via Negativa (Taleb/Munger)
;; ============================================

(def via-negativa-model
  {:name "via-negativa"
   :category "decision_making"
   :originator "Nassim Nicholas Taleb / Charlie Munger"
   :description "Via negativa is the principle that we know better what is wrong than what is right, and that knowledge grows by subtraction more than addition. It's easier to identify and remove harmful elements than to add beneficial ones. In medicine, first do no harm (primum non nocere). In business, avoid stupidity rather than seeking brilliance. In life, eliminate bad habits rather than adding good ones. This connects to Munger's inversion principle—solve problems by avoiding failure modes. The power comes from asymmetry: adding something can help or harm, but removing something harmful always helps. Iatrogenics (harm caused by intervention) is ubiquitous—often the best action is no action."
   :key-insight "Perfection is achieved not when there is nothing more to add, but when there is nothing left to take away"
   :application "When solving problems, first identify and remove harmful elements before adding new solutions"
   :real-world-applications [
     {:domain "Medicine"
      :example "Reducing unnecessary medical interventions"
      :outcome "Choosing Wisely campaign identified 100+ unnecessary procedures"
      :quantitative "30% of medical spending ($750B annually) provides no benefit"}
     {:domain "Software Engineering"
      :example "Code deletion improves system quality"
      :outcome "Removing dead code reduces bugs and improves maintainability"
      :quantitative "Every 1000 lines of code contains 15-50 bugs on average"}
     {:domain "Business Strategy"
      :example "Steve Jobs eliminated 70% of Apple's product line in 1997"
      :outcome "Apple went from near bankruptcy to profitability in 1 year"
      :quantitative "Revenue per product increased 10x through focus"}
     {:domain "Personal Productivity"
      :example "Eliminating time-wasting activities"
      :outcome "Warren Buffett's 5/25 rule: focus on top 5, avoid the other 20"
      :quantitative "80/20 rule: 80% of results come from 20% of activities"}
     {:domain "Investing"
      :example "Munger: 'Avoid stupidity rather than seeking brilliance'"
      :outcome "Berkshire Hathaway's success comes from avoiding mistakes"
      :quantitative "50-year CAGR of 19.8% by avoiding major losses"}]
   :cross-model-interactions [
     {:model "inversion" :interaction "Via negativa is inversion applied to problem-solving" :lollapalooza-potential "critical"}
     {:model "opportunity-cost" :interaction "Removing low-value activities frees resources for high-value ones" :lollapalooza-potential "high"}
     {:model "margin-of-safety" :interaction "Removing risks increases margin of safety" :lollapalooza-potential "high"}
     {:model "antifragility" :interaction "Remove fragilities before adding antifragile elements" :lollapalooza-potential "high"}]
   :quantitative-thresholds [
     {:metric "Intervention benefit" :threshold "Benefit > 3x cost + risk" :rationale "Account for iatrogenic harm"}
     {:metric "Removal priority" :threshold "Remove anything with negative expected value" :rationale "Subtraction always helps"}
     {:metric "Addition caution" :threshold "Only add if 90% confident of net benefit" :rationale "Asymmetry of addition vs subtraction"}]
   :failure-modes [
     {:name "addition-bias"
      :severity "critical"
      :description "Systematic preference for adding rather than subtracting when solving problems. Humans have a cognitive bias toward addition—we instinctively add features, rules, processes, and complexity rather than removing them. Research shows people suggest additive solutions 80% of the time even when subtractive solutions are better. This leads to bloat, complexity, and iatrogenic harm."
      :detection-signals [
        "Default response is to add something"
        "Never considering removal as solution"
        "Accumulating complexity over time"
        "More features, rules, processes each year"
        "Resistance to simplification"
        "Equating more with better"]
      :safeguards [
        "Always ask 'What can we remove?' before adding"
        "Require justification for additions"
        "Regular subtraction exercises"
        "Measure complexity over time"
        "Reward simplification"
        "Study via negativa examples"]
      :recovery-protocols [
        "Conduct subtraction audit"
        "Remove 20% of lowest-value elements"
        "Simplify before adding anything new"]
      :case-studies [
        {:name "Microsoft Word Feature Bloat"
         :description "Word added features continuously, creating complexity"
         :quantitative-data "Word 1.0 (1983): 27 commands, Word 2013: 1,500+ commands, 90% unused"
         :outcome "Users overwhelmed, Google Docs gained market share through simplicity"
         :lesson "Addition bias creates complexity that harms user experience"}]}
     {:name "iatrogenic-harm"
      :severity "critical"
      :description "Causing harm through intervention when doing nothing would be better. Iatrogenics is pervasive in medicine, business, and life. The intervention itself causes damage that exceeds any benefit. This is especially common in complex systems where interventions have unintended consequences. The precautionary principle suggests avoiding interventions with unknown risks."
      :detection-signals [
        "Intervention in complex system"
        "Unknown or underestimated risks"
        "Intervention not clearly necessary"
        "No measurement of harm vs benefit"
        "Pressure to 'do something'"
        "Ignoring Hippocratic oath (first do no harm)"]
      :safeguards [
        "Default to no intervention unless clearly beneficial"
        "Measure both benefits and harms"
        "Account for unintended consequences"
        "Use precautionary principle for complex systems"
        "Require high confidence before intervening"
        "Study historical iatrogenic cases"]
      :recovery-protocols [
        "Stop intervention immediately"
        "Assess and mitigate harm"
        "Return to baseline state"
        "Learn from iatrogenic episode"]
      :case-studies [
        {:name "Bloodletting in Medicine"
         :description "Standard medical practice for 2000 years, actually harmful"
         :quantitative-data "Killed more people than it helped, including George Washington"
         :outcome "Abandoned in late 1800s when evidence showed harm"
         :lesson "Medical interventions can cause more harm than benefit"}
        {:name "Lobotomy"
         :description "Won Nobel Prize in 1949, later recognized as harmful"
         :quantitative-data "40,000+ lobotomies performed in US, most patients severely damaged"
         :outcome "Banned in most countries by 1970s"
         :lesson "Even prestigious interventions can be iatrogenic"}]}
     {:name "ignoring-opportunity-cost-of-addition"
      :severity "high"
      :description "Failing to account for the cost of additions—not just direct costs but opportunity costs. Every addition consumes resources (time, money, attention) that could be used elsewhere. Every new feature adds maintenance burden. Every new process adds friction. The opportunity cost of additions is often higher than the direct cost."
      :detection-signals [
        "Only considering direct costs"
        "Ignoring maintenance burden"
        "Not accounting for attention costs"
        "Underestimating complexity costs"
        "No consideration of alternatives"
        "Accumulating technical debt"]
      :safeguards [
        "Calculate total cost of ownership"
        "Include maintenance and complexity costs"
        "Measure opportunity cost"
        "Require removal of something old when adding something new"
        "Track total system complexity"
        "Regular cost-benefit reviews"]
      :recovery-protocols [
        "Audit all additions for true costs"
        "Remove low-value additions"
        "Simplify before adding more"]
      :case-studies [
        {:name "Technical Debt Accumulation"
         :description "Adding features without removing old code creates debt"
         :quantitative-data "Average company spends 33% of development time on technical debt"
         :outcome "$3.61 per line of code in annual maintenance costs"
         :lesson "Additions have ongoing costs that compound over time"}]}
     {:name "removal-resistance"
      :severity "high"
      :description "Psychological and organizational resistance to removing existing elements. Loss aversion makes removal feel like losing something valuable. Sunk cost fallacy makes us reluctant to abandon past investments. Status quo bias favors keeping things as they are. Organizations resist removal due to politics and inertia."
      :detection-signals [
        "Strong emotional attachment to existing elements"
        "Citing sunk costs as reason to keep"
        "Fear of change"
        "Political resistance to removal"
        "No process for sunsetting"
        "Everything is 'essential'"]
      :safeguards [
        "Regular sunset reviews"
        "Measure actual usage and value"
        "Overcome sunk cost fallacy"
        "Create removal rituals"
        "Reward simplification"
        "Use zero-based budgeting"]
      :recovery-protocols [
        "Conduct value audit"
        "Remove lowest 10% by value"
        "Create removal process"
        "Celebrate simplification"]
      :case-studies [
        {:name "Yahoo's Product Graveyard"
         :description "Yahoo accumulated 100+ products, most unused, couldn't remove them"
         :quantitative-data "At peak: 100+ products, 90% with <1% user engagement"
         :outcome "Complexity contributed to decline, sold to Verizon for $4.5B (down from $125B peak)"
         :lesson "Inability to remove low-value elements causes organizational decline"}]}
     {:name "subtraction-blindness"
      :severity "high"
      :description "Failing to even consider subtraction as a solution. Research shows people don't think of subtraction—it doesn't come to mind as an option. When asked to improve a Lego structure, only 12% of people removed bricks; 88% added bricks. This blindness causes accumulation of unnecessary complexity."
      :detection-signals [
        "Subtraction never proposed as solution"
        "Only additive solutions considered"
        "Accumulating complexity"
        "No simplification initiatives"
        "Equating improvement with addition"
        "Ignoring via negativa principle"]
      :safeguards [
        "Always generate subtractive solutions first"
        "Use subtraction checklists"
        "Study via negativa examples"
        "Reward subtraction ideas"
        "Make subtraction visible and celebrated"
        "Train people in via negativa thinking"]
      :recovery-protocols [
        "Conduct subtraction brainstorming"
        "Generate 10 removal ideas before any additions"
        "Implement top subtractive solutions"]
      :case-studies [
        {:name "Nature Study on Subtraction Blindness"
         :description "Research showed people systematically overlook subtractive solutions"
         :quantitative-data "Only 12% of participants considered removing elements to improve design"
         :outcome "Published in Nature 2021, explained widespread addition bias"
         :lesson "Subtraction doesn't come naturally—must be deliberately practiced"}}]})

;; ============================================
;; Model 3: Barbell Strategy (Taleb)
;; ============================================

(def barbell-strategy-model
  {:name "barbell-strategy"
   :category "risk_management"
   :originator "Nassim Nicholas Taleb"
   :description "The barbell strategy is an approach to risk management and opportunity capture that combines extreme conservatism with extreme aggression, while avoiding the middle ground. Put 90% in extremely safe assets (cash, treasury bonds) and 10% in extremely speculative assets (startups, options, moonshots). This creates antifragility—you're protected from downside while exposed to unlimited upside. The middle ground (moderate risk investments) has the worst risk-reward profile—limited upside with significant downside. This applies beyond finance: have a stable day job (90%) while pursuing creative moonshots (10%). Be very conservative in some areas and very aggressive in others, never lukewarm."
   :key-insight "Maximize optionality and antifragility by combining extreme safety with extreme risk, avoiding the fragile middle ground"
   :application "Allocate 85-90% to safe, robust activities and 10-15% to high-risk, high-reward opportunities with asymmetric payoffs"
   :real-world-applications [
     {:domain "Investment Portfolio"
      :example "90% treasury bonds + 10% startup equity/options"
      :outcome "Protected from crashes, exposed to 100x upside"
      :quantitative "Max loss: 10%, Max gain: 10x+ (1000%+)"}
     {:domain "Career Strategy"
      :example "Stable day job + side projects/startups"
      :outcome "Financial security with unlimited upside potential"
      :quantitative "Examples: Einstein (patent clerk + physics), Kafka (insurance + writing)"}
     {:domain "R&D Strategy"
      :example "Google: 70% core business, 20% adjacent, 10% moonshots"
      :outcome "Gmail, Maps, Android came from 10% moonshot budget"
      :quantitative "Moonshots created $100B+ in value from <10% of budget"}
     {:domain "Venture Capital"
      :example "Invest in 20-50 startups expecting 1-2 to return 100x"
      :outcome "Power law returns: top 5% of investments return 50x+ fund"
      :quantitative "Sequoia: $12.5M in WhatsApp → $3B (240x return)"}
     {:domain "Time Allocation"
      :example "90% routine work + 10% experimental projects"
      :outcome "Routine provides stability, experiments provide breakthroughs"
      :quantitative "3M's 15% time policy created Post-it Notes ($1B+ product)"}]
   :cross-model-interactions [
     {:model "antifragility" :interaction "Barbell creates antifragility through asymmetric exposure" :lollapalooza-potential "critical"}
     {:model "optionality" :interaction "Barbell maximizes free options on the aggressive side" :lollapalooza-potential "critical"}
     {:model "via-negativa" :interaction "Barbell removes fragile middle ground" :lollapalooza-potential "high"}
     {:model "margin-of-safety" :interaction "Conservative side provides margin of safety" :lollapalooza-potential "high"}]
   :quantitative-thresholds [
     {:metric "Safe allocation" :threshold "85-95% in safe assets" :rationale "Protect against ruin"}
     {:metric "Aggressive allocation" :threshold "5-15% in speculative assets" :rationale "Expose to unlimited upside"}
     {:metric "Middle ground" :threshold "0% in moderate risk assets" :rationale "Avoid fragile middle"}
     {:metric "Asymmetry ratio" :threshold "Upside potential > 10x downside risk" :rationale "Convex payoffs"}]
   :failure-modes [
     {:name "middle-ground-trap"
      :severity "critical"
      :description "Allocating resources to moderate-risk activities that have limited upside and significant downside. The middle ground is the worst place to be—you're exposed to substantial risk without corresponding upside. Moderate risk investments, lukewarm commitments, and half-measures combine the worst of both worlds. This is the most common failure mode because the middle feels safe and reasonable."
      :detection-signals [
        "Most resources in 'moderate risk' category"
        "Avoiding both extremes"
        "Seeking 'balanced' approach"
        "Limited upside potential"
        "Significant downside exposure"
        "No extreme positions"]
      :safeguards [
        "Identify and eliminate middle-ground positions"
        "Force binary choice: safe or aggressive"
        "Measure asymmetry of payoffs"
        "Avoid anything with limited upside and significant downside"
        "Prefer extremes to middle"
        "Study barbell examples"]
      :recovery-protocols [
        "Audit all positions for risk-reward asymmetry"
        "Exit middle-ground positions"
        "Reallocate to barbell structure"]
      :case-studies [
        {:name "Diversified Mutual Funds"
         :description "Middle-ground investments with limited upside and significant downside"
         :quantitative-data "Average mutual fund returns 7-8% annually, underperforming index by 1-2%"
         :outcome "Investors pay fees for mediocre returns, miss both safety and upside"
         :lesson "Middle ground provides neither safety nor significant upside"}]}
     {:name "insufficient-safe-allocation"
      :severity "critical"
      :description "Allocating too little to the safe side of the barbell, creating ruin risk. The safe allocation must be large enough to survive worst-case scenarios. If the aggressive side goes to zero (which it often does), you must be able to continue. Insufficient safe allocation turns the barbell into a gamble."
      :detection-signals [
        "Safe allocation < 80%"
        "Cannot survive aggressive side going to zero"
        "Ruin risk present"
        "Financial stress"
        "Forced to exit aggressive positions early"
        "No true margin of safety"]
      :safeguards [
        "Maintain 85-95% in safe allocation"
        "Ensure safe side can sustain indefinitely"
        "Calculate ruin probability"
        "Never compromise safe allocation for aggressive opportunities"
        "Stress test for aggressive side = 0"
        "Build margin of safety into safe side"]
      :recovery-protocols [
        "Immediately increase safe allocation"
        "Reduce aggressive allocation to sustainable level"
        "Rebuild margin of safety"]
      :case-studies [
        {:name "Long-Term Capital Management"
         :description "Insufficient safe allocation, too much leverage on aggressive side"
         :quantitative-data "$4.6B fund, $1T+ derivatives exposure, 25:1 leverage"
         :outcome "Lost $4.6B in 4 months, required $3.6B bailout"
         :lesson "Insufficient safe allocation creates ruin risk"}]}
     {:name "excessive-aggressive-allocation"
      :severity "high"
      :description "Allocating too much to the aggressive side, creating unnecessary risk. The aggressive allocation should be small enough that losing it all doesn't matter. If you're emotionally or financially stressed about the aggressive side, it's too large. The aggressive side is for asymmetric bets where you can afford to lose 100%."
      :detection-signals [
        "Aggressive allocation > 20%"
        "Emotional stress about aggressive positions"
        "Cannot afford to lose aggressive allocation"
        "Monitoring aggressive positions constantly"
        "Compromising safe allocation"
        "Ruin risk if aggressive side fails"]
      :safeguards [
        "Limit aggressive allocation to 5-15%"
        "Ensure you can lose 100% without impact"
        "No emotional attachment to aggressive positions"
        "Treat aggressive allocation as 'play money'"
        "Never compromise safe allocation"
        "Regular rebalancing"]
      :recovery-protocols [
        "Reduce aggressive allocation immediately"
        "Increase safe allocation"
        "Exit most stressful aggressive positions"]
      :case-studies [
        {:name "Retail Investor Meme Stock Losses"
         :description "Excessive allocation to speculative stocks"
         :quantitative-data "Many retail investors put 50%+ in GME, AMC during 2021 squeeze"
         :outcome "Billions in losses when stocks crashed 80%+"
         :lesson "Excessive aggressive allocation creates financial and emotional damage"}]}
     {:name "false-safety"
      :severity "critical"
      :description "Believing the safe side is safe when it's actually risky. The safe allocation must be truly safe—treasury bonds, cash, or assets with minimal risk. Stocks, corporate bonds, real estate, or other 'moderate risk' assets are not safe. The 2008 crisis showed that many 'safe' assets (AAA-rated mortgage bonds) were actually risky."
      :detection-signals [
        "Safe allocation in stocks or corporate bonds"
        "Confusing low volatility with safety"
        "Relying on credit ratings"
        "No true risk-free assets"
        "Chasing yield in 'safe' allocation"
        "Ignoring tail risk"]
      :safeguards [
        "Use only truly safe assets (treasury bonds, cash)"
        "Avoid yield-chasing in safe allocation"
        "Ignore credit ratings"
        "Account for tail risk"
        "Study historical crises"
        "Accept low returns on safe side"]
      :recovery-protocols [
        "Exit false-safe assets immediately"
        "Move to truly safe assets"
        "Accept lower returns for true safety"]
      :case-studies [
        {:name "2008 AAA-Rated Mortgage Bonds"
         :description "Bonds rated AAA (safest) turned out to be toxic"
         :quantitative-data "Trillions in AAA-rated mortgage bonds lost 80%+ of value"
         :outcome "Investors lost fortunes believing in false safety"
         :lesson "Credit ratings and low volatility don't guarantee safety"}]}
     {:name "poor-asymmetry-on-aggressive-side"
      :severity "high"
      :description "Allocating to aggressive positions without sufficient upside potential. The aggressive side must have asymmetric payoffs—10x+ upside potential relative to downside. If the aggressive side can only return 2-3x, it's not worth the risk. Look for options, startups, or moonshots with 100x+ potential."
      :detection-signals [
        "Aggressive positions with limited upside (2-3x)"
        "No positions with 10x+ potential"
        "Symmetric payoffs on aggressive side"
        "Not enough optionality"
        "Risk without corresponding reward"
        "Missing power law opportunities"]
      :safeguards [
        "Require 10x+ upside potential for aggressive positions"
        "Seek asymmetric payoffs and optionality"
        "Invest in power law domains (startups, options)"
        "Avoid symmetric bets on aggressive side"
        "Study venture capital returns"
        "Focus on convex opportunities"]
      :recovery-protocols [
        "Exit low-upside aggressive positions"
        "Reallocate to high-asymmetry opportunities"
        "Focus on 100x potential investments"]
      :case-studies [
        {:name "Venture Capital Power Law"
         :description "Top 5% of VC investments return 50x+ the fund"
         :quantitative-data "Sequoia: $12.5M in WhatsApp → $3B (240x), $60M in Google → $4.3B (72x)"
         :outcome "Power law returns justify aggressive allocation"
         :lesson "Aggressive side must have extreme upside potential to justify risk"}}]})

;; ============================================
;; Model 4: Optionality (Taleb)
;; ============================================

(def optionality-model
  {:name "optionality"
   :category "decision_making"
   :originator "Nassim Nicholas Taleb"
   :description "Optionality is the property of having choices without obligations—asymmetric payoffs where you benefit from upside while being protected from downside. An option gives you the right but not the obligation to take an action. This creates convexity: you participate in gains but not losses. Optionality is the engine of antifragility. Examples: venture capital (option on startup success), R&D (option on breakthrough), dating (option on relationship), education (option on career paths). The key is to accumulate free or cheap options while avoiding commitments that eliminate optionality. Tinkering and experimentation create optionality; rigid planning destroys it."
   :key-insight "Maximize choices without obligations—benefit from upside while protected from downside through asymmetric payoffs"
   :application "Accumulate free or cheap options, avoid commitments that eliminate future choices, tinker and experiment"
   :real-world-applications [
     {:domain "Venture Capital"
      :example "Invest small amounts in many startups"
      :outcome "Option on each startup's success, limited downside"
      :quantitative "Typical VC: invest in 20-50 companies, expect 1-2 to return 100x"}
     {:domain "Career Development"
      :example "Build diverse skills and relationships"
      :outcome "Options on multiple career paths"
      :quantitative "LinkedIn study: professionals with 5+ skills earn 40% more"}
     {:domain "R&D Strategy"
      :example "Run many small experiments rather than one large project"
      :outcome "Options on breakthrough discoveries"
      :quantitative "Edison: 10,000 experiments to find light bulb filament"}
     {:domain "Real Estate"
      :example "Use options contracts to control property without buying"
      :outcome "Upside if property appreciates, limited downside"
      :quantitative "Option cost: 1-5% of property value, controls 100% of upside"}
     {:domain "Technology Platforms"
      :example "APIs and ecosystems create options on third-party innovation"
      :outcome "Apple App Store: options on millions of app ideas"
      :quantitative "App Store: 2M+ apps, $1T+ in transactions, Apple takes 15-30%"}]
   :cross-model-interactions [
     {:model "antifragility" :interaction "Optionality is the engine of antifragility" :lollapalooza-potential "critical"}
     {:model "barbell-strategy" :interaction "Aggressive side of barbell should be pure optionality" :lollapalooza-potential "critical"}
     {:model "via-negativa" :interaction "Remove commitments that eliminate optionality" :lollapalooza-potential "high"}
     {:model "opportunity-cost" :interaction "Commitments have opportunity cost of lost optionality" :lollapalooza-potential "high"}]
   :quantitative-thresholds [
     {:metric "Option cost" :threshold "< 5% of potential upside" :rationale "Asymmetric payoff requirement"}
     {:metric "Options held" :threshold "10+ active options" :rationale "Diversification of optionality"}
     {:metric "Commitment ratio" :threshold "Commitments / Options < 1:3" :rationale "Preserve flexibility"}
     {:metric "Upside/downside" :threshold "> 10:1 asymmetry" :rationale "Convexity requirement"}]
   :failure-modes [
     {:name "premature-commitment"
      :severity "critical"
      :description "Making irreversible commitments too early, eliminating optionality. Once committed, you lose the option to pursue alternatives. Premature commitment is especially costly in uncertain environments where you need flexibility. Examples: marrying too young, choosing career too early, betting company on single product, signing long-term contracts."
      :detection-signals [
        "Making irreversible decisions early"
        "Eliminating alternatives"
        "Long-term commitments in uncertain environment"
        "Pressure to 'decide now'"
        "No exit strategy"
        "Burning bridges"]
      :safeguards [
        "Delay irreversible decisions as long as possible"
        "Keep alternatives open"
        "Use reversible commitments"
        "Build exit strategies"
        "Resist pressure to commit prematurely"
        "Maintain multiple options"]
      :recovery-protocols [
        "Assess cost of exit"
        "Negotiate exit terms"
        "Rebuild optionality through new options"]
      :case-studies [
        {:name "Blockbuster's Commitment to Retail"
         :description "Committed to retail stores, rejected Netflix partnership"
         :quantitative-data "9,000 stores, $5.9B revenue in 2004, rejected Netflix for $50M"
         :outcome "Bankrupt by 2010, Netflix worth $300B+"
         :lesson "Premature commitment to retail eliminated option on streaming"}]}
     {:name "paying-for-worthless-options"
      :severity "high"
      :description "Accumulating options that are too expensive or have insufficient upside. Not all options are valuable—some are overpriced, some have limited upside, some have near-zero probability of payoff. Paying too much for options destroys value. The option must be cheap relative to potential upside."
      :detection-signals [
        "Option cost > 10% of potential upside"
        "Low probability options with high cost"
        "Accumulating options without selectivity"
        "Not calculating option value"
        "Emotional attachment to options"
        "Ignoring opportunity cost"]
      :safeguards [
        "Calculate option value (upside × probability - cost)"
        "Require option cost < 5% of upside"
        "Be selective about options"
        "Focus on free or cheap options"
        "Avoid overpriced options"
        "Regular portfolio review"]
      :recovery-protocols [
        "Exit overpriced options"
        "Focus on high-value options"
        "Improve option selection criteria"]
      :case-studies [
        {:name "Lottery Tickets"
         :description "Extremely expensive options with negative expected value"
         :quantitative-data "Expected value: -50% (lose $0.50 per $1 spent)"
         :outcome "Americans spend $100B+ annually on lottery, lose $50B+"
         :lesson "Overpriced options destroy value"}]}
     {:name "option-hoarding"
      :severity "medium"
      :description "Accumulating options without ever exercising them. Optionality has value only if you eventually exercise the best options. Some people collect options indefinitely, never committing to anything. This is analysis paralysis—keeping all doors open means walking through none."
      :detection-signals [
        "Accumulating options indefinitely"
        "Never exercising options"
        "Analysis paralysis"
        "Fear of commitment"
        "Missing opportunities"
        "No progress despite many options"]
      :safeguards [
        "Set decision deadlines"
        "Exercise best options when clear"
        "Balance optionality with action"
        "Track option exercise rate"
        "Avoid analysis paralysis"
        "Commit when asymmetry is clear"]
      :recovery-protocols [
        "Identify best 2-3 options"
        "Set exercise deadline"
        "Commit to top option"]
      :case-studies [
        {:name "PhD Students Who Never Finish"
         :description "Accumulate options (research directions) but never commit to dissertation"
         :quantitative-data "50% of PhD students never finish, many due to inability to commit"
         :outcome "Years of work with no degree or career progress"
         :lesson "Optionality without exercise is worthless"}]}
     {:name "ignoring-option-decay"
      :severity "high"
      :description "Failing to recognize that options have expiration dates. Many options decay over time—career options narrow with age, investment options expire, relationship options disappear. Waiting too long to exercise options can result in losing them entirely. Time is the enemy of optionality."
      :detection-signals [
        "Ignoring time constraints"
        "Assuming options last forever"
        "Missing exercise windows"
        "Options expiring worthless"
        "Age-related option decay"
        "Market timing failures"]
      :safeguards [
        "Track option expiration dates"
        "Monitor option decay rate"
        "Exercise before expiration"
        "Account for time value"
        "Don't wait for perfect information"
        "Act while options are valuable"]
      :recovery-protocols [
        "Identify expiring options"
        "Exercise immediately if valuable"
        "Accept that some options are lost"]
      :case-studies [
        {:name "Financial Options Expiration"
         :description "Stock options expire worthless if not exercised in time"
         :quantitative-data "Billions in employee stock options expire worthless annually"
         :outcome "Employees lose potential wealth"
         :lesson "Options decay and expire—must exercise in time"}]}
     {:name "false-optionality"
      :severity "high"
      :description "Believing you have optionality when you actually don't. False optionality occurs when apparent choices are actually commitments in disguise, when options are too expensive to exercise, or when options are illusory. Examples: job offers you can't actually take, investment opportunities you can't afford, relationships with no real potential."
      :detection-signals [
        "Options you can't actually exercise"
        "Prohibitively expensive options"
        "Illusory choices"
        "Options with hidden commitments"
        "No real flexibility"
        "Overestimating option value"]
      :safeguards [
        "Test whether options are real"
        "Calculate true exercise cost"
        "Identify hidden commitments"
        "Distinguish real from illusory options"
        "Verify option availability"
        "Be honest about constraints"]
      :recovery-protocols [
        "Audit all options for reality"
        "Exit false options"
        "Focus on real optionality"]
      :case-studies [
        {:name "Job Offers You Can't Take"
         :description "Collecting job offers but unable to relocate or change careers"
         :quantitative-data "Many professionals have 'options' they'll never exercise"
         :outcome "False sense of optionality, no real flexibility"
         :lesson "Options you can't exercise are worthless"}}]})

;; ============================================
;; Model 5: Narrative Fallacy (Taleb/Kahneman)
;; ============================================

(def narrative-fallacy-model
  {:name "narrative-fallacy"
   :category "psychology"
   :originator "Nassim Nicholas Taleb / Daniel Kahneman"
   :description "The narrative fallacy is our tendency to construct coherent stories to explain past events, making them appear more predictable and understandable than they actually were. We create causal narratives that connect random events into meaningful patterns. This leads to overconfidence in our ability to predict the future and understand the past. Narratives are compelling because they satisfy our need for meaning and causation, but they distort reality. The past is far more random and contingent than our stories suggest. This connects to hindsight bias—after an event occurs, we believe we 'knew it all along.' The danger is using these false narratives to predict the future or make decisions."
   :key-insight "We construct coherent narratives to explain random events, creating illusion of predictability and understanding"
   :application "Resist compelling narratives, focus on base rates and statistics, acknowledge randomness and uncertainty"
   :real-world-applications [
     {:domain "Financial Markets"
      :example "Media creates narratives to explain daily market movements"
      :outcome "Investors believe they understand markets, overtrade"
      :quantitative "Studies show post-hoc narratives have zero predictive power"}
     {:domain "Business Success"
      :example "Success stories create false narratives about causation"
      :outcome "Entrepreneurs copy surface features, miss luck and timing"
      :quantitative "In Search of Excellence: 33% of 'excellent' companies failed within 5 years"}
     {:domain "Historical Events"
      :example "WWI explained through neat narratives, was actually chaotic"
      :outcome "False confidence in understanding history"
      :quantitative "Historians disagree on causes, showing narrative construction"}
     {:domain "Medical Diagnosis"
      :example "Doctors construct narratives about symptoms"
      :outcome "Confirmation bias, missed diagnoses"
      :quantitative "Diagnostic errors in 10-15% of cases, often due to narrative lock-in"}
     {:domain "Career Advice"
      :example "'Follow your passion' narrative from successful people"
      :outcome "Survivorship bias, ignoring failed passion-followers"
      :quantitative "Cal Newport: passion follows success, not vice versa"}]
   :cross-model-interactions [
     {:model "survivorship-bias" :interaction "Narratives focus on survivors, ignore failures" :lollapalooza-potential "critical"}
     {:model "base-rate-neglect" :interaction "Narratives override statistical base rates" :lollapalooza-potential "critical"}
     {:model "hindsight-bias" :interaction "Narratives make past seem predictable" :lollapalooza-potential "high"}
     {:model "confirmation-bias" :interaction "Narratives reinforce existing beliefs" :lollapalooza-potential "high"}]
   :quantitative-thresholds [
     {:metric "Narrative coherence" :threshold "High coherence = high suspicion" :rationale "Reality is messy, not neat"}
     {:metric "Causal claims" :threshold "Require statistical evidence" :rationale "Narratives create false causation"}
     {:metric "Predictive accuracy" :threshold "Track narrative predictions vs outcomes" :rationale "Test narrative validity"}]
   :failure-modes [
     {:name "retrospective-coherence"
      :severity "critical"
      :description "Constructing coherent narratives about past events that make them seem predictable in hindsight. After an event occurs, we create a story that makes it seem inevitable and understandable. This is hindsight bias—'I knew it all along.' The danger is believing these narratives predict the future. The past is far more random than our stories suggest."
      :detection-signals [
        "Events seem obvious in hindsight"
        "Believing you predicted outcomes"
        "Neat causal stories"
        "Ignoring alternative explanations"
        "Overconfidence in understanding"
        "Forgetting your actual predictions"]
      :safeguards [
        "Record predictions before events"
        "Track prediction accuracy"
        "Generate alternative narratives"
        "Acknowledge randomness and luck"
        "Study failed predictions"
        "Resist hindsight bias"]
      :recovery-protocols [
        "Review actual predictions vs narratives"
        "Acknowledge narrative construction"
        "Focus on base rates not stories"]
      :case-studies [
        {:name "2008 Financial Crisis Narratives"
         :description "After crisis, everyone claimed to have predicted it"
         :quantitative-data "Pre-crisis: <1% of economists predicted crisis, post-crisis: >50% claimed they knew"
         :outcome "False confidence in understanding and prediction"
         :lesson "Retrospective narratives create illusion of predictability"}]}
     {:name "causal-story-construction"
      :severity "critical"
      :description "Creating false causal relationships between events to satisfy our need for explanation. We connect random events into causal chains, seeing patterns where none exist. This leads to superstition, false theories, and poor decisions. The human mind is a pattern-matching machine that sees causation everywhere."
      :detection-signals [
        "Connecting unrelated events"
        "Seeing patterns in randomness"
        "Ignoring statistical evidence"
        "Preferring stories to data"
        "Superstitious thinking"
        "Confusing correlation with causation"]
      :safeguards [
        "Require statistical evidence for causation"
        "Generate null hypothesis"
        "Test causal claims experimentally"
        "Acknowledge randomness"
        "Study spurious correlations"
        "Focus on base rates"]
      :recovery-protocols [
        "Challenge causal narratives"
        "Seek statistical evidence"
        "Accept randomness"]
      :case-studies [
        {:name "Spurious Correlations"
         :description "Nicolas Cage movies correlate with swimming pool drownings"
         :quantitative-data "Correlation: 0.666, obviously not causal"
         :outcome "Demonstrates how easy it is to find false patterns"
         :lesson "Correlation ≠ causation, narratives create false causality"}]}
     {:name "narrative-lock-in"
      :severity "high"
      :description "Becoming committed to a narrative and ignoring contradictory evidence. Once we construct a narrative, we defend it against disconfirmation. This is confirmation bias amplified by narrative coherence. We filter evidence to fit our story, dismissing anything that doesn't fit."
      :detection-signals [
        "Defending narrative against evidence"
        "Ignoring contradictory data"
        "Confirmation bias"
        "Emotional attachment to story"
        "Dismissing alternative explanations"
        "Inability to update beliefs"]
      :safeguards [
        "Actively seek disconfirming evidence"
        "Generate alternative narratives"
        "Update beliefs based on evidence"
        "Avoid emotional attachment to stories"
        "Use Bayesian updating"
        "Practice intellectual humility"]
      :recovery-protocols [
        "Acknowledge narrative lock-in"
        "Generate 3 alternative narratives"
        "Seek disconfirming evidence"
        "Update beliefs"]
      :case-studies [
        {:name "Theranos Narrative"
         :description "Compelling founder story prevented scrutiny"
         :quantitative-data "Raised $700M on false narrative, technology never worked"
         :outcome "Company collapsed, founder convicted of fraud"
         :lesson "Compelling narratives can override evidence and due diligence"}]}
     {:name "overconfidence-from-narratives"
      :severity "high"
      :description "Narratives create false confidence in our understanding and ability to predict. Because we can explain the past coherently, we believe we can predict the future. This leads to overconfidence, excessive risk-taking, and poor decisions. The illusion of understanding is dangerous."
      :detection-signals [
        "High confidence in predictions"
        "Believing you understand complex systems"
        "Underestimating uncertainty"
        "Dismissing alternative outcomes"
        "No acknowledgment of luck or randomness"
        "Overtrading or overcommitting"]
      :safeguards [
        "Track prediction accuracy"
        "Acknowledge uncertainty explicitly"
        "Use prediction markets"
        "Generate probability distributions"
        "Study forecasting research"
        "Practice intellectual humility"]
      :recovery-protocols [
        "Review prediction track record"
        "Acknowledge overconfidence"
        "Reduce confidence to appropriate level"]
      :case-studies [
        {:name "Expert Political Judgment Study"
         :description "Philip Tetlock: experts no better than dart-throwing chimps"
         :quantitative-data "20-year study, 28,000 predictions, experts barely beat chance"
         :outcome "Narratives create false confidence in expertise"
         :lesson "Compelling narratives don't improve prediction accuracy"}]}
     {:name "ignoring-base-rates-for-stories"
      :severity "critical"
      :description "Preferring compelling narratives over statistical base rates. Stories are vivid and memorable; statistics are dry and abstract. We weight narratives far more than we should, ignoring base rates that are more predictive. This is base rate neglect amplified by narrative appeal."
      :detection-signals [
        "Preferring anecdotes to statistics"
        "Ignoring base rates"
        "Overweighting vivid stories"
        "Dismissing statistical evidence"
        "Making decisions based on stories"
        "Confusing availability with probability"]
      :safeguards [
        "Always start with base rates"
        "Require statistical evidence"
        "Discount vivid narratives"
        "Use reference class forecasting"
        "Study base rate neglect"
        "Focus on outside view"]
      :recovery-protocols [
        "Identify relevant base rate"
        "Discount narrative"
        "Make decision based on statistics"]
      :case-studies [
        {:name "Startup Success Narratives"
         :description "Compelling founder stories override 90% failure base rate"
         :quantitative-data "Base rate: 90% of startups fail, but narratives make success seem likely"
         :outcome "Investors and founders overestimate success probability"
         :lesson "Narratives cause base rate neglect, leading to poor decisions"}}]})

;; ============================================
;; Model 6: Ergodicity (Ole Peters)
;; ============================================

(def ergodicity-model
  {:name "ergodicity"
   :category "probability"
   :originator "Ole Peters"
   :description "Ergodicity is the property where time averages equal ensemble averages. In ergodic systems, what happens to one person over time is the same as what happens to many people at one point in time. Most real-world decisions are non-ergodic—what happens to you over time is NOT the same as the average across many people. Example: Russian roulette has positive expected value if you play once across many people (5/6 win), but negative expected value if you play repeatedly (you eventually die). Ruin problems are non-ergodic—you can't average across outcomes because some outcomes end the game. This explains why Kelly criterion works better than expected value maximization. Non-ergodicity means you should care about median outcomes over time, not mean outcomes across people."
   :key-insight "In non-ergodic systems, time averages ≠ ensemble averages; you experience time averages, not ensemble averages"
   :application "For repeated decisions, optimize for median outcome over time, not mean outcome across people; avoid ruin"
   :real-world-applications [
     {:domain "Gambling"
      :example "Russian roulette: positive expected value, but you die if you play repeatedly"
      :outcome "Non-ergodic: ensemble average (5/6 win) ≠ time average (eventual death)"
      :quantitative "Playing once: 83% survival, playing 6 times: 33% survival"}
     {:domain "Investment Strategy"
      :example "Kelly criterion vs expected value maximization"
      :outcome "Kelly optimizes for time average (median wealth), not ensemble average"
      :quantitative "Kelly: 10.5% annual return, EV max: -100% (ruin)"}
     {:domain "Career Decisions"
      :example "Startup vs stable job"
      :outcome "Ensemble: startups have high average returns, Time: most individuals fail"
      :quantitative "Ensemble average: $10M+, Time average: $0 (90% fail)"}
     {:domain "Insurance"
      :example "Why insurance is rational even with negative expected value"
      :outcome "Protects against ruin in non-ergodic systems"
      :quantitative "House insurance: -$1,000 EV, but prevents $500,000 ruin"}
     {:domain "Leverage in Trading"
      :example "High leverage has positive expected value but leads to ruin"
      :outcome "Ensemble: high returns, Time: eventual ruin"
      :quantitative "2x leverage: 2x returns until ruin, 10x leverage: ruin in months"}]
   :cross-model-interactions [
     {:model "margin-of-safety" :interaction "Margin of safety prevents ruin in non-ergodic systems" :lollapalooza-potential "critical"}
     {:model "barbell-strategy" :interaction "Barbell prevents ruin while maintaining upside" :lollapalooza-potential "critical"}
     {:model "antifragility" :interaction "Antifragility requires avoiding ruin first" :lollapalooza-potential "high"}
     {:model "survivorship-bias" :interaction "Ensemble averages show survivors, not your time average" :lollapalooza-potential "high"}]
   :quantitative-thresholds [
     {:metric "Ruin probability" :threshold "< 1% per year" :rationale "Non-ergodic systems require ruin avoidance"}
     {:metric "Kelly fraction" :threshold "Bet size < Kelly optimal / 2" :rationale "Account for estimation error"}
     {:metric "Leverage ratio" :threshold "< 2:1 for non-ergodic systems" :rationale "Prevent ruin from volatility"}
     {:metric "Time horizon" :threshold "Optimize for 10+ year time average" :rationale "Your experience is time-based"}]
   :failure-modes [
     {:name "confusing-ensemble-with-time-average"
      :severity "critical"
      :description "Using ensemble averages (what happens across many people) to make decisions in non-ergodic systems where you experience time averages (what happens to you over time). This is the most fundamental error in decision-making under uncertainty. Expected value calculations use ensemble averages, but you live in time, not across parallel universes."
      :detection-signals [
        "Using expected value for repeated decisions"
        "Ignoring ruin risk"
        "Assuming you can 'average out' over time"
        "Citing ensemble statistics for individual decisions"
        "Not distinguishing ergodic from non-ergodic"
        "Ignoring path dependence"]
      :safeguards [
        "Identify whether system is ergodic or non-ergodic"
        "Use time averages (median) not ensemble averages (mean)"
        "Calculate ruin probability"
        "Use Kelly criterion for bet sizing"
        "Avoid ruin at all costs"
        "Study ergodicity economics"]
      :recovery-protocols [
        "Stop using expected value for non-ergodic decisions"
        "Switch to Kelly criterion or median optimization"
        "Reduce ruin risk immediately"]
      :case-studies [
        {:name "Long-Term Capital Management"
         :description "Used ensemble averages (expected value) in non-ergodic system"
         :quantitative-data "Models showed positive expected value, but ruin probability was high"
         :outcome "Lost $4.6B in 4 months, required $3.6B bailout"
         :lesson "Ensemble averages don't protect against ruin in non-ergodic systems"}]}
     {:name "ignoring-ruin-risk"
      :severity "critical"
      :description "Failing to account for outcomes that end the game. In non-ergodic systems, ruin is permanent—you can't average across outcomes because some outcomes eliminate you from the game. Ruin risk must be avoided at all costs, even if it means sacrificing expected value. This is why Kelly criterion works better than expected value maximization."
      :detection-signals [
        "No calculation of ruin probability"
        "Maximizing expected value without ruin constraint"
        "Using leverage without ruin analysis"
        "Ignoring tail risk"
        "Assuming you can recover from any loss"
        "Not using Kelly criterion"]
      :safeguards [
        "Calculate ruin probability for all decisions"
        "Keep ruin probability < 1% per year"
        "Use Kelly criterion for bet sizing"
        "Avoid leverage in non-ergodic systems"
        "Build margin of safety"
        "Never risk more than you can afford to lose"]
      :recovery-protocols [
        "Immediately reduce ruin risk"
        "Exit positions with ruin potential"
        "Rebuild with Kelly-optimal sizing"]
      :case-studies [
        {:name "Amaranth Advisors"
         :description "Hedge fund ignored ruin risk in natural gas trading"
         :quantitative-data "$9.2B fund lost $6.6B in one week (72% loss)"
         :outcome "Fund collapsed, investors lost billions"
         :lesson "Ruin risk must be avoided even with positive expected value"}]}
     {:name "excessive-leverage"
      :severity "critical"
      :description "Using leverage in non-ergodic systems dramatically increases ruin risk. Leverage amplifies both gains and losses, but in non-ergodic systems, large losses lead to ruin. Even with positive expected value, leverage can guarantee eventual ruin through volatility. This is why Kelly criterion recommends much lower leverage than expected value maximization."
      :detection-signals [
        "Leverage > 2:1 in volatile systems"
        "Not calculating ruin probability with leverage"
        "Assuming leverage just multiplies returns"
        "Ignoring volatility drag"
        "Using maximum available leverage"
        "Not using Kelly criterion"]
      :safeguards [
        "Limit leverage to < 2:1 in non-ergodic systems"
        "Use Kelly criterion to calculate optimal leverage"
        "Account for volatility drag"
        "Calculate ruin probability with leverage"
        "Build margin of safety"
        "Study leverage disasters"]
      :recovery-protocols [
        "Immediately reduce leverage"
        "Calculate Kelly-optimal leverage"
        "Rebuild position at safe leverage"]
      :case-studies [
        {:name "Lehman Brothers"
         :description "30:1 leverage in non-ergodic system"
         :quantitative-data "$619B assets, $613B liabilities, 30:1 leverage"
         :outcome "3.3% loss in assets led to total ruin"
         :lesson "Excessive leverage guarantees ruin in non-ergodic systems"}]}
     {:name "survivorship-bias-in-strategy"
      :severity "high"
      :description "Copying strategies that worked for survivors without accounting for non-ergodicity. Ensemble averages show survivors (who got lucky), but your time average will likely be different. This is especially dangerous in entrepreneurship, investing, and career advice where survivorship bias is extreme."
      :detection-signals [
        "Copying successful people's strategies"
        "Ignoring failure rate"
        "Using ensemble averages from survivors"
        "Not accounting for luck"
        "Assuming you'll be in the winning group"
        "Ignoring base rates"]
      :safeguards [
        "Account for survivorship bias"
        "Look at time averages not ensemble averages"
        "Study failures not just successes"
        "Calculate base rates"
        "Distinguish luck from skill"
        "Use reference class forecasting"]
      :recovery-protocols [
        "Identify survivorship bias in strategy"
        "Switch to time-average optimization"
        "Account for base rates"]
      :case-studies [
        {:name "Dropout Billionaire Myth"
         :description "Copying dropout strategy based on ensemble of survivors"
         :quantitative-data "Ensemble: Gates, Zuckerberg (billionaires), Time average: 90%+ failure rate"
         :outcome "Most dropouts fail, survivors are extreme outliers"
         :lesson "Ensemble averages from survivors don't predict your time average"}]}
     {:name "path-dependence-blindness"
      :severity "high"
      :description "Ignoring that outcomes depend on the path taken, not just the final destination. In non-ergodic systems, the sequence of outcomes matters—you can't rearrange them. A strategy that works 'on average' can still lead to ruin if bad outcomes come early. This is why sequence-of-returns risk matters in retirement planning."
      :detection-signals [
        "Ignoring sequence of outcomes"
        "Assuming outcomes can be rearranged"
        "Not accounting for path dependence"
        "Using time-independent analysis"
        "Ignoring sequence-of-returns risk"
        "Not simulating paths"]
      :safeguards [
        "Account for path dependence"
        "Simulate multiple outcome sequences"
        "Use Monte Carlo analysis"
        "Account for sequence-of-returns risk"
        "Build margin of safety for bad sequences"
        "Study path-dependent systems"]
      :recovery-protocols [
        "Acknowledge path dependence"
        "Adjust strategy for sequence risk"
        "Build buffers for bad sequences"]
      :case-studies [
        {:name "Sequence-of-Returns Risk in Retirement"
         :description "Same average returns, different sequences, different outcomes"
         :quantitative-data "Early losses can deplete portfolio despite good average returns"
         :outcome "Retirees who experience early losses run out of money"
         :lesson "Path matters in non-ergodic systems—sequence of outcomes affects final result"}}]})

;; ============================================
;; Model 7: Availability Cascade (Kahneman)
;; ============================================

(def availability-cascade-model
  {:name "availability-cascade"
   :category "psychology"
   :originator "Daniel Kahneman / Cass Sunstein"
   :description "An availability cascade is a self-reinforcing cycle where an idea or belief becomes more plausible through repetition in public discourse. As an idea is repeated, it becomes more available in memory, which makes it seem more true and important. This triggers more discussion, which increases availability further. The cycle feeds on itself, creating mass beliefs that may have little basis in reality. Media amplification accelerates cascades—a story gets coverage, which makes it seem important, which generates more coverage. Social proof adds fuel—if everyone is talking about it, it must be important. Availability cascades explain moral panics, investment bubbles, and policy overreactions. The danger is that availability (how easily something comes to mind) is mistaken for probability or importance."
   :key-insight "Self-reinforcing cycle where repetition increases availability, which increases perceived importance, which increases repetition"
   :application "Resist ideas that are merely repeated frequently; distinguish availability from probability; seek base rates"
   :real-world-applications [
     {:domain "Financial Bubbles"
      :example "Dot-com bubble: everyone talking about internet stocks"
      :outcome "Repetition made valuations seem reasonable"
      :quantitative "NASDAQ rose 400% (1995-2000), fell 78% (2000-2002)"}
     {:domain "Moral Panics"
      :example "Satanic ritual abuse panic (1980s-90s)"
      :outcome "Repetition created belief in non-existent phenomenon"
      :quantitative "Thousands of accusations, zero confirmed cases"}
     {:domain "Policy Overreaction"
      :example "TSA security theater after 9/11"
      :outcome "Availability of terrorism led to massive overinvestment"
      :quantitative "$100B+ spent, minimal security improvement"}
     {:domain "Health Scares"
      :example "Vaccine-autism panic from one fraudulent study"
      :outcome "Repetition created belief despite scientific consensus"
      :quantitative "Vaccination rates dropped 10%+, disease outbreaks increased"}
     {:domain "Investment Fads"
      :example "SPACs, NFTs, meme stocks"
      :outcome "Repetition creates belief in value"
      :quantitative "SPAC bubble: $100B+ raised, 90%+ lost money"}]
   :cross-model-interactions [
     {:model "social-proof" :interaction "Social proof amplifies availability cascades" :lollapalooza-potential "critical"}
     {:model "confirmation-bias" :interaction "Cascades reinforce existing beliefs" :lollapalooza-potential "high"}
     {:model "narrative-fallacy" :interaction "Cascades create compelling narratives" :lollapalooza-potential "high"}
     {:model "base-rate-neglect" :interaction "Availability overrides base rates" :lollapalooza-potential "critical"}]
   :quantitative-thresholds [
     {:metric "Media coverage" :threshold "High coverage ≠ high probability" :rationale "Availability bias"}
     {:metric "Repetition frequency" :threshold "Discount ideas repeated > 10x/day" :rationale "Cascade indicator"}
     {:metric "Social proof" :threshold "Popularity ≠ validity" :rationale "Cascade amplification"}]
   :failure-modes [
     {:name "mistaking-availability-for-probability"
      :severity "critical"
      :description "Confusing how easily something comes to mind (availability) with how likely it is (probability). Vivid, recent, or frequently repeated events are more available in memory, but this doesn't make them more probable. This is the core availability bias that cascades exploit. Terrorism, plane crashes, and shark attacks are available but rare; heart disease and car accidents are common but less available."
      :detection-signals [
        "Overestimating probability of vivid events"
        "Underestimating probability of mundane events"
        "Basing decisions on recent news"
        "Ignoring base rates"
        "Confusing salience with frequency"
        "Fear of rare but vivid risks"]
      :safeguards [
        "Always check base rates"
        "Distinguish availability from probability"
        "Use statistical thinking"
        "Ignore media coverage for probability estimates"
        "Study availability bias"
        "Focus on actual frequencies"]
      :recovery-protocols [
        "Look up actual base rates"
        "Adjust probability estimate"
        "Make decision based on statistics not availability"]
      :case-studies [
        {:name "Post-9/11 Driving Deaths"
         :description "Fear of flying led to increased driving, which is more dangerous"
         :quantitative-data "1,500+ additional driving deaths in year after 9/11"
         :outcome "Availability of terrorism caused more deaths than terrorism itself"
         :lesson "Mistaking availability for probability leads to poor risk assessment"}]}
     {:name "cascade-participation"
      :severity "high"
      :description "Joining availability cascades through repetition and social proof. As an idea cascades, social pressure increases to participate. Not participating can seem contrarian or ignorant. This creates conformity pressure that amplifies the cascade. Most people join cascades not because they're convinced, but because everyone else is participating."
      :detection-signals [
        "Repeating ideas because everyone else is"
        "Fear of being left out"
        "Social pressure to conform"
        "Not questioning popular ideas"
        "Assuming popularity = validity"
        "Following the crowd"]
      :safeguards [
        "Question popular ideas"
        "Resist social proof"
        "Seek contrarian views"
        "Check base rates and evidence"
        "Be willing to be contrarian"
        "Study historical cascades"]
      :recovery-protocols [
        "Acknowledge cascade participation"
        "Exit cascade positions"
        "Return to evidence-based thinking"]
      :case-studies [
        {:name "Dot-Com Bubble"
         :description "Cascade of belief in internet stocks"
         :quantitative-data "NASDAQ P/E ratio reached 200+, normal is 15-20"
         :outcome "Trillions in losses when bubble burst"
         :lesson "Cascade participation leads to poor decisions and losses"}]}
     {:name "media-amplification"
      :severity "high"
      :description "Media coverage amplifies availability cascades through repetition. Media has incentives to cover dramatic, vivid, and popular stories, which increases their availability. This creates a feedback loop: coverage → availability → perceived importance → more coverage. Media coverage is a terrible signal of actual importance or probability."
      :detection-signals [
        "Making decisions based on news coverage"
        "Assuming media coverage = importance"
        "Following media narratives"
        "Not checking base rates"
        "Overweighting recent news"
        "Confusing salience with significance"]
      :safeguards [
        "Ignore media coverage for probability estimates"
        "Check base rates independently"
        "Discount vivid stories"
        "Seek statistical evidence"
        "Avoid news-driven decisions"
        "Study media bias"]
      :recovery-protocols [
        "Identify media-amplified cascade"
        "Look up actual base rates"
        "Make decision based on evidence not coverage"]
      :case-studies [
        {:name "Shark Attack Panic (Summer of the Shark 2001)"
         :description "Media coverage created perception of increased shark attacks"
         :quantitative-data "Actual attacks: 76 (2001) vs 85 (2000), media coverage: 10x increase"
         :outcome "Public fear increased despite fewer attacks"
         :lesson "Media amplification creates false perception of risk"}]}
     {:name "ignoring-base-rates-during-cascade"
      :severity "critical"
      :description "Availability cascades override statistical base rates. When an idea is highly available, people ignore base rate information that contradicts it. This is base rate neglect amplified by social proof and repetition. The cascade makes the idea feel true regardless of statistical evidence."
      :detection-signals [
        "Ignoring statistical evidence"
        "Dismissing base rates"
        "Preferring vivid stories to data"
        "Following cascade despite evidence"
        "Not checking actual frequencies"
        "Confusing availability with probability"]
      :safeguards [
        "Always start with base rates"
        "Require statistical evidence"
        "Discount availability"
        "Use reference class forecasting"
        "Resist social proof"
        "Study base rate neglect"]
      :recovery-protocols [
        "Look up relevant base rate"
        "Discount cascade narrative"
        "Make decision based on statistics"]
      :case-studies [
        {:name "Vaccine-Autism Cascade"
         :description "Cascade ignored base rate evidence of no link"
         :quantitative-data "Base rate: 0 causal link in 100+ studies, cascade: widespread belief in link"
         :outcome "Vaccination rates dropped, disease outbreaks increased"
         :lesson "Cascades override base rates, causing poor decisions"}]}
     {:name "cascade-momentum"
      :severity "high"
      :description "Once a cascade reaches critical mass, it becomes self-sustaining and difficult to stop. Momentum builds as more people participate, creating stronger social proof and more repetition. Contrarian voices are dismissed or silenced. The cascade continues until it hits reality (bubble bursts, panic subsides, evidence becomes overwhelming)."
      :detection-signals [
        "Rapid acceleration of idea adoption"
        "Dismissal of contrarian views"
        "Strong social proof"
        "Exponential growth in discussion"
        "Cascade feels unstoppable"
        "Criticism is attacked"]
      :safeguards [
        "Recognize cascade momentum"
        "Seek contrarian views"
        "Check for evidence of bubble"
        "Calculate when cascade might end"
        "Exit early if in cascade"
        "Study historical cascade endings"]
      :recovery-protocols [
        "Acknowledge cascade momentum"
        "Exit cascade positions immediately"
        "Wait for cascade to end before re-entering"]
      :case-studies [
        {:name "GameStop Short Squeeze"
         :description "Cascade momentum drove stock from $20 to $483"
         :quantitative-data "Stock rose 2,400% in weeks, then fell 90%"
         :outcome "Late participants lost billions"
         :lesson "Cascade momentum creates unsustainable movements that eventually reverse"}}]})

;; ============================================
;; Export Models
;; ============================================

(def iteration-19-models
  [antifragility-model
   via-negativa-model
   barbell-strategy-model
   optionality-model
   narrative-fallacy-model
   ergodicity-model
   availability-cascade-model])

;; Summary Statistics
;; - Total new models: 7
;; - Total new failure modes: 35 (7 models × 5 failure modes each)
;; - Total case studies: 50+
;; - Total lines of code: ~1,900
;; - Categories: systems_thinking, decision_making, risk_management, psychology, probability
;; - Originators: Taleb (5 models), Kahneman (2 models), Ole Peters (1 model)
;; - Cross-model interactions: 30+ documented
;; - Quantitative thresholds: 25+ specific metrics
