;; ============================================
;; Iteration 16: Charlie Munger Deep Principles
;; ============================================
;; Date: January 18, 2026
;; Focus: Adding mental models extracted from Munger's actual writings and case studies
;; Source: "Munger__All known writings.pdf" - Blue Chip Stamps case study (1978-1982)
;;
;; These models are derived from Munger's actual business decisions and writings,
;; not just his speeches. They represent "Planck knowledge" (deep understanding)
;; rather than "Chauffeur knowledge" (superficial repetition).
;;
;; Mental Models Added: 5
;; Total Models After: 174 (169 + 5)
;; ============================================

(register-model
 {:name "float-management"
  :category "finance"
  :originator "Charlie Munger / Warren Buffett"
  :description "Use of other people's money at zero or negative cost to generate investment returns; understanding the economic value of deferred liabilities"
  :key-insight "Float is not just free money - it's better than free if you can invest it well while the liability is deferred; the key is ensuring the cost of float remains low"
  :application "Identify businesses with structural float (insurance, trading stamps, subscriptions); calculate cost of float; invest float conservatively with margin of safety; never let float cost exceed investment returns"
  :failure-modes
  [(failure "float-mispricing" "critical"
            "Underestimating the true cost of float or future redemption liability"
            :signals ["Aggressive liability estimates" "Declining reserves" "Actuarial disagreements" "Unexpected redemptions"]
            :safeguards ["Conservative liability estimation" "Regular actuarial review" "Stress testing" "Historical redemption analysis"])
   (failure "float-speculation" "critical"
            "Investing float in risky assets that could disappear when liabilities come due"
            :signals ["High-risk investments" "Illiquid positions" "Leverage on float" "Maturity mismatch"]
            :safeguards ["Conservative investment policy" "Liquidity requirements" "Match duration" "No leverage on float"])
   (failure "shrinking-float" "high"
            "Float declining faster than anticipated, forcing asset liquidation"
            :signals ["Accelerating redemptions" "Business decline" "Competitive pressure" "Forced selling"]
            :safeguards ["Diversified float sources" "Liquid investments" "Growth in float" "Business quality"])
   (failure "float-dependency" "high"
            "Business model depends on float growth that cannot be sustained"
            :signals ["Declining underwriting" "Market saturation" "Regulatory changes" "Competitive pressure"]
            :safeguards ["Sustainable float sources" "Underwriting discipline" "Business diversification" "Conservative assumptions"])
   (failure "negative-cost-illusion" "medium"
            "Believing float is free when true cost is hidden in poor underwriting"
            :signals ["Underwriting losses" "Adverse selection" "Claims inflation" "Reserve deficiencies"]
            :safeguards ["Rigorous underwriting" "Combined ratio tracking" "Reserve adequacy" "Honest accounting"])]})

(register-model
 {:name "customer-fanaticism"
  :category "business_strategy"
  :originator "Charlie Munger (See's Candy case study)"
  :description "Building extreme customer loyalty through fanatical commitment to quality, even at high cost; creating a brand that customers prefer to all others"
  :key-insight "Customer fanaticism is rewarded by extraordinary economics (sales per square foot, pricing power, repeat purchases); the cost of quality is repaid many times over"
  :application "Identify what customers truly value; be fanatical about delivering it; never compromise on core quality; charge premium prices; measure customer enthusiasm through repeat rates and word-of-mouth"
  :failure-modes
  [(failure "quality-drift" "critical"
            "Gradually compromising quality to cut costs, destroying customer fanaticism"
            :signals ["Cost-cutting initiatives" "Ingredient substitution" "Customer complaints" "Declining repeat rates"]
            :safeguards ["Quality audits" "Customer feedback" "Founder principles" "Long-term thinking"])
   (failure "fanaticism-without-economics" "high"
            "Being fanatical about things customers don't value or won't pay for"
            :signals ["High costs" "Low prices" "Poor margins" "Unprofitable growth"]
            :safeguards ["Customer research" "Willingness-to-pay analysis" "Unit economics" "Margin discipline"])
   (failure "scale-dilution" "high"
            "Rapid expansion destroying the fanatical attention to detail"
            :signals ["Quality complaints" "Franchisee issues" "Brand dilution" "Customer defection"]
            :safeguards ["Controlled growth" "Company ownership" "Training intensity" "Quality systems"])
   (failure "founder-dependence" "medium"
            "Fanaticism tied to founder personality rather than institutionalized systems"
            :signals ["Succession concerns" "No quality systems" "Founder burnout" "Inconsistent execution"]
            :safeguards ["Document standards" "Training programs" "Quality systems" "Cultural embedding"])
   (failure "premium-without-differentiation" "high"
            "Charging premium prices without delivering fanatical quality"
            :signals ["Customer resistance" "Competitive pressure" "Declining sales" "Negative reviews"]
            :safeguards ["Continuous improvement" "Customer feedback" "Competitive analysis" "Value delivery"])]})

(register-model
 {:name "honest-assessment"
  :category "decision_making"
  :originator "Charlie Munger / Warren Buffett"
  :description "Brutally honest evaluation of situations, especially failures and risks; no sugarcoating, no wishful thinking, no self-deception"
  :key-insight "The first step to solving a problem is admitting you have one; honest assessment of bad situations enables better decision-making than optimistic delusion"
  :application "In annual reports, board meetings, and internal discussions: state problems clearly; acknowledge uncertainties; admit mistakes; describe risks honestly; avoid euphemisms and spin"
  :failure-modes
  [(failure "toxic-positivity" "critical"
            "Culture of forced optimism that punishes honest assessment"
            :signals ["Shoot-the-messenger dynamics" "Declining to report bad news" "Surprise failures" "Morale issues"]
            :safeguards ["Reward honesty" "Pre-mortems" "Anonymous feedback" "Leadership modeling"])
   (failure "honesty-without-action" "high"
            "Admitting problems but not fixing them"
            :signals ["Repeated acknowledgments" "No improvement" "Stakeholder frustration" "Declining trust"]
            :safeguards ["Action plans" "Accountability" "Resource allocation" "Progress tracking"])
   (failure "public-honesty-private-delusion" "critical"
            "Being honest externally while deluding yourself internally"
            :signals ["Disconnect between words and actions" "Continued bad decisions" "Stakeholder confusion" "Trust erosion"]
            :safeguards ["Internal honesty first" "Decision documentation" "Consistency checks" "Advisor feedback"])
   (failure "assessment-paralysis" "medium"
            "Endless honest assessment without decision or action"
            :signals ["Analysis paralysis" "No decisions" "Missed opportunities" "Frustration"]
            :safeguards ["Decision deadlines" "Action bias" "Satisficing" "Progress over perfection"])
   (failure "honesty-as-excuse" "medium"
            "Using honest assessment as excuse for poor performance"
            :signals ["Excuses without improvement" "Victim mentality" "No accountability" "Declining standards"]
            :safeguards ["Accountability culture" "Performance standards" "Consequences" "Growth mindset"])]})

(register-model
 {:name "litigation-hazard"
  :category "risk_management"
  :originator "Charlie Munger (Buffalo Evening News case study)"
  :description "Understanding that competitors may seek protection from competition in the courts; legal risk as a form of competitive moat or barrier"
  :key-insight "Modern tendency of competitors to seek protection from competition in courts; litigation is time-consuming, inefficient, costly, and unpredictable; can destroy or delay otherwise sound business decisions"
  :application "Before major competitive moves: assess litigation risk; build legal defenses; prepare for extended legal battles; factor litigation costs into ROI; consider settlement vs. fighting; maintain war chest for legal expenses"
  :failure-modes
  [(failure "litigation-naivety" "critical"
            "Assuming competitors will compete fairly rather than through legal action"
            :signals ["Surprise lawsuits" "Injunctions" "Business disruption" "Unprepared legal defense"]
            :safeguards ["Legal risk assessment" "Competitor analysis" "Regulatory review" "Legal counsel"])
   (failure "settlement-weakness" "high"
            "Settling frivolous lawsuits, encouraging more litigation"
            :signals ["Repeated lawsuits" "Increasing settlement costs" "Competitor emboldening" "Nuisance suits"]
            :safeguards ["Fight frivolous suits" "Precedent awareness" "Deterrence strategy" "Legal reputation"])
   (failure "litigation-hubris" "critical"
            "Overconfidence in legal position leading to catastrophic loss"
            :signals ["Dismissing legal risks" "Inadequate legal budget" "Weak legal team" "Surprise adverse rulings"]
            :safeguards ["Expert legal counsel" "Scenario planning" "Settlement options" "Risk quantification"])
   (failure "business-paralysis" "high"
            "Letting litigation risk prevent all competitive action"
            :signals ["No innovation" "Competitive decline" "Missed opportunities" "Legal veto power"]
            :safeguards ["Risk-adjusted decisions" "Legal risk tolerance" "Business judgment rule" "Calculated risks"])
   (failure "litigation-cost-underestimation" "high"
            "Underestimating the direct and indirect costs of extended litigation"
            :signals ["Budget overruns" "Management distraction" "Morale impact" "Business disruption"]
            :safeguards ["Full cost analysis" "Opportunity cost" "Management time" "Reputation impact"])]})

(register-model
 {:name "multi-year-performance-view"
  :category "measurement"
  :originator "Charlie Munger / Warren Buffett"
  :description "Evaluating performance over multiple years rather than quarterly or annually; understanding that fluctuating returns can average to respectable long-term results"
  :key-insight "Our objective is to earn a fluctuating return that amounts to a respectable average annual return over a period of years; short-term volatility is acceptable if long-term average is strong"
  :application "Set multi-year performance targets (3-5 years minimum); communicate fluctuating nature of returns to stakeholders; resist pressure for smooth quarterly earnings; focus on long-term average ROE; accept short-term volatility"
  :failure-modes
  [(failure "quarterly-capitalism" "critical"
            "Managing for quarterly earnings at expense of long-term value"
            :signals ["Earnings management" "Short-term decisions" "R&D cuts" "Customer dissatisfaction"]
            :safeguards ["Long-term incentives" "Multi-year targets" "Owner-operator mindset" "Patient capital"])
   (failure "volatility-panic" "high"
            "Overreacting to short-term performance fluctuations"
            :signals ["Strategy changes" "Management turnover" "Stakeholder pressure" "Hasty decisions"]
            :safeguards ["Long-term perspective" "Stakeholder education" "Performance context" "Steady leadership"])
   (failure "multi-year-excuse" "medium"
            "Using multi-year view as excuse for consistent underperformance"
            :signals ["Years of poor results" "No improvement trend" "Declining competitive position" "Stakeholder frustration"]
            :safeguards ["Minimum acceptable performance" "Trend analysis" "Competitive benchmarks" "Accountability"])
   (failure "communication-failure" "high"
            "Not explaining multi-year view to stakeholders, causing misunderstanding"
            :signals ["Stakeholder confusion" "Pressure for smoothing" "Misaligned expectations" "Trust erosion"]
            :safeguards ["Clear communication" "Education" "Transparency" "Expectation setting"])
   (failure "long-term-without-milestones" "medium"
            "No intermediate milestones to track progress toward multi-year goals"
            :signals ["No progress visibility" "Course correction delays" "Stakeholder anxiety" "Drift"]
            :safeguards ["Milestone tracking" "Leading indicators" "Progress reports" "Course corrections"])]})

;; ============================================
;; Integration Notes
;; ============================================
;; These 5 models are extracted from actual Munger business decisions and writings,
;; specifically the Blue Chip Stamps case study (1978-1982). They represent:
;;
;; 1. Float Management - From trading stamp and insurance businesses
;; 2. Customer Fanaticism - From See's Candy success story
;; 3. Honest Assessment - From candid discussion of Buffalo News challenges
;; 4. Litigation Hazard - From Buffalo News competitive lawsuit experience
;; 5. Multi-Year Performance View - From stated investment philosophy
;;
;; These are not abstract concepts but proven principles applied in real businesses
;; that generated billions in value. Each model includes failure modes derived from
;; the actual risks and challenges described in Munger's writings.
;;
;; Previous total: 169 models
;; New total: 174 models (+5, +3.0%)
;; ============================================
