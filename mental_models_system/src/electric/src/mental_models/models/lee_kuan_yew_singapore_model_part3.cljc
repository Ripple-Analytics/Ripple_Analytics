(ns mental-models.models.lee-kuan-yew-singapore-model-part3
  "Mental Models - Lee Kuan Yew / Singapore Model Category (Part 3)"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; ============================================
;; Category: Lee Kuan Yew / Singapore Model (Part 3)
;; ============================================

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


;; ============================================
;; ITERATION 17 - High-Magnitude Enhancement
;; Date: 2026-01-18 13:30 UTC
;; Adding 5 enhanced models from Taleb and Kahneman
;; ============================================

(register-model
 {:name "skin-in-the-game"
  :category "decision_making"
  :originator "Nassim Nicholas Taleb"
  :description "Skin in the Game is about the symmetry of risk and reward - having something real to lose when making decisions. Those who give advice or make decisions should bear the consequences of being wrong. This principle ensures alignment of incentives and filters out empty suits who talk but don't act. Historically, ship captains were the last to leave sinking ships, surgeons operated on their own family members, and builders lived in the buildings they constructed. Modern society has broken this symmetry, allowing decision-makers to profit from upside while transferring downside to others. The 2008 financial crisis exemplified this: bankers took massive risks with other people's money, collected bonuses when bets paid off, and faced no personal ruin when bets failed. Restoring skin in the game means ensuring decision-makers eat their own cooking."
  :key-insight "Never trust anyone who doesn't have skin in the game - those who profit from upside but don't suffer from downside will eventually blow up the system"
  :application "Before accepting advice: Ask 'What does this person have to lose if they're wrong?' In business: Ensure executives own significant equity. In investing: Follow investors who invest their own money. In hiring: Prefer entrepreneurs who risked their own capital over consultants. In regulation: Make regulators personally liable for catastrophic failures. In war: Ensure leaders send their own children to fight."
  :failure-modes
  [(failure "agency-problem" "critical"
            "Decision-makers profit from upside but transfer downside to others - the fundamental agency problem"
            :signals ["Executives with no equity ownership" "Bonuses for short-term gains" "Limited liability structures" "Asymmetric compensation" "Golden parachutes" "No clawback provisions"]
            :safeguards ["Mandatory equity ownership (50%+ of net worth)" "Long-term vesting (5+ years)" "Clawback provisions for failures" "Personal liability for fraud" "Skin-in-game audits" "Malus provisions"]
            :case-studies [{:name "2008 Financial Crisis"
                           :description "Wall Street bankers took massive risks with depositors' money, collected billions in bonuses 2003-2007, then required $700B taxpayer bailout when bets failed. Zero executives went to jail. Lehman Brothers CEO Richard Fuld collected $500M in compensation 2000-2007 while destroying 158-year-old firm."
                           :impact "$7.4 trillion in losses, 8.7M jobs lost, 10M foreclosures"
                           :lesson "Without skin in game, risk-taking becomes reckless"}
                          {:name "Boeing 737 MAX"
                           :description "Boeing executives prioritized stock buybacks ($43B 2013-2019) over engineering safety. CEO Dennis Muilenburg collected $23.4M in 2018 while MCAS system had fatal flaws. Two crashes killed 346 people. Muilenburg fired but kept $62M in compensation."
                           :impact "346 deaths, $20B+ in losses, criminal charges"
                           :lesson "Executives without engineering skin in game cut safety"}
                          {:name "Theranos"
                           :description "Elizabeth Holmes raised $700M claiming revolutionary blood testing technology. Board included Henry Kissinger, George Shultz, James Mattis - zero medical expertise. Investors and board had no skin in game (no medical licenses at risk). Holmes faces 20 years prison."
                           :impact "$700M+ investor losses, patient harm, fraud charges"
                           :lesson "Advisors without relevant skin in game enable fraud"}])
   (failure "hidden-risk-transfer" "critical"
            "Risks are transferred to others through complex structures that hide the transfer"
            :signals ["Complex financial instruments" "Off-balance-sheet entities" "Derivatives" "Securitization" "Moral hazard" "Too-big-to-fail"]
            :safeguards ["Transparency requirements" "Simple structures only" "No bailouts" "Personal guarantees" "Stress testing" "Risk retention rules"]
            :case-studies [{:name "Enron"
                           :description "Enron used 3,000+ special purpose entities (SPEs) to hide $27B in debt from balance sheet. CFO Andrew Fastow personally profited $30M+ from SPE partnerships while transferring risk to Enron shareholders. Auditor Arthur Andersen had no skin in game - collected fees but faced no liability."
                           :impact "$74B market cap to $0, 20,000 jobs lost, $2B in pensions lost"
                           :lesson "Complex structures hide risk transfer"}])
   (failure "consultants-disease" "high"
            "Hiring advisors who give recommendations but bear no consequences if wrong"
            :signals ["Management consultants with no equity" "Investment bankers with no capital at risk" "Advisors with no track record" "Theoretical expertise" "No personal investment"]
            :safeguards ["Require advisors to invest own capital" "Performance-based fees only" "Track record verification" "Preference for practitioners" "Eat-your-own-cooking rule"]
            :case-studies [{:name "McKinsey & Opioid Crisis"
                           :description "McKinsey advised Purdue Pharma on how to 'turbocharge' OxyContin sales 2004-2019, earning $83M in fees. Recommended targeting high-prescribing doctors and countering concerns about addiction. 500,000+ Americans died from opioids. McKinsey paid $573M settlement but admitted no wrongdoing."
                           :impact "500,000+ deaths, $1 trillion+ economic cost"
                           :lesson "Consultants with no skin in game optimize for fees, not outcomes"}])
   (failure "bureaucrat-risk" "high"
            "Government officials and regulators making decisions with no personal downside"
            :signals ["Regulatory capture" "Revolving door" "No accountability for failures" "Lifetime employment" "Pension guarantees"]
            :safeguards ["Personal liability for gross negligence" "No revolving door (10-year ban)" "Performance-based compensation" "Clawback for failures" "Term limits"]
            :case-studies [{:name "FDA & Vioxx"
                           :description "FDA approved Merck's Vioxx despite cardiovascular risks. Dr. David Graham (FDA scientist) estimated 88,000-139,000 heart attacks, 30-40% fatal. FDA officials who approved drug faced zero consequences. Merck paid $4.85B settlement. FDA officials moved to industry jobs."
                           :impact "38,000+ deaths, $4.85B settlement"
                           :lesson "Regulators without skin in game approve dangerous products"}])
   (failure "academic-theorizing" "medium"
            "Academics giving policy advice based on theory with no real-world testing"
            :signals ["No business experience" "Never risked own capital" "Theoretical models" "No skin in game" "Ivory tower syndrome"]
            :safeguards ["Require real-world experience" "Practitioners over theorists" "Track record verification" "Skin-in-game requirement" "Reality testing"]
            :case-studies [{:name "LTCM Collapse"
                           :description "Long-Term Capital Management founded by Nobel Prize winners Myron Scholes and Robert Merton. Used theoretical models (Black-Scholes) to take massive leveraged bets. Lost $4.6B in 1998, required $3.6B Fed-orchestrated bailout. Partners lost personal wealth but theories remained untested in academia."
                           :impact "$4.6B loss, systemic risk, Fed intervention"
                           :lesson "Academics without skin in game create fragile systems"}])]})