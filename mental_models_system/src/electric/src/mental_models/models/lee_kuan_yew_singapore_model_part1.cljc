(ns mental-models.models.lee-kuan-yew-singapore-model-part1
  "Mental Models - Lee Kuan Yew / Singapore Model Category (Part 1)"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; ============================================
;; Category: Lee Kuan Yew / Singapore Model (Part 1)
;; ============================================

(register-model
 {:name "pragmatic-authoritarianism"
  :category "governance"
  :originator "Lee Kuan Yew / Singapore"
  :description "Results matter more than ideology; strong centralized decision-making combined with pragmatic, evidence-based policies"
  :key-insight "Ideology is a luxury; survival and prosperity require pragmatism; strong leadership enables rapid adaptation; results validate approach"
  :application "Focus on outcomes not ideology; centralize critical decisions; empower execution; measure results rigorously; adapt based on evidence"
  :failure-modes
  [(failure "authoritarianism-without-competence" "critical"
            "Centralized power without capability"
            :signals ["Poor decisions" "No accountability" "Decline" "Oppression without results"]
            :safeguards ["Meritocratic selection" "Competence requirements" "Performance measurement" "Succession planning"])
   (failure "pragmatism-without-principles" "high"
            "Abandoning all principles for expediency"
            :signals ["Corruption" "Short-termism" "Moral decay" "Loss of legitimacy"]
            :safeguards ["Core principles" "Long-term thinking" "Ethical boundaries" "Value preservation"])
   (failure "centralization-bottleneck" "high"
            "Over-centralization causing slow decisions"
            :signals ["Bottlenecks" "Slow adaptation" "Missed opportunities" "Bureaucracy"]
            :safeguards ["Selective centralization" "Delegation" "Clear decision rights" "Rapid execution"])
   (failure "authoritarian-succession" "critical"
            "No mechanism for leadership transition"
            :signals ["Succession crisis" "Instability" "Decline after founder" "System collapse"]
            :safeguards ["Institutionalization" "Succession planning" "System over person" "Smooth transitions"])
   (failure "pragmatism-drift" "high"
            "Losing pragmatism, becoming ideological"
            :signals ["Dogmatism" "Evidence ignored" "Declining performance" "Rigidity"]
            :safeguards ["Continuous learning" "Evidence-based policy" "Adaptation" "Humility"])]})

(register-model
 {:name "long-term-thinking"
  :category "strategy"
  :originator "Lee Kuan Yew / Singapore"
  :description "Multi-generational planning; optimizing for 50-100 year outcomes rather than election cycles; building institutions that outlast individuals"
  :key-insight "Short-term thinking destroys nations; compound growth requires decades; institutions matter more than individuals; plant trees you won't sit under"
  :application "Plan for 50+ years; build institutions not cults of personality; invest in fundamentals (education, infrastructure, rule of law); sacrifice short-term for long-term"
  :failure-modes
  [(failure "short-termism" "critical"
            "Optimizing for immediate results at expense of future"
            :signals ["Deferred maintenance" "Underinvestment" "Declining fundamentals" "Future crisis"]
            :safeguards ["Long-term metrics" "Intergenerational thinking" "Deferred gratification" "Compound thinking"])
   (failure "infinite-horizon" "medium"
            "Planning so long-term that present suffers"
            :signals ["Current hardship" "Unrest" "Legitimacy loss" "Overthrow"]
            :safeguards ["Balance short and long-term" "Minimum viable present" "Communication" "Patience building"])
   (failure "plan-rigidity" "high"
            "Long-term plan that doesn't adapt"
            :signals ["Obsolete plans" "Changing conditions ignored" "Wasted effort" "Failure"]
            :safeguards ["Adaptive planning" "Regular reviews" "Scenario planning" "Flexibility"])
   (failure "institutional-sclerosis" "high"
            "Institutions becoming rigid and dysfunctional"
            :signals ["Bureaucracy" "Inability to adapt" "Declining performance" "Irrelevance"]
            :safeguards ["Institutional renewal" "Performance management" "Adaptation mechanisms" "Creative destruction"])
   (failure "founder-dependence" "critical"
            "Institutions dependent on founder, not self-sustaining"
            :signals ["Cult of personality" "Succession crisis" "Decline after founder" "System collapse"]
            :safeguards ["Institutionalization" "System over person" "Succession planning" "Distributed leadership"])]})

(register-model
 {:name "meritocratic-governance"
  :category "governance"
  :originator "Lee Kuan Yew / Singapore"
  :description "Select the best people for critical positions based on competence, not connections; pay them well; hold them accountable for results"
  :key-insight "Quality of leadership determines outcomes; meritocracy attracts talent; high pay prevents corruption; accountability ensures performance"
  :application "Rigorous selection processes; competitive compensation; clear performance metrics; ruthless accountability; continuous upgrading"
  :failure-modes
  [(failure "meritocracy-theater" "critical"
            "Appearance of meritocracy masking nepotism"
            :signals ["Connected people in key roles" "Poor performance tolerated" "Cynicism" "Talent exodus"]
            :safeguards ["Transparent selection" "Objective criteria" "External validation" "Performance-based retention"])
   (failure "narrow-merit" "high"
            "Defining merit too narrowly (e.g., test scores only)"
            :signals ["Lack of diversity" "Groupthink" "Missed talent" "Brittleness"]
            :safeguards ["Holistic assessment" "Multiple dimensions of merit" "Diverse selection" "Real-world performance"])
   (failure "insufficient-compensation" "high"
            "Not paying enough to attract/retain top talent"
            :signals ["Talent drain" "Corruption" "Mediocrity" "Declining performance"]
            :safeguards ["Competitive compensation" "Market benchmarking" "Retention analysis" "Value of talent"])
   (failure "accountability-failure" "critical"
            "No consequences for poor performance"
            :signals ["Declining standards" "Complacency" "Poor results" "System decay"]
            :safeguards ["Clear metrics" "Regular reviews" "Consequences" "High standards"])
   (failure "meritocracy-ossification" "high"
            "Past merit preventing fresh talent"
            :signals ["Gerontocracy" "Stagnation" "Missed innovation" "Declining performance"]
            :safeguards ["Continuous competition" "Fresh talent infusion" "Retirement" "Renewal"])]})

(register-model
 {:name "economic-pragmatism"
  :category "economics"
  :originator "Lee Kuan Yew / Singapore"
  :description "Whatever works, works; no ideological commitment to capitalism or socialism; adopt policies based on results, not theory"
  :key-insight "Economic ideology is a luxury; survival requires pragmatism; learn from everyone; copy what works; adapt to local context"
  :application "Test policies empirically; measure outcomes rigorously; copy successful models; adapt to context; abandon failures quickly"
  :failure-modes
  [(failure "ideological-rigidity" "high"
            "Commitment to economic ideology over results"
            :signals ["Evidence ignored" "Declining performance" "Dogmatism" "Stagnation"]
            :safeguards ["Pragmatism" "Evidence-based policy" "Learning from all systems" "Results focus"])
   (failure "policy-whiplash" "medium"
            "Changing policies too frequently based on short-term results"
            :signals ["Instability" "No long-term investment" "Confusion" "Lack of credibility"]
            :safeguards ["Policy patience" "Long-term evaluation" "Stability" "Clear communication"])
   (failure "context-blindness" "critical"
            "Copying policies without adapting to local context"
            :signals ["Policy failures" "Unintended consequences" "Wasted resources" "Disillusionment"]
            :safeguards ["Context analysis" "Local adaptation" "Pilot programs" "Careful implementation"])
   (failure "measurement-failure" "high"
            "Not measuring outcomes rigorously"
            :signals ["No learning" "Bad policies persist" "Declining performance" "Ideology over evidence"]
            :safeguards ["Rigorous measurement" "Data-driven policy" "Experimental design" "Honest evaluation"])
   (failure "pragmatism-without-strategy" "high"
            "Reactive pragmatism without long-term direction"
            :signals ["No coherence" "Contradictory policies" "Confusion" "Lack of progress"]
            :safeguards ["Strategic pragmatism" "Long-term vision" "Coherent framework" "Principled flexibility"])]})

;; ============================================
;; Iteration 15 Summary
;; ============================================
;; Added 10 new mental models:
;; 1. Regime Detection (Jim Simons) - Identifying system state changes
;; 2. Factor Decomposition (Renaissance) - Breaking down complex outcomes
;; 3. Mean Reversion (Renaissance) - Statistical arbitrage and reversion
;; 4. Radical Transparency (Ray Dalio) - Extreme openness and honesty
;; 5. Idea Meritocracy (Ray Dalio) - Best ideas win regardless of source
;; 6. Believability-Weighted Decisions (Dalio) - Track record-based weighting
;; 7. Pragmatic Authoritarianism (Lee Kuan Yew) - Results over ideology
;; 8. Long-Term Thinking (LKY) - Multi-generational planning
;; 9. Meritocratic Governance (LKY) - Best people in key positions
;; 10. Economic Pragmatism (LKY) - Whatever works, works
;;
;; Previous total: 147 models (in models.cljc)
;; New total: 157 models (in models.cljc)
;; Combined with new_models.cljc (12 models): 169 total models
;; ============================================
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