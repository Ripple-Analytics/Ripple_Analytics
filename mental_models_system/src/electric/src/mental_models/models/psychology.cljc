(ns mental-models.models.psychology
  "Mental Models - Psychology Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Psychology
;; ============================================

(register-model
 {:name "incentives"
  :category "psychology"
  :originator "Charlie Munger"
  :description "Never think about anything else when you should be thinking about incentives"
  :key-insight "Show me the incentive and I'll show you the outcome"
  :application "Always ask: What are the incentives?"
  :failure-modes
  [(failure "ignoring-incentives" "critical"
            "Not considering what motivates behavior"
            :signals ["Surprised by behavior" "Moral explanations for economic behavior"]
            :safeguards ["Incentive mapping" "Follow the money" "Behavioral prediction"])
   (failure "misaligned-incentives" "high"
            "Creating incentives that produce wrong behavior"
            :signals ["Gaming" "Unintended consequences" "Perverse outcomes"]
            :safeguards ["Incentive design review" "Pilot testing" "Feedback loops"])
   (failure "incentive-blindness" "high"
            "Not seeing your own incentive biases"
            :signals ["Self-serving conclusions" "Rationalization"]
            :safeguards ["Disinterested review" "Incentive disclosure" "Counter-incentive analysis"])
   (failure "over-incentivizing" "medium"
            "Crowding out intrinsic motivation"
            :signals ["Reduced intrinsic interest" "Only working for rewards"]
            :safeguards ["Balance intrinsic/extrinsic" "Purpose alignment" "Autonomy preservation"])
   (failure "static-incentive-thinking" "medium"
            "Not considering how incentives change over time"
            :signals ["Outdated incentive structures" "Changed circumstances"]
            :safeguards ["Regular incentive review" "Dynamic adjustment" "Feedback mechanisms"])]})

(register-model
 {:name "social-proof"
  :category "psychology"
  :originator "Robert Cialdini"
  :description "People look to others to determine correct behavior"
  :key-insight "We assume if many people do something, it must be right"
  :application "Question: Am I doing this because others are?"
  :failure-modes
  [(failure "herd-following" "high"
            "Blindly following the crowd"
            :signals ["No independent analysis" "Comfort in consensus" "FOMO"]
            :safeguards ["Independent thinking" "Contrarian analysis" "First principles"])
   (failure "wrong-reference-group" "medium"
            "Following the wrong crowd"
            :signals ["Inappropriate comparisons" "Wrong peer group"]
            :safeguards ["Choose reference groups carefully" "Expert identification"])
   (failure "manufactured-proof" "high"
            "Being fooled by fake social proof"
            :signals ["Astroturfing" "Fake reviews" "Manipulated metrics"]
            :safeguards ["Verify sources" "Look for authenticity" "Skepticism"])
   (failure "proof-cascade" "high"
            "Contributing to information cascades"
            :signals ["Amplifying without verification" "Viral spread"]
            :safeguards ["Verify before sharing" "Break cascades" "Independent verification"])
   (failure "anti-social-proof" "low"
            "Contrarian for its own sake"
            :signals ["Automatic disagreement" "Ignoring valid consensus"]
            :safeguards ["Evaluate on merits" "Selective contrarianism" "Evidence-based"])]})

(register-model
 {:name "commitment-consistency"
  :category "psychology"
  :originator "Robert Cialdini"
  :description "Once committed, people tend to behave consistently with that commitment"
  :key-insight "Small commitments lead to larger ones"
  :application "Watch for escalating commitments"
  :failure-modes
  [(failure "escalation-of-commitment" "critical"
            "Doubling down on failing courses of action"
            :signals ["Throwing good money after bad" "Ignoring negative feedback"]
            :safeguards ["Kill criteria" "Fresh eyes review" "Sunk cost awareness"])
   (failure "foot-in-door" "medium"
            "Being manipulated through small commitments"
            :signals ["Gradual escalation" "Small yeses leading to big ones"]
            :safeguards ["Recognize the pattern" "Evaluate each request independently"])
   (failure "identity-lock" "high"
            "Commitment becoming part of identity"
            :signals ["Defending positions as self" "Emotional attachment to decisions"]
            :safeguards ["Separate identity from positions" "Update beliefs" "Growth mindset"])
   (failure "public-commitment-trap" "medium"
            "Public commitments preventing course correction"
            :signals ["Can't back down publicly" "Reputation concerns"]
            :safeguards ["Private deliberation" "Graceful pivots" "Learning framing"])
   (failure "consistency-worship" "medium"
            "Valuing consistency over correctness"
            :signals ["Foolish consistency" "Ignoring new information"]
            :safeguards ["Update on evidence" "Intellectual honesty" "Embrace change"])]})

(register-model
 {:name "availability-heuristic"
  :category "psychology"
  :originator "Kahneman & Tversky"
  :description "We judge likelihood by how easily examples come to mind"
  :key-insight "Vivid and recent events are overweighted"
  :application "Ask: Am I overweighting recent/vivid events?"
  :failure-modes
  [(failure "recency-bias" "high"
            "Overweighting recent events"
            :signals ["Recent events dominating" "Ignoring base rates"]
            :safeguards ["Historical data" "Base rate research" "Long-term perspective"])
   (failure "vividness-bias" "high"
            "Overweighting dramatic events"
            :signals ["Emotional reactions" "Ignoring mundane risks"]
            :safeguards ["Statistical thinking" "Probability assessment" "Calm analysis"])
   (failure "media-distortion" "medium"
            "Being influenced by media coverage"
            :signals ["News-driven fears" "Disproportionate concerns"]
            :safeguards ["Seek data" "Ignore news cycle" "Long-term view"])
   (failure "personal-experience-bias" "medium"
            "Overweighting personal experiences"
            :signals ["Anecdotal reasoning" "Sample of one"]
            :safeguards ["Seek broader data" "Statistical samples" "Outside view"])
   (failure "availability-cascade" "high"
            "Self-reinforcing availability through repetition"
            :signals ["Echo chambers" "Repeated claims becoming 'true'"]
            :safeguards ["Source diversity" "Fact checking" "Original sources"])]})

(register-model
 {:name "loss-aversion"
  :category "psychology"
  :originator "Kahneman & Tversky"
  :description "Losses loom larger than equivalent gains"
  :key-insight "The pain of losing is about twice the pleasure of gaining"
  :application "Check if you're irrationally avoiding losses"
  :failure-modes
  [(failure "excessive-risk-aversion" "high"
            "Avoiding reasonable risks due to loss fear"
            :signals ["Missed opportunities" "Excessive caution" "Status quo bias"]
            :safeguards ["Expected value thinking" "Reframe as gains" "Long-term view"])
   (failure "holding-losers" "high"
            "Refusing to realize losses"
            :signals ["Holding losing positions" "Hope over analysis"]
            :safeguards ["Pre-set stop losses" "Regular portfolio review" "Fresh evaluation"])
   (failure "narrow-framing" "medium"
            "Evaluating each decision in isolation"
            :signals ["Single decision focus" "Ignoring portfolio effect"]
            :safeguards ["Portfolio thinking" "Aggregate view" "Long-term perspective"])
   (failure "endowment-effect" "medium"
            "Overvaluing what you already own"
            :signals ["Reluctance to sell" "Higher selling than buying price"]
            :safeguards ["Ownership-blind evaluation" "Market price reference"])
   (failure "loss-framing-manipulation" "medium"
            "Being manipulated through loss framing"
            :signals ["Responding to loss frames" "Fear-based decisions"]
            :safeguards ["Reframe as gains" "Recognize manipulation" "Neutral framing"])]})

;; ============================================