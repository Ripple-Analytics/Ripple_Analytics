(ns mental-models.models.lee-kuan-yew-singapore-model-part7
  "Mental Models - Lee Kuan Yew / Singapore Model Category (Part 7)"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; ============================================
;; Category: Lee Kuan Yew / Singapore Model (Part 7)
;; ============================================

(register-model
 {:name "monopoly-vs-competition"
  :category "business_strategy"
  :originator "Peter Thiel"
  :description "Monopoly vs Competition is Thiel's counterintuitive insight that monopolies are good and competition is bad for value creation. All happy companies are different - each earns a monopoly by solving a unique problem. All failed companies are the same - they failed to escape competition. Competition drives profits to zero, forces survival mode, prevents long-term thinking and innovation. Monopolies capture value, enable long-term thinking, can invest in innovation, treat employees well, and benefit society through superior products. The key is to build a monopoly by starting small (dominate tiny market), scaling up (expand to adjacent markets), avoiding disruption (create new markets don't compete), and achieving last mover advantage (make the last great development). Monopoly characteristics: proprietary technology (10x better), network effects (more valuable as more people use it), economies of scale (better as it gets bigger), and strong branding. Competition characteristics: commoditized products, price competition, thin margins, survival mode, talent drain. Monopolists lie about their monopoly (pretend market is bigger), competitors lie about competition (pretend market is smaller). The critical insight is that you should avoid competition and seek monopoly, which is opposite of what most people believe."
  :key-insight "Competition is for losers - all happy companies earn monopolies, all failed companies failed to escape competition"
  :application "Before entering any market, ask: Can I achieve monopoly here? Am I creating a new market or competing in existing one? Do I have 10x better technology?"
  :real-world-examples
  ["Google: 90%+ search market share, 25%+ profit margins, can invest in moonshots - $280B+ annual revenue, $80B+ profit"
   "Airlines: Perfect competition, 1-2% profit margins, constant bankruptcies - $800B annual revenue, $10-20B profit (entire industry)"
   "Microsoft: 95%+ OS market share (peak), 30%+ profit margins, dominated for decades - $200B+ annual revenue, $70B+ profit"
   "Restaurants: High competition, 3-5% profit margins, 60% fail within 3 years - thin margins, high failure rate"
   "Facebook: 70%+ social network market share, 30%+ profit margins, $130B+ annual revenue, $40B+ profit"]
  :quantitative-thresholds
  {:apply-when "Starting companies, entering markets, making strategic decisions, investing"
   :monopoly-threshold "50%+ market share in well-defined market indicates monopoly"
   :profit-margin-test "Monopolies have 20-30%+ margins, competitive markets have <5% margins"
   :technology-advantage "Need 10x better technology to achieve monopoly, not 10% better"
   :market-size "Start with market small enough to dominate (1000-10000 customers), then scale"}
  :failure-modes
  [(failure "competition-trap" "critical"
            "Entering competitive markets instead of creating new ones"
            :signals ["Many competitors" "Price competition" "Thin margins" "Commoditized products" "Fighting for survival"]
            :safeguards ["Avoid competitive markets" "Create new categories" "Seek monopoly positions" "10x better technology" "Start small and dominate"]
            :recovery-protocols ["When stuck in competition, pivot to create new category" "Find unique positioning" "Achieve 10x improvement"]
            :case-studies ["Uber: Competed with taxis, thin margins, struggled with profitability - $28B revenue, minimal profit"
                          "Google: Created new search category, monopoly position, high margins - $280B+ revenue, $80B+ profit"
                          "Airlines: Perfect competition, bankruptcies, thin margins - entire industry less profitable than Google"
                          "Restaurants: High competition, 60% fail, thin margins - difficult to build lasting value"])
   (failure "market-size-delusion" "high"
            "Defining market too broadly (monopolist lying) or too narrowly (competitor lying)"
            :signals ["Monopolist: 'We're only 1% of $X trillion market'" "Competitor: 'We're the leader in our niche'" "Misleading market definitions" "Ignoring real competition"]
            :safeguards ["Define market honestly" "Look at real competition" "Use multiple market definitions" "Test with customers" "Revenue-based market sizing"]
            :recovery-protocols ["When market definition misleads, redefine based on reality" "Look at who customers consider alternatives" "Revenue-based sizing"]
            :case-studies ["Google: Says 'small player in $X trillion ad market' but has 90%+ search monopoly - market definition matters"
                          "Small restaurant: Says 'leader in vegan Mexican food in neighborhood' but competes with all restaurants - too narrow"
                          "Microsoft: Defined market as 'personal computing' not 'all computing' to claim monopoly - accurate definition"
                          "Startups: Often define market too narrowly to claim leadership, too broadly to raise funding - need honest definition"])
   (failure "disruption-obsession" "medium"
            "Trying to disrupt existing markets instead of creating new ones"
            :signals ["'We're disrupting X'" "Competing with incumbents" "Better/faster/cheaper positioning" "Entering crowded markets"]
            :safeguards ["Create new markets don't disrupt" "Avoid direct competition" "Find uncontested space" "Make competition irrelevant" "Blue ocean strategy"]
            :recovery-protocols ["When disruption fails, pivot to create new category" "Find uncontested space" "Reframe value proposition"]
            :case-studies ["Tesla: Didn't disrupt gas cars, created electric car category - $800B+ valuation, high margins"
                          "Uber: Disrupted taxis, competed on price, thin margins - struggled with profitability"
                          "Airbnb: Created home-sharing category, didn't disrupt hotels - $80B+ valuation, growing margins"
                          "Most disruptors: Compete on price, thin margins, difficult to build lasting value"])
   (failure "first-mover-fallacy" "high"
            "Being first instead of being last (and best) - confusing first mover with last mover advantage"
            :signals ["'First to market'" "Speed over quality" "Land grab mentality" "Ignoring that most first movers fail"]
            :safeguards ["Focus on last mover advantage" "Be best not first" "Learn from first movers' mistakes" "Perfect the product" "Timing matters more than being first"]
            :recovery-protocols ["When first mover fails, learn and come back as last mover" "Study why first movers failed" "Build better product"]
            :case-studies ["Google: Not first search engine (AltaVista, Yahoo first), but last and best - dominates market"
                          "Facebook: Not first social network (Friendster, MySpace first), but last and best - dominates market"
                          "Apple: Not first smartphone (BlackBerry first), but last and best - captures 80%+ of industry profits"
                          "First movers: Most fail (Friendster, MySpace, BlackBerry, Palm, Netscape) - last mover advantage matters more"])
   (failure "scale-too-early" "high"
            "Scaling before achieving monopoly in small market"
            :signals ["Expanding geographically before dominating locally" "Adding features before perfecting core" "Hiring rapidly before product-market fit" "Burning cash to grow"]
            :safeguards ["Dominate small market first" "Perfect product before scaling" "Achieve monopoly then expand" "Adjacent market expansion" "Sustainable unit economics"]
            :recovery-protocols ["When scaling fails, return to small market" "Perfect product" "Achieve monopoly in niche then expand"]
            :case-studies ["Facebook: Dominated Harvard, then Ivy League, then colleges, then everyone - methodical expansion from monopoly position"
                          "Amazon: Dominated books, then adjacent categories, then everything - started with monopoly in books"
                          "WeWork: Scaled globally before achieving monopoly anywhere - burned $10B+, nearly collapsed"
                          "Many startups: Scale too early, burn cash, fail to achieve monopoly anywhere - most fail"])]
  :cross-model-interactions
  ["Combines with Zero to One: 0 to 1 creates monopolies, 1 to n creates competition"
   "Combines with Network Effects: Network effects create and defend monopolies"
   "Combines with Economies of Scale: Scale advantages create monopoly positions"
   "Countered by Antitrust: Monopolies face regulatory pressure, must be careful"
   "Enhanced by Brand: Strong brands create monopoly-like positions"]})

(register-model
 {:name "radical-truth-and-transparency"
  :category "organizational_excellence"
  :originator "Ray Dalio"
  :description "Radical Truth and Transparency is Dalio's principle for creating an idea meritocracy where the best ideas win regardless of who has them. Radical truth means saying what you really think, not what's polite - embracing reality and dealing with it. Radical transparency means making information accessible to everyone, recording all meetings, sharing mistakes publicly, and transparent decision-making. Believability-weighted decision making means weighting opinions by track record in relevant domain, not by rank or popularity. The goal is to align people around truth, not politics or hierarchy. Pain + Reflection = Progress: pain signals something needs to change, reflection reveals root cause, converting learnings into principles enables systematic improvement. The key insight is that most organizations fail because people don't say what they think (political correctness, fear, hierarchy) and information is hidden (politics, power games). Radical truth and transparency eliminate these problems but require courage and thick skin. Bridgewater built $160B+ AUM and best risk-adjusted returns using these principles, proving they work at scale."
  :key-insight "Create idea meritocracy through radical truth (say what you think) and radical transparency (share all information) - best ideas win not politics"
  :application "In every interaction, ask: Am I saying what I really think? Is information being shared openly? Are we weighting by believability or rank?"
  :real-world-examples
  ["Bridgewater: Records all meetings, shares mistakes publicly, believability-weighted voting - $160B AUM, best risk-adjusted returns for 40+ years"
   "Ray Dalio's mistakes: Publicly shared 1982 debt crisis prediction failure, learned from it, built principles - turned failure into systematic improvement"
   "Junior analyst saves Bridgewater: Believability weighting allowed junior analyst's insight to override seniors, saved fund from major loss"
   "Traditional firms: Hide mistakes, political decision-making, hierarchy-based - average performance, high politics"
   "Enron: Opposite of radical transparency, culture of hiding problems - collapsed in fraud scandal, $74B loss"]
  :quantitative-thresholds
  {:apply-when "Building organizations, making decisions, creating culture, managing teams"
   :transparency-threshold "Record and share 80%+ of meetings (exception: personal/confidential)"
   :believability-minimum "Need 3+ successful outcomes in domain to have high believability"
   :feedback-frequency "Real-time feedback in meetings, not annual reviews"
   :pain-reflection-cycle "Reflect on failures within 24-48 hours while fresh"}
  :failure-modes
  [(failure "fake-harmony" "critical"
            "Avoiding conflict, not saying what you think - prioritizing comfort over truth"
            :signals ["Agreeing in meetings, disagreeing in hallways" "No challenging questions" "Polite disagreement only" "Avoiding difficult conversations" "Consensus without debate"]
            :safeguards ["Make thoughtful disagreement mandatory" "Reward truth-telling" "Punish fake harmony" "Create psychological safety" "Model radical truth from top"]
            :recovery-protocols ["When fake harmony causes problems, create culture where truth-telling is rewarded" "Start with leadership modeling" "Make disagreement expected"]
            :case-studies ["Nokia: Fake harmony, no one told CEO about iPhone threat - market share collapsed from 40% to 3%"
                          "Bridgewater: Mandatory disagreement, truth-telling rewarded - best risk-adjusted returns in industry"
                          "Enron: Fake harmony, no one challenged accounting - collapsed in fraud scandal"
                          "Most organizations: Fake harmony is norm, politics over truth - mediocre performance"])
   (failure "political-correctness" "high"
            "Prioritizing politeness over truth - saying what's acceptable not what's true"
            :signals ["Euphemisms for problems" "Avoiding direct feedback" "Sugar-coating bad news" "Focus on feelings over facts" "Truth is 'offensive'"]
            :safeguards ["Truth over politeness" "Direct feedback culture" "Facts over feelings in decisions" "Distinguish personal attacks from truth" "Thick skin requirement"]
            :recovery-protocols ["When political correctness hides truth, create culture where truth is expected" "Distinguish kindness from political correctness" "Focus on outcomes"]
            :case-studies ["Yahoo: Political correctness prevented honest feedback about declining product - lost to Google"
                          "GE: Political correctness around earnings quality - accounting fraud, collapsed from $600B to $50B"
                          "Bridgewater: Radical truth over politeness - best performance in industry"
                          "Most organizations: Political correctness hides problems until crisis - reactive not proactive"])
   (failure "hierarchy-worship" "high"
            "Weighting opinions by rank instead of believability - senior people always right"
            :signals ["Senior opinions always win" "Junior people don't speak up" "Rank-based decision making" "No track record consideration" "Deference to authority"]
            :safeguards ["Believability-weighted decision making" "Track record matters not rank" "Junior people can override seniors" "Transparent believability scores" "Idea meritocracy"]
            :recovery-protocols ["When hierarchy causes bad decisions, implement believability weighting" "Track decision outcomes by person" "Make track record visible"]
            :case-studies ["Bridgewater: Junior analyst's insight overrode seniors based on believability - saved fund from major loss"
                          "NASA Challenger: Engineers' concerns overridden by managers - 7 deaths, $4B loss"
                          "2008 Financial Crisis: Junior analysts saw problems, overridden by senior bankers - $10T+ loss"
                          "Most organizations: Hierarchy trumps truth - leads to preventable failures"])
   (failure "information-hoarding" "high"
            "Hiding information for power or politics - information is power mentality"
            :signals ["Need-to-know basis" "Information silos" "Selective sharing" "Politics around information" "Surprise decisions"]
            :safeguards ["Radical transparency default" "Share all information (except personal/confidential)" "Record and share meetings" "Open decision-making" "Information accessibility"]
            :recovery-protocols ["When information hoarding causes problems, implement radical transparency" "Make sharing default" "Punish hoarding"]
            :case-studies ["Bridgewater: Records all meetings, shares widely - best decision quality in industry"
                          "Enron: Information hoarding enabled fraud - collapsed with $74B loss"
                          "Nokia: Information about iPhone threat not shared - market share collapsed"
                          "Most organizations: Information hoarding creates politics and bad decisions"])
   (failure "pain-avoidance" "critical"
            "Not reflecting on failures, repeating mistakes - avoiding painful truth"
            :signals ["No post-mortems" "Blame others for failures" "Repeat same mistakes" "Defensive about errors" "No systematic learning"]
            :safeguards ["Pain + Reflection = Progress" "Mandatory post-mortems" "Blameless culture" "Convert learnings to principles" "Systematic improvement"]
            :recovery-protocols ["When repeating mistakes, implement pain/reflection cycle" "Make post-mortems mandatory" "Focus on learning not blame"]
            :case-studies ["Bridgewater: Every failure triggers reflection and principle creation - continuous improvement for 40+ years"
                          "NASA: After Challenger, improved safety culture - no loss of crew for 17 years (until Columbia)"
                          "Most organizations: Repeat same mistakes, no systematic learning - preventable failures"
                          "Toyota: Systematic reflection (5 Whys), continuous improvement - industry-leading quality"])]
  :cross-model-interactions
  ["Combines with Day 1 Thinking: Resisting proxies requires radical truth about what's really happening"
   "Combines with Zero to One: Finding secrets requires radical truth about what's really true"
   "Combines with Skin in the Game: Radical transparency reveals who has skin in game"
   "Countered by Political Correctness: Social pressure for politeness conflicts with radical truth"
   "Enhanced by Inversion: Inverting (what causes failure?) benefits from radical truth about failures"]})