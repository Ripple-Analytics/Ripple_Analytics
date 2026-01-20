(ns mental-models.models.lee-kuan-yew-singapore-model-part6
  "Mental Models - Lee Kuan Yew / Singapore Model Category (Part 6)"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; ============================================
;; Category: Lee Kuan Yew / Singapore Model (Part 6)
;; ============================================

(register-model
 {:name "day-1-thinking"
  :category "business_strategy"
  :originator "Jeff Bezos"
  :description "Day 1 thinking is Amazon's philosophy of maintaining the energy, urgency, and customer obsession of a startup, even at massive scale. Bezos defines Day 2 as 'stasis, followed by irrelevance, followed by excruciating, painful decline, followed by death.' Day 1 companies focus on results over process, make high-velocity decisions, resist managing to proxies, and eagerly embrace external trends. The philosophy has four pillars: true customer obsession (customers are always beautifully, wonderfully dissatisfied), resist proxies (don't let process become the thing), embrace external trends (fighting trends means fighting the future), and high-velocity decision making (most decisions are reversible two-way doors). This mental model is critical because it addresses the fundamental challenge of maintaining innovation and agility as organizations scale. Amazon grew from $148M to $575B in revenue while maintaining Day 1 culture, proving that scale and startup mentality are not mutually exclusive. The key insight is that Day 2 happens gradually through seemingly reasonable decisions (following process, requiring consensus, resisting trends), making vigilance essential."
  :key-insight "Day 2 is stasis followed by death - maintain Day 1 energy by obsessing over customers, resisting proxies, embracing trends, and deciding with high velocity"
  :application "Before any decision, ask: Is this Day 1 or Day 2 thinking? Are we managing to process/proxies or results? Are we moving fast or requiring consensus? Are we embracing or fighting trends?"
  :real-world-examples
  ["Amazon Prime: No customer asked for it, but customer obsession drove invention - now 200M+ members generating $35B+ annually"
   "AWS: Embraced cloud computing trend early despite skepticism - now $90B+ annual revenue, 32% market share"
   "Alexa: Bet on voice/AI trend, used disagree-and-commit for fast decisions - became fastest-selling Amazon product"
   "Amazon Go: Embraced machine vision trend to eliminate checkout - opened 30+ stores despite initial technical challenges"
   "Two-way door decisions: Bezos uses 'disagree and commit' to maintain velocity - committed to Prime Video series he disagreed with, became hit"]
  :quantitative-thresholds
  {:apply-when "Building or scaling organizations, making strategic decisions, evaluating company culture"
   :decision-velocity "Reversible decisions should take hours/days, not weeks/months - 70%+ of decisions are reversible"
   :customer-obsession "Spend 80%+ of time on customer needs vs 20% on competition"
   :process-check "If process takes longer than outcome delivery, it's Day 2"
   :trend-adoption "Adopt major trends within 6-12 months of emergence, not 2-3 years"}
  :failure-modes
  [(failure "process-worship" "critical"
            "Following process instead of achieving results - process becomes the thing, not the outcome"
            :signals ["'We followed the process' defense for bad outcomes" "Process takes longer than delivery" "Junior leaders cite process compliance" "No one questions whether process serves customers" "Process documentation exceeds product documentation"]
            :safeguards ["Ask 'Do we own the process or does it own us?'" "Measure outcomes not process compliance" "Empower teams to modify process" "Regular process audits" "Customer-back process design"]
            :recovery-protocols ["When bad outcome occurs despite process compliance, investigate and improve process" "Create culture where questioning process is rewarded" "Implement outcome-based metrics"]
            :case-studies ["Sears: Followed retail process perfectly while Amazon innovated - filed bankruptcy 2018 after 125 years"
                          "Nokia: Followed mobile phone process while Apple created smartphones - market share collapsed from 40% to 3%"
                          "Blockbuster: Followed video rental process while Netflix innovated streaming - bankrupt 2010"
                          "Kodak: Followed film process despite inventing digital camera - bankrupt 2012 after 131 years"])
   (failure "proxy-confusion" "critical"
            "Managing to proxies (surveys, metrics, presentations) instead of reality (customers, outcomes)"
            :signals ["Decisions based on surveys not customer understanding" "PowerPoint culture over written narratives" "Metrics gaming" "Focus on lagging indicators" "Proxy metrics replace actual goals"]
            :safeguards ["Deep customer understanding over surveys" "Written narratives over PowerPoint" "Leading indicators over lagging" "Regular reality checks" "Anecdotes alongside data"]
            :recovery-protocols ["When proxy misleads, return to first principles" "Build direct customer feedback loops" "Replace proxy with reality measurement"]
            :case-studies ["Wells Fargo: Managed to account opening metric (proxy), created fake accounts, $3B fine"
                          "Yahoo: Managed to page views (proxy), degraded user experience, lost to Google"
                          "GE: Managed to quarterly earnings (proxy), accounting fraud, collapsed from $600B to $50B"
                          "Boeing 737 MAX: Managed to schedule/cost proxies over safety, 346 deaths, $20B+ losses"])
   (failure "trend-resistance" "high"
            "Fighting powerful external trends instead of embracing them - fighting the future"
            :signals ["'This trend doesn't apply to us'" "'We're different'" "Dismissing new technologies" "Defending status quo" "Slow adoption of obvious trends"]
            :safeguards ["Actively scan for major trends" "Assume trends apply to you" "Fast experimentation with trends" "Embrace trends as tailwinds" "Dedicated trend adoption teams"]
            :recovery-protocols ["When trend disrupts you, rapidly pivot to embrace it" "Create skunkworks to explore trends" "Acquire trend-native companies"]
            :case-studies ["Taxi industry: Fought Uber/Lyft instead of embracing ride-sharing - lost 30%+ market share"
                          "Hotels: Fought Airbnb instead of embracing home-sharing - Airbnb now $80B+ valuation"
                          "Retail: Fought e-commerce instead of embracing it - 15,000+ store closures 2017-2023"
                          "Media: Fought streaming instead of embracing it - cable subscribers down 40% since 2015"])
   (failure "decision-paralysis" "high"
            "Making good decisions too slowly - requiring consensus, over-analyzing reversible decisions"
            :signals ["Weeks/months for reversible decisions" "Requiring unanimous agreement" "Endless analysis" "Fear of being wrong" "One-size-fits-all decision process"]
            :safeguards ["Classify decisions: one-way vs two-way doors" "Lightweight process for reversible decisions" "Disagree and commit culture" "Time-box decisions" "Bias for action"]
            :recovery-protocols ["When slow decisions cause missed opportunities, implement two-way door framework" "Create disagree-and-commit culture" "Measure decision velocity"]
            :case-studies ["Microsoft (pre-Nadella): Slow decisions, missed mobile, cloud - market cap stagnant $200-300B for decade"
                          "Microsoft (post-Nadella): Fast decisions, embraced cloud - market cap $200B to $3T in 8 years"
                          "IBM: Slow decisions, missed cloud, AI - revenue declined from $107B (2011) to $57B (2023)"
                          "Amazon: Fast decisions, embraced cloud, AI - revenue grew from $48B (2011) to $575B (2023)"])
   (failure "false-consensus" "medium"
            "Requiring everyone to agree before moving forward - confusing alignment with agreement"
            :signals ["Waiting for unanimous agreement" "Endless debate" "Lowest common denominator decisions" "Avoiding disagreement" "Consensus as goal"]
            :safeguards ["Disagree and commit framework" "Distinguish alignment from agreement" "Time-bound debate" "Decision-maker clarity" "Commitment over consensus"]
            :recovery-protocols ["When consensus blocks progress, implement disagree-and-commit" "Clarify decision rights" "Measure commitment not agreement"]
            :case-studies ["Yahoo: Required consensus, slow decisions, missed search/social/mobile - sold for $4.5B after $125B peak"
                          "Amazon: Disagree and commit, fast decisions, dominated e-commerce/cloud - $1.7T market cap"
                          "Apple: Steve Jobs made decisions without consensus, created iPhone/iPad - $3T market cap"
                          "Committee-designed products: Consensus leads to mediocrity - most fail in market"])]
  :cross-model-interactions
  ["Combines with Zero to One: Day 1 thinking enables 0 to 1 innovation through customer obsession and high velocity"
   "Combines with Antifragility: Day 1 companies benefit from volatility through rapid experimentation"
   "Combines with Optionality: Two-way door decisions create optionality through reversibility"
   "Countered by Efficiency: Optimizing for efficiency can lead to Day 2 (process worship, proxy management)"
   "Enhanced by Radical Truth: Resisting proxies requires radical truth about what's really happening"]})

(register-model
 {:name "zero-to-one"
  :category "innovation"
  :originator "Peter Thiel"
  :description "Zero to One is Thiel's framework distinguishing vertical progress (creating new things, 0 to 1) from horizontal progress (copying things that work, 1 to n). Vertical progress is technology - doing new things that have never been done. Horizontal progress is globalization - copying things that work to new places. Going from 0 to 1 is harder to imagine because it hasn't been done before, but it's where real value creation happens. Going from 1 to n is easier to imagine but leads to competition and commoditization. Every great business is built on a secret - an important truth that few people agree with you on. Secrets are neither obvious (everyone knows) nor impossible (no one can know), they exist in between. Most people don't look for secrets because of incrementalism (small steps only), risk aversion (fear of being wrong), complacency (comfort with status quo), and flatness (belief that everything important is already known). The key insight is that real value comes from creating new things, not from competing to do existing things better. This requires believing secrets exist and actively looking for them."
  :key-insight "Real value comes from creating new things (0 to 1) not copying existing things (1 to n) - every great business is built on a secret"
  :application "Before starting anything, ask: Is this 0 to 1 or 1 to n? Am I creating something new or copying something that exists? What secret do I know that others don't?"
  :real-world-examples
  ["PayPal: Created digital payments (0 to 1), not better credit cards (1 to n) - sold for $1.5B, enabled e-commerce revolution"
   "SpaceX: Created reusable rockets (0 to 1), not better disposable rockets (1 to n) - reduced launch costs 10x, dominates space industry"
   "Tesla: Created electric car company (0 to 1), not better gas cars (1 to n) - $800B+ valuation, transformed auto industry"
   "Airbnb: Created home-sharing platform (0 to 1), not better hotels (1 to n) - $80B+ valuation, disrupted hospitality"
   "Uber: Copied taxi service with app (1 to n), struggled with profitability - competed with taxis instead of creating new category"]
  :quantitative-thresholds
  {:apply-when "Starting companies, developing products, making career decisions, investing"
   :value-creation "0 to 1 creates 10-100x more value than 1 to n"
   :competition-test "If 5+ competitors exist doing same thing, it's 1 to n not 0 to 1"
   :secret-test "If most people agree with you, it's not a secret - need contrarian truth"
   :technology-threshold "0 to 1 requires 10x improvement over existing solutions, not 10% improvement"}
  :failure-modes
  [(failure "horizontal-thinking" "critical"
            "Copying what works (1 to n) instead of creating new things (0 to 1)"
            :signals ["'Better/faster/cheaper' positioning" "Competing in existing categories" "Incremental improvements" "Following best practices" "Me-too products"]
            :safeguards ["Ask 'What are we creating that's new?'" "Avoid existing categories" "Seek 10x improvements not 10%" "Question best practices" "Look for secrets"]
            :recovery-protocols ["When stuck in competition, pivot to create new category" "Find the 0 to 1 insight" "Reframe problem"]
            :case-studies ["Uber: Copied taxis with app (1 to n), competed with taxis, struggled with profitability - $28B revenue, minimal profit"
                          "Tesla: Created electric car company (0 to 1), no direct competition, highly profitable - $97B revenue, $15B profit"
                          "Google+: Copied Facebook (1 to n), failed despite Google's resources - shut down 2019"
                          "Google Search: Created new search algorithm (0 to 1), dominated market - $280B+ annual revenue"])
   (failure "secret-blindness" "critical"
            "Not believing secrets exist or not looking for them"
            :signals ["'Everything important is known'" "'No new ideas left'" "Incrementalism only" "Risk aversion" "Comfort with status quo"]
            :safeguards ["Ask 'What important truth do few people agree with me on?'" "Actively seek secrets" "Question conventional wisdom" "Look in overlooked areas" "Talk to contrarians"]
            :recovery-protocols ["When stuck, actively search for secrets" "Interview people with contrarian views" "Look at what everyone dismisses"]
            :case-studies ["Airbnb secret: People will rent rooms to strangers - everyone said it wouldn't work, now $80B+ company"
                          "SpaceX secret: Rockets can be reusable - everyone said impossible, now dominates space industry"
                          "Bitcoin secret: Digital scarcity is possible - everyone said impossible, now $1T+ market cap"
                          "Missed secrets: Many dismissed internet, mobile, social media, crypto - those who found secrets captured value"])
   (failure "incremental-trap" "high"
            "Making small improvements instead of seeking breakthrough innovations"
            :signals ["10% improvements" "Best practices focus" "Benchmarking competitors" "Continuous improvement only" "No moonshots"]
            :safeguards ["Seek 10x improvements not 10%" "Question assumptions" "Allow moonshot projects" "Separate incremental from breakthrough teams" "Protect breakthrough projects"]
            :recovery-protocols ["When stuck in incremental mode, create skunkworks for 0 to 1 projects" "Separate teams/budgets" "Different success metrics"]
            :case-studies ["Kodak: Incremental improvements to film while digital emerged - invented digital camera but didn't pursue, bankrupt 2012"
                          "Apple: Breakthrough innovations (iPhone, iPad) not incremental - became most valuable company"
                          "Nokia: Incremental improvements to phones while smartphones emerged - market share collapsed"
                          "Amazon: Breakthrough innovations (AWS, Prime, Alexa) alongside incremental - $1.7T market cap"])
   (failure "best-practice-worship" "medium"
            "Following best practices instead of creating new practices"
            :signals ["'Industry best practices'" "Benchmarking competitors" "Copying successful companies" "MBA playbook" "Conventional wisdom"]
            :safeguards ["Question best practices" "Ask 'What if opposite is true?'" "Create your own practices" "Learn from different industries" "First principles thinking"]
            :recovery-protocols ["When best practices fail, return to first principles" "Look at what contrarians do" "Create new playbook"]
            :case-studies ["Southwest Airlines: Violated airline best practices (no hubs, no meals, one aircraft type) - most profitable airline"
                          "Netflix: Violated video rental best practices (no late fees, no stores) - destroyed Blockbuster, $250B+ valuation"
                          "Amazon: Violated retail best practices (no profits for years, customer obsession) - $1.7T market cap"
                          "Best practice followers: Usually mediocre results, rarely breakthrough success"])
   (failure "obvious-vs-impossible" "high"
            "Only pursuing obvious ideas or dismissing everything as impossible - missing secrets in between"
            :signals ["Only obvious ideas pursued" "Dismissing hard ideas as impossible" "No middle ground" "Binary thinking" "Missing contrarian opportunities"]
            :safeguards ["Look for ideas that are hard but possible" "Question 'impossible'" "Seek contrarian but achievable" "Validate with small experiments" "Talk to domain experts"]
            :recovery-protocols ["When missing opportunities, look in the 'hard but possible' zone" "Challenge assumptions about impossibility" "Run experiments"]
            :case-studies ["SpaceX: Reusable rockets seemed impossible, actually hard but possible - now standard"
                          "Airbnb: Strangers renting rooms seemed impossible, actually hard but possible - now $80B+ company"
                          "Bitcoin: Digital scarcity seemed impossible, actually hard but possible - now $1T+ market cap"
                          "Many 'impossible' things: Heavier-than-air flight, moon landing, internet, smartphones - all were secrets"])]
  :cross-model-interactions
  ["Combines with Day 1 Thinking: Customer obsession and high velocity enable 0 to 1 innovation"
   "Combines with Monopoly: 0 to 1 creates monopolies, 1 to n creates competition"
   "Combines with Radical Truth: Finding secrets requires radical truth about what's really true"
   "Countered by Efficiency: Optimizing existing things (1 to n) is easier than creating new things (0 to 1)"
   "Enhanced by First Principles: First principles thinking enables 0 to 1 by questioning assumptions"]})