(ns mental-models.data.models
  "Complete Mental Models Database - 129 models across 8 categories
   Each model includes description, example, failure modes, and related models")

;; -- Categories --------------------------------------------------------------

(def categories
  [{:id 1 :name "Cognitive Biases" :slug "cognitive-biases" :color "#dc2626" :icon "brain"}
   {:id 2 :name "Systems Thinking" :slug "systems-thinking" :color "#525252" :icon "network"}
   {:id 3 :name "Economics & Finance" :slug "economics-finance" :color "#525252" :icon "chart"}
   {:id 4 :name "Decision Making" :slug "decision-making" :color "#dc2626" :icon "target"}
   {:id 5 :name "Psychology" :slug "psychology" :color "#525252" :icon "user"}
   {:id 6 :name "Physics & Engineering" :slug "physics-engineering" :color "#525252" :icon "cog"}
   {:id 7 :name "Biology & Evolution" :slug "biology-evolution" :color "#525252" :icon "leaf"}
   {:id 8 :name "Mathematics & Logic" :slug "mathematics-logic" :color "#525252" :icon "calculator"}])

;; -- Mental Models -----------------------------------------------------------

(def mental-models
  [;; === COGNITIVE BIASES (1-20) ===
   {:id 1 :name "Confirmation Bias" :slug "confirmation-bias" :category-id 1
    :description "The tendency to search for, interpret, and recall information that confirms pre-existing beliefs."
    :example "An investor who believes a stock will rise only reads bullish analyst reports, ignoring bearish signals."
    :thinker "Peter Wason"
    :failure-modes [{:name "Echo Chamber" :description "Surrounding yourself only with agreeing voices" :risk-level "high"}
                    {:name "Cherry Picking" :description "Selecting only supporting evidence" :risk-level "high"}
                    {:name "Motivated Reasoning" :description "Rationalizing desired conclusions" :risk-level "medium"}]}
   
   {:id 2 :name "Availability Heuristic" :slug "availability-heuristic" :category-id 1
    :description "Judging probability by how easily examples come to mind, not actual frequency."
    :example "After seeing news about plane crashes, overestimating the danger of flying vs. driving."
    :thinker "Amos Tversky & Daniel Kahneman"
    :failure-modes [{:name "Recency Bias" :description "Overweighting recent events" :risk-level "high"}
                    {:name "Vividness Effect" :description "Memorable events seem more common" :risk-level "medium"}]}
   
   {:id 3 :name "Anchoring" :slug "anchoring" :category-id 1
    :description "Over-relying on the first piece of information encountered when making decisions."
    :example "A car salesman starts with a high price, making subsequent 'discounts' seem like great deals."
    :thinker "Amos Tversky & Daniel Kahneman"
    :failure-modes [{:name "Arbitrary Anchor" :description "Being influenced by irrelevant numbers" :risk-level "high"}
                    {:name "Insufficient Adjustment" :description "Not moving far enough from anchor" :risk-level "medium"}]}
   
   {:id 4 :name "Hindsight Bias" :slug "hindsight-bias" :category-id 1
    :description "The tendency to see past events as having been predictable, though they weren't."
    :example "After a market crash, saying 'I knew it would happen' despite no prior action."
    :thinker "Baruch Fischhoff"
    :failure-modes [{:name "Overconfidence" :description "Believing you can predict future events" :risk-level "high"}
                    {:name "Blame Misattribution" :description "Unfairly judging past decisions" :risk-level "medium"}]}
   
   {:id 5 :name "Survivorship Bias" :slug "survivorship-bias" :category-id 1
    :description "Focusing on successful examples while ignoring failures that are no longer visible."
    :example "Studying only successful entrepreneurs without examining failed businesses."
    :thinker "Abraham Wald"
    :failure-modes [{:name "False Pattern Recognition" :description "Finding patterns only in survivors" :risk-level "high"}
                    {:name "Overoptimism" :description "Underestimating failure rates" :risk-level "high"}]}
   
   {:id 6 :name "Dunning-Kruger Effect" :slug "dunning-kruger" :category-id 1
    :description "Unskilled individuals overestimate their ability; experts underestimate theirs."
    :example "A novice trader confident in beating the market; a veteran uncertain despite success."
    :thinker "David Dunning & Justin Kruger"
    :failure-modes [{:name "Overconfident Novice" :description "Taking excessive risks early" :risk-level "high"}
                    {:name "Imposter Syndrome" :description "Experts doubting valid expertise" :risk-level "low"}]}
   
   {:id 7 :name "Sunk Cost Fallacy" :slug "sunk-cost-fallacy" :category-id 1
    :description "Continuing an endeavor because of previously invested resources rather than future value."
    :example "Finishing a bad movie because you paid for the ticket."
    :thinker "Richard Thaler"
    :failure-modes [{:name "Escalation of Commitment" :description "Doubling down on losing positions" :risk-level "high"}
                    {:name "Emotional Attachment" :description "Can't let go of past investments" :risk-level "medium"}]}
   
   {:id 8 :name "Loss Aversion" :slug "loss-aversion" :category-id 1
    :description "Losses feel roughly twice as painful as equivalent gains feel good."
    :example "Refusing a 50/50 bet to win $110 or lose $100, despite positive expected value."
    :thinker "Daniel Kahneman & Amos Tversky"
    :failure-modes [{:name "Risk Aversion" :description "Avoiding beneficial risks" :risk-level "medium"}
                    {:name "Status Quo Bias" :description "Preferring current state to avoid loss" :risk-level "medium"}]}
   
   {:id 9 :name "Framing Effect" :slug "framing-effect" :category-id 1
    :description "Drawing different conclusions from the same information depending on how it's presented."
    :example "'90% fat-free' sounds better than '10% fat' despite being identical."
    :thinker "Amos Tversky & Daniel Kahneman"
    :failure-modes [{:name "Manipulation Vulnerability" :description "Being swayed by presentation" :risk-level "high"}
                    {:name "Inconsistent Decisions" :description "Different choices for same problem" :risk-level "medium"}]}
   
   {:id 10 :name "Bandwagon Effect" :slug "bandwagon-effect" :category-id 1
    :description "Adopting beliefs or behaviors because many others do the same."
    :example "Buying a stock because 'everyone is buying it' during a bubble."
    :thinker "Solomon Asch"
    :failure-modes [{:name "Herd Mentality" :description "Following crowds into bad decisions" :risk-level "high"}
                    {:name "Bubble Formation" :description "Contributing to market manias" :risk-level "high"}]}
   
   {:id 11 :name "Overconfidence Bias" :slug "overconfidence-bias" :category-id 1
    :description "Excessive confidence in one's own answers, predictions, or abilities."
    :example "90% of drivers believe they're above average - statistically impossible."
    :thinker "Sarah Lichtenstein"
    :failure-modes [{:name "Underpreparation" :description "Not planning for failure" :risk-level "high"}
                    {:name "Excessive Risk" :description "Taking on too much" :risk-level "high"}]}
   
   {:id 12 :name "Recency Bias" :slug "recency-bias" :category-id 1
    :description "Giving more weight to recent events than earlier ones."
    :example "Assuming recent market trends will continue indefinitely."
    :thinker "Hermann Ebbinghaus"
    :failure-modes [{:name "Trend Extrapolation" :description "Projecting recent past into future" :risk-level "high"}
                    {:name "Ignoring Base Rates" :description "Forgetting long-term averages" :risk-level "medium"}]}
   
   {:id 13 :name "Attribution Error" :slug "attribution-error" :category-id 1
    :description "Attributing others' behavior to character while attributing own behavior to circumstances."
    :example "Thinking a colleague is lazy (character) when late, but you're late due to traffic (circumstance)."
    :thinker "Lee Ross"
    :failure-modes [{:name "Unfair Judgment" :description "Judging others too harshly" :risk-level "medium"}
                    {:name "Self-Serving Bias" :description "Taking credit, deflecting blame" :risk-level "medium"}]}
   
   {:id 14 :name "Negativity Bias" :slug "negativity-bias" :category-id 1
    :description "Negative events have greater psychological impact than positive ones of equal magnitude."
    :example "One critical review affecting you more than ten positive ones."
    :thinker "Paul Rozin & Edward Royzman"
    :failure-modes [{:name "Pessimism" :description "Overweighting potential downsides" :risk-level "medium"}
                    {:name "Risk Paralysis" :description "Avoiding action due to fear" :risk-level "medium"}]}
   
   {:id 15 :name "Normalcy Bias" :slug "normalcy-bias" :category-id 1
    :description "Underestimating the possibility and impact of disasters or major changes."
    :example "Ignoring evacuation warnings because 'it's never happened before.'"
    :thinker "Various researchers"
    :failure-modes [{:name "Underpreparation" :description "Not preparing for rare events" :risk-level "high"}
                    {:name "Slow Response" :description "Delayed reaction to crises" :risk-level "high"}]}
   
   {:id 16 :name "Halo Effect" :slug "halo-effect" :category-id 1
    :description "Letting one positive trait influence perception of other unrelated traits."
    :example "Assuming an attractive person is also intelligent and kind."
    :thinker "Edward Thorndike"
    :failure-modes [{:name "Misjudgment" :description "Wrong assessment of capabilities" :risk-level "medium"}
                    {:name "Hiring Errors" :description "Selecting based on irrelevant traits" :risk-level "medium"}]}
   
   {:id 17 :name "Endowment Effect" :slug "endowment-effect" :category-id 1
    :description "Valuing things more highly simply because you own them."
    :example "Demanding more to sell a mug than you'd pay to buy the same mug."
    :thinker "Richard Thaler"
    :failure-modes [{:name "Overvaluation" :description "Holding assets too long" :risk-level "medium"}
                    {:name "Poor Trading" :description "Bad buy/sell decisions" :risk-level "medium"}]}
   
   {:id 18 :name "Projection Bias" :slug "projection-bias" :category-id 1
    :description "Assuming others share your beliefs, values, or current emotional state."
    :example "Assuming everyone finds your hobby as interesting as you do."
    :thinker "Various researchers"
    :failure-modes [{:name "Miscommunication" :description "Failing to explain adequately" :risk-level "medium"}
                    {:name "Market Misjudgment" :description "Wrong product-market assumptions" :risk-level "high"}]}
   
   {:id 19 :name "Optimism Bias" :slug "optimism-bias" :category-id 1
    :description "Believing you're less likely to experience negative events than others."
    :example "Smokers believing they're less likely to get cancer than other smokers."
    :thinker "Tali Sharot"
    :failure-modes [{:name "Underinsurance" :description "Not protecting against risks" :risk-level "high"}
                    {:name "Planning Fallacy" :description "Underestimating time/cost" :risk-level "high"}]}
   
   {:id 20 :name "Status Quo Bias" :slug "status-quo-bias" :category-id 1
    :description "Preference for the current state of affairs over change."
    :example "Staying with a suboptimal service provider because switching seems hard."
    :thinker "William Samuelson & Richard Zeckhauser"
    :failure-modes [{:name "Missed Opportunities" :description "Not pursuing better options" :risk-level "medium"}
                    {:name "Inertia" :description "Failing to adapt to change" :risk-level "high"}]}
   
   ;; === SYSTEMS THINKING (21-40) ===
   {:id 21 :name "Feedback Loops" :slug "feedback-loops" :category-id 2
    :description "Circular cause-and-effect where outputs become inputs, either amplifying or dampening."
    :example "Positive: compound interest. Negative: thermostat maintaining temperature."
    :thinker "Norbert Wiener"
    :failure-modes [{:name "Runaway Effects" :description "Positive loops spiraling out of control" :risk-level "high"}
                    {:name "Oscillation" :description "Overcorrection causing instability" :risk-level "medium"}]}
   
   {:id 22 :name "Second-Order Thinking" :slug "second-order-thinking" :category-id 2
    :description "Considering the consequences of consequences, not just immediate effects."
    :example "Rent control (1st: lower rents) leads to housing shortages (2nd order)."
    :thinker "Howard Marks"
    :failure-modes [{:name "Analysis Paralysis" :description "Overthinking to inaction" :risk-level "medium"}
                    {:name "Incomplete Chain" :description "Stopping analysis too early" :risk-level "high"}]}
   
   {:id 23 :name "Emergence" :slug "emergence" :category-id 2
    :description "Complex patterns arising from simple rules; the whole is greater than the sum of parts."
    :example "Consciousness emerging from neurons; traffic jams from individual drivers."
    :thinker "John Holland"
    :failure-modes [{:name "Reductionism" :description "Missing emergent properties" :risk-level "medium"}
                    {:name "Unpredictability" :description "Can't predict emergent behavior" :risk-level "medium"}]}
   
   {:id 24 :name "Bottlenecks" :slug "bottlenecks" :category-id 2
    :description "The constraint that limits the throughput of an entire system."
    :example "A slow approval process limiting how fast a company can ship products."
    :thinker "Eliyahu Goldratt"
    :failure-modes [{:name "Wrong Focus" :description "Optimizing non-bottlenecks" :risk-level "high"}
                    {:name "Shifting Bottleneck" :description "New constraint after fixing old" :risk-level "medium"}]}
   
   {:id 25 :name "Leverage Points" :slug "leverage-points" :category-id 2
    :description "Places in a system where small changes can produce large effects."
    :example "Changing the goal of a system has more impact than changing parameters."
    :thinker "Donella Meadows"
    :failure-modes [{:name "Wrong Lever" :description "Pushing ineffective points" :risk-level "medium"}
                    {:name "Unintended Consequences" :description "Leverage causing harm" :risk-level "high"}]}
   
   {:id 26 :name "Network Effects" :slug "network-effects" :category-id 2
    :description "A product becomes more valuable as more people use it."
    :example "Telephones, social networks, marketplaces - value grows with users."
    :thinker "Robert Metcalfe"
    :failure-modes [{:name "Winner-Take-All" :description "Market concentration" :risk-level "medium"}
                    {:name "Lock-In" :description "Users trapped in inferior networks" :risk-level "medium"}]}
   
   {:id 27 :name "Redundancy" :slug "redundancy" :category-id 2
    :description "Having backup systems or excess capacity to handle failures or surges."
    :example "Airplanes with multiple engines; data centers with backup power."
    :thinker "Various engineers"
    :failure-modes [{:name "Cost Overhead" :description "Expensive to maintain backups" :risk-level "low"}
                    {:name "False Security" :description "Redundancy failing simultaneously" :risk-level "medium"}]}
   
   {:id 28 :name "Homeostasis" :slug "homeostasis" :category-id 2
    :description "Systems tend to maintain stability and resist change from equilibrium."
    :example "Body temperature regulation; organizational resistance to change."
    :thinker "Walter Cannon"
    :failure-modes [{:name "Change Resistance" :description "Difficulty implementing improvements" :risk-level "medium"}
                    {:name "Equilibrium Trap" :description "Stuck in suboptimal state" :risk-level "medium"}]}
   
   {:id 29 :name "Complexity" :slug "complexity" :category-id 2
    :description "Systems with many interacting parts that create unpredictable behavior."
    :example "Weather systems, economies, ecosystems - too complex for simple prediction."
    :thinker "Herbert Simon"
    :failure-modes [{:name "Oversimplification" :description "Missing crucial interactions" :risk-level "high"}
                    {:name "Unpredictability" :description "Can't forecast complex systems" :risk-level "medium"}]}
   
   {:id 30 :name "Nonlinearity" :slug "nonlinearity" :category-id 2
    :description "Outputs not proportional to inputs; small changes can have large effects."
    :example "Tipping points in climate; viral content spreading exponentially."
    :thinker "Edward Lorenz"
    :failure-modes [{:name "Linear Thinking" :description "Assuming proportional responses" :risk-level "high"}
                    {:name "Threshold Blindness" :description "Missing tipping points" :risk-level "high"}]}
   
   {:id 31 :name "Resilience" :slug "resilience" :category-id 2
    :description "A system's ability to absorb disturbance and reorganize while retaining function."
    :example "Diverse ecosystems recovering from fires; companies surviving market crashes."
    :thinker "C.S. Holling"
    :failure-modes [{:name "Brittleness" :description "Optimizing away resilience" :risk-level "high"}
                    {:name "False Stability" :description "Appearing stable until collapse" :risk-level "high"}]}
   
   {:id 32 :name "Stocks and Flows" :slug "stocks-and-flows" :category-id 2
    :description "Accumulations (stocks) change through rates of change (flows)."
    :example "Bank balance (stock) changes through income and expenses (flows)."
    :thinker "Jay Forrester"
    :failure-modes [{:name "Flow Confusion" :description "Confusing stocks with flows" :risk-level "medium"}
                    {:name "Delay Blindness" :description "Ignoring accumulation delays" :risk-level "medium"}]}
   
   {:id 33 :name "Delays" :slug "delays" :category-id 2
    :description "Time gaps between actions and their effects in systems."
    :example "Monetary policy takes 12-18 months to affect inflation."
    :thinker "Jay Forrester"
    :failure-modes [{:name "Overcorrection" :description "Adjusting before seeing effects" :risk-level "high"}
                    {:name "Impatience" :description "Abandoning strategies too early" :risk-level "medium"}]}
   
   {:id 34 :name "Scaling" :slug "scaling" :category-id 2
    :description "How properties change as systems grow or shrink in size."
    :example "Surface area grows slower than volume; large animals need proportionally thicker bones."
    :thinker "Galileo Galilei"
    :failure-modes [{:name "Scale Blindness" :description "Assuming linear scaling" :risk-level "high"}
                    {:name "Size Limits" :description "Missing natural size constraints" :risk-level "medium"}]}
   
   {:id 35 :name "Pareto Principle" :slug "pareto-principle" :category-id 2
    :description "Roughly 80% of effects come from 20% of causes."
    :example "80% of revenue from 20% of customers; 80% of bugs from 20% of code."
    :thinker "Vilfredo Pareto"
    :failure-modes [{:name "Neglecting the 80%" :description "Ignoring long tail entirely" :risk-level "medium"}
                    {:name "Misapplication" :description "Forcing 80/20 where it doesn't apply" :risk-level "low"}]}
   
   {:id 36 :name "Critical Mass" :slug "critical-mass" :category-id 2
    :description "The minimum amount needed to sustain a reaction or achieve viability."
    :example "Nuclear chain reactions; social movements reaching tipping point."
    :thinker "Various physicists"
    :failure-modes [{:name "Premature Scaling" :description "Growing before critical mass" :risk-level "high"}
                    {:name "Threshold Misjudgment" :description "Wrong estimate of critical mass" :risk-level "medium"}]}
   
   {:id 37 :name "Path Dependence" :slug "path-dependence" :category-id 2
    :description "Current options are constrained by past decisions and historical accidents."
    :example "QWERTY keyboard layout persists despite better alternatives."
    :thinker "Paul David"
    :failure-modes [{:name "Lock-In" :description "Trapped by historical choices" :risk-level "medium"}
                    {:name "Ignoring History" :description "Not understanding why things are as they are" :risk-level "low"}]}
   
   {:id 38 :name "Hysteresis" :slug "hysteresis" :category-id 2
    :description "A system's state depends on its history, not just current inputs."
    :example "Unemployment remaining high even after economy recovers."
    :thinker "James Alfred Ewing"
    :failure-modes [{:name "Reversibility Assumption" :description "Assuming changes can be undone" :risk-level "medium"}
                    {:name "History Blindness" :description "Ignoring path effects" :risk-level "medium"}]}
   
   {:id 39 :name "Antifragility" :slug "antifragility" :category-id 2
    :description "Systems that gain from disorder, stress, and volatility."
    :example "Muscles grow stronger from stress; some businesses thrive in chaos."
    :thinker "Nassim Taleb"
    :failure-modes [{:name "Fragility Disguised" :description "Appearing robust until breaking" :risk-level "high"}
                    {:name "Excessive Stress" :description "Too much disorder destroys" :risk-level "medium"}]}
   
   {:id 40 :name "Chesterton's Fence" :slug "chestertons-fence" :category-id 2
    :description "Don't remove something until you understand why it was put there."
    :example "Before eliminating a regulation, understand the problem it was solving."
    :thinker "G.K. Chesterton"
    :failure-modes [{:name "Paralysis" :description "Never changing anything" :risk-level "low"}
                    {:name "Hidden Dependencies" :description "Missing non-obvious purposes" :risk-level "medium"}]}
   
   ;; === ECONOMICS & FINANCE (41-60) ===
   {:id 41 :name "Opportunity Cost" :slug "opportunity-cost" :category-id 3
    :description "The value of the next best alternative foregone when making a choice."
    :example "Time spent on Project A is time not spent on potentially more valuable Project B."
    :thinker "Friedrich von Wieser"
    :failure-modes [{:name "Ignoring Alternatives" :description "Not considering what's given up" :risk-level "high"}
                    {:name "Sunk Cost Confusion" :description "Mixing with past costs" :risk-level "medium"}]}
   
   {:id 42 :name "Margin of Safety" :slug "margin-of-safety" :category-id 3
    :description "Building in a buffer between estimated value and price paid."
    :example "Only buying stocks at 30% below intrinsic value to account for estimation errors."
    :thinker "Benjamin Graham"
    :failure-modes [{:name "Excessive Caution" :description "Missing good opportunities" :risk-level "low"}
                    {:name "False Precision" :description "Overconfident value estimates" :risk-level "high"}]}
   
   {:id 43 :name "Compound Interest" :slug "compound-interest" :category-id 3
    :description "Interest earning interest; exponential growth over time."
    :example "10% annual returns double money in ~7 years; quadruple in ~14 years."
    :thinker "Albert Einstein (attributed)"
    :failure-modes [{:name "Impatience" :description "Not allowing time for compounding" :risk-level "medium"}
                    {:name "Negative Compounding" :description "Debt spiraling out of control" :risk-level "high"}]}
   
   {:id 44 :name "Supply and Demand" :slug "supply-and-demand" :category-id 3
    :description "Prices adjust to balance the quantity supplied with quantity demanded."
    :example "Concert ticket prices rise when demand exceeds venue capacity."
    :thinker "Alfred Marshall"
    :failure-modes [{:name "Price Controls" :description "Distorting natural equilibrium" :risk-level "medium"}
                    {:name "Elasticity Blindness" :description "Ignoring sensitivity to price" :risk-level "medium"}]}
   
   {:id 45 :name "Incentives" :slug "incentives" :category-id 3
    :description "People respond to rewards and punishments; behavior follows incentives."
    :example "Sales commissions drive behavior; what gets measured gets managed."
    :thinker "Charlie Munger"
    :failure-modes [{:name "Perverse Incentives" :description "Rewarding wrong behavior" :risk-level "high"}
                    {:name "Gaming" :description "Optimizing metrics, not outcomes" :risk-level "high"}]}
   
   {:id 46 :name "Comparative Advantage" :slug "comparative-advantage" :category-id 3
    :description "Specializing in what you do relatively better, even if not absolutely best."
    :example "A lawyer who types faster than their secretary should still delegate typing."
    :thinker "David Ricardo"
    :failure-modes [{:name "Absolute Thinking" :description "Only doing what you're best at" :risk-level "medium"}
                    {:name "Over-Specialization" :description "Becoming too narrow" :risk-level "medium"}]}
   
   {:id 47 :name "Diminishing Returns" :slug "diminishing-returns" :category-id 3
    :description "Each additional unit of input produces less additional output."
    :example "First employee doubles output; 100th employee adds marginally."
    :thinker "David Ricardo"
    :failure-modes [{:name "Over-Investment" :description "Continuing past optimal point" :risk-level "medium"}
                    {:name "Linear Projection" :description "Assuming constant returns" :risk-level "medium"}]}
   
   {:id 48 :name "Economies of Scale" :slug "economies-of-scale" :category-id 3
    :description "Cost per unit decreases as production volume increases."
    :example "Manufacturing costs drop as fixed costs spread over more units."
    :thinker "Adam Smith"
    :failure-modes [{:name "Diseconomies" :description "Growing too large, losing efficiency" :risk-level "medium"}
                    {:name "Scale Obsession" :description "Pursuing size over profitability" :risk-level "medium"}]}
   
   {:id 49 :name "Creative Destruction" :slug "creative-destruction" :category-id 3
    :description "Innovation destroys old industries while creating new ones."
    :example "Digital cameras destroying film; streaming destroying video rental."
    :thinker "Joseph Schumpeter"
    :failure-modes [{:name "Disruption Blindness" :description "Not seeing threats until too late" :risk-level "high"}
                    {:name "Nostalgia" :description "Protecting obsolete industries" :risk-level "medium"}]}
   
   {:id 50 :name "Moral Hazard" :slug "moral-hazard" :category-id 3
    :description "Taking more risk when protected from consequences."
    :example "Banks taking excessive risks knowing government will bail them out."
    :thinker "Kenneth Arrow"
    :failure-modes [{:name "Risk Shifting" :description "Transferring risk to others" :risk-level "high"}
                    {:name "Bailout Expectation" :description "Assuming rescue will come" :risk-level "high"}]}
   
   {:id 51 :name "Adverse Selection" :slug "adverse-selection" :category-id 3
    :description "Information asymmetry causing bad actors to dominate markets."
    :example "Used car market: sellers know defects, buyers don't - lemons dominate."
    :thinker "George Akerlof"
    :failure-modes [{:name "Market Collapse" :description "Good products driven out" :risk-level "high"}
                    {:name "Information Hiding" :description "Incentive to conceal problems" :risk-level "medium"}]}
   
   {:id 52 :name "Tragedy of the Commons" :slug "tragedy-of-commons" :category-id 3
    :description "Shared resources depleted when individuals act in self-interest."
    :example "Overfishing oceans; overgrazing shared pastures."
    :thinker "Garrett Hardin"
    :failure-modes [{:name "Free Rider Problem" :description "Benefiting without contributing" :risk-level "high"}
                    {:name "Resource Depletion" :description "Exhausting shared resources" :risk-level "high"}]}
   
   {:id 53 :name "Asymmetric Information" :slug "asymmetric-information" :category-id 3
    :description "One party in a transaction has more relevant information than the other."
    :example "Doctors know more than patients; managers know more than shareholders."
    :thinker "George Akerlof"
    :failure-modes [{:name "Exploitation" :description "Informed party taking advantage" :risk-level "high"}
                    {:name "Trust Breakdown" :description "Markets failing due to suspicion" :risk-level "medium"}]}
   
   {:id 54 :name "Principal-Agent Problem" :slug "principal-agent" :category-id 3
    :description "Agents may not act in the best interest of principals who hired them."
    :example "CEOs prioritizing personal gain over shareholder value."
    :thinker "Michael Jensen & William Meckling"
    :failure-modes [{:name "Misaligned Incentives" :description "Agent goals differ from principal" :risk-level "high"}
                    {:name "Monitoring Costs" :description "Expensive to verify agent behavior" :risk-level "medium"}]}
   
   {:id 55 :name "Arbitrage" :slug "arbitrage" :category-id 3
    :description "Profiting from price differences of identical assets in different markets."
    :example "Buying gold in London, selling in New York when prices differ."
    :thinker "Various traders"
    :failure-modes [{:name "Execution Risk" :description "Prices moving before completing" :risk-level "medium"}
                    {:name "Hidden Costs" :description "Transaction costs eliminating profit" :risk-level "medium"}]}
   
   {:id 56 :name "Mean Reversion" :slug "mean-reversion" :category-id 3
    :description "Extreme values tend to return toward the average over time."
    :example "Unusually high stock returns often followed by below-average returns."
    :thinker "Francis Galton"
    :failure-modes [{:name "Timing Error" :description "Reversion takes longer than expected" :risk-level "high"}
                    {:name "Regime Change" :description "Mean itself shifting" :risk-level "medium"}]}
   
   {:id 57 :name "Optionality" :slug "optionality" :category-id 3
    :description "Having choices without obligation; asymmetric upside with limited downside."
    :example "Venture capital: most investments fail, but winners pay for all losses."
    :thinker "Nassim Taleb"
    :failure-modes [{:name "Option Decay" :description "Options losing value over time" :risk-level "medium"}
                    {:name "Paralysis" :description "Keeping options open too long" :risk-level "medium"}]}
   
   {:id 58 :name "Reflexivity" :slug "reflexivity" :category-id 3
    :description "Beliefs affect reality, which then affects beliefs in a feedback loop."
    :example "If investors believe a bank will fail, they withdraw funds, causing failure."
    :thinker "George Soros"
    :failure-modes [{:name "Self-Fulfilling Prophecy" :description "Beliefs creating reality" :risk-level "high"}
                    {:name "Bubble Formation" :description "Optimism feeding more optimism" :risk-level "high"}]}
   
   {:id 59 :name "Gresham's Law" :slug "greshams-law" :category-id 3
    :description "Bad money drives out good when both circulate at same face value."
    :example "People hoard gold coins and spend debased coins."
    :thinker "Thomas Gresham"
    :failure-modes [{:name "Quality Degradation" :description "Good products driven from market" :risk-level "medium"}
                    {:name "Race to Bottom" :description "Competition on price, not quality" :risk-level "medium"}]}
   
   {:id 60 :name "Regression to the Mean" :slug "regression-to-mean" :category-id 3
    :description "Extreme observations tend to be followed by more moderate ones."
    :example "Sports Illustrated cover jinx - peak performance followed by average."
    :thinker "Francis Galton"
    :failure-modes [{:name "Skill Attribution" :description "Attributing luck to skill" :risk-level "medium"}
                    {:name "Intervention Illusion" :description "Crediting treatment for natural regression" :risk-level "medium"}]}
   
   ;; === DECISION MAKING (61-80) ===
   {:id 61 :name "First Principles Thinking" :slug "first-principles" :category-id 4
    :description "Breaking problems down to fundamental truths and reasoning up from there."
    :example "Elon Musk calculating battery costs from raw materials, not market prices."
    :thinker "Aristotle / Elon Musk"
    :failure-modes [{:name "Over-Analysis" :description "Spending too long on fundamentals" :risk-level "low"}
                    {:name "Wrong Principles" :description "Starting from flawed assumptions" :risk-level "high"}]}
   
   {:id 62 :name "Inversion" :slug "inversion" :category-id 4
    :description "Solving problems by considering the opposite - what to avoid, not just what to do."
    :example "Instead of 'how to succeed,' ask 'how would I guarantee failure?' and avoid that."
    :thinker "Carl Jacobi / Charlie Munger"
    :failure-modes [{:name "Negativity Spiral" :description "Focusing only on problems" :risk-level "low"}
                    {:name "Incomplete Inversion" :description "Missing some failure modes" :risk-level "medium"}]}
   
   {:id 63 :name "Circle of Competence" :slug "circle-of-competence" :category-id 4
    :description "Knowing the boundaries of your expertise and staying within them."
    :example "Warren Buffett avoiding tech stocks he doesn't understand."
    :thinker "Warren Buffett"
    :failure-modes [{:name "Too Narrow" :description "Never expanding competence" :risk-level "medium"}
                    {:name "Overestimation" :description "Thinking circle is larger than it is" :risk-level "high"}]}
   
   {:id 64 :name "Occam's Razor" :slug "occams-razor" :category-id 4
    :description "The simplest explanation is usually correct; don't multiply entities unnecessarily."
    :example "Hearing hoofbeats? Think horses, not zebras."
    :thinker "William of Ockham"
    :failure-modes [{:name "Oversimplification" :description "Missing genuine complexity" :risk-level "medium"}
                    {:name "Bias Toward Simple" :description "Rejecting complex but true explanations" :risk-level "medium"}]}
   
   {:id 65 :name "Hanlon's Razor" :slug "hanlons-razor" :category-id 4
    :description "Never attribute to malice what can be explained by incompetence or ignorance."
    :example "A colleague missing a deadline is more likely overwhelmed than sabotaging you."
    :thinker "Robert Hanlon"
    :failure-modes [{:name "Naivety" :description "Missing actual malice" :risk-level "medium"}
                    {:name "Excuse Making" :description "Tolerating repeated incompetence" :risk-level "medium"}]}
   
   {:id 66 :name "Reversibility" :slug "reversibility" :category-id 4
    :description "Preferring decisions that can be undone over irreversible ones."
    :example "Renting before buying; dating before marriage."
    :thinker "Jeff Bezos"
    :failure-modes [{:name "Commitment Avoidance" :description "Never making permanent choices" :risk-level "medium"}
                    {:name "False Reversibility" :description "Thinking something is reversible when it's not" :risk-level "high"}]}
   
   {:id 67 :name "Expected Value" :slug "expected-value" :category-id 4
    :description "Probability-weighted average of all possible outcomes."
    :example "A 10% chance of $1M is worth $100K in expected value."
    :thinker "Blaise Pascal"
    :failure-modes [{:name "Ignoring Variance" :description "Only looking at average" :risk-level "medium"}
                    {:name "Wrong Probabilities" :description "Miscalculating likelihoods" :risk-level "high"}]}
   
   {:id 68 :name "Bayesian Thinking" :slug "bayesian-thinking" :category-id 4
    :description "Updating beliefs based on new evidence, weighted by prior probability."
    :example "A positive medical test is less alarming for a rare disease than a common one."
    :thinker "Thomas Bayes"
    :failure-modes [{:name "Base Rate Neglect" :description "Ignoring prior probabilities" :risk-level "high"}
                    {:name "Anchoring on Priors" :description "Not updating enough with evidence" :risk-level "medium"}]}
   
   {:id 69 :name "Satisficing" :slug "satisficing" :category-id 4
    :description "Choosing an option that meets minimum criteria rather than optimizing."
    :example "Hiring the first candidate who meets requirements vs. interviewing everyone."
    :thinker "Herbert Simon"
    :failure-modes [{:name "Settling" :description "Accepting mediocre when great is available" :risk-level "medium"}
                    {:name "Threshold Error" :description "Setting criteria too low or high" :risk-level "medium"}]}
   
   {:id 70 :name "Regret Minimization" :slug "regret-minimization" :category-id 4
    :description "Making decisions by imagining which choice you'd regret least in the future."
    :example "Jeff Bezos leaving Wall Street for Amazon - would regret not trying."
    :thinker "Jeff Bezos"
    :failure-modes [{:name "Risk Aversion" :description "Avoiding all regret-possible actions" :risk-level "medium"}
                    {:name "Hindsight Contamination" :description "Judging by outcomes, not process" :risk-level "medium"}]}
   
   {:id 71 :name "Pre-Mortem" :slug "pre-mortem" :category-id 4
    :description "Imagining a project has failed and working backward to identify causes."
    :example "Before launching, ask 'It's 6 months later and this failed. Why?'"
    :thinker "Gary Klein"
    :failure-modes [{:name "Pessimism Spiral" :description "Finding too many failure modes" :risk-level "low"}
                    {:name "Incomplete Analysis" :description "Missing key failure modes" :risk-level "medium"}]}
   
   {:id 72 :name "Red Team" :slug "red-team" :category-id 4
    :description "Assigning a group to challenge plans and find weaknesses."
    :example "Military war games; security penetration testing."
    :thinker "Military tradition"
    :failure-modes [{:name "Groupthink" :description "Red team not truly independent" :risk-level "medium"}
                    {:name "Adversarial Culture" :description "Creating conflict, not insight" :risk-level "medium"}]}
   
   {:id 73 :name "Decision Trees" :slug "decision-trees" :category-id 4
    :description "Mapping out choices and their consequences in a branching structure."
    :example "If X happens, do A; if Y happens, do B - with probabilities and values."
    :thinker "Howard Raiffa"
    :failure-modes [{:name "Complexity Explosion" :description "Too many branches to analyze" :risk-level "medium"}
                    {:name "False Precision" :description "Overconfident probability estimates" :risk-level "medium"}]}
   
   {:id 74 :name "Scenario Planning" :slug "scenario-planning" :category-id 4
    :description "Developing multiple plausible futures and preparing for each."
    :example "Shell Oil's scenarios for oil prices and geopolitics."
    :thinker "Herman Kahn / Pierre Wack"
    :failure-modes [{:name "Scenario Fixation" :description "Believing one scenario is certain" :risk-level "medium"}
                    {:name "Missing Scenarios" :description "Not imagining what actually happens" :risk-level "high"}]}
   
   {:id 75 :name "Margin of Error" :slug "margin-of-error" :category-id 4
    :description "Acknowledging uncertainty in estimates and building in buffers."
    :example "Project estimated at 3 months? Plan for 4-5 months."
    :thinker "Various statisticians"
    :failure-modes [{:name "Excessive Buffer" :description "Too much padding wastes resources" :risk-level "low"}
                    {:name "Buffer Erosion" :description "Margin gets consumed by scope creep" :risk-level "medium"}]}
   
   {:id 76 :name "Falsifiability" :slug "falsifiability" :category-id 4
    :description "A claim must be testable and potentially provable wrong to be meaningful."
    :example "'The market will go up or down' is unfalsifiable and useless."
    :thinker "Karl Popper"
    :failure-modes [{:name "Unfalsifiable Beliefs" :description "Holding untestable convictions" :risk-level "medium"}
                    {:name "Moving Goalposts" :description "Redefining claims when disproven" :risk-level "medium"}]}
   
   {:id 77 :name "Probabilistic Thinking" :slug "probabilistic-thinking" :category-id 4
    :description "Thinking in probabilities rather than certainties."
    :example "Not 'will it rain?' but 'what's the probability of rain?'"
    :thinker "Various statisticians"
    :failure-modes [{:name "Overconfidence" :description "Assigning too-high probabilities" :risk-level "high"}
                    {:name "Binary Thinking" :description "Reverting to yes/no" :risk-level "medium"}]}
   
   {:id 78 :name "Thought Experiments" :slug "thought-experiments" :category-id 4
    :description "Using imagination to explore concepts and test theories."
    :example "Einstein's elevator thought experiment for general relativity."
    :thinker "Albert Einstein"
    :failure-modes [{:name "Unrealistic Assumptions" :description "Thought experiment too abstract" :risk-level "medium"}
                    {:name "Confirmation Bias" :description "Designing experiments to confirm beliefs" :risk-level "medium"}]}
   
   {:id 79 :name "Fermi Estimation" :slug "fermi-estimation" :category-id 4
    :description "Making rough calculations with limited data using logical assumptions."
    :example "How many piano tuners in Chicago? Break into population, pianos per capita, tunings per year."
    :thinker "Enrico Fermi"
    :failure-modes [{:name "Compounding Errors" :description "Small errors multiplying" :risk-level "medium"}
                    {:name "Wrong Assumptions" :description "Starting with flawed premises" :risk-level "medium"}]}
   
   {:id 80 :name "Skin in the Game" :slug "skin-in-the-game" :category-id 4
    :description "Having personal stake in outcomes; bearing consequences of decisions."
    :example "Surgeons who would undergo their own recommended procedures."
    :thinker "Nassim Taleb"
    :failure-modes [{:name "Risk Aversion" :description "Too cautious when personally exposed" :risk-level "medium"}
                    {:name "Conflict of Interest" :description "Personal stake distorting judgment" :risk-level "medium"}]}
   
   ;; === PSYCHOLOGY (81-100) ===
   {:id 81 :name "Social Proof" :slug "social-proof" :category-id 5
    :description "Looking to others' behavior to determine correct action in uncertain situations."
    :example "Choosing a busy restaurant over an empty one."
    :thinker "Robert Cialdini"
    :failure-modes [{:name "Herd Behavior" :description "Following crowds into mistakes" :risk-level "high"}
                    {:name "Manipulation" :description "Fake social proof deceiving" :risk-level "medium"}]}
   
   {:id 82 :name "Reciprocity" :slug "reciprocity" :category-id 5
    :description "Feeling obligated to return favors and match others' behavior."
    :example "Free samples creating obligation to buy; favors expecting return."
    :thinker "Robert Cialdini"
    :failure-modes [{:name "Manipulation" :description "Being exploited through small gifts" :risk-level "medium"}
                    {:name "Obligation Trap" :description "Feeling indebted inappropriately" :risk-level "medium"}]}
   
   {:id 83 :name "Commitment and Consistency" :slug "commitment-consistency" :category-id 5
    :description "Once committed to something, people tend to remain consistent with that commitment."
    :example "Foot-in-the-door technique: small yes leads to bigger yes."
    :thinker "Robert Cialdini"
    :failure-modes [{:name "Escalation" :description "Continuing bad commitments" :risk-level "high"}
                    {:name "Identity Lock-In" :description "Can't change due to past statements" :risk-level "medium"}]}
   
   {:id 84 :name "Liking" :slug "liking" :category-id 5
    :description "People are more easily influenced by those they like."
    :example "Buying from friends; attractive salespeople selling more."
    :thinker "Robert Cialdini"
    :failure-modes [{:name "Bias" :description "Favoring likeable over competent" :risk-level "medium"}
                    {:name "Manipulation" :description "Manufactured likability deceiving" :risk-level "medium"}]}
   
   {:id 85 :name "Authority" :slug "authority" :category-id 5
    :description "Tendency to comply with authority figures, even when inappropriate."
    :example "Milgram experiment: people shocking others when told by authority."
    :thinker "Stanley Milgram"
    :failure-modes [{:name "Blind Obedience" :description "Following harmful orders" :risk-level "high"}
                    {:name "False Authority" :description "Deferring to fake experts" :risk-level "high"}]}
   
   {:id 86 :name "Scarcity" :slug "scarcity" :category-id 5
    :description "Perceiving things as more valuable when they're rare or diminishing."
    :example "'Limited time offer' and 'Only 3 left' driving purchases."
    :thinker "Robert Cialdini"
    :failure-modes [{:name "Artificial Scarcity" :description "Manufactured urgency manipulating" :risk-level "medium"}
                    {:name "Panic Buying" :description "Hoarding due to perceived scarcity" :risk-level "medium"}]}
   
   {:id 87 :name "Narrative Fallacy" :slug "narrative-fallacy" :category-id 5
    :description "Creating coherent stories from random events; seeing patterns in noise."
    :example "Explaining stock movements with post-hoc narratives."
    :thinker "Nassim Taleb"
    :failure-modes [{:name "False Causation" :description "Seeing cause where there's coincidence" :risk-level "high"}
                    {:name "Overconfidence" :description "Stories making us feel we understand" :risk-level "medium"}]}
   
   {:id 88 :name "Cognitive Dissonance" :slug "cognitive-dissonance" :category-id 5
    :description "Mental discomfort from holding contradictory beliefs, leading to rationalization."
    :example "Smokers minimizing health risks to reduce dissonance."
    :thinker "Leon Festinger"
    :failure-modes [{:name "Rationalization" :description "Justifying bad decisions" :risk-level "high"}
                    {:name "Belief Persistence" :description "Holding wrong beliefs despite evidence" :risk-level "high"}]}
   
   {:id 89 :name "Hedonic Adaptation" :slug "hedonic-adaptation" :category-id 5
    :description "Returning to baseline happiness regardless of positive or negative events."
    :example "Lottery winners returning to normal happiness levels within months."
    :thinker "Philip Brickman"
    :failure-modes [{:name "Hedonic Treadmill" :description "Chasing happiness that fades" :risk-level "medium"}
                    {:name "Undervaluing Present" :description "Thinking future will be better" :risk-level "medium"}]}
   
   {:id 90 :name "Mere Exposure Effect" :slug "mere-exposure" :category-id 5
    :description "Developing preference for things simply because they're familiar."
    :example "Liking songs more after hearing them repeatedly."
    :thinker "Robert Zajonc"
    :failure-modes [{:name "Familiarity Bias" :description "Preferring known over better" :risk-level "medium"}
                    {:name "Marketing Manipulation" :description "Repeated exposure creating false preference" :risk-level "medium"}]}
   
   {:id 91 :name "In-Group Bias" :slug "in-group-bias" :category-id 5
    :description "Favoring members of your own group over outsiders."
    :example "Hiring from your alma mater; favoring your department."
    :thinker "Henri Tajfel"
    :failure-modes [{:name "Discrimination" :description "Unfair treatment of out-groups" :risk-level "high"}
                    {:name "Echo Chambers" :description "Only hearing similar views" :risk-level "medium"}]}
   
   {:id 92 :name "Reactance" :slug "reactance" :category-id 5
    :description "Doing the opposite when feeling freedom is threatened."
    :example "Teenagers rebelling against parental restrictions."
    :thinker "Jack Brehm"
    :failure-modes [{:name "Counterproductive Rebellion" :description "Opposing good advice" :risk-level "medium"}
                    {:name "Manipulation" :description "Reverse psychology exploiting reactance" :risk-level "low"}]}
   
   {:id 93 :name "Peak-End Rule" :slug "peak-end-rule" :category-id 5
    :description "Judging experiences by their peak intensity and ending, not average."
    :example "A vacation remembered by its best moment and final day."
    :thinker "Daniel Kahneman"
    :failure-modes [{:name "Duration Neglect" :description "Ignoring how long something lasted" :risk-level "medium"}
                    {:name "Ending Manipulation" :description "Good endings masking bad experiences" :risk-level "medium"}]}
   
   {:id 94 :name "Spotlight Effect" :slug "spotlight-effect" :category-id 5
    :description "Overestimating how much others notice your appearance or behavior."
    :example "Thinking everyone noticed your minor mistake in a presentation."
    :thinker "Thomas Gilovich"
    :failure-modes [{:name "Self-Consciousness" :description "Excessive worry about perception" :risk-level "low"}
                    {:name "Inaction" :description "Not acting due to imagined scrutiny" :risk-level "medium"}]}
   
   {:id 95 :name "Illusion of Control" :slug "illusion-of-control" :category-id 5
    :description "Believing you have more control over outcomes than you actually do."
    :example "Gamblers thinking they can influence dice rolls."
    :thinker "Ellen Langer"
    :failure-modes [{:name "Overconfidence" :description "Taking excessive risks" :risk-level "high"}
                    {:name "Blame" :description "Self-blame for uncontrollable outcomes" :risk-level "medium"}]}
   
   {:id 96 :name "Curse of Knowledge" :slug "curse-of-knowledge" :category-id 5
    :description "Difficulty imagining what it's like to not know something you know."
    :example "Experts unable to explain concepts simply to beginners."
    :thinker "Robin Hogarth"
    :failure-modes [{:name "Poor Communication" :description "Failing to explain clearly" :risk-level "medium"}
                    {:name "Assumption of Knowledge" :description "Expecting others to know what you know" :risk-level "medium"}]}
   
   {:id 97 :name "Fundamental Attribution Error" :slug "fundamental-attribution" :category-id 5
    :description "Attributing others' behavior to character while attributing own to circumstances."
    :example "They're late because they're irresponsible; I'm late because of traffic."
    :thinker "Lee Ross"
    :failure-modes [{:name "Unfair Judgment" :description "Judging others too harshly" :risk-level "medium"}
                    {:name "Self-Serving Bias" :description "Excusing own behavior" :risk-level "medium"}]}
   
   {:id 98 :name "Groupthink" :slug "groupthink" :category-id 5
    :description "Desire for harmony causing irrational or dysfunctional decision-making."
    :example "Bay of Pigs invasion - advisors not voicing concerns."
    :thinker "Irving Janis"
    :failure-modes [{:name "Suppressed Dissent" :description "Valuable objections silenced" :risk-level "high"}
                    {:name "False Consensus" :description "Assuming everyone agrees" :risk-level "high"}]}
   
   {:id 99 :name "Self-Serving Bias" :slug "self-serving-bias" :category-id 5
    :description "Attributing success to skill and failure to external factors."
    :example "Taking credit for wins, blaming losses on bad luck."
    :thinker "Dale Miller"
    :failure-modes [{:name "No Learning" :description "Not learning from failures" :risk-level "high"}
                    {:name "Overconfidence" :description "Inflated sense of ability" :risk-level "medium"}]}
   
   {:id 100 :name "Affect Heuristic" :slug "affect-heuristic" :category-id 5
    :description "Making judgments based on current emotions rather than objective analysis."
    :example "Rating risks lower when in a good mood."
    :thinker "Paul Slovic"
    :failure-modes [{:name "Emotional Decisions" :description "Letting mood drive choices" :risk-level "high"}
                    {:name "Risk Misjudgment" :description "Emotions distorting risk perception" :risk-level "high"}]}
   
   ;; === PHYSICS & ENGINEERING (101-115) ===
   {:id 101 :name "Entropy" :slug "entropy" :category-id 6
    :description "Systems naturally tend toward disorder; maintaining order requires energy."
    :example "Gardens become overgrown; relationships require maintenance."
    :thinker "Rudolf Clausius"
    :failure-modes [{:name "Neglect" :description "Assuming systems maintain themselves" :risk-level "high"}
                    {:name "Fighting Entropy" :description "Wasting energy on inevitable decay" :risk-level "medium"}]}
   
   {:id 102 :name "Inertia" :slug "inertia" :category-id 6
    :description "Objects in motion stay in motion; objects at rest stay at rest."
    :example "Starting a new habit is hard; maintaining momentum is easier."
    :thinker "Isaac Newton"
    :failure-modes [{:name "Stagnation" :description "Difficulty starting change" :risk-level "medium"}
                    {:name "Runaway" :description "Can't stop negative momentum" :risk-level "medium"}]}
   
   {:id 103 :name "Friction" :slug "friction" :category-id 6
    :description "Resistance that slows or prevents motion between surfaces."
    :example "Bureaucracy as organizational friction; UX friction reducing conversions."
    :thinker "Various physicists"
    :failure-modes [{:name "Excessive Friction" :description "Too much resistance to action" :risk-level "medium"}
                    {:name "Removing Good Friction" :description "Eliminating helpful barriers" :risk-level "medium"}]}
   
   {:id 104 :name "Activation Energy" :slug "activation-energy" :category-id 6
    :description "The initial energy needed to start a reaction or process."
    :example "Overcoming procrastination to start a task; first customer hardest to get."
    :thinker "Svante Arrhenius"
    :failure-modes [{:name "Never Starting" :description "Barrier too high to overcome" :risk-level "high"}
                    {:name "Underestimating" :description "Not allocating enough initial effort" :risk-level "medium"}]}
   
   {:id 105 :name "Catalysts" :slug "catalysts" :category-id 6
    :description "Agents that speed up reactions without being consumed."
    :example "A connector who introduces people; a framework that accelerates development."
    :thinker "Jöns Jacob Berzelius"
    :failure-modes [{:name "Dependency" :description "Relying too much on catalysts" :risk-level "low"}
                    {:name "Wrong Catalyst" :description "Using ineffective accelerants" :risk-level "medium"}]}
   
   {:id 106 :name "Leverage" :slug "leverage" :category-id 6
    :description "Using mechanical advantage to amplify force or effort."
    :example "Using debt to amplify returns; using technology to scale impact."
    :thinker "Archimedes"
    :failure-modes [{:name "Over-Leverage" :description "Amplifying losses as well as gains" :risk-level "high"}
                    {:name "Wrong Fulcrum" :description "Applying leverage at wrong point" :risk-level "medium"}]}
   
   {:id 107 :name "Equilibrium" :slug "equilibrium" :category-id 6
    :description "A state of balance where opposing forces are equal."
    :example "Market prices finding equilibrium; work-life balance."
    :thinker "Various physicists"
    :failure-modes [{:name "False Equilibrium" :description "Mistaking temporary for stable" :risk-level "medium"}
                    {:name "Equilibrium Trap" :description "Stuck in suboptimal balance" :risk-level "medium"}]}
   
   {:id 108 :name "Margin of Safety (Engineering)" :slug "margin-of-safety-engineering" :category-id 6
    :description "Designing systems to handle loads beyond expected maximum."
    :example "Bridges designed for 3x expected weight; servers provisioned for 2x traffic."
    :thinker "Various engineers"
    :failure-modes [{:name "Over-Engineering" :description "Excessive safety margins wasting resources" :risk-level "low"}
                    {:name "Insufficient Margin" :description "Failing under unexpected load" :risk-level "high"}]}
   
   {:id 109 :name "Breakpoints" :slug "breakpoints" :category-id 6
    :description "Thresholds where systems change behavior or fail."
    :example "Material stress limits; tipping points in climate."
    :thinker "Various engineers"
    :failure-modes [{:name "Threshold Blindness" :description "Not knowing where breakpoints are" :risk-level "high"}
                    {:name "Gradual Degradation" :description "Missing slow approach to breakpoint" :risk-level "medium"}]}
   
   {:id 110 :name "Backup Systems" :slug "backup-systems" :category-id 6
    :description "Redundant systems that take over when primary systems fail."
    :example "Backup generators; disaster recovery sites; succession planning."
    :thinker "Various engineers"
    :failure-modes [{:name "Untested Backups" :description "Backups failing when needed" :risk-level "high"}
                    {:name "Common Mode Failure" :description "Primary and backup failing together" :risk-level "high"}]}
   
   {:id 111 :name "Forcing Functions" :slug "forcing-functions" :category-id 6
    :description "Design elements that force correct behavior or prevent errors."
    :example "Car won't start without seatbelt; microwave stops when door opens."
    :thinker "Various designers"
    :failure-modes [{:name "Workarounds" :description "People bypassing forcing functions" :risk-level "medium"}
                    {:name "Over-Constraint" :description "Preventing legitimate actions" :risk-level "medium"}]}
   
   {:id 112 :name "Signal vs Noise" :slug "signal-vs-noise" :category-id 6
    :description "Distinguishing meaningful information from random variation."
    :example "Finding real trends in noisy data; filtering important emails."
    :thinker "Claude Shannon"
    :failure-modes [{:name "False Signals" :description "Seeing patterns in noise" :risk-level "high"}
                    {:name "Missing Signals" :description "Filtering out important information" :risk-level "high"}]}
   
   {:id 113 :name "Velocity vs Speed" :slug "velocity-vs-speed" :category-id 6
    :description "Speed is magnitude; velocity includes direction. Progress requires both."
    :example "Running fast in circles vs. walking steadily toward goal."
    :thinker "Various physicists"
    :failure-modes [{:name "Busy but Unproductive" :description "High speed, wrong direction" :risk-level "high"}
                    {:name "Direction Obsession" :description "Planning without action" :risk-level "medium"}]}
   
   {:id 114 :name "Half-Life" :slug "half-life" :category-id 6
    :description "Time for something to decay to half its value."
    :example "Knowledge half-life; skills becoming obsolete; news relevance."
    :thinker "Ernest Rutherford"
    :failure-modes [{:name "Obsolescence" :description "Skills/knowledge becoming outdated" :risk-level "medium"}
                    {:name "Overvaluing Old" :description "Relying on decayed information" :risk-level "medium"}]}
   
   {:id 115 :name "Resonance" :slug "resonance" :category-id 6
    :description "Amplification when frequency matches natural frequency of a system."
    :example "Tacoma Narrows Bridge collapse; viral content hitting cultural moment."
    :thinker "Various physicists"
    :failure-modes [{:name "Destructive Resonance" :description "Amplification causing failure" :risk-level "high"}
                    {:name "Missing Resonance" :description "Not finding the right frequency" :risk-level "medium"}]}
   
   ;; === BIOLOGY & EVOLUTION (116-125) ===
   {:id 116 :name "Natural Selection" :slug "natural-selection" :category-id 7
    :description "Traits that improve survival and reproduction become more common."
    :example "Businesses that serve customers well survive; bad products die."
    :thinker "Charles Darwin"
    :failure-modes [{:name "Local Optima" :description "Stuck at good-enough solution" :risk-level "medium"}
                    {:name "Slow Adaptation" :description "Environment changing faster than evolution" :risk-level "high"}]}
   
   {:id 117 :name "Adaptation" :slug "adaptation" :category-id 7
    :description "Adjusting to environmental conditions to improve fitness."
    :example "Companies pivoting to new markets; organisms developing camouflage."
    :thinker "Charles Darwin"
    :failure-modes [{:name "Over-Adaptation" :description "Too specialized for one environment" :risk-level "high"}
                    {:name "Slow Adaptation" :description "Not changing fast enough" :risk-level "high"}]}
   
   {:id 118 :name "Red Queen Effect" :slug "red-queen-effect" :category-id 7
    :description "Running just to stay in place; continuous adaptation to maintain position."
    :example "Arms races; competitive markets requiring constant innovation."
    :thinker "Leigh Van Valen"
    :failure-modes [{:name "Exhaustion" :description "Burning out from constant competition" :risk-level "medium"}
                    {:name "Falling Behind" :description "Stopping while others continue" :risk-level "high"}]}
   
   {:id 119 :name "Niches" :slug "niches" :category-id 7
    :description "Specialized roles or positions in an ecosystem or market."
    :example "Specialized businesses serving underserved markets."
    :thinker "Joseph Grinnell"
    :failure-modes [{:name "Niche Collapse" :description "Environment eliminating your niche" :risk-level "high"}
                    {:name "Over-Specialization" :description "Too narrow to adapt" :risk-level "medium"}]}
   
   {:id 120 :name "Symbiosis" :slug "symbiosis" :category-id 7
    :description "Close relationships between different species, often mutually beneficial."
    :example "Business partnerships; platform ecosystems; gut bacteria."
    :thinker "Heinrich Anton de Bary"
    :failure-modes [{:name "Parasitism" :description "One party exploiting the other" :risk-level "medium"}
                    {:name "Dependency" :description "Can't survive without partner" :risk-level "medium"}]}
   
   {:id 121 :name "Extinction" :slug "extinction" :category-id 7
    :description "Complete disappearance of a species or type."
    :example "Kodak, Blockbuster - companies failing to adapt."
    :thinker "Georges Cuvier"
    :failure-modes [{:name "Denial" :description "Not recognizing extinction risk" :risk-level "high"}
                    {:name "Too Late" :description "Recognizing threat after point of no return" :risk-level "high"}]}
   
   {:id 122 :name "Survival of the Fittest" :slug "survival-of-fittest" :category-id 7
    :description "Those best adapted to environment survive and reproduce."
    :example "Startups that find product-market fit survive."
    :thinker "Herbert Spencer"
    :failure-modes [{:name "Misdefining Fitness" :description "Wrong criteria for success" :risk-level "medium"}
                    {:name "Short-Term Fitness" :description "Optimizing for now, not future" :risk-level "high"}]}
   
   {:id 123 :name "Mutation" :slug "mutation" :category-id 7
    :description "Random changes that can be beneficial, neutral, or harmful."
    :example "Experimentation in business; random variation leading to innovation."
    :thinker "Hugo de Vries"
    :failure-modes [{:name "Harmful Mutations" :description "Most changes are negative" :risk-level "medium"}
                    {:name "No Variation" :description "Not experimenting enough" :risk-level "medium"}]}
   
   {:id 124 :name "Cooperation" :slug "cooperation" :category-id 7
    :description "Working together for mutual benefit, even among competitors."
    :example "Industry standards; open source; trade associations."
    :thinker "Robert Axelrod"
    :failure-modes [{:name "Free Riders" :description "Some benefiting without contributing" :risk-level "medium"}
                    {:name "Defection" :description "Partners breaking agreements" :risk-level "medium"}]}
   
   {:id 125 :name "Ecosystem" :slug "ecosystem" :category-id 7
    :description "Interconnected community of organisms and their environment."
    :example "Business ecosystems; platform economies; supply chains."
    :thinker "Arthur Tansley"
    :failure-modes [{:name "Cascade Failure" :description "One failure triggering others" :risk-level "high"}
                    {:name "Ecosystem Collapse" :description "Keystone removal destroying system" :risk-level "high"}]}
   
   ;; === MATHEMATICS & LOGIC (126-129) ===
   {:id 126 :name "Power Laws" :slug "power-laws" :category-id 8
    :description "Distributions where small number of items account for majority of impact."
    :example "Wealth distribution; city sizes; word frequency."
    :thinker "Vilfredo Pareto"
    :failure-modes [{:name "Average Thinking" :description "Using means for power law data" :risk-level "high"}
                    {:name "Tail Blindness" :description "Ignoring extreme events" :risk-level "high"}]}
   
   {:id 127 :name "Compounding" :slug "compounding" :category-id 8
    :description "Growth building on previous growth; exponential increase over time."
    :example "Knowledge compounding; relationship networks; skill development."
    :thinker "Various mathematicians"
    :failure-modes [{:name "Impatience" :description "Not waiting for compounding effects" :risk-level "medium"}
                    {:name "Interruption" :description "Breaking compound chains" :risk-level "medium"}]}
   
   {:id 128 :name "Multiplying by Zero" :slug "multiplying-by-zero" :category-id 8
    :description "One zero in a chain of multiplication makes everything zero."
    :example "A great product with zero distribution = zero sales."
    :thinker "Basic mathematics"
    :failure-modes [{:name "Hidden Zeros" :description "Not identifying critical failure points" :risk-level "high"}
                    {:name "Chain Weakness" :description "One weak link breaking everything" :risk-level "high"}]}
   
   {:id 129 :name "Law of Large Numbers" :slug "law-of-large-numbers" :category-id 8
    :description "As sample size increases, results converge to expected value."
    :example "Casinos profit long-term; insurance works with many policies."
    :thinker "Jacob Bernoulli"
    :failure-modes [{:name "Small Sample" :description "Drawing conclusions from too few data points" :risk-level "high"}
                    {:name "Independence Assumption" :description "Assuming events are independent when they're not" :risk-level "medium"}]}])

;; -- Helper Functions --------------------------------------------------------

(defn get-model-by-id [id]
  (first (filter #(= id (:id %)) mental-models)))

(defn get-model-by-slug [slug]
  (first (filter #(= slug (:slug %)) mental-models)))

(defn get-models-by-category [category-id]
  (filter #(= category-id (:category-id %)) mental-models))

(defn get-category-by-id [id]
  (first (filter #(= id (:id %)) categories)))

(defn search-models [query]
  (let [q (clojure.string/lower-case query)]
    (filter #(or (clojure.string/includes? (clojure.string/lower-case (:name %)) q)
                 (clojure.string/includes? (clojure.string/lower-case (:description %)) q))
            mental-models)))

(defn get-all-models []
  mental-models)

(defn get-all-categories []
  categories)

(defn model-count []
  (count mental-models))

(defn category-count []
  (count categories))
