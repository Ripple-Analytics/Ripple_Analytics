(ns mental-models.models.lee-kuan-yew-singapore-model-part4
  "Mental Models - Lee Kuan Yew / Singapore Model Category (Part 4)"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; ============================================
;; Category: Lee Kuan Yew / Singapore Model (Part 4)
;; ============================================

(register-model
 {:name "base-rate-neglect"
  :category "cognitive_bias"
  :originator "Daniel Kahneman / Amos Tversky"
  :description "Base Rate Neglect is the tendency to ignore statistical baseline information (base rates) in favor of specific, individuating information. When estimating probability, people overweight vivid, specific details and underweight abstract statistical data. For example, when told 'Steve is shy and withdrawn, likes order and detail' and asked if Steve is a librarian or farmer, most say librarian - despite farmers outnumbering librarians 20:1 in the population. The base rate (20:1 ratio) should dominate, but the specific description (shy, detail-oriented) captures attention. This bias causes systematic errors in prediction, planning, and decision-making. Entrepreneurs ignore the 90% startup failure rate because their specific idea seems special. Investors ignore the base rate of fund performance because this fund's story is compelling. Doctors ignore disease prevalence because this patient's symptoms are vivid. The solution is to always start with base rates, then adjust conservatively for specific information."
  :key-insight "Always start with the base rate - the statistical baseline probability - before considering specific information; most predictions fail because they ignore base rates"
  :application "Before any prediction: (1) Identify the reference class (e.g., 'tech startups' not 'my startup'), (2) Find the base rate (e.g., 90% fail within 5 years), (3) Start prediction at base rate (90% chance of failure), (4) Adjust conservatively for specific factors (maybe 80% chance of failure), (5) Never adjust more than 30% from base rate without extraordinary evidence. In business: Use industry failure rates. In investing: Use historical fund performance. In hiring: Use role-specific success rates. In medicine: Use disease prevalence."
  :failure-modes
  [(failure "inside-view-bias" "critical"
            "Focusing on specific details of the case while ignoring statistical baselines from similar cases"
            :signals ["'This time is different' thinking" "Ignoring historical data" "Overconfidence in specific case" "Narrative focus" "Dismissing statistics as 'not relevant'"]
            :safeguards ["Mandatory reference class forecasting" "Start with base rate" "Adjust conservatively (max 30%)" "Document adjustment rationale" "Track prediction accuracy"]
            :case-studies [{:name "Startup Failure Rates"
                           :description "90% of startups fail within 5 years (base rate). Yet entrepreneurs consistently estimate their own success probability at 70-90%. This overconfidence leads to insufficient capital reserves, poor risk management, and surprise when failure occurs. Y Combinator data: Of 3,000+ funded startups, only ~300 (10%) achieved significant success."
                           :impact "90% failure rate, billions in lost capital"
                           :lesson "Entrepreneurs ignore base rates because their specific idea seems special"}
                          {:name "Mutual Fund Performance"
                           :description "Base rate: 95% of actively managed funds underperform index over 15 years (S&P SPIVA data). Yet investors consistently choose active funds based on recent performance, manager credentials, and compelling narratives. Result: $100B+ in annual excess fees for inferior performance."
                           :impact "$100B+ annual excess fees, inferior returns"
                           :lesson "Investors ignore base rates because this fund's story is compelling"}])
   (failure "planning-fallacy" "critical"
            "Underestimating time, costs, and risks because you ignore base rates of similar projects"
            :signals ["Optimistic schedules" "Budget underestimates" "Ignoring historical data" "Best-case planning" "'We're different' thinking"]
            :safeguards ["Reference class forecasting" "Use historical data from similar projects" "Add 50-100% buffer" "Outside view" "Track actual vs. estimated"]
            :case-studies [{:name "Sydney Opera House"
                           :description "Original estimate: $7M, 4 years (1957). Actual: $102M, 14 years (1973). Cost overrun: 1,357%. Time overrun: 250%. Base rate for major infrastructure projects: 90% over budget, 70% over schedule. Planners ignored base rate and used inside view."
                           :impact "$95M cost overrun, 10-year delay"
                           :lesson "Major projects ignore base rates and suffer predictable overruns"}
                          {:name "Denver International Airport"
                           :description "Original estimate: $1.7B, 1993 opening. Actual: $4.8B, 1995 opening. Automated baggage system alone: $193M over budget. Base rate for airport projects: 65% over budget. Planners ignored base rate."
                           :impact "$3.1B cost overrun, 16-month delay"
                           :lesson "Complex projects ignore base rates despite consistent historical data"}])
   (failure "conjunction-fallacy" "high"
            "Judging specific scenarios as more probable than general ones because they're more vivid"
            :signals ["Detailed scenarios seem more likely" "Narrative coherence over probability" "Adding details increases belief" "Story-based predictions"]
            :safeguards ["Probability training" "Check if A&B > A (impossible)" "Base rate anchor" "Simplify scenarios" "Mathematical checks"]
            :case-studies [{:name "Linda Problem (Kahneman & Tversky)"
                           :description "Linda is 31, single, outspoken, philosophy major, concerned with discrimination and social justice. Which is more probable? (A) Linda is a bank teller, or (B) Linda is a bank teller and active in feminist movement. 85% of subjects chose B, despite B being logically impossible to be more probable than A (conjunction fallacy). Base rate: Bank tellers ~0.2% of population. Bank tellers who are feminists: ~0.02% of population."
                           :impact "Systematic probability errors in judgment"
                           :lesson "Vivid details make impossible scenarios seem probable"}])
   (failure "sample-size-neglect" "high"
            "Ignoring the size of the sample when making inferences, treating small samples like large ones"
            :signals ["Conclusions from small samples" "Ignoring statistical significance" "Anecdotal evidence" "N=1 decisions" "Recency bias"]
            :safeguards ["Minimum sample size requirements" "Statistical significance testing" "Confidence intervals" "Replication" "Meta-analysis"]
            :case-studies [{:name "Hospital Birth Rates"
                           :description "Two hospitals: Large (45 babies/day) and Small (15 babies/day). Which recorded more days with >60% boys? Most people say 'equal' or 'large hospital'. Correct answer: Small hospital (larger variance in small samples). Base rate: 50% boys. Small samples deviate more from base rate. This error causes systematic misjudgments in business, medicine, and investing."
                           :impact "Systematic errors in inference from data"
                           :lesson "Small samples deviate more from base rates but feel equally reliable"}])
   (failure "representativeness-heuristic" "medium"
            "Judging probability by how much something resembles a stereotype, ignoring base rates"
            :signals ["Stereotyping" "Ignoring prior probabilities" "Focusing on similarity" "Narrative matching" "Ignoring diagnosticity"]
            :safeguards ["Always start with base rate" "Bayes' theorem" "Prior probability" "Diagnostic value assessment" "Statistical training"]
            :case-studies [{:name "Cab Problem (Kahneman & Tversky)"
                           :description "Hit-and-run by cab at night. 85% of cabs are Green, 15% are Blue. Witness says Blue. Witness is 80% reliable. What's probability it was Blue cab? Most people say 80%. Correct answer: 41% (using Bayes' theorem with base rate). Base rate (15% Blue) should anchor judgment, then adjust for witness reliability. Ignoring base rate causes systematic errors."
                           :impact "Systematic errors in probability judgment"
                           :lesson "Specific information (witness) overwhelms base rate (15% Blue)"}])]})

(register-model
 {:name "survivorship-bias"
  :category "cognitive_bias"
  :originator "Abraham Wald / Nassim Nicholas Taleb"
  :description "Survivorship Bias is the logical error of focusing on entities that survived a selection process while ignoring those that didn't, leading to false conclusions. The classic example: In WWII, the military wanted to armor planes where they saw bullet holes on returning aircraft. Statistician Abraham Wald pointed out the error - planes hit in those areas survived and returned; planes hit elsewhere didn't return (they crashed). Armor should go where there are no bullet holes on survivors. This bias pervades business, investing, and life. We study successful companies and copy their strategies, ignoring the graveyard of failed companies that used identical strategies. We read books by successful entrepreneurs, ignoring the silent majority who failed. We see mutual funds with great track records, ignoring the funds that closed due to poor performance. The solution is to study failures as much as successes, and always ask: What am I not seeing because it didn't survive?"
  :key-insight "The dead can't speak - when analyzing success, always ask what you're not seeing because it didn't survive the selection process"
  :application "Before drawing conclusions from successful examples: (1) Identify the selection process (e.g., 'companies still in business'), (2) Estimate the survival rate (e.g., 50% of companies fail in 5 years), (3) Actively seek data on failures (e.g., bankrupt companies), (4) Compare survivors vs. non-survivors, (5) Look for strategies common to both groups (not predictive) vs. strategies unique to survivors (potentially predictive). In business: Study failed companies. In investing: Include dead funds in analysis. In hiring: Interview people who left. In research: Publish negative results."
  :failure-modes
  [(failure "success-study-only" "critical"
            "Studying only successful examples and inferring their strategies caused success, ignoring failures that used same strategies"
            :signals ["'Best practices' from successful companies" "Studying only winners" "Ignoring failures" "'Secrets of success' books" "No control group"]
            :safeguards ["Study failures equally" "Control groups" "Match survivors with non-survivors" "Randomized trials" "Publish negative results"]
            :case-studies [{:name "In Search of Excellence"
                           :description "Tom Peters' 1982 bestseller identified 43 'excellent' companies based on 8 success principles. Within 5 years, 14 of 43 companies (33%) were in financial trouble or bankrupt. Problem: Peters studied only successful companies, ignoring failed companies that used identical principles. Survivorship bias made random strategies appear causal."
                           :impact "Millions misled by false success principles"
                           :lesson "Studying only survivors makes random strategies appear causal"}
                          {:name "Mutual Fund Performance"
                           :description "Mutual fund industry reports average returns of 10% annually. But this includes only surviving funds. 50% of funds close within 10 years due to poor performance. When including dead funds, average return drops to 6-7%. Survivorship bias inflates reported performance by 3-4% annually, causing investors to overestimate expected returns."
                           :impact "$100B+ in investor losses from inflated expectations"
                           :lesson "Dead funds disappear from databases, inflating survivor performance"}])
   (failure "silent-evidence" "critical"
            "Not seeking evidence that was destroyed, hidden, or never recorded because entities didn't survive"
            :signals ["No data on failures" "Databases exclude dead entities" "Archives incomplete" "Winners write history" "Publication bias"]
            :safeguards ["Actively seek silent evidence" "Estimate missing data" "Survivor-adjusted statistics" "Include dead entities" "Negative result publication"]
            :case-studies [{:name "Startup Success Rates"
                           :description "Media covers successful startups extensively (Facebook, Uber, Airbnb). Failed startups get no coverage and often delete websites/social media. Result: Aspiring entrepreneurs see only successes, underestimate failure rates, and overestimate their own chances. Reality: 90% of startups fail within 5 years, but silent evidence makes it appear much lower."
                           :impact "Billions in capital misallocation"
                           :lesson "Failed startups leave no trace, creating false impression of high success rates"}])
   (failure "regression-to-mean-confusion" "high"
            "Attributing regression to the mean to skill or strategy rather than statistical artifact"
            :signals ["'Sophomore slump' explanations" "Attributing mean reversion to actions" "Ignoring base rates" "Causal stories for random fluctuation"]
            :safeguards ["Understand regression to mean" "Control groups" "Longer time periods" "Statistical testing" "Avoid causal stories for mean reversion"]
            :case-studies [{:name "Sports Illustrated Cover Curse"
                           :description "Athletes who appear on Sports Illustrated cover often perform worse afterward ('SI Cover Curse'). Explanation: Athletes appear on cover after exceptional performance (far above their mean). Subsequent performance regresses toward their true mean, appearing as a 'curse'. This is pure statistical regression, not causation. But survivorship bias makes the pattern seem real."
                           :impact "False causal beliefs from statistical artifact"
                           :lesson "Extreme performance regresses to mean, but we attribute it to causes"}])
   (failure "historical-determinism" "medium"
            "Believing historical outcomes were inevitable because we only see the path that survived"
            :signals ["'It had to happen' thinking" "Hindsight bias" "Ignoring alternative paths" "Deterministic history" "Narrative fallacy"]
            :safeguards ["Consider alternative histories" "Counterfactual thinking" "Acknowledge contingency" "Study near-misses" "Probabilistic thinking"]
            :case-studies [{:name "World War II Outcome"
                           :description "Allied victory in WWII seems inevitable in retrospect. But many contingent events could have changed outcome: Hitler not invading Russia, Japan not attacking Pearl Harbor, D-Day weather, atomic bomb development. We study the path that survived (Allied victory) and construct deterministic narrative, ignoring the many paths that didn't survive (Axis victory scenarios)."
                           :impact "False sense of inevitability in historical outcomes"
                           :lesson "History seems inevitable because we only see the path that survived"}])
   (failure "selection-process-ignorance" "high"
            "Not understanding the selection process that determined which entities survived"
            :signals ["Ignoring selection criteria" "Assuming random sampling" "Not questioning data source" "Incomplete databases" "Convenience sampling"]
            :safeguards ["Understand selection process" "Question data sources" "Identify survival criteria" "Estimate survival rate" "Adjust for selection"]
            :case-studies [{:name "College Dropout Billionaires"
                           :description "Media highlights college dropout billionaires (Gates, Zuckerberg, Jobs). This creates impression that dropping out causes success. Reality: Millions drop out and fail in obscurity (silent evidence). Selection process: Only billionaires get media coverage. Survival rate: ~0.0001% of dropouts become billionaires. Conclusion: Dropping out doesn't cause success; exceptional people succeed despite dropping out."
                           :impact "Millions misled into dropping out"
                           :lesson "Selection process (media coverage) creates false causal impression"}])]})