;; Enhanced Model: Base Rate Neglect
;; Critical mental model for probabilistic thinking and avoiding costly errors

(def base-rate-neglect-enhanced
  {:name "base-rate-neglect"
   :category "cognitive_biases"
   :description "The tendency to ignore statistical base rates (prior probabilities) in favor of specific case information. Failing to consider how common something is in the general population."
   :key-insight "Specific details feel more relevant than abstract statistics, but base rates are often more predictive than case-specific information."
   :application "Always start with base rates. Ask: 'What's the general frequency of this outcome?' before considering specific details. Use Bayes' Theorem to properly weight base rates and new evidence."
   :munger-quote "If you don't get this elementary, but mildly unnatural, mathematics of elementary probability into your repertoire, then you go through a long life like a one-legged man in an ass-kicking contest."
   
   :failure-modes
   [{:name "ignoring-base-rates"
     :severity "critical"
     :description "Completely disregarding statistical base rates when making predictions or decisions, focusing only on case-specific information."
     :mechanism "Representativeness heuristic overrides statistical reasoning. Specific narratives feel more compelling than abstract numbers."
     :psychological-root "Human brain evolved for pattern recognition in small samples, not statistical reasoning. Narrative bias makes stories more memorable than statistics."
     :evolutionary-origin "Ancestral environments had small sample sizes where individual cases mattered more than population statistics. No evolutionary pressure for statistical thinking."
     
     :signals
     ["Making predictions without checking historical frequencies"
      "Saying 'this case is different' without quantitative justification"
      "Ignoring industry failure rates"
      "Overweighting anecdotes vs data"
      "Surprise when outcomes match base rates"
      "Not asking 'How often does this usually happen?'"
      "Focusing on unique features while ignoring category membership"
      "Treating all cases as equally likely regardless of frequency"]
     
     :case-studies
     [{:name "Startup Success Delusion"
       :year "2020s"
       :entity "Venture-backed startups"
       :description "Entrepreneurs routinely ignore 90% startup failure rate, believing their case is different. VCs understand base rates and structure portfolios accordingly."
       :quantitative-impact "90% of startups fail. 75% of VC-backed startups fail to return capital. Yet founders consistently estimate >50% personal success probability."
       :root-cause "Founders focus on their unique idea/team (specific information) while ignoring category base rate (90% failure)"
       :should-have-done "Start with 10% base rate, then adjust upward with concrete evidence. Plan for most likely outcome (failure) while working toward success."
       :sources ["CB Insights Startup Failure Report", "Harvard Business School startup studies"]}
      
      {:name "Medical Diagnosis Errors"
       :year "Ongoing"
       :entity "Healthcare system"
       :description "Doctors often ignore disease base rates, leading to false positives. Example: Rare disease with 1/10,000 prevalence, 99% accurate test still means 99% of positives are false positives."
       :quantitative-impact "Estimated 12 million diagnostic errors annually in US. Many from base rate neglect. Unnecessary treatments, anxiety, costs."
       :root-cause "Doctors focus on test results (specific information) while ignoring disease rarity (base rate)"
       :should-have-done "Apply Bayes' Theorem. Calculate posterior probability using base rate, test sensitivity, and specificity."
       :sources ["Graber et al. (2005) Diagnostic Error in Internal Medicine", "Tversky & Kahneman base rate studies"]}
      
      {:name "Terrorism vs Traffic Deaths"
       :year "2001-2020"
       :entity "US Government and public"
       :description "After 9/11, massive resources devoted to terrorism prevention despite traffic deaths being 100x more common. Base rate of terrorism deaths: ~3,000 over 20 years. Traffic deaths: ~40,000 per year."
       :quantitative-impact "$7.6 trillion spent on war on terror (2001-2019). Could have saved more lives investing in traffic safety at fraction of cost."
       :root-cause "Vivid terrorist attacks (specific, emotional) override statistical reality (base rates) in resource allocation"
       :should-have-done "Allocate resources proportional to base rates of deaths, adjusted for preventability and cost-effectiveness"
       :sources ["Costs of War Project, Brown University", "NHTSA traffic fatality statistics"]}
      
      {:name "M&A Success Rate Ignorance"
       :year "Ongoing"
       :entity "Corporate acquirers"
       :description "70-90% of M&A deals fail to create value, yet acquirers consistently believe their deal is different. Investment banks encourage this belief."
       :quantitative-impact "Trillions in destroyed shareholder value. AOL-Time Warner: $99B loss. HP-Autonomy: $8.8B writedown. Microsoft-Nokia: $7.6B writedown."
       :root-cause "CEOs focus on strategic rationale (specific case) while ignoring M&A failure base rate (70-90%)"
       :should-have-done "Start with base rate: 'Most M&A fails.' Require extraordinary evidence to proceed. Demand premium for base rate risk."
       :sources ["KPMG M&A studies", "Harvard Business Review M&A research", "McKinsey M&A reports"]}
      
      {:name "Expert Witness Testimony"
       :year "Ongoing"
       :entity "Legal system"
       :description "Expert witnesses often present case-specific analysis without base rates, leading to wrongful convictions. Example: 'DNA match' without explaining random match probability."
       :quantitative-impact "Innocence Project: 375+ DNA exonerations. Many involved expert testimony ignoring base rates (e.g., bite mark analysis, hair comparison)."
       :root-cause "Experts present specific evidence (this hair matches) without base rate (how often do hairs match by chance)"
       :should-have-done "Require experts to state base rates. 'This evidence occurs in X% of innocent cases and Y% of guilty cases.'"
       :sources ["Innocence Project data", "National Academy of Sciences forensic science report"]}]
     
     :quantitative-thresholds
     [{:metric "Base Rate Consultation Frequency"
       :warning-threshold "< 50% of decisions"
       :critical-threshold "< 25% of decisions"
       :measurement "Percentage of decisions where base rates are explicitly checked"
       :frequency "Per decision cycle"
       :example "Making 10 hiring decisions without checking industry retention rates"}
      
      {:metric "Base Rate vs Specific Information Weight"
       :warning-threshold "< 30% weight on base rate"
       :critical-threshold "< 10% weight on base rate"
       :measurement "Relative importance given to base rates vs case-specific information in decision"
       :frequency "Per prediction or decision"
       :example "90% weight on 'impressive interview' vs 10% weight on 'most hires fail in first year'"}
      
      {:metric "Prediction Calibration vs Base Rates"
       :warning-threshold "> 20% deviation from base rate"
       :critical-threshold "> 50% deviation from base rate"
       :measurement "Difference between personal prediction and base rate without strong justification"
       :frequency "Per forecast"
       :example "Predicting 70% startup success when base rate is 10%, without quantitative evidence"}]
     
     :environmental-triggers
     ["Vivid, memorable case examples"
      "Emotional investment in specific outcome"
      "Lack of statistical training"
      "Pressure to appear confident"
      "Narrative-focused culture"
      "Absence of base rate data"
      "Incentives to ignore base rates (e.g., investment bankers selling M&A)"
      "Overconfidence in ability to beat the odds"]
     
     :structural-safeguards
     ["Mandatory base rate lookup before major decisions"
      "Decision templates requiring base rate section"
      "Statistical review of major forecasts"
      "Track predictions vs base rates over time"
      "Require justification for deviating from base rates"
      "Build base rate databases for common decisions"
      "Incentivize accurate forecasting, not optimistic forecasting"
      "Reference class forecasting methodology"]
     
     :cognitive-safeguards
     ["Always ask: 'What's the base rate for this category?'"
      "Use Bayes' Theorem for updating beliefs"
      "Apply reference class forecasting"
      "Distinguish inside view (specific case) from outside view (base rate)"
      "Weight base rate higher than intuition suggests"
      "Look for statistical twins of current situation"]
     
     :social-safeguards
     ["Appoint 'base rate guardian' in decision meetings"
      "Require statistician review of major forecasts"
      "Create culture where citing base rates is valued"
      "Share calibration data publicly"
      "Celebrate accurate predictions, not optimistic ones"]
     
     :recovery-protocol
     ["Acknowledge base rate was ignored"
      "Calculate what base rate would have predicted"
      "Analyze cost of ignoring base rate"
      "Update decision process to include base rates"
      "Build base rate database for future decisions"
      "Train team on Bayesian reasoning"]
     
     :lollapalooza-risk
     {:description "Base rate neglect combines catastrophically with confirmation bias, overconfidence, and narrative fallacy"
      :amplifying-biases ["confirmation-bias" "overconfidence" "narrative-fallacy" "availability-heuristic"]
      :example "Startup founder (base rate neglect) + ignores warning signs (confirmation bias) + believes in unique genius (overconfidence) + compelling story (narrative fallacy) = predictable failure"
      :severity "extreme"}
     
     :mitigating-models
     ["probabilistic-thinking" "outside-view" "reference-class-forecasting" "bayesian-updating" "statistical-thinking"]}
    
    {:name "false-positive-trap"
     :severity "high"
     :description "When testing for rare events, even accurate tests produce mostly false positives if base rate is low enough. Leads to costly overreaction."
     :mechanism "P(Disease|Positive Test) ≠ P(Positive Test|Disease). Confusion between conditional probabilities."
     :psychological-root "Conditional probability is cognitively unnatural. Brain doesn't intuitively apply Bayes' Theorem."
     :evolutionary-origin "No evolutionary environment required reasoning about conditional probabilities with rare events."
     
     :signals
     ["Treating positive test as definitive without considering base rate"
      "Not calculating posterior probability"
      "Surprise at high false positive rate"
      "Implementing screening for very rare conditions"
      "Panic over positive test for rare disease"
      "Not asking 'How rare is this condition?'"
      "Confusing test accuracy with predictive value"]
     
     :case-studies
     [{:name "Airport Security False Positives"
       :year "2001-present"
       :entity "TSA and airport security"
       :description "Terrorist base rate extremely low (~1 in 100 million passengers). Even 99.9% accurate screening produces massive false positives. Cost: delays, harassment, wasted resources."
       :quantitative-impact "TSA screens 800M passengers/year. If 99.9% accurate and 1 terrorist per 100M passengers, produces 800,000 false positives per 8 true positives."
       :root-cause "Ignoring base rate of terrorism (extremely rare) when implementing screening"
       :should-have-done "Calculate expected false positive rate before implementation. Consider cost-benefit of screening rare events."
       :sources ["TSA statistics", "Schneier, Bruce 'Beyond Fear' (2003)"]}
      
      {:name "Cancer Screening Controversies"
       :year "Ongoing"
       :entity "Medical screening programs"
       :description "Screening for rare cancers in low-risk populations produces many false positives, leading to unnecessary biopsies, anxiety, and costs."
       :quantitative-impact "Prostate cancer screening: 1,000 men screened, 100-120 false positives, 1-2 lives saved. Mammography: 1,000 women screened, 100 false positives, 1 life saved."
       :root-cause "Not accounting for low base rate of cancer in screening population"
       :should-have-done "Target screening to high-risk populations where base rate is higher. Calculate number needed to screen."
       :sources ["US Preventive Services Task Force", "Cochrane Reviews of screening programs"]}]
     
     :quantitative-thresholds
     [{:metric "False Positive Rate"
       :warning-threshold "> 50% of positives are false"
       :critical-threshold "> 90% of positives are false"
       :measurement "Percentage of positive results that are false positives"
       :frequency "Per screening program"
       :example "Drug testing program where 95% of positives are false due to low base rate"}]
     
     :structural-safeguards
     ["Calculate expected false positive rate before implementing screening"
      "Require cost-benefit analysis including false positive costs"
      "Use two-stage testing for rare events"
      "Target screening to high-risk populations"
      "Educate on difference between test accuracy and predictive value"]
     
     :cognitive-safeguards
     ["Apply Bayes' Theorem: P(A|B) = P(B|A) * P(A) / P(B)"
      "Calculate positive predictive value before acting on test"
      "Remember: rare events produce mostly false positives even with accurate tests"]
     
     :recovery-protocol
     ["Calculate actual false positive rate"
      "Implement confirmatory testing"
      "Adjust screening criteria to higher-risk populations"
      "Educate stakeholders on base rate effects"]}
    
    {:name "prosecutor-fallacy"
     :severity "critical"
     :description "Confusing P(Evidence|Innocent) with P(Innocent|Evidence). Leads to wrongful convictions and injustice."
     :mechanism "Prosecutor presents: 'Probability of this evidence if defendant is innocent is 1 in 1 million, therefore defendant is guilty.' Ignores base rate of innocence."
     :psychological-root "Conditional probability confusion. Narrative bias toward guilt when presented with rare evidence."
     :evolutionary-origin "No evolutionary pressure to reason correctly about conditional probabilities."
     
     :signals
     ["Presenting P(Evidence|Innocent) as if it were P(Guilty|Evidence)"
      "Not considering prior probability of guilt"
      "Ignoring alternative explanations with similar evidence"
      "Treating rare evidence as proof of guilt"
      "Not applying Bayes' Theorem in legal reasoning"]
     
     :case-studies
     [{:name "Sally Clark Case"
       :year "1999"
       :entity "UK Legal System"
       :description "Sally Clark convicted of murdering two infants. Expert testified: 'Probability of two SIDS deaths is 1 in 73 million.' Ignored base rate: probability of double murder even rarer."
       :quantitative-impact "Clark imprisoned for 3 years before conviction overturned. Died from alcohol poisoning 4 years after release, attributed to trauma."
       :root-cause "Prosecutor's fallacy: presented P(Two SIDS|Innocent) as if it were P(Innocent|Two Deaths)"
       :should-have-done "Compare P(Evidence|Innocent) with P(Evidence|Guilty). Consider base rate of double infant murder vs double SIDS."
       :sources ["Royal Statistical Society statement on Clark case", "Hill, Ray 'Multiple Sudden Infant Deaths' (2004)"]}
      
      {:name "DNA Database Searches"
       :year "Ongoing"
       :entity "Criminal justice system"
       :description "Searching DNA database of 1 million people for match to crime scene DNA. Even with 1 in 1 billion random match probability, expect 1 false match in database."
       :quantitative-impact "Unknown number of wrongful convictions from database searches without proper statistical analysis."
       :root-cause "Presenting random match probability without considering database search increased chance of false match"
       :should-have-done "Adjust match probability for database size. Require additional evidence beyond database match."
       :sources ["National Research Council DNA report", "Balding & Donnelly (1996) Nature"]}]
     
     :quantitative-thresholds
     [{:metric "Conditional Probability Confusion"
       :warning-threshold "Any instance in legal proceeding"
       :critical-threshold "Any instance in legal proceeding"
       :measurement "Instances of P(A|B) presented as P(B|A)"
       :frequency "Per trial"
       :example "Expert testifies 'Probability of this evidence if innocent is 0.001%' implying guilt"}]
     
     :structural-safeguards
     ["Require statistical review of expert testimony"
      "Mandate Bayesian analysis in cases with probabilistic evidence"
      "Educate judges and juries on conditional probability"
      "Require experts to state both P(E|I) and P(E|G)"
      "Appoint statistical expert witnesses"]
     
     :cognitive-safeguards
     ["Always distinguish P(A|B) from P(B|A)"
      "Apply Bayes' Theorem explicitly"
      "Consider base rate of guilt/innocence"
      "Compare likelihood ratio: P(E|G) / P(E|I)"]
     
     :recovery-protocol
     ["Review convictions based on probabilistic evidence"
      "Provide statistical training to legal professionals"
      "Establish standards for presenting probabilistic evidence"
      "Create statistical review boards for criminal cases"]}]
   
   :total-documented-impact "$10+ trillion in destroyed value from M&A failures, startup failures, medical errors, wrongful convictions, and misallocated resources"
   
   :munger-connection "Munger emphasizes elementary probability as essential. Base rate neglect is failure to apply this elementary mathematics."
   
   :related-models ["probabilistic-thinking" "bayesian-updating" "outside-view" "reference-class-forecasting" "statistical-thinking"]})
