(ns mental-models.models.munger-25-tendencies
  "Charlie Munger's 25 Standard Causes of Human Misjudgment
   Detection algorithms for text analysis based on Poor Charlie's Almanack.
   Each tendency has keywords, patterns, and LLM prompts for detection."
  (:require [hyperfiddle.electric :as e]
            [clojure.string :as str]))

;; =============================================================================
;; MUNGER'S 25 PSYCHOLOGICAL TENDENCIES
;; From 'The Psychology of Human Misjudgment' - Poor Charlie's Almanack
;; =============================================================================

(def munger-25-tendencies
  "Complete list of Munger's 25 Standard Causes of Human Misjudgment"
  
  [{:id 1
    :name "Reward and Punishment Superresponse Tendency"
    :short-name "Incentive-Caused Bias"
    :description "People respond to incentives, often in ways that are not in their best interest. 'Never think about something else when you should be thinking about the power of incentives.'"
    :keywords ["incentive" "bonus" "commission" "reward" "punishment" "compensation" "pay" "salary" "stock options" "profit sharing" "kickback" "bribe" "conflict of interest"]
    :patterns [#"(?i)incentive.{0,20}(align|misalign|structure)"
               #"(?i)(bonus|commission).{0,20}(drive|motivate|cause)"
               #"(?i)conflict.{0,10}interest"
               #"(?i)(paid|compensat).{0,20}(to|for).{0,20}(sell|recommend|approve)"]
    :antidotes ["Remove conflicting incentives" "Align incentives with desired outcomes" "Audit for hidden incentives"]
    :examples ["Salespeople pushing products with highest commissions"
               "Doctors ordering unnecessary tests"
               "Auditors approving clients who pay them"
               "Real estate agents rushing sales for commission"]
    :llm-prompt "Analyze this text for evidence of incentive-caused bias. Look for situations where financial rewards, bonuses, commissions, or other incentives may be influencing behavior or recommendations in ways that conflict with the best interests of others. Identify specific incentive structures and their potential distorting effects."
    :severity :high
    :category :psychology}
   
   {:id 2
    :name "Liking/Loving Tendency"
    :short-name "Liking Bias"
    :description "We tend to ignore faults and comply with wishes of those we like or love. We also distort facts to facilitate love."
    :keywords ["like" "love" "admire" "friend" "attractive" "similar" "rapport" "charisma" "charm" "likeable" "affection" "fondness"]
    :patterns [#"(?i)(like|love|admire).{0,20}(therefore|so|thus)"
               #"(?i)(friend|colleague).{0,20}(recommend|suggest|trust)"
               #"(?i)(charming|likeable|attractive).{0,20}(person|individual)"]
    :antidotes ["Evaluate ideas separately from people" "Seek disconfirming evidence about liked people" "Use checklists"]
    :examples ["Hiring friends over more qualified candidates"
               "Trusting advice from likeable salespeople"
               "Ignoring red flags in romantic relationships"
               "Following celebrity endorsements"]
    :llm-prompt "Analyze this text for evidence of liking/loving bias. Look for situations where positive feelings toward a person, brand, or entity may be clouding judgment, leading to overlooked faults, unwarranted trust, or distorted evaluation of facts."
    :severity :medium
    :category :psychology}
   
   {:id 3
    :name "Disliking/Hating Tendency"
    :short-name "Disliking Bias"
    :description "We tend to ignore virtues and distort facts to facilitate hatred. We also transfer dislike to associated people and things."
    :keywords ["hate" "dislike" "enemy" "rival" "competitor" "opponent" "distrust" "suspicion" "animosity" "hostility" "resentment"]
    :patterns [#"(?i)(hate|dislike|distrust).{0,20}(therefore|so|thus)"
               #"(?i)(enemy|rival|competitor).{0,20}(wrong|bad|fail)"
               #"(?i)(never|always).{0,10}(trust|believe).{0,20}(them|those)"]
    :antidotes ["Evaluate ideas separately from people" "Seek virtues in disliked people" "Consider opposite viewpoint"]
    :examples ["Dismissing good ideas from rivals"
               "Attributing malice to competitors' actions"
               "Refusing to learn from enemies"
               "Political tribalism"]
    :llm-prompt "Analyze this text for evidence of disliking/hating bias. Look for situations where negative feelings toward a person, group, or entity may be leading to ignored virtues, distorted facts, or unfair attributions of negative intent."
    :severity :medium
    :category :psychology}
   
   {:id 4
    :name "Doubt-Avoidance Tendency"
    :short-name "Doubt Avoidance"
    :description "The brain is designed to remove doubt by reaching a decision quickly. This is especially true under stress or pressure."
    :keywords ["certain" "sure" "definite" "clear" "obvious" "no doubt" "undoubtedly" "clearly" "obviously" "must be" "has to be" "quick decision" "snap judgment"]
    :patterns [#"(?i)(must|has to|clearly|obviously).{0,10}(be|is)"
               #"(?i)(no|without).{0,5}doubt"
               #"(?i)(quick|fast|immediate).{0,10}(decision|judgment|conclusion)"
               #"(?i)(certain|sure|definite).{0,20}(that|about)"]
    :antidotes ["Delay decisions when possible" "Embrace uncertainty" "Seek more information" "Use probabilistic thinking"]
    :examples ["Rushing to judgment under pressure"
               "Accepting first plausible explanation"
               "Premature closure in investigations"
               "Snap hiring decisions"]
    :llm-prompt "Analyze this text for evidence of doubt-avoidance tendency. Look for situations where decisions are being made too quickly to remove uncomfortable uncertainty, premature conclusions are being drawn, or there's excessive certainty where doubt would be appropriate."
    :severity :medium
    :category :psychology}
   
   {:id 5
    :name "Inconsistency-Avoidance Tendency"
    :short-name "Consistency Bias"
    :description "The brain conserves programming space by being reluctant to change. This creates resistance to new ideas and change."
    :keywords ["consistent" "always" "never change" "tradition" "habit" "routine" "precedent" "status quo" "we've always" "that's how we" "resist change"]
    :patterns [#"(?i)(always|never).{0,10}(done|been|worked)"
               #"(?i)(tradition|habit|routine|precedent)"
               #"(?i)(resist|oppose|reject).{0,10}(change|new|different)"
               #"(?i)status.?quo"]
    :antidotes ["Actively seek disconfirming evidence" "Question long-held beliefs" "Embrace change as opportunity"]
    :examples ["Refusing to update investment thesis despite new evidence"
               "Maintaining failing strategies due to past commitment"
               "Organizational resistance to change"
               "Clinging to outdated beliefs"]
    :llm-prompt "Analyze this text for evidence of inconsistency-avoidance tendency. Look for situations where there's resistance to changing established patterns, reluctance to update beliefs despite new evidence, or excessive commitment to past decisions or traditions."
    :severity :high
    :category :psychology}
   
   {:id 6
    :name "Curiosity Tendency"
    :short-name "Curiosity"
    :description "Curiosity helps counteract other psychological tendencies. It drives learning and discovery."
    :keywords ["curious" "wonder" "explore" "investigate" "learn" "discover" "question" "inquire" "research" "study" "understand"]
    :patterns [#"(?i)(curious|wonder|explore|investigate)"
               #"(?i)(want|need|try).{0,10}(understand|learn|know)"
               #"(?i)(question|inquire|research|study)"]
    :antidotes ["Cultivate curiosity" "Ask why repeatedly" "Never stop learning"]
    :examples ["Scientists pursuing knowledge for its own sake"
               "Investors deeply researching companies"
               "Children asking endless questions"
               "Lifelong learners"]
    :llm-prompt "Analyze this text for evidence of curiosity tendency. Look for genuine intellectual curiosity, desire to understand deeply, questioning of assumptions, and pursuit of knowledge. Note: This is generally a POSITIVE tendency that counteracts other biases."
    :severity :low
    :positive true
    :category :psychology}
   
   {:id 7
    :name "Kantian Fairness Tendency"
    :short-name "Fairness Bias"
    :description "People expect fair exchanges and become very upset when fairness is violated. This can be exploited or lead to irrational behavior."
    :keywords ["fair" "unfair" "just" "unjust" "equal" "deserve" "entitled" "should" "ought" "right" "wrong" "cheat" "exploit"]
    :patterns [#"(?i)(not|un)fair"
               #"(?i)(deserve|entitled|should|ought).{0,10}(get|receive|have)"
               #"(?i)(cheat|exploit|take advantage)"
               #"(?i)(equal|same).{0,10}(treatment|pay|opportunity)"]
    :antidotes ["Recognize fairness is subjective" "Focus on outcomes not process" "Accept some unfairness as inevitable"]
    :examples ["Rejecting beneficial deals perceived as unfair"
               "Ultimatum game behavior"
               "Employee resentment over pay disparities"
               "Customer outrage over price increases"]
    :llm-prompt "Analyze this text for evidence of Kantian fairness tendency. Look for situations where concerns about fairness may be overriding rational economic decisions, where perceived unfairness is causing disproportionate reactions, or where fairness expectations are being exploited."
    :severity :medium
    :category :psychology}
   
   {:id 8
    :name "Envy/Jealousy Tendency"
    :short-name "Envy Bias"
    :description "Envy is a powerful emotion that can drive irrational behavior. It's often hidden but very influential."
    :keywords ["envy" "jealous" "covet" "resent" "compare" "better than" "worse than" "keeping up" "status" "prestige" "outdo"]
    :patterns [#"(?i)(envy|jealous|covet|resent)"
               #"(?i)(better|worse).{0,10}than.{0,20}(them|others|neighbor)"
               #"(?i)(keeping up|compete|outdo).{0,20}(jones|neighbor|peer)"]
    :antidotes ["Focus on absolute not relative gains" "Practice gratitude" "Avoid social comparison"]
    :examples ["Buying things to impress others"
               "Career decisions driven by peer comparison"
               "Investment FOMO"
               "Lifestyle inflation"]
    :llm-prompt "Analyze this text for evidence of envy/jealousy tendency. Look for situations where social comparison, status concerns, or resentment of others' success may be driving decisions or causing distorted thinking."
    :severity :medium
    :category :psychology}
   
   {:id 9
    :name "Reciprocation Tendency"
    :short-name "Reciprocity Bias"
    :description "Humans have a strong tendency to reciprocate both favors and disfavors. This can be exploited through small initial gifts."
    :keywords ["reciprocate" "return favor" "payback" "owe" "obligation" "gift" "favor" "free sample" "quid pro quo" "tit for tat"]
    :patterns [#"(?i)(reciprocat|return.{0,5}favor|pay.?back)"
               #"(?i)(owe|obligat).{0,20}(them|something|favor)"
               #"(?i)(free|complimentary).{0,10}(sample|gift|trial)"
               #"(?i)quid.{0,5}pro.{0,5}quo"]
    :antidotes ["Recognize manipulation tactics" "Evaluate offers on merit" "Don't feel obligated by unsolicited gifts"]
    :examples ["Free samples leading to purchases"
               "Political donations expecting favors"
               "Revenge cycles"
               "Gift-giving in business negotiations"]
    :llm-prompt "Analyze this text for evidence of reciprocation tendency. Look for situations where gifts, favors, or concessions may be creating feelings of obligation, or where reciprocity is being used as a manipulation tactic."
    :severity :high
    :category :psychology}
   
   {:id 10
    :name "Influence-from-Mere-Association Tendency"
    :short-name "Association Bias"
    :description "We tend to be influenced by associations, even when they're irrelevant. This includes shooting the messenger."
    :keywords ["associate" "connection" "linked" "related" "remind" "similar" "messenger" "bearer" "news" "brand" "celebrity" "endorsement"]
    :patterns [#"(?i)(associat|connect|link|relat).{0,20}(with|to)"
               #"(?i)(remind|similar|like).{0,20}(previous|past|other)"
               #"(?i)(shoot|blame|punish).{0,10}(messenger|bearer)"
               #"(?i)(celebrity|famous|star).{0,10}(endorse|promote|recommend)"]
    :antidotes ["Evaluate information on merit" "Don't shoot the messenger" "Be aware of conditioning"]
    :examples ["Disliking bearers of bad news"
               "Celebrity endorsements influencing purchases"
               "Avoiding places associated with bad memories"
               "Brand associations affecting perception"]
    :llm-prompt "Analyze this text for evidence of influence-from-mere-association tendency. Look for situations where irrelevant associations are affecting judgment, where messengers are being blamed for their messages, or where conditioning is creating irrational preferences."
    :severity :medium
    :category :psychology}
   
   {:id 11
    :name "Simple, Pain-Avoiding Psychological Denial"
    :short-name "Denial"
    :description "The brain protects itself from painful realities through denial. This can prevent necessary action."
    :keywords ["denial" "ignore" "refuse to see" "blind to" "won't accept" "can't believe" "impossible" "not happening" "wishful thinking"]
    :patterns [#"(?i)(deny|denial|ignore|refuse)"
               #"(?i)(can't|won't|refuse to).{0,10}(believe|accept|see)"
               #"(?i)(impossible|not happening|wishful)"
               #"(?i)(blind|oblivious).{0,10}(to|about)"]
    :antidotes ["Face reality squarely" "Seek objective feedback" "Use checklists to force confrontation"]
    :examples ["Ignoring health symptoms"
               "Refusing to acknowledge business problems"
               "Denial about relationship issues"
               "Ignoring market signals"]
    :llm-prompt "Analyze this text for evidence of psychological denial. Look for situations where painful realities are being ignored, where there's refusal to acknowledge obvious problems, or where wishful thinking is replacing objective assessment."
    :severity :high
    :category :psychology}
   
   {:id 12
    :name "Excessive Self-Regard Tendency"
    :short-name "Overconfidence"
    :description "People tend to overestimate their own abilities and the value of their possessions. This leads to overconfidence."
    :keywords ["confident" "certain" "sure" "best" "superior" "expert" "special" "unique" "talented" "skilled" "endowment" "mine" "my"]
    :patterns [#"(?i)(over|too).{0,5}confident"
               #"(?i)(best|superior|expert|special|unique)"
               #"(?i)(my|mine|our).{0,10}(better|superior|special)"
               #"(?i)(certain|sure|know).{0,10}(will|can|able)"]
    :antidotes ["Seek disconfirming evidence" "Get objective feedback" "Study base rates" "Practice humility"]
    :examples ["Overestimating investment skill"
               "Endowment effect in negotiations"
               "Overconfidence in predictions"
               "Illusory superiority"]
    :llm-prompt "Analyze this text for evidence of excessive self-regard tendency. Look for overconfidence, inflated self-assessment, endowment effect (overvaluing what one owns), or failure to recognize personal limitations."
    :severity :high
    :category :psychology}
   
   {:id 13
    :name "Over-Optimism Tendency"
    :short-name "Optimism Bias"
    :description "People tend to be overly optimistic about outcomes, especially for themselves. This leads to poor planning."
    :keywords ["optimistic" "hopeful" "expect" "plan" "forecast" "project" "estimate" "predict" "best case" "upside" "potential"]
    :patterns [#"(?i)(over|too).{0,5}optimis"
               #"(?i)(expect|hope|plan|forecast).{0,20}(growth|success|profit)"
               #"(?i)(best|bull).{0,5}case"
               #"(?i)(potential|upside|opportunity)"]
    :antidotes ["Use base rates" "Consider worst case" "Add margin of safety" "Use pre-mortem analysis"]
    :examples ["Underestimating project timelines"
               "Overestimating investment returns"
               "Planning fallacy"
               "Ignoring downside risks"]
    :llm-prompt "Analyze this text for evidence of over-optimism tendency. Look for unrealistic expectations, planning fallacy, underestimation of risks, or failure to consider base rates and worst-case scenarios."
    :severity :high
    :category :psychology}
   
   {:id 14
    :name "Deprival-Superreaction Tendency"
    :short-name "Loss Aversion"
    :description "People react more strongly to losses than equivalent gains. Near-misses are particularly painful."
    :keywords ["loss" "lose" "lost" "take away" "remove" "deprive" "miss" "almost" "nearly" "close" "sunk cost" "hold on"]
    :patterns [#"(?i)(loss|lose|lost|deprive)"
               #"(?i)(take|taken).{0,10}away"
               #"(?i)(almost|nearly|close).{0,10}(won|had|got)"
               #"(?i)sunk.{0,5}cost"
               #"(?i)(hold|holding).{0,10}on"]
    :antidotes ["Focus on opportunity cost" "Ignore sunk costs" "Frame as gains vs non-gains" "Use pre-commitment"]
    :examples ["Holding losing investments too long"
               "Sunk cost fallacy"
               "Gambling after near-misses"
               "Overreaction to price drops"]
    :llm-prompt "Analyze this text for evidence of deprival-superreaction tendency. Look for loss aversion, sunk cost fallacy, overreaction to losses or near-misses, or irrational attachment to possessions or positions."
    :severity :high
    :category :psychology}
   
   {:id 15
    :name "Social-Proof Tendency"
    :short-name "Social Proof"
    :description "People tend to think and act as they see others thinking and acting. This is especially strong in uncertainty."
    :keywords ["everyone" "everybody" "popular" "trend" "crowd" "herd" "follow" "conform" "peer" "social" "consensus" "majority"]
    :patterns [#"(?i)(everyone|everybody|all).{0,10}(doing|buying|saying)"
               #"(?i)(popular|trending|viral|hot)"
               #"(?i)(crowd|herd|follow|conform)"
               #"(?i)(peer|social).{0,10}(pressure|influence)"
               #"(?i)(consensus|majority|most people)"]
    :antidotes ["Think independently" "Be contrarian when warranted" "Question consensus" "Avoid crowds"]
    :examples ["Market bubbles"
               "Fashion trends"
               "Bystander effect"
               "Groupthink in organizations"]
    :llm-prompt "Analyze this text for evidence of social-proof tendency. Look for herd behavior, conformity, following trends without independent analysis, or decisions driven by what others are doing rather than independent judgment."
    :severity :high
    :category :psychology}
   
   {:id 16
    :name "Contrast-Misreaction Tendency"
    :short-name "Contrast Effect"
    :description "Perception is relative, not absolute. Small differences can be missed when compared to large ones."
    :keywords ["compare" "contrast" "relative" "compared to" "versus" "than" "anchor" "reference" "benchmark" "originally" "was"]
    :patterns [#"(?i)(compar|contrast).{0,20}(to|with|versus)"
               #"(?i)(relative|compared).{0,10}to"
               #"(?i)(anchor|reference|benchmark)"
               #"(?i)(originally|was|before).{0,20}(now|today|current)"]
    :antidotes ["Use absolute standards" "Be aware of anchoring" "Evaluate in isolation" "Use multiple reference points"]
    :examples ["Anchoring in negotiations"
               "Boiling frog syndrome"
               "Real estate pricing tactics"
               "Gradual ethical erosion"]
    :llm-prompt "Analyze this text for evidence of contrast-misreaction tendency. Look for anchoring effects, relative comparisons masking absolute values, gradual changes being missed, or manipulation through strategic comparisons."
    :severity :medium
    :category :psychology}
   
   {:id 17
    :name "Stress-Influence Tendency"
    :short-name "Stress Bias"
    :description "Stress can cause both extreme reactions and complete shutdown. Light stress can improve performance, but heavy stress impairs it."
    :keywords ["stress" "pressure" "urgent" "emergency" "crisis" "panic" "anxiety" "overwhelm" "deadline" "rush" "hurry"]
    :patterns [#"(?i)(stress|pressure|urgent|emergency|crisis)"
               #"(?i)(panic|anxiety|overwhelm)"
               #"(?i)(deadline|rush|hurry)"
               #"(?i)(under|extreme).{0,10}(pressure|stress)"]
    :antidotes ["Reduce stress before decisions" "Build in buffers" "Practice stress management" "Avoid time pressure"]
    :examples ["Poor decisions under deadline pressure"
               "Panic selling in market crashes"
               "Stress-induced health problems"
               "Emergency decision errors"]
    :llm-prompt "Analyze this text for evidence of stress-influence tendency. Look for decisions made under excessive pressure, panic reactions, stress-induced errors, or situations where stress may be impairing judgment."
    :severity :high
    :category :psychology}
   
   {:id 18
    :name "Availability-Misweighing Tendency"
    :short-name "Availability Bias"
    :description "We overweight what's easily available in memory, especially vivid, recent, or emotional information."
    :keywords ["remember" "recall" "recent" "vivid" "dramatic" "memorable" "news" "media" "headline" "story" "example"]
    :patterns [#"(?i)(remember|recall|think of)"
               #"(?i)(recent|vivid|dramatic|memorable)"
               #"(?i)(news|media|headline)"
               #"(?i)(example|story|case).{0,10}(of|about)"]
    :antidotes ["Use base rates and statistics" "Seek representative samples" "Use checklists" "Consult data not memory"]
    :examples ["Overestimating plane crash risk after news coverage"
               "Investment decisions based on recent performance"
               "Fear of rare but vivid dangers"
               "Recency bias in evaluations"]
    :llm-prompt "Analyze this text for evidence of availability-misweighing tendency. Look for overweighting of recent, vivid, or easily recalled information, neglect of base rates, or decisions driven by memorable examples rather than representative data."
    :severity :high
    :category :psychology}
   
   {:id 19
    :name "Use-It-or-Lose-It Tendency"
    :short-name "Skill Atrophy"
    :description "Skills deteriorate without practice. This applies to both physical and mental skills."
    :keywords ["practice" "skill" "atrophy" "rust" "forget" "unused" "dormant" "maintain" "exercise" "train" "rehearse"]
    :patterns [#"(?i)(practice|skill|ability).{0,20}(atrophy|rust|decline)"
               #"(?i)(forget|forgot|forgotten).{0,10}how"
               #"(?i)(unused|dormant|neglect)"
               #"(?i)(maintain|exercise|train|rehearse)"]
    :antidotes ["Regular practice" "Use checklists as reminders" "Continuous learning" "Periodic review"]
    :examples ["Forgetting foreign languages"
               "Skills declining after career change"
               "Mental models fading without use"
               "Physical fitness declining"]
    :llm-prompt "Analyze this text for evidence of use-it-or-lose-it tendency. Look for skill atrophy, forgotten knowledge, neglected abilities, or the need for continuous practice to maintain competence."
    :severity :low
    :category :psychology}
   
   {:id 20
    :name "Drug-Misinfluence Tendency"
    :short-name "Substance Influence"
    :description "Drugs and alcohol impair judgment and can lead to addiction. This includes behavioral addictions."
    :keywords ["drug" "alcohol" "addiction" "substance" "intoxicated" "impaired" "habit" "compulsion" "dependence" "withdrawal"]
    :patterns [#"(?i)(drug|alcohol|substance)"
               #"(?i)(addict|addiction|dependent)"
               #"(?i)(intoxicat|impair|under the influence)"
               #"(?i)(habit|compulsion|craving)"]
    :antidotes ["Avoid addictive substances" "Recognize addiction patterns" "Seek help early" "Build healthy habits"]
    :examples ["Decisions made while intoxicated"
               "Addiction affecting judgment"
               "Behavioral addictions (gambling, social media)"
               "Withdrawal affecting decisions"]
    :llm-prompt "Analyze this text for evidence of drug-misinfluence tendency. Look for substance use affecting judgment, addiction patterns, or behavioral compulsions that may be impairing decision-making."
    :severity :high
    :category :psychology}
   
   {:id 21
    :name "Senescence-Misinfluence Tendency"
    :short-name "Aging Bias"
    :description "Aging can impair cognitive function. However, continuous learning can help maintain mental acuity."
    :keywords ["age" "aging" "old" "elderly" "senior" "decline" "cognitive" "memory" "dementia" "experience" "wisdom"]
    :patterns [#"(?i)(age|aging|old|elderly|senior)"
               #"(?i)(cognitive|mental).{0,10}(decline|impair)"
               #"(?i)(memory|dementia|senile)"
               #"(?i)(experience|wisdom|veteran)"]
    :antidotes ["Continuous learning" "Mental exercise" "Physical exercise" "Social engagement" "Recognize limitations"]
    :examples ["Cognitive decline affecting decisions"
               "Overreliance on past experience"
               "Resistance to new ideas with age"
               "Wisdom from experience"]
    :llm-prompt "Analyze this text for evidence of senescence-misinfluence tendency. Look for age-related cognitive changes affecting judgment, overreliance on past experience, or resistance to new information due to age."
    :severity :medium
    :category :psychology}
   
   {:id 22
    :name "Authority-Misinfluence Tendency"
    :short-name "Authority Bias"
    :description "People tend to follow authority figures, even when they shouldn't. This can lead to blind obedience."
    :keywords ["authority" "expert" "leader" "boss" "official" "credential" "title" "position" "rank" "obey" "follow" "trust"]
    :patterns [#"(?i)(authority|expert|leader|boss|official)"
               #"(?i)(credential|title|position|rank)"
               #"(?i)(obey|follow|trust).{0,20}(authority|expert|leader)"
               #"(?i)(because|since).{0,10}(expert|authority|leader).{0,10}(said|says)"]
    :antidotes ["Question authority" "Verify independently" "Consider incentives of authorities" "Seek multiple opinions"]
    :examples ["Milgram experiment obedience"
               "Following bad advice from experts"
               "Corporate hierarchy preventing dissent"
               "Medical authority affecting patient decisions"]
    :llm-prompt "Analyze this text for evidence of authority-misinfluence tendency. Look for blind obedience to authority, unquestioning acceptance of expert opinions, or situations where authority is being used to override independent judgment."
    :severity :high
    :category :psychology}
   
   {:id 23
    :name "Twaddle Tendency"
    :short-name "Nonsense Tolerance"
    :description "People produce and tolerate a lot of nonsense. This wastes time and can lead to poor decisions."
    :keywords ["nonsense" "twaddle" "bullshit" "jargon" "buzzword" "meaningless" "empty" "vague" "unclear" "confusing" "obfuscate"]
    :patterns [#"(?i)(nonsense|twaddle|bullshit|gibberish)"
               #"(?i)(jargon|buzzword|meaningless)"
               #"(?i)(vague|unclear|confusing|obfuscat)"
               #"(?i)(empty|hollow).{0,10}(words|rhetoric|promise)"]
    :antidotes ["Demand clarity" "Ask for specifics" "Cut through jargon" "Simplify communication"]
    :examples ["Corporate jargon hiding problems"
               "Political doublespeak"
               "Academic obscurantism"
               "Marketing buzzwords"]
    :llm-prompt "Analyze this text for evidence of twaddle tendency. Look for meaningless jargon, vague language, obfuscation, or nonsense that may be hiding lack of substance or clear thinking."
    :severity :medium
    :category :psychology}
   
   {:id 24
    :name "Reason-Respecting Tendency"
    :short-name "Reason Seeking"
    :description "People comply better when given reasons, even bad ones. The word 'because' is powerful."
    :keywords ["because" "reason" "why" "explain" "justify" "rationale" "logic" "argument" "therefore" "thus" "hence"]
    :patterns [#"(?i)because.{0,30}(will|can|should|must)"
               #"(?i)(reason|rationale|logic|argument).{0,10}(is|was|being)"
               #"(?i)(explain|justify|rationalize)"
               #"(?i)(therefore|thus|hence|so)"]
    :antidotes ["Evaluate reasons critically" "Don't accept bad reasons" "Ask for evidence" "Check logic"]
    :examples ["Compliance increasing with any reason given"
               "Accepting weak justifications"
               "Rationalization of poor decisions"
               "Persuasion through pseudo-logic"]
    :llm-prompt "Analyze this text for evidence of reason-respecting tendency. Look for acceptance of weak or circular reasoning, compliance based on any reason given, or rationalization that sounds logical but isn't substantive."
    :severity :medium
    :category :psychology}
   
   {:id 25
    :name "Lollapalooza Tendency"
    :short-name "Lollapalooza Effect"
    :description "Multiple psychological tendencies acting together create extreme outcomes. The whole is greater than the sum of parts."
    :keywords ["lollapalooza" "confluence" "combination" "multiple" "compound" "synergy" "cascade" "perfect storm" "convergence" "amplify"]
    :patterns [#"(?i)lollapalooza"
               #"(?i)(confluence|combination|convergence).{0,20}(factor|cause|tendency)"
               #"(?i)(multiple|several|many).{0,10}(bias|tendency|factor)"
               #"(?i)(compound|amplif|cascade|snowball)"
               #"(?i)perfect.{0,5}storm"]
    :antidotes ["Identify all tendencies at play" "Use checklists" "Seek diverse perspectives" "Slow down in complex situations"]
    :examples ["Enron collapse (multiple biases)"
               "Market bubbles and crashes"
               "Cult behavior"
               "Corporate fraud cascades"]
    :llm-prompt "Analyze this text for evidence of lollapalooza effect. Look for situations where multiple psychological tendencies are combining to create extreme outcomes, cascading effects, or 'perfect storm' scenarios where several biases reinforce each other."
    :severity :critical
    :category :psychology}])

;; =============================================================================
;; DETECTION FUNCTIONS
;; =============================================================================

(defn keyword-score
  "Calculate keyword density score for a tendency"
  [text keywords]
  (let [text-lower (str/lower-case text)
        word-count (count (str/split text #"\s+"))
        matches (reduce (fn [acc kw]
                          (+ acc (count (re-seq (re-pattern (str "(?i)" kw)) text))))
                        0 keywords)]
    (if (zero? word-count)
      0.0
      (min 1.0 (* 10.0 (/ matches word-count))))))

(defn pattern-score
  "Calculate pattern match score for a tendency"
  [text patterns]
  (let [matches (reduce (fn [acc pattern]
                          (+ acc (if (re-find pattern text) 1 0)))
                        0 patterns)]
    (if (empty? patterns)
      0.0
      (/ matches (count patterns)))))

(defn detect-tendency
  "Detect a single tendency in text"
  [text tendency]
  (let [kw-score (keyword-score text (:keywords tendency))
        pt-score (pattern-score text (:patterns tendency))
        combined-score (+ (* 0.4 kw-score) (* 0.6 pt-score))]
    {:id (:id tendency)
     :name (:name tendency)
     :short-name (:short-name tendency)
     :score combined-score
     :keyword-score kw-score
     :pattern-score pt-score
     :confidence (cond
                   (>= combined-score 0.7) :high
                   (>= combined-score 0.4) :medium
                   (>= combined-score 0.2) :low
                   :else :none)
     :severity (:severity tendency)
     :positive (:positive tendency false)
     :antidotes (:antidotes tendency)
     :llm-prompt (:llm-prompt tendency)}))

(defn detect-all-tendencies
  "Detect all 25 tendencies in text"
  [text]
  (let [results (map #(detect-tendency text %) munger-25-tendencies)
        detected (filter #(> (:score %) 0.2) results)
        sorted (sort-by :score > detected)]
    {:total-detected (count detected)
     :tendencies sorted
     :lollapalooza? (>= (count (filter #(>= (:score %) 0.5) detected)) 3)
     :top-3 (take 3 sorted)
     :high-severity (filter #(= :high (:severity %)) detected)}))

(defn detect-lollapalooza
  "Specifically detect lollapalooza effects (3+ tendencies converging)"
  [text]
  (let [results (detect-all-tendencies text)
        high-scoring (filter #(>= (:score %) 0.5) (:tendencies results))]
    (when (>= (count high-scoring) 3)
      {:lollapalooza true
       :converging-tendencies (map :short-name high-scoring)
       :severity :critical
       :warning "LOLLAPALOOZA DETECTED: Multiple psychological tendencies converging"
       :recommendation "Exercise extreme caution. Seek independent verification. Use comprehensive checklist."})))

;; =============================================================================
;; ELECTRIC REACTIVE COMPONENTS
;; =============================================================================

(e/defn TendencyDetector [text]
  "Reactive tendency detection component"
  (e/server
    (let [results (e/offload #(detect-all-tendencies text))]
      results)))

(e/defn LollapaloozaAlert [text]
  "Reactive lollapalooza detection with alert"
  (e/server
    (let [lolla (e/offload #(detect-lollapalooza text))]
      (when lolla
        (e/client
          ;; Trigger alert in UI
          lolla)))))

;; =============================================================================
;; EXPORT FOR USE IN OTHER MODULES
;; =============================================================================

(def tendency-names
  "List of all tendency names for reference"
  (mapv :name munger-25-tendencies))

(def tendency-by-id
  "Map of tendencies by ID"
  (into {} (map (juxt :id identity) munger-25-tendencies)))

(def tendency-by-short-name
  "Map of tendencies by short name"
  (into {} (map (juxt :short-name identity) munger-25-tendencies)))
