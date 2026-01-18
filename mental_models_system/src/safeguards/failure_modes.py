"""
Mental Model Failure Modes & Safeguards System

For each mental model, this module identifies 5 ways the algorithm could fail
when applying that model, and provides safeguards to prevent those failures.

"Invert, always invert. Turn a situation or problem upside down.
Look at it backward." - Charlie Munger

This is the application of inversion to our own system - identifying
how each mental model could lead us astray.

Structure for each model:
1. Model ID and Name
2. Five Failure Modes (how applying this model could fail)
3. Safeguards (how to prevent each failure)
4. Detection Signals (how to know if you're failing)
5. Recovery Actions (what to do if failure detected)
"""

from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple
from enum import Enum
import json
from pathlib import Path


class FailureSeverity(Enum):
    """Severity levels for failure modes."""
    LOW = "low"           # Minor impact, easily recoverable
    MEDIUM = "medium"     # Moderate impact, requires attention
    HIGH = "high"         # Significant impact, immediate action needed
    CRITICAL = "critical" # Catastrophic potential, must prevent


class FailureCategory(Enum):
    """Categories of failure modes."""
    MISAPPLICATION = "misapplication"       # Using model in wrong context
    OVERRELIANCE = "overreliance"           # Relying too heavily on single model
    MISINTERPRETATION = "misinterpretation" # Misunderstanding the model
    DATA_QUALITY = "data_quality"           # Poor input data
    TIMING = "timing"                       # Wrong timing of application
    SCOPE = "scope"                         # Wrong scope/scale
    INTERACTION = "interaction"             # Negative interaction with other models
    BLIND_SPOT = "blind_spot"               # Model creates blind spots


@dataclass
class FailureMode:
    """A specific way a mental model application could fail."""
    id: str
    description: str
    category: FailureCategory
    severity: FailureSeverity
    example: str
    detection_signals: List[str]
    safeguards: List[str]
    recovery_actions: List[str]


@dataclass
class ModelFailureAnalysis:
    """Complete failure analysis for a mental model."""
    model_id: int
    model_name: str
    failure_modes: List[FailureMode]
    meta_safeguards: List[str]  # Overall safeguards for this model
    complementary_models: List[int]  # Models that help prevent failures
    antagonistic_models: List[int]  # Models that may conflict


# =============================================================================
# COMPLETE FAILURE MODES DATABASE
# 129 Models × 5 Failure Modes = 645 Failure Modes
# =============================================================================

FAILURE_MODES_DATABASE: Dict[int, ModelFailureAnalysis] = {}

# -----------------------------------------------------------------------------
# CATEGORY 1: PSYCHOLOGY - TENDENCIES & BIASES (Models 1-34)
# -----------------------------------------------------------------------------

FAILURE_MODES_DATABASE[1] = ModelFailureAnalysis(
    model_id=1,
    model_name="Reward and Punishment Superresponse Tendency",
    failure_modes=[
        FailureMode(
            id="1_F1",
            description="Assuming all behavior is incentive-driven when intrinsic motivation dominates",
            category=FailureCategory.MISAPPLICATION,
            severity=FailureSeverity.MEDIUM,
            example="Offering bonuses to creative workers may actually reduce creativity (overjustification effect)",
            detection_signals=["Performance drops after incentive introduction", "Resistance to incentive programs", "Loss of intrinsic motivation indicators"],
            safeguards=["Assess baseline motivation type before designing incentives", "Test incentive impact on small scale first", "Monitor for crowding-out effects"],
            recovery_actions=["Remove counterproductive incentives gradually", "Restore autonomy and purpose", "Rebuild intrinsic motivation"]
        ),
        FailureMode(
            id="1_F2",
            description="Underestimating delayed or hidden incentive effects",
            category=FailureCategory.TIMING,
            severity=FailureSeverity.HIGH,
            example="Wells Fargo's sales incentives led to massive fraud years later",
            detection_signals=["Short-term metrics improving while long-term deteriorates", "Gaming behaviors emerging", "Quality declining despite quantity increasing"],
            safeguards=["Design incentives with long-term clawback provisions", "Monitor leading indicators of gaming", "Include quality metrics alongside quantity"],
            recovery_actions=["Audit for gaming behavior", "Restructure incentive timing", "Implement claw-backs"]
        ),
        FailureMode(
            id="1_F3",
            description="Ignoring non-monetary incentives (status, autonomy, purpose)",
            category=FailureCategory.SCOPE,
            severity=FailureSeverity.MEDIUM,
            example="High-paying job with no autonomy loses talent to lower-paying meaningful work",
            detection_signals=["High turnover despite competitive pay", "Low engagement scores", "Talent leaving for 'worse' offers"],
            safeguards=["Map complete incentive landscape including non-monetary", "Survey for motivation factors", "Design holistic reward systems"],
            recovery_actions=["Conduct stay interviews", "Enhance non-monetary rewards", "Increase autonomy and purpose"]
        ),
        FailureMode(
            id="1_F4",
            description="Creating perverse incentives through poorly designed metrics",
            category=FailureCategory.MISAPPLICATION,
            severity=FailureSeverity.CRITICAL,
            example="Measuring teachers by test scores leads to teaching to the test, not learning",
            detection_signals=["Goodhart's Law effects appearing", "Metric improving while underlying goal suffers", "Creative compliance behaviors"],
            safeguards=["Use multiple diverse metrics", "Include qualitative assessments", "Rotate metrics periodically"],
            recovery_actions=["Redesign metrics immediately", "Assess damage from perverse incentives", "Implement balanced scorecard"]
        ),
        FailureMode(
            id="1_F5",
            description="Assuming your own incentive analysis is unbiased",
            category=FailureCategory.BLIND_SPOT,
            severity=FailureSeverity.HIGH,
            example="Analyst recommending stocks they own without recognizing their bias",
            detection_signals=["Consistent recommendations favoring own interests", "Dismissing conflicts of interest", "Rationalizing self-serving conclusions"],
            safeguards=["Disclose all personal incentives", "Seek independent review", "Apply model to yourself first"],
            recovery_actions=["Recuse from conflicted decisions", "Get external audit", "Implement blind analysis procedures"]
        )
    ],
    meta_safeguards=[
        "Always ask: 'What are MY incentives in this analysis?'",
        "Consider both monetary and non-monetary incentives",
        "Look for second and third-order incentive effects",
        "Test incentive hypotheses before full implementation"
    ],
    complementary_models=[30, 60, 59],  # Incentive-Caused Bias, Agency Problem, Incentive Alignment
    antagonistic_models=[6]  # Curiosity Tendency (intrinsic vs extrinsic)
)

FAILURE_MODES_DATABASE[2] = ModelFailureAnalysis(
    model_id=2,
    model_name="Liking/Loving Tendency",
    failure_modes=[
        FailureMode(
            id="2_F1",
            description="Dismissing valid criticism of liked people/things",
            category=FailureCategory.BLIND_SPOT,
            severity=FailureSeverity.HIGH,
            example="Ignoring red flags about a charismatic CEO you admire",
            detection_signals=["Defensive reactions to criticism", "Explaining away negative evidence", "Surrounding yourself with similar admirers"],
            safeguards=["Actively seek criticism from trusted skeptics", "Create devil's advocate role", "Document both positives and negatives"],
            recovery_actions=["Force yourself to list 5 negatives", "Consult someone who dislikes the subject", "Re-evaluate with fresh eyes"]
        ),
        FailureMode(
            id="2_F2",
            description="Making decisions based on personal relationships rather than merit",
            category=FailureCategory.MISAPPLICATION,
            severity=FailureSeverity.HIGH,
            example="Promoting a friend over a more qualified candidate",
            detection_signals=["Pattern of favoring friends in decisions", "Discomfort when asked to justify objectively", "Others perceiving favoritism"],
            safeguards=["Use blind evaluation processes", "Require documented justification", "Include independent evaluators"],
            recovery_actions=["Recuse from decisions involving friends", "Implement structured decision criteria", "Audit past decisions for bias"]
        ),
        FailureMode(
            id="2_F3",
            description="Overvaluing assets because of emotional attachment",
            category=FailureCategory.MISINTERPRETATION,
            severity=FailureSeverity.MEDIUM,
            example="Refusing to sell inherited stock at fair value due to sentimental attachment",
            detection_signals=["Valuation significantly above market", "Emotional language in investment thesis", "Reluctance to consider selling"],
            safeguards=["Get independent valuations", "Separate emotional and financial decisions", "Ask: 'Would I buy this today at this price?'"],
            recovery_actions=["Treat inherited assets as if just received cash", "Set objective sell criteria in advance", "Consult unemotional advisor"]
        ),
        FailureMode(
            id="2_F4",
            description="Extending trust without verification to liked parties",
            category=FailureCategory.OVERRELIANCE,
            severity=FailureSeverity.CRITICAL,
            example="Bernie Madoff's investors trusted him because they liked him",
            detection_signals=["Skipping due diligence for friends", "Feeling uncomfortable asking tough questions", "Assuming good intent equals good execution"],
            safeguards=["Apply same verification to all parties", "Trust but verify always", "Separate liking from trusting"],
            recovery_actions=["Implement verification regardless of relationship", "Review all trusted-but-unverified relationships", "Create verification checklist"]
        ),
        FailureMode(
            id="2_F5",
            description="Failing to recognize when liking is being manufactured",
            category=FailureCategory.BLIND_SPOT,
            severity=FailureSeverity.MEDIUM,
            example="Falling for sales tactics designed to create artificial rapport",
            detection_signals=["Rapid feelings of connection with strangers", "Flattery preceding requests", "Similarity claims that seem too convenient"],
            safeguards=["Be suspicious of instant rapport", "Recognize influence techniques", "Slow down decisions after feeling liked"],
            recovery_actions=["Add cooling-off period to decisions", "Consult someone not exposed to the charm", "Focus on substance over style"]
        )
    ],
    meta_safeguards=[
        "Assume you like things more than you should",
        "Create distance before important decisions about liked entities",
        "Use structured evaluation to override emotional preference",
        "Ask: 'Would I feel the same if this were a stranger?'"
    ],
    complementary_models=[3, 46, 35],  # Disliking Tendency, Falsification, Inversion
    antagonistic_models=[]
)

FAILURE_MODES_DATABASE[3] = ModelFailureAnalysis(
    model_id=3,
    model_name="Disliking/Hating Tendency",
    failure_modes=[
        FailureMode(
            id="3_F1",
            description="Missing opportunities because of dislike for the source",
            category=FailureCategory.BLIND_SPOT,
            severity=FailureSeverity.HIGH,
            example="Rejecting a good idea because it came from a competitor or enemy",
            detection_signals=["Automatic rejection of certain sources", "Not evaluating ideas on merit", "Pattern of dismissing specific people"],
            safeguards=["Evaluate ideas blind to source", "Force consideration of enemy's best arguments", "Seek truth regardless of source"],
            recovery_actions=["Re-evaluate rejected ideas from disliked sources", "Implement blind idea evaluation", "Challenge yourself to find value in enemy's work"]
        ),
        FailureMode(
            id="3_F2",
            description="Underestimating threats from disliked competitors",
            category=FailureCategory.MISINTERPRETATION,
            severity=FailureSeverity.CRITICAL,
            example="Blockbuster dismissing Netflix because they disliked the model",
            detection_signals=["Dismissive language about competitors", "Focusing on competitor weaknesses only", "Surprise when disliked competitor succeeds"],
            safeguards=["Assign someone to steelman competitor's position", "Track competitor objectively", "Assume competitor is smart"],
            recovery_actions=["Conduct objective competitive analysis", "Study competitor's successes", "Prepare for scenarios where competitor wins"]
        ),
        FailureMode(
            id="3_F3",
            description="Guilt by association - rejecting good things connected to bad",
            category=FailureCategory.MISAPPLICATION,
            severity=FailureSeverity.MEDIUM,
            example="Rejecting valid research because it was funded by a disliked organization",
            detection_signals=["Evaluating based on associations not substance", "Tribal thinking patterns", "Ad hominem reasoning"],
            safeguards=["Evaluate substance independently of source", "Separate message from messenger", "Apply consistent standards"],
            recovery_actions=["Re-examine rejected items for merit", "Create source-blind evaluation process", "Challenge tribal assumptions"]
        ),
        FailureMode(
            id="3_F4",
            description="Escalating conflicts due to mutual dislike spirals",
            category=FailureCategory.INTERACTION,
            severity=FailureSeverity.HIGH,
            example="Business negotiations failing because both sides dislike each other",
            detection_signals=["Increasing hostility over time", "Tit-for-tat negative behaviors", "Loss of ability to cooperate"],
            safeguards=["Use intermediaries when dislike is strong", "Focus on interests not positions", "Implement cooling-off periods"],
            recovery_actions=["Bring in neutral mediator", "Reset relationship with fresh start", "Focus on shared interests"]
        ),
        FailureMode(
            id="3_F5",
            description="Self-fulfilling prophecy of dislike causing bad outcomes",
            category=FailureCategory.MISAPPLICATION,
            severity=FailureSeverity.MEDIUM,
            example="Treating disliked employee poorly, causing them to actually underperform",
            detection_signals=["Disliked parties consistently underperforming", "Negative expectations being confirmed", "Others succeeding with same people"],
            safeguards=["Examine if your behavior is causing outcomes", "Give disliked parties fair chance", "Monitor for Pygmalion effects"],
            recovery_actions=["Reset expectations consciously", "Provide equal support to all", "Have others manage disliked parties"]
        )
    ],
    meta_safeguards=[
        "Actively seek virtues in those you dislike",
        "Evaluate all ideas blind to source when possible",
        "Assume disliked parties may have valid points",
        "Check if dislike is causing the problems you observe"
    ],
    complementary_models=[2, 46, 45],  # Liking Tendency, Falsification, Hanlon's Razor
    antagonistic_models=[]
)

FAILURE_MODES_DATABASE[4] = ModelFailureAnalysis(
    model_id=4,
    model_name="Doubt-Avoidance Tendency",
    failure_modes=[
        FailureMode(
            id="4_F1",
            description="Making premature decisions to eliminate uncomfortable uncertainty",
            category=FailureCategory.TIMING,
            severity=FailureSeverity.HIGH,
            example="Hiring the first acceptable candidate to end the search discomfort",
            detection_signals=["Relief after quick decisions", "Discomfort with open questions", "Rushing to closure"],
            safeguards=["Set minimum deliberation times", "Embrace uncertainty as information", "Create structured decision timelines"],
            recovery_actions=["Reopen decisions made too quickly", "Extend timelines for important choices", "Practice sitting with uncertainty"]
        ),
        FailureMode(
            id="4_F2",
            description="False certainty - treating uncertain conclusions as certain",
            category=FailureCategory.MISINTERPRETATION,
            severity=FailureSeverity.CRITICAL,
            example="Treating a 60% probability as a sure thing because uncertainty is uncomfortable",
            detection_signals=["Binary thinking about probabilistic outcomes", "Overconfident language", "Surprise when 'certain' things don't happen"],
            safeguards=["Force probabilistic expression of beliefs", "Track prediction accuracy", "Use confidence intervals"],
            recovery_actions=["Recalibrate confidence levels", "Review past 'certainties' that failed", "Implement probability training"]
        ),
        FailureMode(
            id="4_F3",
            description="Choosing inferior certain option over superior uncertain one",
            category=FailureCategory.MISAPPLICATION,
            severity=FailureSeverity.MEDIUM,
            example="Taking guaranteed $80 over 90% chance of $100 due to uncertainty aversion",
            detection_signals=["Systematic preference for certainty", "Paying large premiums for guarantees", "Avoiding all probabilistic choices"],
            safeguards=["Calculate expected values explicitly", "Recognize uncertainty premium you're paying", "Make uncertainty tolerance explicit"],
            recovery_actions=["Review decisions for excess certainty premium", "Practice with small uncertain bets", "Reframe uncertainty as opportunity"]
        ),
        FailureMode(
            id="4_F4",
            description="Anchoring to first conclusion to avoid doubt",
            category=FailureCategory.INTERACTION,
            severity=FailureSeverity.HIGH,
            example="Sticking with initial diagnosis despite contradictory evidence",
            detection_signals=["Defending first conclusion strongly", "Dismissing contradictory evidence", "Reluctance to revise views"],
            safeguards=["Explicitly consider alternatives before concluding", "Set revision triggers", "Seek disconfirming evidence actively"],
            recovery_actions=["Force consideration of opposite conclusion", "Implement Bayesian updating", "Create decision review checkpoints"]
        ),
        FailureMode(
            id="4_F5",
            description="Stress-induced snap decisions that avoid doubt",
            category=FailureCategory.TIMING,
            severity=FailureSeverity.CRITICAL,
            example="Making major decisions during crisis without adequate analysis",
            detection_signals=["Decisions made under extreme stress", "Feeling of needing to act immediately", "Relief after deciding regardless of quality"],
            safeguards=["Create pre-commitment to slow down under stress", "Designate cooling-off periods", "Have trusted advisor for stress decisions"],
            recovery_actions=["Review all stress decisions afterward", "Implement mandatory delays for big decisions", "Practice stress inoculation"]
        )
    ],
    meta_safeguards=[
        "Embrace uncertainty as the natural state",
        "Set minimum deliberation times for important decisions",
        "Express beliefs probabilistically, not certainly",
        "Create systems that force you to sit with doubt"
    ],
    complementary_models=[95, 96, 47],  # Probabilistic Thinking, Bayes Rule, Scenario Analysis
    antagonistic_models=[28]  # First-Conclusion Bias
)

FAILURE_MODES_DATABASE[5] = ModelFailureAnalysis(
    model_id=5,
    model_name="Inconsistency-Avoidance Tendency",
    failure_modes=[
        FailureMode(
            id="5_F1",
            description="Refusing to update beliefs despite overwhelming new evidence",
            category=FailureCategory.OVERRELIANCE,
            severity=FailureSeverity.CRITICAL,
            example="Kodak executives maintaining film-first strategy despite digital revolution",
            detection_signals=["Dismissing contradictory evidence", "Rationalizing away disconfirmation", "Increasing commitment to failing strategy"],
            safeguards=["Set explicit belief revision triggers", "Assign devil's advocate role", "Schedule regular assumption reviews"],
            recovery_actions=["Force complete re-evaluation from scratch", "Bring in outside perspective", "Create kill criteria for strategies"]
        ),
        FailureMode(
            id="5_F2",
            description="Escalation of commitment to failing course of action",
            category=FailureCategory.MISAPPLICATION,
            severity=FailureSeverity.HIGH,
            example="Continuing to invest in failing project to justify past investments",
            detection_signals=["Sunk cost arguments dominating", "Increasing investment despite poor returns", "Difficulty admitting mistakes"],
            safeguards=["Evaluate decisions as if starting fresh", "Set predetermined exit criteria", "Separate decision-maker from past decisions"],
            recovery_actions=["Apply fresh-start evaluation", "Implement automatic stop-losses", "Rotate decision-makers"]
        ),
        FailureMode(
            id="5_F3",
            description="Identity-protective cognition preventing learning",
            category=FailureCategory.BLIND_SPOT,
            severity=FailureSeverity.HIGH,
            example="Expert refusing to acknowledge error because it threatens professional identity",
            detection_signals=["Defensive reactions to challenges", "Conflating beliefs with identity", "Viewing disagreement as personal attack"],
            safeguards=["Separate identity from beliefs", "Celebrate belief updates as growth", "Create psychologically safe environment"],
            recovery_actions=["Reframe changing mind as strength", "Model belief updating publicly", "Reward intellectual honility"]
        ),
        FailureMode(
            id="5_F4",
            description="Maintaining consistency with public commitments despite private doubts",
            category=FailureCategory.MISAPPLICATION,
            severity=FailureSeverity.MEDIUM,
            example="CEO continuing strategy publicly committed to despite growing doubts",
            detection_signals=["Gap between public and private views", "Avoiding situations that might require reversal", "Doubling down on public statements"],
            safeguards=["Make commitments provisional", "Build in review points", "Create face-saving exit ramps"],
            recovery_actions=["Reframe reversal as responding to new information", "Acknowledge uncertainty publicly", "Create culture accepting of pivots"]
        ),
        FailureMode(
            id="5_F5",
            description="Applying past successful patterns to inappropriate new contexts",
            category=FailureCategory.MISAPPLICATION,
            severity=FailureSeverity.HIGH,
            example="Applying industrial-era management to knowledge workers",
            detection_signals=["'This worked before' reasoning", "Ignoring contextual differences", "Surprise when past approaches fail"],
            safeguards=["Explicitly identify context differences", "Test assumptions about transferability", "Start small in new contexts"],
            recovery_actions=["Analyze why past pattern doesn't fit", "Adapt approach to new context", "Seek domain experts for new context"]
        )
    ],
    meta_safeguards=[
        "Schedule regular 'assumption audits'",
        "Celebrate changing your mind publicly",
        "Create systems that force re-evaluation",
        "Separate past decisions from current choices"
    ],
    complementary_models=[46, 35, 96],  # Falsification, Inversion, Bayesian Updating
    antagonistic_models=[34]  # Status Quo Bias
)

# Continue with models 6-34...
FAILURE_MODES_DATABASE[6] = ModelFailureAnalysis(
    model_id=6,
    model_name="Curiosity Tendency",
    failure_modes=[
        FailureMode(
            id="6_F1",
            description="Analysis paralysis from endless curiosity without action",
            category=FailureCategory.OVERRELIANCE,
            severity=FailureSeverity.MEDIUM,
            example="Researching endlessly instead of making a decision",
            detection_signals=["Constant requests for more information", "Delayed decisions", "Diminishing returns on research"],
            safeguards=["Set research deadlines", "Define 'good enough' information thresholds", "Balance learning with doing"],
            recovery_actions=["Force decision with current information", "Accept uncertainty", "Learn by doing"]
        ),
        FailureMode(
            id="6_F2",
            description="Curiosity about irrelevant topics distracting from important ones",
            category=FailureCategory.SCOPE,
            severity=FailureSeverity.LOW,
            example="Going down rabbit holes unrelated to the task at hand",
            detection_signals=["Time spent on tangential topics", "Difficulty focusing", "Interesting but useless knowledge accumulation"],
            safeguards=["Define scope before research", "Time-box exploration", "Distinguish need-to-know from nice-to-know"],
            recovery_actions=["Refocus on core questions", "Park interesting tangents for later", "Prioritize ruthlessly"]
        ),
        FailureMode(
            id="6_F3",
            description="Assuming curiosity equals competence",
            category=FailureCategory.MISINTERPRETATION,
            severity=FailureSeverity.MEDIUM,
            example="Thinking reading about investing makes you a good investor",
            detection_signals=["Knowledge without application", "Overconfidence from learning", "Gap between theory and practice"],
            safeguards=["Test knowledge through application", "Seek feedback on competence", "Distinguish learning from mastery"],
            recovery_actions=["Apply knowledge in low-stakes settings", "Get expert assessment", "Focus on deliberate practice"]
        ),
        FailureMode(
            id="6_F4",
            description="Curiosity leading to information overload",
            category=FailureCategory.SCOPE,
            severity=FailureSeverity.MEDIUM,
            example="Subscribing to so many sources that none get proper attention",
            detection_signals=["Unread backlogs growing", "Shallow engagement with many topics", "Feeling overwhelmed by information"],
            safeguards=["Curate information sources ruthlessly", "Depth over breadth", "Regular information diet reviews"],
            recovery_actions=["Prune information sources", "Focus on fewer topics deeply", "Create processing systems"]
        ),
        FailureMode(
            id="6_F5",
            description="Curiosity without critical evaluation",
            category=FailureCategory.MISAPPLICATION,
            severity=FailureSeverity.HIGH,
            example="Accepting interesting but false information because it's fascinating",
            detection_signals=["Sharing unverified interesting facts", "Preferring interesting over accurate", "Weak source evaluation"],
            safeguards=["Verify before accepting", "Prefer boring truth to exciting falsehood", "Check sources systematically"],
            recovery_actions=["Audit beliefs for unverified 'facts'", "Strengthen critical evaluation", "Correct misinformation shared"]
        )
    ],
    meta_safeguards=[
        "Channel curiosity toward important questions",
        "Balance exploration with exploitation",
        "Verify interesting information before accepting",
        "Set boundaries on curiosity-driven exploration"
    ],
    complementary_models=[41, 46, 36],  # Circle of Competence, Falsification, Checklist
    antagonistic_models=[23]  # Twaddle Tendency
)

# Models 7-25: Psychology Biases
FAILURE_MODES_DATABASE[7] = ModelFailureAnalysis(
    model_id=7,
    model_name="Kantian Fairness Tendency",
    failure_modes=[
        FailureMode(
            id="7_F1",
            description="Expecting fairness in inherently unfair systems",
            category=FailureCategory.MISAPPLICATION,
            severity=FailureSeverity.MEDIUM,
            example="Expecting fair treatment in zero-sum competitive situations",
            detection_signals=["Surprise at unfair outcomes", "Frustration with 'unfair' systems", "Naive expectations of reciprocity"],
            safeguards=["Assess system incentives realistically", "Don't assume fairness", "Prepare for unfair treatment"],
            recovery_actions=["Adjust expectations to reality", "Protect against unfairness", "Choose fairer systems when possible"]
        ),
        FailureMode(
            id="7_F2",
            description="Rejecting beneficial but 'unfair' deals",
            category=FailureCategory.MISINTERPRETATION,
            severity=FailureSeverity.HIGH,
            example="Rejecting a profitable deal because the other party profits more",
            detection_signals=["Focusing on relative rather than absolute gains", "Ultimatum game rejections", "Spite-driven decisions"],
            safeguards=["Focus on absolute value to you", "Separate fairness from profitability", "Ask: 'Am I better off?'"],
            recovery_actions=["Reconsider rejected 'unfair' opportunities", "Calculate absolute benefit", "Manage fairness emotions"]
        ),
        FailureMode(
            id="7_F3",
            description="Overreacting to perceived unfairness, damaging relationships",
            category=FailureCategory.MISAPPLICATION,
            severity=FailureSeverity.HIGH,
            example="Destroying business relationship over minor perceived slight",
            detection_signals=["Disproportionate reactions to unfairness", "Burning bridges", "Revenge-seeking behavior"],
            safeguards=["Proportionate responses", "Consider long-term relationship value", "Verify unfairness before reacting"],
            recovery_actions=["Cool down before responding", "Seek to understand other perspective", "Repair damaged relationships"]
        ),
        FailureMode(
            id="7_F4",
            description="Assuming others share your fairness standards",
            category=FailureCategory.BLIND_SPOT,
            severity=FailureSeverity.MEDIUM,
            example="Being exploited by those with different fairness norms",
            detection_signals=["Repeated exploitation by same parties", "Surprise at others' behavior", "Projecting your values onto others"],
            safeguards=["Verify fairness norms before trusting", "Protect against exploitation", "Observe behavior, not words"],
            recovery_actions=["Adjust trust based on demonstrated behavior", "Implement verification", "Set boundaries with unfair actors"]
        ),
        FailureMode(
            id="7_F5",
            description="Using fairness as excuse to avoid difficult decisions",
            category=FailureCategory.MISAPPLICATION,
            severity=FailureSeverity.MEDIUM,
            example="Not firing underperformer because it wouldn't be 'fair'",
            detection_signals=["Fairness arguments blocking necessary actions", "Equality over effectiveness", "Avoiding hard choices"],
            safeguards=["Distinguish fairness from effectiveness", "Consider fairness to all stakeholders", "Make hard decisions fairly"],
            recovery_actions=["Reframe fairness to include all affected", "Make necessary decisions with fair process", "Communicate reasoning transparently"]
        )
    ],
    meta_safeguards=[
        "Expect unfairness in competitive situations",
        "Focus on absolute gains, not relative fairness",
        "Verify fairness norms before assuming shared values",
        "Respond proportionately to unfairness"
    ],
    complementary_models=[9, 59, 1],  # Reciprocation, Incentive Alignment, Reward/Punishment
    antagonistic_models=[8]  # Envy/Jealousy
)

# Continue with remaining psychology models...
FAILURE_MODES_DATABASE[8] = ModelFailureAnalysis(
    model_id=8,
    model_name="Envy/Jealousy Tendency",
    failure_modes=[
        FailureMode(id="8_F1", description="Making decisions to 'keep up' rather than based on own needs", category=FailureCategory.MISAPPLICATION, severity=FailureSeverity.MEDIUM, example="Buying expensive car because neighbor did", detection_signals=["Comparison-driven purchases", "Dissatisfaction despite having enough", "Lifestyle inflation"], safeguards=["Define own values and goals", "Limit social comparison", "Focus on absolute not relative position"], recovery_actions=["Audit comparison-driven decisions", "Practice gratitude", "Reduce exposure to triggers"]),
        FailureMode(id="8_F2", description="Underestimating envy as motivator in others' behavior", category=FailureCategory.BLIND_SPOT, severity=FailureSeverity.HIGH, example="Not anticipating sabotage from envious colleague", detection_signals=["Unexpected opposition from peers", "Subtle undermining", "Backhanded compliments"], safeguards=["Recognize envy triggers in others", "Share credit generously", "Downplay success publicly"], recovery_actions=["Address envy-driven conflicts", "Build alliances", "Reduce envy triggers"]),
        FailureMode(id="8_F3", description="Envy preventing learning from successful others", category=FailureCategory.BLIND_SPOT, severity=FailureSeverity.MEDIUM, example="Dismissing successful person's advice due to envy", detection_signals=["Criticizing successful people", "Not seeking mentorship", "Attributing others' success to luck"], safeguards=["Reframe envy as admiration", "Learn from those you envy", "Separate emotion from information"], recovery_actions=["Identify what you can learn from envied parties", "Seek mentorship", "Transform envy into motivation"]),
        FailureMode(id="8_F4", description="Envy-driven risk-taking to catch up", category=FailureCategory.MISAPPLICATION, severity=FailureSeverity.HIGH, example="Taking excessive investment risk to match peers' returns", detection_signals=["Increased risk after seeing others' gains", "FOMO-driven decisions", "Abandoning sound strategy"], safeguards=["Stick to own risk tolerance", "Ignore others' returns", "Focus on process not outcomes"], recovery_actions=["Return to sound strategy", "Assess damage from envy-driven risks", "Implement comparison-blocking"]),
        FailureMode(id="8_F5", description="Schadenfreude leading to poor decisions", category=FailureCategory.MISAPPLICATION, severity=FailureSeverity.MEDIUM, example="Enjoying competitor's failure instead of learning from it", detection_signals=["Pleasure at others' misfortune", "Missing lessons from failures", "Complacency from competitors' struggles"], safeguards=["Learn from all failures including competitors'", "Maintain humility", "Focus on own improvement"], recovery_actions=["Study competitor failures for lessons", "Check for similar vulnerabilities", "Maintain vigilance"])
    ],
    meta_safeguards=["Recognize envy in yourself and others", "Transform envy into motivation to improve", "Focus on absolute progress not relative position", "Share success to reduce others' envy"],
    complementary_models=[12, 15, 7],
    antagonistic_models=[]
)

# Models 9-34 with condensed failure modes...
FAILURE_MODES_DATABASE[9] = ModelFailureAnalysis(model_id=9, model_name="Reciprocation Tendency", failure_modes=[
    FailureMode(id="9_F1", description="Being manipulated through unsolicited favors", category=FailureCategory.BLIND_SPOT, severity=FailureSeverity.HIGH, example="Feeling obligated to buy after receiving free sample", detection_signals=["Obligation feelings after gifts", "Difficulty saying no after favors", "Disproportionate reciprocation"], safeguards=["Recognize manipulation tactics", "Evaluate requests independently of favors", "Feel free to reject unsolicited gifts"], recovery_actions=["Decline manipulative reciprocation", "Return unwanted gifts", "Set boundaries"]),
    FailureMode(id="9_F2", description="Reciprocating negative behavior, escalating conflicts", category=FailureCategory.MISAPPLICATION, severity=FailureSeverity.HIGH, example="Retaliating against slight, starting conflict spiral", detection_signals=["Tit-for-tat negative exchanges", "Escalating hostility", "Inability to break negative cycles"], safeguards=["Break negative reciprocation cycles", "Respond proportionately", "Consider long-term relationship"], recovery_actions=["De-escalate unilaterally", "Seek mediation", "Reset relationship"]),
    FailureMode(id="9_F3", description="Over-reciprocating small favors with large ones", category=FailureCategory.MISINTERPRETATION, severity=FailureSeverity.MEDIUM, example="Giving major concession after minor one received", detection_signals=["Disproportionate responses", "Feeling exploited after reciprocating", "Pattern of over-giving"], safeguards=["Match reciprocation to favor size", "Pause before reciprocating", "Track exchange balance"], recovery_actions=["Recalibrate reciprocation", "Address imbalanced relationships", "Set reciprocation limits"]),
    FailureMode(id="9_F4", description="Expecting reciprocation that won't come", category=FailureCategory.MISAPPLICATION, severity=FailureSeverity.MEDIUM, example="Doing favors expecting return that never comes", detection_signals=["Resentment from unreciprocated favors", "Keeping score", "Disappointment in relationships"], safeguards=["Give without expectation", "Assess likelihood of reciprocation", "Communicate expectations"], recovery_actions=["Release expectation of reciprocation", "Adjust future giving", "Have explicit conversations"]),
    FailureMode(id="9_F5", description="Reciprocation obligation overriding better judgment", category=FailureCategory.OVERRELIANCE, severity=FailureSeverity.HIGH, example="Agreeing to bad deal because of past favor", detection_signals=["Decisions driven by obligation", "Ignoring red flags due to relationship", "Guilt-driven choices"], safeguards=["Separate obligation from decision quality", "Evaluate deals on merit", "Repay favors appropriately, not excessively"], recovery_actions=["Renegotiate obligation-driven agreements", "Find appropriate ways to reciprocate", "Set boundaries on obligation"])
], meta_safeguards=["Recognize when reciprocation is being exploited", "Match reciprocation to favor magnitude", "Don't let obligation override judgment", "Break negative reciprocation cycles"], complementary_models=[1, 7, 2], antagonistic_models=[])

# Continue with models 10-34...
for model_id in range(10, 35):
    model_names = {
        10: "Influence-from-Mere-Association Tendency",
        11: "Simple, Pain-Avoiding Psychological Denial",
        12: "Excessive Self-Regard Tendency",
        13: "Overoptimism Tendency",
        14: "Deprival-Superreaction Tendency",
        15: "Social-Proof Tendency",
        16: "Contrast-Misreaction Tendency",
        17: "Stress-Influence Tendency",
        18: "Availability-Misweighing Tendency",
        19: "Use-It-or-Lose-It Tendency",
        20: "Drug-Misinfluence Tendency",
        21: "Senescence-Misinfluence Tendency",
        22: "Authority-Misinfluence Tendency",
        23: "Twaddle Tendency",
        24: "Reason-Respecting Tendency",
        25: "Lollapalooza Effect",
        26: "Confirmation Bias",
        27: "Hindsight Bias",
        28: "First-Conclusion Bias",
        29: "Anchoring Bias",
        30: "Incentive-Caused Bias",
        31: "Pavlovian Association/Conditioning",
        32: "Hyperbolic Discounting/Short-Termism",
        33: "Representativeness Bias",
        34: "Status Quo Bias/Habit Persistence"
    }
    
    # Generate comprehensive failure modes for each
    FAILURE_MODES_DATABASE[model_id] = ModelFailureAnalysis(
        model_id=model_id,
        model_name=model_names[model_id],
        failure_modes=[
            FailureMode(
                id=f"{model_id}_F1",
                description=f"Misapplying {model_names[model_id]} to inappropriate context",
                category=FailureCategory.MISAPPLICATION,
                severity=FailureSeverity.MEDIUM,
                example=f"Using {model_names[model_id]} when other factors dominate",
                detection_signals=["Model predictions not matching reality", "Forced application", "Ignoring contradictory evidence"],
                safeguards=["Verify context appropriateness", "Consider multiple models", "Test predictions"],
                recovery_actions=["Re-evaluate context", "Apply alternative models", "Update approach"]
            ),
            FailureMode(
                id=f"{model_id}_F2",
                description=f"Over-relying on {model_names[model_id]} as sole explanation",
                category=FailureCategory.OVERRELIANCE,
                severity=FailureSeverity.HIGH,
                example=f"Attributing all outcomes to {model_names[model_id]} ignoring other causes",
                detection_signals=["Single-model explanations", "Ignoring complexity", "Confirmation of model everywhere"],
                safeguards=["Use multiple models", "Seek disconfirming evidence", "Consider alternative explanations"],
                recovery_actions=["Broaden analytical framework", "Consult diverse perspectives", "Test alternative hypotheses"]
            ),
            FailureMode(
                id=f"{model_id}_F3",
                description=f"Misinterpreting the mechanism of {model_names[model_id]}",
                category=FailureCategory.MISINTERPRETATION,
                severity=FailureSeverity.MEDIUM,
                example=f"Superficial understanding leading to wrong conclusions",
                detection_signals=["Predictions consistently wrong", "Inability to explain mechanism", "Confusion about application"],
                safeguards=["Study model deeply", "Test understanding", "Seek expert guidance"],
                recovery_actions=["Deepen model understanding", "Study original sources", "Practice application"]
            ),
            FailureMode(
                id=f"{model_id}_F4",
                description=f"Failing to account for {model_names[model_id]} in self",
                category=FailureCategory.BLIND_SPOT,
                severity=FailureSeverity.HIGH,
                example=f"Seeing bias in others but not recognizing it in yourself",
                detection_signals=["Confident you're immune", "Seeing it everywhere except self", "Defensive when pointed out"],
                safeguards=["Assume you're affected", "Seek external feedback", "Create structural safeguards"],
                recovery_actions=["Audit own decisions for bias", "Implement checks", "Cultivate humility"]
            ),
            FailureMode(
                id=f"{model_id}_F5",
                description=f"Interaction effects with other biases amplifying {model_names[model_id]}",
                category=FailureCategory.INTERACTION,
                severity=FailureSeverity.HIGH,
                example=f"Multiple biases combining to create Lollapalooza effect",
                detection_signals=["Extreme outcomes", "Multiple biases present", "Cascading errors"],
                safeguards=["Check for bias combinations", "Use checklists", "Slow down under pressure"],
                recovery_actions=["Identify all active biases", "Address systematically", "Implement cooling-off period"]
            )
        ],
        meta_safeguards=[
            f"Assume {model_names[model_id]} affects you",
            "Use structured decision processes",
            "Seek diverse perspectives",
            "Create environmental safeguards"
        ],
        complementary_models=[35, 36, 46],
        antagonistic_models=[]
    )

# -----------------------------------------------------------------------------
# CATEGORY 2: THINKING TOOLS (Models 35-52)
# -----------------------------------------------------------------------------

FAILURE_MODES_DATABASE[35] = ModelFailureAnalysis(
    model_id=35,
    model_name="Inversion",
    failure_modes=[
        FailureMode(id="35_F1", description="Inverting the wrong problem", category=FailureCategory.MISAPPLICATION, severity=FailureSeverity.MEDIUM, example="Asking 'how to fail' when the real question is different", detection_signals=["Inversion insights not actionable", "Missing the core issue", "Elegant but irrelevant analysis"], safeguards=["Verify problem definition first", "Invert multiple framings", "Connect inversion to action"], recovery_actions=["Redefine the problem", "Try different inversions", "Focus on actionable insights"]),
        FailureMode(id="35_F2", description="Stopping at inversion without positive vision", category=FailureCategory.SCOPE, severity=FailureSeverity.MEDIUM, example="Knowing what to avoid but not what to pursue", detection_signals=["Only negative guidance", "Paralysis from fear of failure", "No positive direction"], safeguards=["Balance inversion with positive goals", "Use inversion as input not output", "Develop both avoid and pursue lists"], recovery_actions=["Develop positive vision", "Convert avoidances to pursuits", "Balance negative and positive"]),
        FailureMode(id="35_F3", description="Over-applying inversion to simple problems", category=FailureCategory.OVERRELIANCE, severity=FailureSeverity.LOW, example="Complex inversion analysis for straightforward decisions", detection_signals=["Overthinking simple issues", "Diminishing returns on analysis", "Delayed decisions"], safeguards=["Match analysis depth to decision importance", "Use simple heuristics for simple problems", "Know when to stop"], recovery_actions=["Simplify approach", "Make decision with available information", "Reserve inversion for complex problems"]),
        FailureMode(id="35_F4", description="Inversion creating excessive pessimism", category=FailureCategory.MISINTERPRETATION, severity=FailureSeverity.MEDIUM, example="Focusing so much on failure modes that you never act", detection_signals=["Paralysis by analysis", "Excessive risk aversion", "Missing opportunities"], safeguards=["Balance risk identification with opportunity", "Set action thresholds", "Accept some risk is necessary"], recovery_actions=["Force action despite identified risks", "Implement risk mitigation not avoidance", "Rebalance optimism and pessimism"]),
        FailureMode(id="35_F5", description="Assuming inverted insights are complete", category=FailureCategory.BLIND_SPOT, severity=FailureSeverity.MEDIUM, example="Thinking avoiding known failures guarantees success", detection_signals=["Overconfidence after inversion", "Missing novel failure modes", "Surprise failures"], safeguards=["Recognize inversion is incomplete", "Continue monitoring for new risks", "Combine with other analytical tools"], recovery_actions=["Expand failure mode analysis", "Implement ongoing monitoring", "Stay humble about completeness"])
    ],
    meta_safeguards=["Invert the right problem", "Balance inversion with positive vision", "Don't let inversion create paralysis", "Recognize inversion is incomplete"],
    complementary_models=[46, 47, 36],
    antagonistic_models=[13]
)

# Generate remaining thinking tools (36-52)
thinking_tool_names = {
    36: "Checklist Approach", 37: "Two-Track Analysis", 38: "Elementary Worldly Wisdom",
    39: "Latticework of Mental Models", 40: "Multidisciplinary Approach", 41: "Circle of Competence",
    42: "First Principles Thinking", 43: "Second-Order Thinking", 44: "Occam's Razor",
    45: "Hanlon's Razor", 46: "Falsification/Disconfirming Evidence", 47: "Scenario Analysis",
    48: "Mental Accounting", 49: "Black Swan Events", 50: "Gray Rhino Events",
    51: "Vivification", 52: "Man-with-a-Hammer Tendency"
}

for model_id in range(36, 53):
    FAILURE_MODES_DATABASE[model_id] = ModelFailureAnalysis(
        model_id=model_id,
        model_name=thinking_tool_names[model_id],
        failure_modes=[
            FailureMode(id=f"{model_id}_F1", description=f"Mechanical application of {thinking_tool_names[model_id]} without judgment", category=FailureCategory.MISAPPLICATION, severity=FailureSeverity.MEDIUM, example="Following process without understanding purpose", detection_signals=["Rote application", "Missing context", "Process over outcome"], safeguards=["Understand why, not just how", "Adapt to context", "Focus on outcomes"], recovery_actions=["Reconnect with purpose", "Customize approach", "Evaluate effectiveness"]),
            FailureMode(id=f"{model_id}_F2", description=f"Overconfidence from using {thinking_tool_names[model_id]}", category=FailureCategory.OVERRELIANCE, severity=FailureSeverity.HIGH, example="Believing tool use guarantees good outcomes", detection_signals=["False sense of security", "Reduced vigilance", "Surprise failures"], safeguards=["Tools reduce but don't eliminate error", "Maintain humility", "Continue monitoring"], recovery_actions=["Recalibrate confidence", "Add redundant checks", "Learn from failures"]),
            FailureMode(id=f"{model_id}_F3", description=f"Using {thinking_tool_names[model_id]} as substitute for domain knowledge", category=FailureCategory.MISAPPLICATION, severity=FailureSeverity.HIGH, example="Thinking framework compensates for ignorance", detection_signals=["Shallow analysis despite good process", "Missing domain-specific factors", "Expert disagreement"], safeguards=["Combine tools with domain expertise", "Recognize knowledge gaps", "Consult experts"], recovery_actions=["Develop domain knowledge", "Partner with experts", "Acknowledge limitations"]),
            FailureMode(id=f"{model_id}_F4", description=f"Rigid application of {thinking_tool_names[model_id]} to dynamic situations", category=FailureCategory.TIMING, severity=FailureSeverity.MEDIUM, example="Static analysis of changing situation", detection_signals=["Analysis quickly outdated", "Failure to adapt", "Surprise from changes"], safeguards=["Build in review cycles", "Monitor for changes", "Maintain flexibility"], recovery_actions=["Update analysis regularly", "Build adaptive processes", "Stay alert to changes"]),
            FailureMode(id=f"{model_id}_F5", description=f"Social proof in applying {thinking_tool_names[model_id]}", category=FailureCategory.INTERACTION, severity=FailureSeverity.MEDIUM, example="Using tool because others do, not because appropriate", detection_signals=["Following trends", "Not questioning fit", "Cargo cult application"], safeguards=["Evaluate tool fit independently", "Understand when tool applies", "Be willing to use different approaches"], recovery_actions=["Assess tool appropriateness", "Consider alternatives", "Customize to situation"])
        ],
        meta_safeguards=["Tools are aids not guarantees", "Combine with domain knowledge", "Adapt to context", "Maintain humility"],
        complementary_models=[35, 39, 41],
        antagonistic_models=[]
    )

# -----------------------------------------------------------------------------
# CATEGORIES 3-8: Economics, Moats, Math, Physics, Biology, Organizational
# Generate comprehensive failure modes for models 53-129
# -----------------------------------------------------------------------------

remaining_models = {
    # Economics & Business (53-72)
    53: "Supply and Demand", 54: "Elasticity", 55: "Opportunity Cost",
    56: "Comparative Advantage", 57: "Marginal Utility/Diminishing Returns",
    58: "Time Value of Money", 59: "Incentives & Incentive Alignment",
    60: "Agency Problem/Principal-Agent Problem", 61: "Information Asymmetry",
    62: "Adverse Selection", 63: "Moral Hazard", 64: "Pareto Principle (80/20 Rule)",
    65: "Gresham's Law", 66: "Creative/Competitive Destruction",
    67: "Value to a Private Owner", 68: "Mr. Market", 69: "Margin of Safety",
    70: "Tax Deferral & Compounding Advantage", 71: "Value Creation vs. Value Capture",
    72: "Obsolescence Risk",
    # Moats (73-91)
    73: "Economic Moats", 74: "Cost Advantages", 75: "Differentiation & Brand Power",
    76: "Switching Costs", 77: "Network Effects", 78: "Scale Economies - Supply Side",
    79: "Scale Economies - Demand Side", 80: "Learning/Experience Curve",
    81: "Lock-In via Distribution/Physical Network", 82: "Winner-Take-All/Winner-Take-Most Markets",
    83: "Moat Durability", 84: "Industry Structure", 85: "Rational vs. Cutthroat Competition",
    86: "Platform Economics", 87: "Capacity & Supply Discipline",
    88: "Surfing Long Waves", 89: "Technology as Friend vs. Killer",
    90: "Bureaucracy/Diseconomies of Scale", 91: "Cancer-Surgery Formula",
    # Math (92-103)
    92: "Basic Arithmetic Fluency", 93: "Permutations & Combinations",
    94: "Expected Value", 95: "Probabilistic Thinking/Base Rates",
    96: "Bayes' Rule/Bayesian Updating", 97: "Regression to the Mean",
    98: "Normal Distribution vs. Fat Tails", 99: "Power Laws",
    100: "Cost-Benefit Analysis", 101: "Compounding",
    102: "Optionality/Asymmetric Payoffs", 103: "Kelly-Type Thinking",
    # Physics/Systems (104-114)
    104: "Critical Mass", 105: "Leverage", 106: "Redundancy/Backup Systems",
    107: "Breakpoints/Phase Transitions", 108: "Friction & Efficiency Losses",
    109: "Reliability Engineering/Safety Margins", 110: "Feedback Loops - Positive (Reinforcing)",
    111: "Feedback Loops - Negative (Stabilizing)", 112: "Bottlenecks & Constraints",
    113: "Equilibrium", 114: "System Resilience vs. Fragility",
    # Biology (115-120)
    115: "Evolution by Natural Selection", 116: "Adaptation & Fitness Landscapes",
    117: "Red Queen Effect", 118: "Niches & Ecological Competition",
    119: "Population Dynamics", 120: "Autopsy Learning",
    # Organizational (121-129)
    121: "Corporate Governance", 122: "Management Incentives & Compensation Design",
    123: "Culture as a Control System", 124: "Bureaucratic Inertia & Empire Building",
    125: "Information Suppression/Shooting the Messenger",
    126: "Five W's Rule in Communication", 127: "Checklists & Standard Operating Procedures",
    128: "Talent, Trust, and Delegation", 129: "Avoiding Madness in Crowds"
}

for model_id, model_name in remaining_models.items():
    FAILURE_MODES_DATABASE[model_id] = ModelFailureAnalysis(
        model_id=model_id,
        model_name=model_name,
        failure_modes=[
            FailureMode(
                id=f"{model_id}_F1",
                description=f"Applying {model_name} outside its valid domain",
                category=FailureCategory.MISAPPLICATION,
                severity=FailureSeverity.HIGH,
                example=f"Using {model_name} where assumptions don't hold",
                detection_signals=["Predictions fail", "Experts disagree", "Assumptions violated"],
                safeguards=["Verify assumptions", "Check domain boundaries", "Consult experts"],
                recovery_actions=["Identify assumption violations", "Find appropriate model", "Adjust approach"]
            ),
            FailureMode(
                id=f"{model_id}_F2",
                description=f"Quantifying {model_name} with false precision",
                category=FailureCategory.MISINTERPRETATION,
                severity=FailureSeverity.MEDIUM,
                example=f"Precise numbers hiding uncertainty in {model_name} application",
                detection_signals=["Overconfident estimates", "Ignoring uncertainty", "Spurious precision"],
                safeguards=["Use ranges not points", "Acknowledge uncertainty", "Sensitivity analysis"],
                recovery_actions=["Add uncertainty bounds", "Test sensitivity", "Communicate uncertainty"]
            ),
            FailureMode(
                id=f"{model_id}_F3",
                description=f"Static application of {model_name} to dynamic situation",
                category=FailureCategory.TIMING,
                severity=FailureSeverity.HIGH,
                example=f"Assuming {model_name} parameters stay constant",
                detection_signals=["Analysis outdated quickly", "Surprise from changes", "Model drift"],
                safeguards=["Monitor for changes", "Build in review cycles", "Stress test assumptions"],
                recovery_actions=["Update analysis", "Shorten review cycles", "Build adaptive approach"]
            ),
            FailureMode(
                id=f"{model_id}_F4",
                description=f"Ignoring interaction of {model_name} with other factors",
                category=FailureCategory.INTERACTION,
                severity=FailureSeverity.HIGH,
                example=f"{model_name} correct in isolation but wrong in context",
                detection_signals=["Partial analysis", "Missing factors", "Unexpected outcomes"],
                safeguards=["Consider system interactions", "Use multiple models", "Map dependencies"],
                recovery_actions=["Expand analysis scope", "Add interacting factors", "System-level view"]
            ),
            FailureMode(
                id=f"{model_id}_F5",
                description=f"Confirmation bias in applying {model_name}",
                category=FailureCategory.BLIND_SPOT,
                severity=FailureSeverity.HIGH,
                example=f"Seeing {model_name} everywhere because you're looking for it",
                detection_signals=["Model fits too well", "Ignoring contradictions", "Selective evidence"],
                safeguards=["Seek disconfirmation", "Pre-register predictions", "Devil's advocate"],
                recovery_actions=["Active disconfirmation", "Consider alternatives", "External review"]
            )
        ],
        meta_safeguards=[
            f"Verify {model_name} assumptions hold",
            "Acknowledge uncertainty in application",
            "Consider interactions with other factors",
            "Seek disconfirming evidence"
        ],
        complementary_models=[35, 46, 39],
        antagonistic_models=[]
    )


# =============================================================================
# FAILURE PREVENTION ENGINE
# =============================================================================

class FailurePreventionEngine:
    """
    Engine for preventing mental model application failures.
    
    Uses the failure modes database to:
    1. Warn about potential failures before they occur
    2. Detect failures as they happen
    3. Recommend recovery actions
    4. Learn from past failures
    """
    
    def __init__(self):
        self.failure_modes = FAILURE_MODES_DATABASE
        self.failure_history: List[Dict] = []
        
    def get_failure_modes(self, model_id: int) -> List[Dict]:
        """Get failure modes for a specific model as list of dicts."""
        analysis = self.failure_modes.get(model_id)
        if not analysis:
            return []
        
        return [
            {
                "id": fm.id,
                "name": fm.description.split('.')[0] if fm.description else fm.id,
                "description": fm.description,
                "category": fm.category.value,
                "severity": fm.severity.value,
                "example": fm.example,
                "detection_signals": fm.detection_signals,
                "safeguards": fm.safeguards,
                "recovery_actions": fm.recovery_actions
            }
            for fm in analysis.failure_modes
        ]
    
    def get_failure_analysis(self, model_id: int) -> Optional[ModelFailureAnalysis]:
        """Get full failure analysis object for a specific model."""
        return self.failure_modes.get(model_id)
    
    def get_all_safeguards(self, model_ids: List[int]) -> Dict[int, List[str]]:
        """Get all safeguards for a set of models being used together."""
        safeguards = {}
        for model_id in model_ids:
            analysis = self.failure_modes.get(model_id)
            if analysis:
                model_safeguards = analysis.meta_safeguards.copy()
                for fm in analysis.failure_modes:
                    model_safeguards.extend(fm.safeguards)
                safeguards[model_id] = list(set(model_safeguards))
        return safeguards
    
    def check_for_interactions(self, model_ids: List[int]) -> List[Dict]:
        """Check for potential negative interactions between models."""
        warnings = []
        
        for model_id in model_ids:
            analysis = self.failure_modes.get(model_id)
            if analysis:
                # Check for antagonistic models being used together
                for antagonist in analysis.antagonistic_models:
                    if antagonist in model_ids:
                        warnings.append({
                            "type": "antagonistic_interaction",
                            "models": [model_id, antagonist],
                            "warning": f"Models {analysis.model_name} and {self.failure_modes[antagonist].model_name} may conflict"
                        })
        
        # Check for Lollapalooza risk (many biases combining)
        psychology_models = [m for m in model_ids if m <= 34]
        if len(psychology_models) >= 4:
            warnings.append({
                "type": "lollapalooza_risk",
                "models": psychology_models,
                "warning": f"{len(psychology_models)} psychological biases active - high risk of Lollapalooza effect"
            })
        
        return warnings
    
    def generate_pre_decision_checklist(self, model_ids: List[int]) -> List[str]:
        """Generate a pre-decision checklist based on models being used."""
        checklist = [
            "□ Have I verified the assumptions of each model hold in this context?",
            "□ Have I sought disconfirming evidence for my conclusions?",
            "□ Have I considered how these models might interact?",
            "□ Have I acknowledged uncertainty in my analysis?",
            "□ Have I checked for my own biases affecting the analysis?"
        ]
        
        # Add model-specific checks
        for model_id in model_ids:
            analysis = self.failure_modes.get(model_id)
            if analysis:
                for fm in analysis.failure_modes[:2]:  # Top 2 failure modes
                    checklist.append(f"□ {analysis.model_name}: {fm.safeguards[0]}")
        
        return checklist
    
    def detect_failure_signals(self, model_id: int, observations: List[str]) -> List[Dict]:
        """Check observations against known failure signals."""
        detected = []
        analysis = self.failure_modes.get(model_id)
        
        if analysis:
            for fm in analysis.failure_modes:
                for signal in fm.detection_signals:
                    for obs in observations:
                        if signal.lower() in obs.lower():
                            detected.append({
                                "failure_mode": fm.id,
                                "description": fm.description,
                                "signal_matched": signal,
                                "observation": obs,
                                "severity": fm.severity.value,
                                "recovery_actions": fm.recovery_actions
                            })
        
        return detected
    
    def get_recovery_actions(self, failure_mode_id: str) -> List[str]:
        """Get recovery actions for a specific failure mode."""
        model_id = int(failure_mode_id.split("_")[0])
        analysis = self.failure_modes.get(model_id)
        
        if analysis:
            for fm in analysis.failure_modes:
                if fm.id == failure_mode_id:
                    return fm.recovery_actions
        
        return []
    
    def record_failure(self, model_id: int, failure_mode_id: str, 
                      context: str, outcome: str):
        """Record a failure for learning."""
        self.failure_history.append({
            "model_id": model_id,
            "failure_mode_id": failure_mode_id,
            "context": context,
            "outcome": outcome,
            "timestamp": str(datetime.now()) if 'datetime' in dir() else "unknown"
        })
    
    def get_failure_statistics(self) -> Dict:
        """Get statistics on recorded failures."""
        if not self.failure_history:
            return {"total_failures": 0}
        
        from collections import Counter
        
        model_failures = Counter(f["model_id"] for f in self.failure_history)
        mode_failures = Counter(f["failure_mode_id"] for f in self.failure_history)
        
        return {
            "total_failures": len(self.failure_history),
            "failures_by_model": dict(model_failures.most_common(10)),
            "failures_by_mode": dict(mode_failures.most_common(10))
        }
    
    def export_failure_modes_json(self, output_path: str):
        """Export all failure modes to JSON for reference."""
        export_data = {}
        
        for model_id, analysis in self.failure_modes.items():
            export_data[model_id] = {
                "model_name": analysis.model_name,
                "failure_modes": [
                    {
                        "id": fm.id,
                        "description": fm.description,
                        "category": fm.category.value,
                        "severity": fm.severity.value,
                        "example": fm.example,
                        "detection_signals": fm.detection_signals,
                        "safeguards": fm.safeguards,
                        "recovery_actions": fm.recovery_actions
                    }
                    for fm in analysis.failure_modes
                ],
                "meta_safeguards": analysis.meta_safeguards,
                "complementary_models": analysis.complementary_models,
                "antagonistic_models": analysis.antagonistic_models
            }
        
        with open(output_path, 'w') as f:
            json.dump(export_data, f, indent=2)
        
        return output_path
    
    # =========================================================================
    # ADDITIONAL METHODS (for test compatibility)
    # =========================================================================
    
    def get_safeguards(self, model_id: int) -> List[str]:
        """Get safeguards for a specific model."""
        analysis = self.failure_modes.get(model_id)
        if not analysis:
            return []
        
        safeguards = analysis.meta_safeguards.copy()
        for fm in analysis.failure_modes:
            safeguards.extend(fm.safeguards)
        return list(set(safeguards))
    
    def check_decision(self, decision: Dict) -> List[Dict]:
        """Check a decision for potential failures."""
        warnings = []
        model_ids = decision.get("models_applied", [])
        
        # Check each model for relevant failure modes
        for model_id in model_ids:
            analysis = self.failure_modes.get(model_id)
            if analysis:
                for fm in analysis.failure_modes:
                    warnings.append({
                        "model_id": model_id,
                        "model_name": analysis.model_name,
                        "failure_mode": fm.id,
                        "description": fm.description,
                        "severity": fm.severity.value,
                        "safeguards": fm.safeguards
                    })
        
        # Add interaction warnings
        warnings.extend(self.check_for_interactions(model_ids))
        
        return warnings
    
    def generate_checklist(self, model_ids: List[int]) -> Dict:
        """Generate a safeguard checklist for given models."""
        items = self.generate_pre_decision_checklist(model_ids)
        return {
            "items": items,
            "model_count": len(model_ids),
            "total_items": len(items)
        }
    
    def calculate_risk_score(self, model_ids: List[int]) -> float:
        """Calculate risk score for a combination of models."""
        if not model_ids:
            return 0.0
        
        # Base risk from number of models
        base_risk = min(len(model_ids) / 10.0, 0.5)
        
        # Add risk from psychology biases
        psychology_count = sum(1 for m in model_ids if m <= 34)
        psychology_risk = min(psychology_count / 10.0, 0.3)
        
        # Add risk from antagonistic interactions
        interaction_risk = 0.0
        for model_id in model_ids:
            analysis = self.failure_modes.get(model_id)
            if analysis:
                for antagonist in analysis.antagonistic_models:
                    if antagonist in model_ids:
                        interaction_risk += 0.1
        interaction_risk = min(interaction_risk, 0.2)
        
        return min(base_risk + psychology_risk + interaction_risk, 1.0)
    
    def get_amplifying_biases(self, model_id: int) -> List[int]:
        """Get biases that amplify a model's failure modes."""
        analysis = self.failure_modes.get(model_id)
        if not analysis:
            return []
        return analysis.antagonistic_models
    
    def get_mitigating_models(self, model_id: int) -> List[int]:
        """Get models that mitigate failure modes."""
        analysis = self.failure_modes.get(model_id)
        if not analysis:
            return []
        return analysis.complementary_models
    
    def generate_structural_safeguards(self, model_ids: List[int]) -> List[str]:
        """Generate structural safeguards for given models."""
        safeguards = [
            "Implement independent review processes",
            "Create separation of duties",
            "Establish clear decision criteria before analysis",
            "Document assumptions and reasoning"
        ]
        for model_id in model_ids:
            analysis = self.failure_modes.get(model_id)
            if analysis:
                safeguards.extend([s for s in analysis.meta_safeguards if "structure" in s.lower() or "process" in s.lower()])
        return list(set(safeguards))
    
    def generate_cognitive_safeguards(self, model_ids: List[int]) -> List[str]:
        """Generate cognitive safeguards for given models."""
        safeguards = [
            "Actively seek disconfirming evidence",
            "Consider the opposite hypothesis",
            "Sleep on major decisions",
            "Write down reasoning before deciding"
        ]
        for model_id in model_ids:
            analysis = self.failure_modes.get(model_id)
            if analysis:
                safeguards.extend([s for s in analysis.meta_safeguards if "think" in s.lower() or "consider" in s.lower()])
        return list(set(safeguards))
    
    def generate_social_safeguards(self, model_ids: List[int]) -> List[str]:
        """Generate social safeguards for given models."""
        safeguards = [
            "Consult with devil's advocate",
            "Seek input from diverse perspectives",
            "Create psychological safety for dissent",
            "Establish anonymous feedback channels"
        ]
        return safeguards
    
    def generate_recovery_protocol(self, model_id: int, failure_mode_id: str) -> Dict:
        """Generate recovery protocol for a failure mode."""
        actions = self.get_recovery_actions(failure_mode_id)
        return {
            "model_id": model_id,
            "failure_mode_id": failure_mode_id,
            "immediate_actions": actions[:2] if len(actions) >= 2 else actions,
            "follow_up_actions": actions[2:] if len(actions) > 2 else [],
            "prevention_measures": self.get_safeguards(model_id)[:3]
        }
    
    def calculate_lollapalooza_risk(self, model_ids: List[int]) -> float:
        """Calculate Lollapalooza risk for converging models."""
        if len(model_ids) < 3:
            return 0.0
        
        # Risk increases with number of converging models
        base_risk = min((len(model_ids) - 2) / 5.0, 0.6)
        
        # Higher risk if many are from same category (psychology)
        psychology_count = sum(1 for m in model_ids if m <= 34)
        category_risk = min(psychology_count / 8.0, 0.4)
        
        return min(base_risk + category_risk, 1.0)
    
    def get_high_risk_combinations(self) -> List[Dict]:
        """Get known high-risk model combinations."""
        return [
            {
                "models": [1, 2, 3],
                "names": ["Incentive-Caused Bias", "Social Proof", "Denial"],
                "risk": "Groupthink in compensation decisions",
                "mitigation": "Independent compensation committee"
            },
            {
                "models": [4, 5, 6],
                "names": ["Inconsistency-Avoidance", "Curiosity", "Kantian Fairness"],
                "risk": "Confirmation bias in research",
                "mitigation": "Pre-registration of hypotheses"
            },
            {
                "models": [1, 7, 8],
                "names": ["Incentive-Caused Bias", "Envy/Jealousy", "Reciprocation"],
                "risk": "Corrupt business relationships",
                "mitigation": "Transparent procurement processes"
            }
        ]
    
    def get_mitigation_suggestions(self, model_ids: List[int]) -> List[str]:
        """Get mitigation suggestions for a set of models."""
        suggestions = []
        
        # Get safeguards from all models
        for model_id in model_ids:
            safeguards = self.get_safeguards(model_id)
            suggestions.extend(safeguards[:2])  # Top 2 from each
        
        # Add general mitigations
        suggestions.extend([
            "Document decision rationale",
            "Establish pre-commitment criteria",
            "Seek external review"
        ])
        
        return list(set(suggestions))
    
    def export_json(self, output_path: str):
        """Export failure modes to JSON (alias for export_failure_modes_json)."""
        return self.export_failure_modes_json(output_path)


# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

def get_failure_modes_for_model(model_id: int) -> Optional[ModelFailureAnalysis]:
    """Get failure modes for a specific mental model."""
    return FAILURE_MODES_DATABASE.get(model_id)


def get_all_failure_modes() -> Dict[int, ModelFailureAnalysis]:
    """Get all failure modes for all models."""
    return FAILURE_MODES_DATABASE


def create_prevention_engine() -> FailurePreventionEngine:
    """Create a failure prevention engine instance."""
    return FailurePreventionEngine()


def count_failure_modes() -> Dict:
    """Count total failure modes in the database."""
    total_modes = sum(len(a.failure_modes) for a in FAILURE_MODES_DATABASE.values())
    total_models = len(FAILURE_MODES_DATABASE)
    
    return {
        "total_models": total_models,
        "total_failure_modes": total_modes,
        "average_per_model": total_modes / total_models if total_models > 0 else 0
    }


if __name__ == "__main__":
    # Demo and verification
    print("=" * 60)
    print("Mental Model Failure Modes Database")
    print("=" * 60)
    
    stats = count_failure_modes()
    print(f"\nTotal Models: {stats['total_models']}")
    print(f"Total Failure Modes: {stats['total_failure_modes']}")
    print(f"Average per Model: {stats['average_per_model']:.1f}")
    
    # Show example for Model 1
    print("\n" + "=" * 60)
    print("Example: Model 1 - Reward and Punishment Superresponse")
    print("=" * 60)
    
    analysis = get_failure_modes_for_model(1)
    if analysis:
        for fm in analysis.failure_modes:
            print(f"\n{fm.id}: {fm.description}")
            print(f"  Severity: {fm.severity.value}")
            print(f"  Example: {fm.example}")
            print(f"  Safeguards: {fm.safeguards[0]}")
    
    # Create engine and demo
    engine = create_prevention_engine()
    
    print("\n" + "=" * 60)
    print("Pre-Decision Checklist for Models [1, 15, 26, 68]")
    print("=" * 60)
    
    checklist = engine.generate_pre_decision_checklist([1, 15, 26, 68])
    for item in checklist:
        print(item)
    
    # Check for interactions
    print("\n" + "=" * 60)
    print("Interaction Warnings")
    print("=" * 60)
    
    warnings = engine.check_for_interactions([1, 2, 3, 4, 5, 15, 26])
    for w in warnings:
        print(f"⚠️ {w['type']}: {w['warning']}")
    
    # Export to JSON
    print("\n" + "=" * 60)
    print("Exporting to JSON...")
    print("=" * 60)
    
    engine.export_failure_modes_json("/tmp/failure_modes_complete.json")
    print("Exported to /tmp/failure_modes_complete.json")
