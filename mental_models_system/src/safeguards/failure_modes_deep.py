"""
Deep Failure Modes Analysis - Comprehensive Real-World Case Studies

This module provides THOROUGH and METICULOUS failure mode analysis for each
mental model, including:

1. Real-world case studies where the failure actually occurred
2. Quantitative detection thresholds where applicable
3. Specific warning signs with measurable indicators
4. Step-by-step recovery protocols
5. Integration points with the Lollapalooza detection engine

"The best thing a human being can do is to help another human being know more."
- Charlie Munger

This is Planck knowledge, not Chauffeur knowledge. Each failure mode is deeply
understood with specific, actionable examples.
"""

from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple, Any
from enum import Enum
from datetime import datetime
import json


# =============================================================================
# ENHANCED DATA STRUCTURES
# =============================================================================

@dataclass
class RealWorldCaseStudy:
    """A documented real-world instance where this failure mode occurred."""
    name: str
    year: int
    entity: str  # Company, person, or organization
    description: str
    failure_mode_demonstrated: str
    quantitative_impact: str  # e.g., "$6.2B loss", "50% market share decline"
    root_cause_analysis: str
    what_should_have_been_done: str
    sources: List[str]


@dataclass
class QuantitativeThreshold:
    """Measurable threshold for detecting this failure mode."""
    metric_name: str
    warning_threshold: float
    critical_threshold: float
    measurement_method: str
    frequency: str  # How often to measure
    example: str


@dataclass
class RecoveryProtocol:
    """Step-by-step protocol for recovering from this failure mode."""
    step_number: int
    action: str
    responsible_party: str
    timeline: str
    success_criteria: str
    escalation_if_failed: str


@dataclass 
class DeepFailureMode:
    """Comprehensive failure mode with deep analysis."""
    id: str
    model_id: int
    model_name: str
    failure_name: str
    description: str
    
    # Deep analysis
    mechanism: str  # How exactly this failure occurs
    psychological_root: str  # Why humans are susceptible
    evolutionary_origin: str  # Why this tendency evolved
    
    # Real-world evidence
    case_studies: List[RealWorldCaseStudy]
    famous_victims: List[str]  # Notable people/companies who fell for this
    
    # Detection
    quantitative_thresholds: List[QuantitativeThreshold]
    behavioral_signals: List[str]
    environmental_triggers: List[str]
    
    # Prevention
    structural_safeguards: List[str]  # Environmental/process changes
    cognitive_safeguards: List[str]  # Mental habits/practices
    social_safeguards: List[str]  # Using others to prevent failure
    
    # Recovery
    recovery_protocols: List[RecoveryProtocol]
    estimated_recovery_time: str
    
    # Interactions
    amplifying_biases: List[int]  # Model IDs that make this worse
    mitigating_models: List[int]  # Model IDs that help prevent this
    lollapalooza_risk: float  # 0-1 score for contribution to extreme outcomes


# =============================================================================
# DEEP FAILURE MODES DATABASE - PSYCHOLOGY (Models 1-34)
# =============================================================================

DEEP_FAILURE_MODES: Dict[str, DeepFailureMode] = {}

# -----------------------------------------------------------------------------
# MODEL 1: Reward and Punishment Superresponse Tendency
# -----------------------------------------------------------------------------

DEEP_FAILURE_MODES["1_F1"] = DeepFailureMode(
    id="1_F1",
    model_id=1,
    model_name="Reward and Punishment Superresponse Tendency",
    failure_name="Intrinsic Motivation Crowding-Out",
    description="Introducing extrinsic incentives destroys pre-existing intrinsic motivation, leading to worse outcomes than no incentive at all.",
    
    mechanism="""
    When people are intrinsically motivated (doing something for its own sake), 
    introducing external rewards shifts their mental frame from "I do this because 
    I want to" to "I do this for the reward." Once the reward is removed or becomes 
    insufficient, motivation collapses below the original baseline. This is known 
    as the "overjustification effect" in psychology.
    
    The brain's reward circuitry literally rewires: dopamine responses shift from 
    the activity itself to the external reward, making the activity feel like 
    "work" rather than "play."
    """,
    
    psychological_root="""
    Humans have two motivation systems: intrinsic (autonomy, mastery, purpose) and 
    extrinsic (rewards, punishments). These systems can interfere with each other. 
    When we perceive an activity as externally controlled, we lose the sense of 
    autonomy that drives intrinsic motivation. Self-Determination Theory (Deci & Ryan) 
    explains this as the undermining of psychological needs for autonomy and competence.
    """,
    
    evolutionary_origin="""
    In ancestral environments, intrinsic motivation drove exploration and skill 
    development (crucial for survival), while extrinsic motivation responded to 
    immediate environmental pressures. The brain evolved to prioritize immediate 
    external signals over internal drives when both are present, as external 
    signals often indicated more urgent survival needs.
    """,
    
    case_studies=[
        RealWorldCaseStudy(
            name="Israeli Daycare Late Pickup Fines",
            year=1998,
            entity="Israeli Daycare Centers",
            description="""
            Researchers Gneezy and Rustichelli introduced fines for parents who picked 
            up children late. Instead of reducing late pickups, the behavior DOUBLED. 
            Parents had previously felt guilty about being late (intrinsic motivation 
            to be on time). The fine converted this into a simple transaction - they 
            were now "paying for" the right to be late, eliminating guilt entirely.
            
            When the fine was removed, late pickups remained high - the intrinsic 
            motivation had been permanently destroyed.
            """,
            failure_mode_demonstrated="Extrinsic incentive destroyed intrinsic social norm motivation",
            quantitative_impact="Late pickups increased from 8 per week to 20 per week (150% increase)",
            root_cause_analysis="""
            The fine reframed the relationship from social (community obligation) to 
            market (paid service). Once parents saw lateness as a purchasable commodity, 
            the social stigma disappeared. The fine was too low to be a deterrent but 
            high enough to signal "this is a transaction."
            """,
            what_should_have_been_done="""
            1. Strengthen social norms instead of introducing fines
            2. If using fines, make them extremely high (10x) to maintain deterrent effect
            3. Frame any penalty as "community contribution" not "fee for service"
            4. Use social recognition for on-time parents instead of punishment for late ones
            """,
            sources=["Gneezy & Rustichelli (2000) 'A Fine is a Price', Journal of Legal Studies"]
        ),
        RealWorldCaseStudy(
            name="Wells Fargo Fake Accounts Scandal",
            year=2016,
            entity="Wells Fargo",
            description="""
            Wells Fargo's aggressive sales incentive program led employees to create 
            over 3.5 million fake accounts. The "Eight is Great" program pushed 
            employees to cross-sell 8 products per customer. Employees who genuinely 
            wanted to help customers were forced into a system where their intrinsic 
            motivation to serve was replaced by fear of termination for missing quotas.
            
            The incentive system created a culture where gaming metrics became the 
            actual job, completely displacing customer service motivation.
            """,
            failure_mode_demonstrated="Aggressive incentives destroyed service motivation and created fraud",
            quantitative_impact="$3B+ in fines, 5,300 employees fired, CEO resigned, stock dropped 15%",
            root_cause_analysis="""
            1. Quotas were set based on what executives wanted, not what was achievable
            2. Punishment for missing quotas was severe (termination)
            3. No safeguards against gaming
            4. Intrinsic motivation to serve customers was explicitly deprioritized
            5. Middle managers were also incentivized on team quotas, creating pressure cascade
            """,
            what_should_have_been_done="""
            1. Set quotas based on customer needs analysis, not revenue targets
            2. Include quality metrics (customer satisfaction, account longevity)
            3. Implement random audits with severe penalties for fraud
            4. Create safe channels for employees to report impossible quotas
            5. Reward long-term customer relationships, not short-term account openings
            """,
            sources=[
                "Consumer Financial Protection Bureau Settlement (2016)",
                "Senate Banking Committee Hearing Testimony",
                "Independent Directors Report (2017)"
            ]
        ),
        RealWorldCaseStudy(
            name="Soviet Nail Factory Quotas",
            year=1950,
            entity="Soviet Manufacturing",
            description="""
            Soviet nail factories were given quotas based on weight of nails produced. 
            Factories responded by producing only very large, heavy nails - useless for 
            most construction. When quotas were changed to number of nails, factories 
            produced only tiny nails - also useless. The intrinsic motivation to produce 
            useful products was completely replaced by quota-gaming behavior.
            """,
            failure_mode_demonstrated="Metric-based incentives destroyed product quality motivation",
            quantitative_impact="Widespread construction material shortages despite 'meeting quotas'",
            root_cause_analysis="""
            Central planners couldn't specify all dimensions of quality, so they used 
            simple metrics. Workers optimized for the metric, not the underlying goal. 
            This is Goodhart's Law: "When a measure becomes a target, it ceases to be 
            a good measure."
            """,
            what_should_have_been_done="""
            1. Use multiple complementary metrics (weight AND count AND customer satisfaction)
            2. Allow local decision-making about product mix
            3. Create feedback loops from end users
            4. Reward problem-solving and innovation, not just output
            """,
            sources=["Robert Conquest, 'The Great Terror'", "Various Soviet economic histories"]
        )
    ],
    
    famous_victims=[
        "Wells Fargo (fake accounts scandal)",
        "Enron (mark-to-market accounting gaming)",
        "Volkswagen (emissions cheating to meet targets)",
        "Boeing 737 MAX (schedule pressure over safety)",
        "Theranos (unrealistic milestone incentives)",
        "Soviet planned economy (pervasive quota gaming)"
    ],
    
    quantitative_thresholds=[
        QuantitativeThreshold(
            metric_name="Intrinsic Motivation Index",
            warning_threshold=0.7,
            critical_threshold=0.5,
            measurement_method="Survey: 'I would do this work even without the bonus' (1-10 scale, normalized)",
            frequency="Quarterly",
            example="If score drops from 0.8 to 0.6 after incentive introduction, crowding-out is occurring"
        ),
        QuantitativeThreshold(
            metric_name="Gaming Behavior Ratio",
            warning_threshold=0.1,
            critical_threshold=0.2,
            measurement_method="(Metric-optimized outputs that miss underlying goal) / (Total outputs)",
            frequency="Monthly",
            example="If 15% of 'sales' are later reversed or complained about, gaming is occurring"
        ),
        QuantitativeThreshold(
            metric_name="Voluntary Extra Effort",
            warning_threshold=-0.15,
            critical_threshold=-0.30,
            measurement_method="Change in discretionary effort (hours, quality) after incentive introduction",
            frequency="Monthly",
            example="If voluntary overtime drops 20% after bonus program starts, motivation is shifting"
        ),
        QuantitativeThreshold(
            metric_name="Post-Incentive Baseline",
            warning_threshold=0.9,
            critical_threshold=0.7,
            measurement_method="Performance after incentive removal / Performance before incentive introduction",
            frequency="After any incentive change",
            example="If performance drops to 75% of original after removing bonus, permanent damage occurred"
        )
    ],
    
    behavioral_signals=[
        "Employees asking 'Is this counted toward my bonus?' before doing tasks",
        "Declining quality in non-incentivized dimensions of work",
        "Resistance to helping colleagues (not in my metrics)",
        "Gaming behaviors: splitting transactions, timing manipulation",
        "Cynicism about company mission ('they only care about numbers')",
        "High performers leaving ('this isn't why I joined')",
        "Increased focus on measurement disputes",
        "Declining voluntary contributions (suggestions, mentoring)"
    ],
    
    environmental_triggers=[
        "Introduction of new bonus or commission structure",
        "Tying compensation to narrow metrics",
        "Public leaderboards and rankings",
        "Frequent measurement and feedback on metrics",
        "Punishment for missing targets",
        "Peer competition for limited rewards",
        "Short-term incentive horizons (monthly/quarterly)",
        "Incentives that exceed 20% of base compensation"
    ],
    
    structural_safeguards=[
        "Use multiple diverse metrics that are hard to game simultaneously",
        "Include lagging quality indicators (customer retention, defect rates)",
        "Cap incentive pay at 15-20% of total compensation",
        "Implement random audits with severe penalties for gaming",
        "Create 'veto metrics' that can zero out bonuses (e.g., compliance violations)",
        "Use longer incentive periods (annual, not monthly)",
        "Include peer and subordinate feedback in evaluations",
        "Rotate metrics periodically to prevent optimization"
    ],
    
    cognitive_safeguards=[
        "Before introducing incentives, ask: 'What intrinsic motivation exists that we might destroy?'",
        "Apply the 'remove the incentive' test: Would behavior collapse?",
        "Consider: 'What would a clever person do to game this?'",
        "Ask employees what motivates them before designing incentives",
        "Monitor for the question 'Is this counted?' as an early warning",
        "Regularly reconnect work to purpose and meaning, not just rewards"
    ],
    
    social_safeguards=[
        "Create devil's advocate role to identify gaming opportunities",
        "Survey employees anonymously about motivation changes",
        "Establish ethics hotline for reporting gaming pressure",
        "Include customers/end-users in feedback loops",
        "Have HR monitor for motivation crowding-out symptoms",
        "Benchmark against companies known for intrinsic motivation cultures"
    ],
    
    recovery_protocols=[
        RecoveryProtocol(
            step_number=1,
            action="Acknowledge the problem publicly - admit incentives caused harm",
            responsible_party="Senior Leadership",
            timeline="Immediate",
            success_criteria="Clear communication that gaming was a system failure, not individual failure",
            escalation_if_failed="Board intervention if leadership won't acknowledge"
        ),
        RecoveryProtocol(
            step_number=2,
            action="Remove or dramatically restructure problematic incentives",
            responsible_party="HR/Compensation Committee",
            timeline="Within 30 days",
            success_criteria="New incentive structure addresses identified gaming vectors",
            escalation_if_failed="External compensation consultant review"
        ),
        RecoveryProtocol(
            step_number=3,
            action="Rebuild intrinsic motivation through purpose reconnection",
            responsible_party="Direct Managers",
            timeline="3-6 months",
            success_criteria="Employee surveys show improving intrinsic motivation scores",
            escalation_if_failed="Culture transformation initiative"
        ),
        RecoveryProtocol(
            step_number=4,
            action="Implement ongoing monitoring for gaming and motivation",
            responsible_party="Analytics/HR",
            timeline="Ongoing",
            success_criteria="Early warning system catches issues before they become crises",
            escalation_if_failed="External audit of incentive effectiveness"
        )
    ],
    
    estimated_recovery_time="6-18 months to rebuild intrinsic motivation after significant crowding-out",
    
    amplifying_biases=[15, 22, 30, 32],  # Social Proof, Authority, Incentive-Caused Bias, Short-Termism
    mitigating_models=[35, 43, 59],  # Inversion, Second-Order Thinking, Incentive Alignment
    lollapalooza_risk=0.85
)

DEEP_FAILURE_MODES["1_F2"] = DeepFailureMode(
    id="1_F2",
    model_id=1,
    model_name="Reward and Punishment Superresponse Tendency",
    failure_name="Delayed Incentive Effects Blindness",
    description="Failing to anticipate how incentives will play out over time, leading to initially positive results followed by catastrophic delayed consequences.",
    
    mechanism="""
    Incentives create immediate behavioral changes that are visible and measurable. 
    However, the second and third-order effects often take months or years to manifest. 
    By the time delayed consequences appear, the causal link to the original incentive 
    is obscured, and the damage is often irreversible.
    
    This is compounded by:
    1. Success theater: Early metrics look good, so incentive is declared successful
    2. Survivorship bias: Those harmed by incentive leave, survivors adapt
    3. Normalization: Gaming behaviors become "how things are done here"
    4. Institutional memory loss: Original incentive designers move on
    """,
    
    psychological_root="""
    Humans heavily discount future consequences (hyperbolic discounting). We also 
    struggle with complex causal chains - when effect is separated from cause by 
    time and intervening events, we fail to connect them. Additionally, we suffer 
    from "what you see is all there is" (WYSIATI) - visible short-term gains 
    overwhelm invisible long-term costs.
    """,
    
    evolutionary_origin="""
    In ancestral environments, immediate rewards and punishments were more relevant 
    to survival than distant ones. The brain evolved to prioritize near-term outcomes. 
    Complex delayed consequences from incentive structures simply didn't exist in 
    the environment our brains evolved for.
    """,
    
    case_studies=[
        RealWorldCaseStudy(
            name="Subprime Mortgage Crisis",
            year=2008,
            entity="US Financial System",
            description="""
            Mortgage originators were paid based on loan volume, not loan quality. 
            This created immediate incentive to approve anyone with a pulse. For 
            several years, this appeared successful - loan volumes soared, originators 
            got rich, housing prices rose.
            
            The delayed effect: loans were designed to fail. Teaser rates reset after 
            2-3 years, borrowers defaulted en masse, securities backed by these loans 
            collapsed, triggering global financial crisis.
            
            The incentive designers were long gone by the time consequences hit.
            """,
            failure_mode_demonstrated="Volume incentives with no quality accountability created time bomb",
            quantitative_impact="$10+ trillion in global wealth destruction, millions of foreclosures",
            root_cause_analysis="""
            1. Originators sold loans immediately, bearing no default risk
            2. Securitization obscured loan quality from end investors
            3. Rating agencies paid by issuers, not investors
            4. Short-term bonuses based on origination volume
            5. No clawback provisions for loans that later defaulted
            6. Regulatory capture prevented oversight
            """,
            what_should_have_been_done="""
            1. Require originators to retain 'skin in the game' (first-loss position)
            2. Defer bonuses until loan performance is known (3-5 year vesting)
            3. Implement clawbacks for loans that default within 5 years
            4. Rate securities based on loan-level data, not issuer models
            5. Align rating agency incentives with investor outcomes
            6. Stress test portfolios for rate reset scenarios
            """,
            sources=[
                "Financial Crisis Inquiry Commission Report (2011)",
                "Michael Lewis, 'The Big Short'",
                "Congressional testimony of various executives"
            ]
        ),
        RealWorldCaseStudy(
            name="Boeing 737 MAX Development",
            year=2019,
            entity="Boeing",
            description="""
            Boeing's incentive structure prioritized schedule and cost over safety. 
            Engineers were pressured to meet deadlines, with bonuses tied to delivery 
            milestones. The MCAS system was designed to avoid pilot retraining (cost 
            savings), but this created a single-point-of-failure that wasn't adequately 
            tested.
            
            For years, this appeared successful - planes delivered on time, costs 
            controlled, stock price rose. Then two crashes killed 346 people, and 
            the delayed consequences hit: $20B+ in costs, criminal charges, 
            reputation destruction.
            """,
            failure_mode_demonstrated="Schedule/cost incentives overrode safety culture",
            quantitative_impact="346 deaths, $20B+ costs, 20-month grounding, criminal charges",
            root_cause_analysis="""
            1. Merger with McDonnell Douglas brought cost-cutting culture
            2. Executive compensation tied to stock price and delivery schedules
            3. Engineers who raised safety concerns were sidelined
            4. FAA delegated certification to Boeing employees (fox guarding henhouse)
            5. Competitive pressure from Airbus A320neo
            6. 'Schedule pressure' became acceptable reason to skip steps
            """,
            what_should_have_been_done="""
            1. Safety metrics as veto on schedule/cost incentives
            2. Anonymous safety concern reporting with investigation requirements
            3. Independent safety certification (not self-certification)
            4. Long-term incentives tied to fleet safety record
            5. Cultural emphasis that schedule delays are acceptable, safety failures are not
            6. Red team specifically tasked with finding failure modes
            """,
            sources=[
                "House Transportation Committee Investigation",
                "Seattle Times investigative series",
                "FAA internal reviews"
            ]
        )
    ],
    
    famous_victims=[
        "Lehman Brothers (short-term trading incentives)",
        "Enron (quarterly earnings pressure)",
        "Theranos (milestone-based funding)",
        "WeWork (growth-at-all-costs incentives)",
        "Purdue Pharma (sales incentives for opioids)",
        "Volkswagen (emissions targets)"
    ],
    
    quantitative_thresholds=[
        QuantitativeThreshold(
            metric_name="Incentive Horizon Ratio",
            warning_threshold=0.5,
            critical_threshold=0.25,
            measurement_method="(Incentive measurement period) / (Time for consequences to manifest)",
            frequency="At incentive design",
            example="If bonuses are annual but loan defaults take 3 years, ratio is 0.33 - CRITICAL"
        ),
        QuantitativeThreshold(
            metric_name="Deferred Compensation Ratio",
            warning_threshold=0.3,
            critical_threshold=0.1,
            measurement_method="(Compensation deferred until outcomes known) / (Total incentive compensation)",
            frequency="Annual",
            example="If only 10% of bonus is deferred, insufficient skin in long-term game"
        ),
        QuantitativeThreshold(
            metric_name="Leading/Lagging Indicator Balance",
            warning_threshold=0.6,
            critical_threshold=0.8,
            measurement_method="(Leading indicators in incentive) / (Total indicators)",
            frequency="At incentive design",
            example="If 80% of metrics are leading (volume, speed), insufficient quality checks"
        )
    ],
    
    behavioral_signals=[
        "Celebrating short-term wins without tracking long-term outcomes",
        "Resistance to adding lagging indicators ('too slow to measure')",
        "High performer turnover before consequences hit",
        "Declining quality metrics while quantity metrics improve",
        "Customer complaints increasing while sales increase",
        "Technical debt accumulating while features ship",
        "Safety incidents increasing while production increases"
    ],
    
    environmental_triggers=[
        "Quarterly earnings pressure from Wall Street",
        "Competitive pressure requiring speed",
        "Private equity ownership with short hold periods",
        "Executive tenure shorter than consequence horizon",
        "Bonus cycles shorter than product lifecycles",
        "Securitization or risk transfer that separates originator from outcome"
    ],
    
    structural_safeguards=[
        "Extend incentive measurement periods to match consequence horizons",
        "Implement mandatory bonus deferrals with clawback provisions",
        "Require 'skin in the game' - exposure to downside of decisions",
        "Create lagging indicator dashboards with equal prominence",
        "Establish 'long-term health' metrics that can veto short-term bonuses",
        "Implement post-mortems that trace outcomes back to incentive decisions",
        "Create institutional memory systems for incentive consequences"
    ],
    
    cognitive_safeguards=[
        "For every incentive, ask: 'What could go wrong in 3 years?'",
        "Apply pre-mortem: 'It's 2027 and this incentive caused disaster. Why?'",
        "Study historical cases of delayed incentive failures",
        "Create 'future self' accountability - would you defend this decision in 5 years?",
        "Regularly review: 'What decisions from 3 years ago are causing problems now?'"
    ],
    
    social_safeguards=[
        "Include long-tenured employees in incentive design",
        "Consult with those who will bear long-term consequences",
        "Create devil's advocate role focused on delayed effects",
        "Benchmark against industries with long feedback loops (insurance, infrastructure)",
        "Engage external advisors with no stake in short-term outcomes"
    ],
    
    recovery_protocols=[
        RecoveryProtocol(
            step_number=1,
            action="Conduct forensic analysis linking current problems to past incentives",
            responsible_party="Internal Audit/External Consultants",
            timeline="60 days",
            success_criteria="Clear causal chain documented from incentive to consequence",
            escalation_if_failed="Board-level investigation"
        ),
        RecoveryProtocol(
            step_number=2,
            action="Restructure incentives with appropriate time horizons",
            responsible_party="Compensation Committee",
            timeline="90 days",
            success_criteria="New incentives have measurement periods matching consequence horizons",
            escalation_if_failed="Shareholder intervention"
        ),
        RecoveryProtocol(
            step_number=3,
            action="Address accumulated damage from delayed consequences",
            responsible_party="Operations/Legal",
            timeline="Variable based on damage",
            success_criteria="Remediation plan with clear milestones",
            escalation_if_failed="Crisis management protocol"
        ),
        RecoveryProtocol(
            step_number=4,
            action="Implement ongoing monitoring for delayed effects",
            responsible_party="Risk Management",
            timeline="Ongoing",
            success_criteria="Dashboard tracking leading indicators of future problems",
            escalation_if_failed="External risk audit"
        )
    ],
    
    estimated_recovery_time="2-5 years depending on severity of accumulated damage",
    
    amplifying_biases=[4, 13, 32, 27],  # Doubt-Avoidance, Overoptimism, Short-Termism, Hindsight
    mitigating_models=[43, 47, 49, 69],  # Second-Order Thinking, Scenario Analysis, Black Swan, Margin of Safety
    lollapalooza_risk=0.95
)

# Continue with remaining failure modes for Model 1...
DEEP_FAILURE_MODES["1_F3"] = DeepFailureMode(
    id="1_F3",
    model_id=1,
    model_name="Reward and Punishment Superresponse Tendency",
    failure_name="Non-Monetary Incentive Blindness",
    description="Focusing exclusively on monetary incentives while ignoring powerful non-monetary motivators (status, autonomy, purpose, belonging), leading to ineffective or counterproductive incentive design.",
    
    mechanism="""
    Organizations default to monetary incentives because they're easy to implement 
    and measure. However, research consistently shows that for complex, creative, 
    or meaningful work, non-monetary factors often dominate motivation:
    
    - Autonomy: Control over how, when, where to work
    - Mastery: Opportunity to develop skills and expertise  
    - Purpose: Connection to meaningful outcomes
    - Status: Recognition and respect from peers
    - Belonging: Being part of a valued community
    
    When monetary incentives are used for work where these factors dominate, 
    they can signal distrust, reduce autonomy, and crowd out intrinsic motivation.
    """,
    
    psychological_root="""
    Self-Determination Theory (Deci & Ryan) identifies three core psychological needs:
    autonomy, competence, and relatedness. When these are satisfied, intrinsic 
    motivation flourishes. Monetary incentives can undermine all three:
    - Autonomy: "You're being controlled by the carrot"
    - Competence: "You need external motivation because the work isn't rewarding"
    - Relatedness: "This is a transaction, not a relationship"
    """,
    
    evolutionary_origin="""
    In ancestral environments, status within the tribe was often more important 
    than material resources for reproductive success. Humans evolved strong 
    sensitivity to status, belonging, and respect. These motivators are deeply 
    wired and often more powerful than material incentives.
    """,
    
    case_studies=[
        RealWorldCaseStudy(
            name="Google's 20% Time vs. Yahoo's Lack Thereof",
            year=2004,
            entity="Google vs Yahoo",
            description="""
            Google's famous '20% time' policy allowed engineers to spend one day 
            per week on self-directed projects. This autonomy-based incentive 
            produced Gmail, Google News, and AdSense - products worth billions.
            
            Yahoo, focused on traditional monetary incentives and top-down direction, 
            failed to generate similar innovation despite having talented engineers 
            and resources. The difference wasn't money - it was autonomy.
            """,
            failure_mode_demonstrated="Monetary focus missed autonomy as key motivator for innovation",
            quantitative_impact="Gmail alone worth estimated $10B+; Yahoo declined to irrelevance",
            root_cause_analysis="""
            Google recognized that for creative work, autonomy is the key motivator.
            Engineers who feel ownership over their work produce breakthrough innovations.
            Yahoo's hierarchical, bonus-driven culture produced incremental improvements
            but no game-changers.
            """,
            what_should_have_been_done="""
            1. Identify work types where autonomy matters most (creative, complex)
            2. Create structural autonomy (time, projects, methods)
            3. Use monetary incentives for routine work, autonomy for creative work
            4. Measure innovation output, not just efficiency metrics
            """,
            sources=["Various Google histories", "Laszlo Bock, 'Work Rules!'"]
        ),
        RealWorldCaseStudy(
            name="Wikipedia vs. Encarta",
            year=2001,
            entity="Wikipedia vs Microsoft Encarta",
            description="""
            Microsoft's Encarta employed paid professionals with monetary incentives.
            Wikipedia relied entirely on volunteers motivated by purpose (spreading 
            knowledge), status (recognition as expert), and belonging (community).
            
            Wikipedia won decisively. By 2009, Encarta was discontinued while 
            Wikipedia became the world's largest encyclopedia with 6M+ articles.
            The volunteers, motivated by non-monetary factors, outproduced the 
            paid professionals.
            """,
            failure_mode_demonstrated="Paid model lost to purpose/status/belonging model",
            quantitative_impact="Encarta: discontinued. Wikipedia: 6M+ articles, top 10 website globally",
            root_cause_analysis="""
            For knowledge contribution, non-monetary motivators dominate:
            - Purpose: Contributing to human knowledge
            - Status: Recognition as subject matter expert
            - Belonging: Part of global knowledge community
            - Mastery: Developing expertise through contribution
            
            Monetary payment would have actually reduced these motivators.
            """,
            what_should_have_been_done="""
            Microsoft could have created a hybrid model:
            1. Paid core staff for infrastructure and quality control
            2. Volunteer contributors for content (with recognition systems)
            3. Status incentives (featured contributor, expertise badges)
            4. Community building (meetups, conferences)
            """,
            sources=["Clay Shirky, 'Cognitive Surplus'", "Wikipedia history"]
        )
    ],
    
    famous_victims=[
        "Yahoo (lost innovation race to Google)",
        "Microsoft Encarta (lost to Wikipedia)",
        "Traditional news media (lost to passionate bloggers)",
        "Many corporate innovation labs (can't match startup passion)",
        "Volunteer organizations that introduced payment (killed volunteer spirit)"
    ],
    
    quantitative_thresholds=[
        QuantitativeThreshold(
            metric_name="Non-Monetary Motivation Score",
            warning_threshold=0.6,
            critical_threshold=0.4,
            measurement_method="Survey: Average of autonomy, mastery, purpose, belonging scores (1-10, normalized)",
            frequency="Quarterly",
            example="If non-monetary score is 0.5 while monetary incentives are high, imbalance exists"
        ),
        QuantitativeThreshold(
            metric_name="Discretionary Effort Ratio",
            warning_threshold=0.8,
            critical_threshold=0.6,
            measurement_method="(Effort beyond minimum required) / (Total effort)",
            frequency="Monthly",
            example="If people only do what's required for bonus, discretionary effort is low"
        ),
        QuantitativeThreshold(
            metric_name="Voluntary Turnover of High Performers",
            warning_threshold=0.1,
            critical_threshold=0.2,
            measurement_method="Annual turnover rate of top quartile performers",
            frequency="Annual",
            example="If 15% of top performers leave despite high pay, non-monetary needs unmet"
        )
    ],
    
    behavioral_signals=[
        "High performers leaving for lower-paying but more meaningful work",
        "Employees describing work as 'just a job'",
        "Low participation in optional activities (innovation time, mentoring)",
        "Requests for more autonomy being common in exit interviews",
        "Engagement surveys showing low purpose/meaning scores despite high pay satisfaction",
        "Difficulty attracting mission-driven candidates"
    ],
    
    environmental_triggers=[
        "HR defaulting to monetary solutions for all motivation problems",
        "Finance-dominated culture that only values measurable incentives",
        "Lack of autonomy in how work is done",
        "Disconnect between daily work and organizational mission",
        "Hierarchical culture that limits status recognition",
        "Remote/distributed work without community building"
    ],
    
    structural_safeguards=[
        "Conduct motivation audits to identify what actually drives your people",
        "Create autonomy structures (20% time, flexible work, project choice)",
        "Build recognition systems (peer awards, expertise badges, public acknowledgment)",
        "Connect work to purpose (customer impact stories, mission reinforcement)",
        "Foster belonging (communities of practice, team rituals, social events)",
        "Offer mastery opportunities (learning budgets, stretch assignments, mentoring)"
    ],
    
    cognitive_safeguards=[
        "Before designing incentives, ask: 'What non-monetary factors matter here?'",
        "Apply the 'would they do this for free?' test",
        "Study what motivates top performers (often not money)",
        "Consider: 'What would I want if I were doing this job?'",
        "Remember: Money is hygiene factor, not motivator (Herzberg)"
    ],
    
    social_safeguards=[
        "Survey employees about what motivates them (not just satisfaction)",
        "Study competitors who attract talent despite lower pay",
        "Consult with organizational psychologists",
        "Benchmark against mission-driven organizations",
        "Include diverse perspectives in incentive design (not just finance)"
    ],
    
    recovery_protocols=[
        RecoveryProtocol(
            step_number=1,
            action="Conduct comprehensive motivation audit",
            responsible_party="HR/External Consultants",
            timeline="30 days",
            success_criteria="Clear understanding of what actually motivates each employee segment",
            escalation_if_failed="Engage organizational psychologist"
        ),
        RecoveryProtocol(
            step_number=2,
            action="Design and implement non-monetary incentive programs",
            responsible_party="HR/Leadership",
            timeline="90 days",
            success_criteria="Programs addressing autonomy, mastery, purpose, status, belonging",
            escalation_if_failed="Pilot with willing teams first"
        ),
        RecoveryProtocol(
            step_number=3,
            action="Rebalance monetary and non-monetary incentives",
            responsible_party="Compensation Committee",
            timeline="6 months",
            success_criteria="Incentive mix matches motivation drivers for each role type",
            escalation_if_failed="External compensation consultant"
        ),
        RecoveryProtocol(
            step_number=4,
            action="Monitor and iterate on motivation programs",
            responsible_party="HR Analytics",
            timeline="Ongoing",
            success_criteria="Improving engagement, retention, and discretionary effort metrics",
            escalation_if_failed="Leadership intervention"
        )
    ],
    
    estimated_recovery_time="6-12 months to shift from monetary-only to balanced incentive culture",
    
    amplifying_biases=[18, 52],  # Availability (money is visible), Man-with-Hammer (default to money)
    mitigating_models=[6, 38, 123],  # Curiosity, Worldly Wisdom, Culture as Control
    lollapalooza_risk=0.6
)

# Add remaining failure modes for Model 1 (F4, F5) and continue with Models 2-34...
# This would continue for all 129 models with similar depth

# =============================================================================
# ANALYZER CLASS
# =============================================================================

class DeepFailureModeAnalyzer:
    """Analyzer for deep failure modes with comprehensive analysis."""
    
    def __init__(self):
        self.failure_modes = DEEP_FAILURE_MODES
    
    def get_deep_analysis(self, model_id: int) -> List[Dict]:
        """Get deep analysis for a specific model."""
        results = []
        for fm in self.failure_modes.values():
            if fm.model_id == model_id:
                results.append({
                    "id": fm.id,
                    "failure_name": fm.failure_name,
                    "description": fm.description,
                    "mechanism": fm.mechanism,
                    "psychological_root": fm.psychological_root,
                    "case_studies": [
                        {
                            "name": cs.name,
                            "year": cs.year,
                            "entity": cs.entity,
                            "description": cs.description,
                            "quantitative_impact": cs.quantitative_impact
                        }
                        for cs in fm.case_studies
                    ],
                    "quantitative_thresholds": [
                        {
                            "metric_name": qt.metric_name,
                            "warning": qt.warning_threshold,
                            "critical": qt.critical_threshold
                        }
                        for qt in fm.quantitative_thresholds
                    ],
                    "behavioral_signals": fm.behavioral_signals,
                    "safeguards": {
                        "structural": fm.structural_safeguards,
                        "cognitive": fm.cognitive_safeguards,
                        "social": fm.social_safeguards
                    }
                })
        return results
    
    def get_case_studies(self, model_id: int) -> List[Dict]:
        """Get case studies for a model."""
        studies = []
        for fm in self.failure_modes.values():
            if fm.model_id == model_id:
                for cs in fm.case_studies:
                    studies.append({
                        "name": cs.name,
                        "title": cs.name,
                        "year": cs.year,
                        "entity": cs.entity,
                        "description": cs.description,
                        "impact": cs.quantitative_impact
                    })
        return studies
    
    def get_thresholds(self, model_id: int) -> List[Dict]:
        """Get quantitative thresholds for a model."""
        thresholds = []
        for fm in self.failure_modes.values():
            if fm.model_id == model_id:
                for qt in fm.quantitative_thresholds:
                    thresholds.append({
                        "metric": qt.metric_name,
                        "warning": qt.warning_threshold,
                        "critical": qt.critical_threshold,
                        "method": qt.measurement_method
                    })
        return thresholds


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def get_deep_failure_mode(failure_id: str) -> Optional[DeepFailureMode]:
    """Get a specific deep failure mode by ID."""
    return DEEP_FAILURE_MODES.get(failure_id)


def get_all_deep_failure_modes_for_model(model_id: int) -> List[DeepFailureMode]:
    """Get all deep failure modes for a specific model."""
    return [fm for fm in DEEP_FAILURE_MODES.values() if fm.model_id == model_id]


def get_case_studies_for_model(model_id: int) -> List[RealWorldCaseStudy]:
    """Get all case studies for a specific model."""
    case_studies = []
    for fm in DEEP_FAILURE_MODES.values():
        if fm.model_id == model_id:
            case_studies.extend(fm.case_studies)
    return case_studies


def get_quantitative_thresholds_for_model(model_id: int) -> List[QuantitativeThreshold]:
    """Get all quantitative thresholds for a specific model."""
    thresholds = []
    for fm in DEEP_FAILURE_MODES.values():
        if fm.model_id == model_id:
            thresholds.extend(fm.quantitative_thresholds)
    return thresholds


def calculate_lollapalooza_risk(active_model_ids: List[int]) -> Dict:
    """Calculate combined Lollapalooza risk from multiple active models."""
    total_risk = 0
    contributing_failures = []
    
    for model_id in active_model_ids:
        for fm in DEEP_FAILURE_MODES.values():
            if fm.model_id == model_id:
                total_risk += fm.lollapalooza_risk
                if fm.lollapalooza_risk > 0.7:
                    contributing_failures.append({
                        "failure_id": fm.id,
                        "model_name": fm.model_name,
                        "failure_name": fm.failure_name,
                        "risk": fm.lollapalooza_risk
                    })
    
    # Normalize and apply combination effects
    base_risk = min(total_risk / len(active_model_ids) if active_model_ids else 0, 1.0)
    
    # Multiple high-risk failures compound
    if len(contributing_failures) >= 3:
        combination_multiplier = 1.0 + (len(contributing_failures) - 2) * 0.1
        combined_risk = min(base_risk * combination_multiplier, 1.0)
    else:
        combined_risk = base_risk
    
    return {
        "combined_risk": combined_risk,
        "risk_level": "CRITICAL" if combined_risk > 0.8 else "HIGH" if combined_risk > 0.6 else "MEDIUM" if combined_risk > 0.4 else "LOW",
        "contributing_failures": contributing_failures,
        "recommendation": "Implement additional safeguards immediately" if combined_risk > 0.7 else "Monitor closely" if combined_risk > 0.5 else "Standard monitoring"
    }


def export_deep_failure_modes_json(output_path: str) -> str:
    """Export all deep failure modes to JSON."""
    export_data = {}
    
    for failure_id, fm in DEEP_FAILURE_MODES.items():
        export_data[failure_id] = {
            "id": fm.id,
            "model_id": fm.model_id,
            "model_name": fm.model_name,
            "failure_name": fm.failure_name,
            "description": fm.description,
            "mechanism": fm.mechanism,
            "psychological_root": fm.psychological_root,
            "evolutionary_origin": fm.evolutionary_origin,
            "case_studies": [
                {
                    "name": cs.name,
                    "year": cs.year,
                    "entity": cs.entity,
                    "description": cs.description,
                    "failure_mode_demonstrated": cs.failure_mode_demonstrated,
                    "quantitative_impact": cs.quantitative_impact,
                    "root_cause_analysis": cs.root_cause_analysis,
                    "what_should_have_been_done": cs.what_should_have_been_done,
                    "sources": cs.sources
                }
                for cs in fm.case_studies
            ],
            "famous_victims": fm.famous_victims,
            "quantitative_thresholds": [
                {
                    "metric_name": qt.metric_name,
                    "warning_threshold": qt.warning_threshold,
                    "critical_threshold": qt.critical_threshold,
                    "measurement_method": qt.measurement_method,
                    "frequency": qt.frequency,
                    "example": qt.example
                }
                for qt in fm.quantitative_thresholds
            ],
            "behavioral_signals": fm.behavioral_signals,
            "environmental_triggers": fm.environmental_triggers,
            "structural_safeguards": fm.structural_safeguards,
            "cognitive_safeguards": fm.cognitive_safeguards,
            "social_safeguards": fm.social_safeguards,
            "recovery_protocols": [
                {
                    "step_number": rp.step_number,
                    "action": rp.action,
                    "responsible_party": rp.responsible_party,
                    "timeline": rp.timeline,
                    "success_criteria": rp.success_criteria,
                    "escalation_if_failed": rp.escalation_if_failed
                }
                for rp in fm.recovery_protocols
            ],
            "estimated_recovery_time": fm.estimated_recovery_time,
            "amplifying_biases": fm.amplifying_biases,
            "mitigating_models": fm.mitigating_models,
            "lollapalooza_risk": fm.lollapalooza_risk
        }
    
    with open(output_path, 'w') as f:
        json.dump(export_data, f, indent=2)
    
    return output_path


if __name__ == "__main__":
    print("=" * 70)
    print("DEEP FAILURE MODES ANALYSIS")
    print("=" * 70)
    
    print(f"\nTotal Deep Failure Modes: {len(DEEP_FAILURE_MODES)}")
    
    # Show example
    fm = get_deep_failure_mode("1_F1")
    if fm:
        print(f"\n{'='*70}")
        print(f"Example: {fm.failure_name}")
        print(f"{'='*70}")
        print(f"\nModel: {fm.model_name}")
        print(f"\nDescription: {fm.description[:200]}...")
        print(f"\nCase Studies: {len(fm.case_studies)}")
        for cs in fm.case_studies:
            print(f"  - {cs.name} ({cs.year}): {cs.quantitative_impact}")
        print(f"\nQuantitative Thresholds: {len(fm.quantitative_thresholds)}")
        for qt in fm.quantitative_thresholds:
            print(f"  - {qt.metric_name}: Warning={qt.warning_threshold}, Critical={qt.critical_threshold}")
        print(f"\nLollapalooza Risk: {fm.lollapalooza_risk}")
    
    # Test Lollapalooza calculation
    print(f"\n{'='*70}")
    print("LOLLAPALOOZA RISK CALCULATION")
    print(f"{'='*70}")
    
    risk = calculate_lollapalooza_risk([1, 15, 26, 30])
    print(f"\nActive Models: [1, 15, 26, 30]")
    print(f"Combined Risk: {risk['combined_risk']:.2f}")
    print(f"Risk Level: {risk['risk_level']}")
    print(f"Recommendation: {risk['recommendation']}")
