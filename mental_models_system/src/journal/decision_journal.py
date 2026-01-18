"""
Decision Journal System

A systematic approach to recording, analyzing, and learning from decisions.
Based on principles from:
- Charlie Munger's checklist approach
- Ray Dalio's radical transparency
- Daniel Kahneman's pre-mortem technique
- Jim Simons' data-driven feedback loops

"The only way to improve decision-making is to keep score."
- Charlie Munger

This system:
1. Records decisions with context and reasoning
2. Links decisions to mental models used
3. Tracks outcomes and calibrates confidence
4. Identifies patterns in decision quality
5. Generates insights for improvement
"""

import json
import uuid
import hashlib
from dataclasses import dataclass, field, asdict
from datetime import datetime, timedelta
from enum import Enum
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Any
from collections import defaultdict
import statistics
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class DecisionType(Enum):
    """Categories of decisions."""
    INVESTMENT = "investment"
    STRATEGIC = "strategic"
    OPERATIONAL = "operational"
    HIRING = "hiring"
    PRODUCT = "product"
    RISK = "risk"
    ALLOCATION = "allocation"
    EXIT = "exit"
    PARTNERSHIP = "partnership"
    OTHER = "other"


class DecisionOutcome(Enum):
    """Possible outcomes of a decision."""
    PENDING = "pending"
    SUCCESS = "success"
    PARTIAL_SUCCESS = "partial_success"
    NEUTRAL = "neutral"
    PARTIAL_FAILURE = "partial_failure"
    FAILURE = "failure"
    UNKNOWN = "unknown"


class ConfidenceLevel(Enum):
    """Confidence levels for predictions."""
    VERY_LOW = 0.2
    LOW = 0.4
    MEDIUM = 0.6
    HIGH = 0.8
    VERY_HIGH = 0.95


@dataclass
class DecisionContext:
    """Context surrounding a decision."""
    market_conditions: str = ""
    time_pressure: str = ""  # low, medium, high, extreme
    information_quality: str = ""  # poor, limited, adequate, good, excellent
    emotional_state: str = ""  # calm, anxious, excited, fearful, confident
    key_uncertainties: List[str] = field(default_factory=list)
    stakeholders: List[str] = field(default_factory=list)
    constraints: List[str] = field(default_factory=list)
    alternatives_considered: List[str] = field(default_factory=list)


@dataclass
class PreMortem:
    """Pre-mortem analysis - imagining future failure."""
    failure_scenarios: List[str] = field(default_factory=list)
    warning_signs: List[str] = field(default_factory=list)
    mitigation_strategies: List[str] = field(default_factory=list)
    kill_criteria: List[str] = field(default_factory=list)  # When to abandon


@dataclass
class PostMortem:
    """Post-mortem analysis after outcome is known."""
    actual_outcome: DecisionOutcome = DecisionOutcome.PENDING
    outcome_date: Optional[datetime] = None
    outcome_description: str = ""
    what_went_right: List[str] = field(default_factory=list)
    what_went_wrong: List[str] = field(default_factory=list)
    luck_vs_skill: str = ""  # mostly_luck, mixed, mostly_skill
    lessons_learned: List[str] = field(default_factory=list)
    would_decide_differently: bool = False
    different_decision_reasoning: str = ""
    process_quality_score: float = 0.0  # 0-1, independent of outcome


@dataclass
class Decision:
    """A recorded decision with full context and analysis."""
    id: str
    created_at: datetime
    decision_type: DecisionType
    title: str
    description: str
    
    # The actual decision
    decision_made: str
    rationale: str
    
    # Mental models applied
    mental_models_used: List[int]  # Model IDs
    model_reasoning: Dict[int, str]  # How each model was applied
    
    # Predictions
    predicted_outcome: str
    confidence: float  # 0-1
    expected_value: Optional[float] = None
    time_horizon: str = ""  # e.g., "6 months", "2 years"
    
    # Analysis
    context: DecisionContext = field(default_factory=DecisionContext)
    pre_mortem: PreMortem = field(default_factory=PreMortem)
    post_mortem: PostMortem = field(default_factory=PostMortem)
    
    # Metadata
    tags: List[str] = field(default_factory=list)
    related_decisions: List[str] = field(default_factory=list)
    notes: str = ""
    
    # Checklist completion
    checklist_items_completed: List[str] = field(default_factory=list)
    checklist_items_skipped: List[str] = field(default_factory=list)


class DecisionChecklist:
    """
    Munger-style checklist for decision quality.
    
    "No wise pilot, no matter how great his talent and experience,
    fails to use his checklist." - Charlie Munger
    """
    
    STANDARD_CHECKLIST = [
        # Inversion
        ("inversion", "Have I inverted the problem? What would guarantee failure?"),
        ("second_order", "Have I considered second and third-order effects?"),
        
        # Biases
        ("incentives", "What are the incentives at play? Who benefits?"),
        ("social_proof", "Am I being influenced by what others are doing?"),
        ("confirmation", "Have I actively sought disconfirming evidence?"),
        ("anchoring", "Am I anchored to an initial number or idea?"),
        ("availability", "Am I overweighting recent or vivid information?"),
        ("sunk_cost", "Am I considering sunk costs that shouldn't matter?"),
        
        # Circle of Competence
        ("competence", "Is this within my circle of competence?"),
        ("expert_check", "Have I consulted someone with relevant expertise?"),
        
        # Risk
        ("downside", "What is the maximum downside? Can I survive it?"),
        ("reversibility", "Is this decision reversible? At what cost?"),
        ("optionality", "Does this preserve or destroy future options?"),
        
        # Time
        ("time_pressure", "Is the time pressure real or artificial?"),
        ("opportunity_cost", "What am I giving up by choosing this?"),
        
        # Quality
        ("information", "Do I have enough information to decide?"),
        ("sleep_test", "Would I be comfortable if this decision were public?"),
        ("10_10_10", "How will I feel about this in 10 minutes, 10 months, 10 years?"),
    ]
    
    @classmethod
    def get_checklist(cls) -> List[Tuple[str, str]]:
        """Get the standard decision checklist."""
        return cls.STANDARD_CHECKLIST.copy()
    
    @classmethod
    def evaluate_completion(cls, completed: List[str]) -> Tuple[float, List[str]]:
        """
        Evaluate checklist completion.
        
        Returns:
            Tuple of (completion_score, missing_items)
        """
        all_items = set(item[0] for item in cls.STANDARD_CHECKLIST)
        completed_set = set(completed)
        
        missing = all_items - completed_set
        score = len(completed_set) / len(all_items)
        
        return score, list(missing)


class DecisionJournal:
    """
    Main decision journal system for recording and analyzing decisions.
    """
    
    def __init__(self, storage_path: Optional[str] = None):
        """
        Initialize the decision journal.
        
        Args:
            storage_path: Path to store journal data (JSON file)
        """
        self.storage_path = Path(storage_path) if storage_path else None
        self.decisions: Dict[str, Decision] = {}
        self.models_data: Dict[int, Dict] = {}
        
        # Load existing data
        if self.storage_path and self.storage_path.exists():
            self._load()
    
    def _load(self):
        """Load journal from storage."""
        try:
            with open(self.storage_path, 'r') as f:
                data = json.load(f)
            
            for d in data.get('decisions', []):
                # Reconstruct Decision objects
                d['created_at'] = datetime.fromisoformat(d['created_at'])
                d['decision_type'] = DecisionType(d['decision_type'])
                d['context'] = DecisionContext(**d.get('context', {}))
                d['pre_mortem'] = PreMortem(**d.get('pre_mortem', {}))
                
                pm = d.get('post_mortem', {})
                if pm.get('outcome_date'):
                    pm['outcome_date'] = datetime.fromisoformat(pm['outcome_date'])
                pm['actual_outcome'] = DecisionOutcome(pm.get('actual_outcome', 'pending'))
                d['post_mortem'] = PostMortem(**pm)
                
                decision = Decision(**d)
                self.decisions[decision.id] = decision
            
            logger.info(f"Loaded {len(self.decisions)} decisions from journal")
        except Exception as e:
            logger.error(f"Error loading journal: {e}")
    
    def _save(self):
        """Save journal to storage."""
        if not self.storage_path:
            return
        
        try:
            self.storage_path.parent.mkdir(parents=True, exist_ok=True)
            
            data = {
                'decisions': [],
                'metadata': {
                    'last_updated': datetime.now().isoformat(),
                    'total_decisions': len(self.decisions)
                }
            }
            
            for d in self.decisions.values():
                d_dict = asdict(d)
                d_dict['created_at'] = d.created_at.isoformat()
                d_dict['decision_type'] = d.decision_type.value
                d_dict['post_mortem']['actual_outcome'] = d.post_mortem.actual_outcome.value
                if d.post_mortem.outcome_date:
                    d_dict['post_mortem']['outcome_date'] = d.post_mortem.outcome_date.isoformat()
                data['decisions'].append(d_dict)
            
            with open(self.storage_path, 'w') as f:
                json.dump(data, f, indent=2)
            
            logger.info(f"Saved {len(self.decisions)} decisions to journal")
        except Exception as e:
            logger.error(f"Error saving journal: {e}")
    
    def load_models(self, models_path: str):
        """Load mental models data for reference."""
        try:
            with open(models_path, 'r') as f:
                data = json.load(f)
            for model in data.get('mental_models', []):
                self.models_data[model['id']] = model
            logger.info(f"Loaded {len(self.models_data)} mental models")
        except Exception as e:
            logger.error(f"Error loading models: {e}")
    
    def create_decision(self,
                       decision_type: DecisionType,
                       title: str,
                       description: str,
                       decision_made: str,
                       rationale: str,
                       mental_models_used: List[int],
                       predicted_outcome: str,
                       confidence: float,
                       **kwargs) -> Decision:
        """
        Create and record a new decision.
        
        Args:
            decision_type: Category of decision
            title: Brief title
            description: Full description of the situation
            decision_made: The actual decision
            rationale: Reasoning behind the decision
            mental_models_used: List of mental model IDs applied
            predicted_outcome: Expected result
            confidence: Confidence level (0-1)
            **kwargs: Additional Decision fields
        
        Returns:
            The created Decision object
        """
        decision_id = str(uuid.uuid4())[:8]
        
        decision = Decision(
            id=decision_id,
            created_at=datetime.now(),
            decision_type=decision_type,
            title=title,
            description=description,
            decision_made=decision_made,
            rationale=rationale,
            mental_models_used=mental_models_used,
            model_reasoning=kwargs.get('model_reasoning', {}),
            predicted_outcome=predicted_outcome,
            confidence=confidence,
            expected_value=kwargs.get('expected_value'),
            time_horizon=kwargs.get('time_horizon', ''),
            context=kwargs.get('context', DecisionContext()),
            pre_mortem=kwargs.get('pre_mortem', PreMortem()),
            tags=kwargs.get('tags', []),
            notes=kwargs.get('notes', '')
        )
        
        self.decisions[decision_id] = decision
        self._save()
        
        logger.info(f"Created decision: {decision_id} - {title}")
        return decision
    
    def record_outcome(self,
                      decision_id: str,
                      outcome: DecisionOutcome,
                      outcome_description: str,
                      what_went_right: List[str] = None,
                      what_went_wrong: List[str] = None,
                      lessons_learned: List[str] = None,
                      luck_vs_skill: str = "mixed",
                      process_quality_score: float = 0.5,
                      would_decide_differently: bool = False,
                      different_decision_reasoning: str = "") -> Decision:
        """
        Record the outcome of a decision.
        
        Args:
            decision_id: ID of the decision
            outcome: The actual outcome
            outcome_description: Description of what happened
            what_went_right: List of things that went well
            what_went_wrong: List of things that went poorly
            lessons_learned: Key takeaways
            luck_vs_skill: Assessment of luck vs skill contribution
            process_quality_score: Quality of decision process (0-1)
            would_decide_differently: Whether you'd decide differently
            different_decision_reasoning: Why you'd decide differently
        
        Returns:
            Updated Decision object
        """
        if decision_id not in self.decisions:
            raise ValueError(f"Decision {decision_id} not found")
        
        decision = self.decisions[decision_id]
        decision.post_mortem = PostMortem(
            actual_outcome=outcome,
            outcome_date=datetime.now(),
            outcome_description=outcome_description,
            what_went_right=what_went_right or [],
            what_went_wrong=what_went_wrong or [],
            luck_vs_skill=luck_vs_skill,
            lessons_learned=lessons_learned or [],
            would_decide_differently=would_decide_differently,
            different_decision_reasoning=different_decision_reasoning,
            process_quality_score=process_quality_score
        )
        
        self._save()
        logger.info(f"Recorded outcome for decision: {decision_id}")
        return decision
    
    def get_decision(self, decision_id: str) -> Optional[Decision]:
        """Get a decision by ID."""
        return self.decisions.get(decision_id)
    
    def search_decisions(self,
                        decision_type: Optional[DecisionType] = None,
                        outcome: Optional[DecisionOutcome] = None,
                        model_id: Optional[int] = None,
                        tag: Optional[str] = None,
                        start_date: Optional[datetime] = None,
                        end_date: Optional[datetime] = None) -> List[Decision]:
        """Search decisions with filters."""
        results = []
        
        for d in self.decisions.values():
            if decision_type and d.decision_type != decision_type:
                continue
            if outcome and d.post_mortem.actual_outcome != outcome:
                continue
            if model_id and model_id not in d.mental_models_used:
                continue
            if tag and tag not in d.tags:
                continue
            if start_date and d.created_at < start_date:
                continue
            if end_date and d.created_at > end_date:
                continue
            results.append(d)
        
        return sorted(results, key=lambda x: x.created_at, reverse=True)
    
    def calculate_calibration(self) -> Dict[str, Any]:
        """
        Calculate confidence calibration.
        
        Compares predicted confidence levels with actual success rates.
        Well-calibrated means 80% confidence predictions succeed 80% of the time.
        """
        # Group decisions by confidence buckets
        buckets = defaultdict(list)
        
        for d in self.decisions.values():
            if d.post_mortem.actual_outcome == DecisionOutcome.PENDING:
                continue
            
            # Bucket by confidence (0.1 increments)
            bucket = round(d.confidence, 1)
            
            # Determine if successful
            success = d.post_mortem.actual_outcome in [
                DecisionOutcome.SUCCESS,
                DecisionOutcome.PARTIAL_SUCCESS
            ]
            
            buckets[bucket].append(success)
        
        # Calculate actual success rate per bucket
        calibration = {}
        for bucket, outcomes in sorted(buckets.items()):
            if outcomes:
                actual_rate = sum(outcomes) / len(outcomes)
                calibration[bucket] = {
                    'predicted_confidence': bucket,
                    'actual_success_rate': actual_rate,
                    'sample_size': len(outcomes),
                    'calibration_error': abs(bucket - actual_rate)
                }
        
        # Overall calibration score (lower is better)
        if calibration:
            total_error = sum(c['calibration_error'] * c['sample_size'] 
                           for c in calibration.values())
            total_samples = sum(c['sample_size'] for c in calibration.values())
            overall_error = total_error / total_samples if total_samples > 0 else 0
        else:
            overall_error = None
        
        return {
            'buckets': calibration,
            'overall_calibration_error': overall_error,
            'interpretation': self._interpret_calibration(overall_error)
        }
    
    def _interpret_calibration(self, error: Optional[float]) -> str:
        """Interpret calibration error."""
        if error is None:
            return "Insufficient data for calibration"
        elif error < 0.05:
            return "Excellent calibration - predictions match reality well"
        elif error < 0.10:
            return "Good calibration - minor adjustments needed"
        elif error < 0.20:
            return "Fair calibration - consider adjusting confidence levels"
        else:
            return "Poor calibration - significant overconfidence or underconfidence"
    
    def analyze_model_effectiveness(self) -> Dict[int, Dict]:
        """
        Analyze effectiveness of each mental model.
        
        Returns success rates and usage patterns for each model.
        """
        model_stats = defaultdict(lambda: {
            'uses': 0,
            'successes': 0,
            'failures': 0,
            'pending': 0,
            'avg_confidence': [],
            'decisions': []
        })
        
        for d in self.decisions.values():
            outcome = d.post_mortem.actual_outcome
            
            for model_id in d.mental_models_used:
                stats = model_stats[model_id]
                stats['uses'] += 1
                stats['avg_confidence'].append(d.confidence)
                stats['decisions'].append(d.id)
                
                if outcome == DecisionOutcome.PENDING:
                    stats['pending'] += 1
                elif outcome in [DecisionOutcome.SUCCESS, DecisionOutcome.PARTIAL_SUCCESS]:
                    stats['successes'] += 1
                elif outcome in [DecisionOutcome.FAILURE, DecisionOutcome.PARTIAL_FAILURE]:
                    stats['failures'] += 1
        
        # Calculate derived metrics
        results = {}
        for model_id, stats in model_stats.items():
            resolved = stats['successes'] + stats['failures']
            success_rate = stats['successes'] / resolved if resolved > 0 else None
            
            model_name = self.models_data.get(model_id, {}).get('name', f'Model {model_id}')
            
            results[model_id] = {
                'model_name': model_name,
                'total_uses': stats['uses'],
                'success_rate': success_rate,
                'successes': stats['successes'],
                'failures': stats['failures'],
                'pending': stats['pending'],
                'avg_confidence': statistics.mean(stats['avg_confidence']) if stats['avg_confidence'] else 0,
                'decisions': stats['decisions']
            }
        
        return dict(sorted(results.items(), key=lambda x: x[1]['total_uses'], reverse=True))
    
    def generate_insights(self) -> Dict[str, Any]:
        """
        Generate insights from the decision journal.
        
        Identifies patterns, common mistakes, and areas for improvement.
        """
        if not self.decisions:
            return {'message': 'No decisions recorded yet'}
        
        insights = {
            'summary': {},
            'patterns': [],
            'recommendations': [],
            'model_insights': {}
        }
        
        # Summary statistics
        total = len(self.decisions)
        resolved = [d for d in self.decisions.values() 
                   if d.post_mortem.actual_outcome != DecisionOutcome.PENDING]
        
        outcomes = defaultdict(int)
        for d in resolved:
            outcomes[d.post_mortem.actual_outcome.value] += 1
        
        insights['summary'] = {
            'total_decisions': total,
            'resolved_decisions': len(resolved),
            'pending_decisions': total - len(resolved),
            'outcomes': dict(outcomes),
            'success_rate': (outcomes['success'] + outcomes['partial_success']) / len(resolved) if resolved else 0
        }
        
        # Calibration
        calibration = self.calculate_calibration()
        insights['calibration'] = calibration
        
        # Model effectiveness
        model_effectiveness = self.analyze_model_effectiveness()
        insights['model_insights'] = model_effectiveness
        
        # Pattern detection
        patterns = self._detect_patterns()
        insights['patterns'] = patterns
        
        # Generate recommendations
        recommendations = self._generate_recommendations(insights)
        insights['recommendations'] = recommendations
        
        return insights
    
    def _detect_patterns(self) -> List[Dict]:
        """Detect patterns in decision-making."""
        patterns = []
        
        # Check for overconfidence
        high_conf_failures = [
            d for d in self.decisions.values()
            if d.confidence >= 0.8 and d.post_mortem.actual_outcome in [
                DecisionOutcome.FAILURE, DecisionOutcome.PARTIAL_FAILURE
            ]
        ]
        if len(high_conf_failures) >= 3:
            patterns.append({
                'type': 'overconfidence',
                'description': f'{len(high_conf_failures)} high-confidence decisions failed',
                'severity': 'high',
                'decisions': [d.id for d in high_conf_failures]
            })
        
        # Check for underconfidence
        low_conf_successes = [
            d for d in self.decisions.values()
            if d.confidence <= 0.4 and d.post_mortem.actual_outcome in [
                DecisionOutcome.SUCCESS, DecisionOutcome.PARTIAL_SUCCESS
            ]
        ]
        if len(low_conf_successes) >= 3:
            patterns.append({
                'type': 'underconfidence',
                'description': f'{len(low_conf_successes)} low-confidence decisions succeeded',
                'severity': 'medium',
                'decisions': [d.id for d in low_conf_successes]
            })
        
        # Check for time pressure correlation
        rushed_decisions = [
            d for d in self.decisions.values()
            if d.context.time_pressure in ['high', 'extreme']
        ]
        if rushed_decisions:
            rushed_failures = [
                d for d in rushed_decisions
                if d.post_mortem.actual_outcome in [
                    DecisionOutcome.FAILURE, DecisionOutcome.PARTIAL_FAILURE
                ]
            ]
            if len(rushed_failures) / len(rushed_decisions) > 0.5:
                patterns.append({
                    'type': 'time_pressure_correlation',
                    'description': 'High failure rate on time-pressured decisions',
                    'severity': 'high',
                    'recommendation': 'Create more buffer time for important decisions'
                })
        
        return patterns
    
    def _generate_recommendations(self, insights: Dict) -> List[str]:
        """Generate actionable recommendations."""
        recommendations = []
        
        # Calibration recommendations
        cal_error = insights.get('calibration', {}).get('overall_calibration_error')
        if cal_error and cal_error > 0.15:
            recommendations.append(
                "Your confidence calibration needs work. Consider using reference class forecasting."
            )
        
        # Model usage recommendations
        model_insights = insights.get('model_insights', {})
        underused_effective = [
            (mid, data) for mid, data in model_insights.items()
            if data['success_rate'] and data['success_rate'] > 0.7 and data['total_uses'] < 5
        ]
        if underused_effective:
            model_names = [data['model_name'] for _, data in underused_effective[:3]]
            recommendations.append(
                f"Consider using these effective models more often: {', '.join(model_names)}"
            )
        
        # Pattern-based recommendations
        for pattern in insights.get('patterns', []):
            if pattern['type'] == 'overconfidence':
                recommendations.append(
                    "Practice epistemic humility. Your high-confidence predictions are failing too often."
                )
            elif pattern['type'] == 'time_pressure_correlation':
                recommendations.append(
                    "Avoid making important decisions under time pressure when possible."
                )
        
        # General recommendations
        if insights['summary']['pending_decisions'] > insights['summary']['resolved_decisions']:
            recommendations.append(
                "Many decisions are pending outcomes. Schedule time to review and record results."
            )
        
        return recommendations
    
    def export_to_markdown(self, output_path: str):
        """Export journal to readable Markdown format."""
        lines = [
            "# Decision Journal",
            f"\nGenerated: {datetime.now().strftime('%Y-%m-%d %H:%M')}",
            f"\nTotal Decisions: {len(self.decisions)}",
            "\n---\n"
        ]
        
        # Add insights summary
        insights = self.generate_insights()
        lines.append("## Summary\n")
        lines.append(f"- Success Rate: {insights['summary'].get('success_rate', 0):.1%}")
        lines.append(f"- Calibration Error: {insights.get('calibration', {}).get('overall_calibration_error', 'N/A')}")
        lines.append("\n---\n")
        
        # Add each decision
        lines.append("## Decisions\n")
        for d in sorted(self.decisions.values(), key=lambda x: x.created_at, reverse=True):
            lines.append(f"### {d.title}")
            lines.append(f"**ID:** {d.id} | **Date:** {d.created_at.strftime('%Y-%m-%d')} | **Type:** {d.decision_type.value}")
            lines.append(f"\n**Decision:** {d.decision_made}")
            lines.append(f"\n**Rationale:** {d.rationale}")
            lines.append(f"\n**Confidence:** {d.confidence:.0%}")
            lines.append(f"\n**Predicted Outcome:** {d.predicted_outcome}")
            
            if d.mental_models_used:
                model_names = [self.models_data.get(m, {}).get('name', f'Model {m}') 
                              for m in d.mental_models_used]
                lines.append(f"\n**Mental Models Used:** {', '.join(model_names)}")
            
            if d.post_mortem.actual_outcome != DecisionOutcome.PENDING:
                lines.append(f"\n**Actual Outcome:** {d.post_mortem.actual_outcome.value}")
                lines.append(f"\n**Outcome Description:** {d.post_mortem.outcome_description}")
                if d.post_mortem.lessons_learned:
                    lines.append("\n**Lessons Learned:**")
                    for lesson in d.post_mortem.lessons_learned:
                        lines.append(f"- {lesson}")
            
            lines.append("\n---\n")
        
        with open(output_path, 'w') as f:
            f.write('\n'.join(lines))
        
        logger.info(f"Exported journal to {output_path}")


# Convenience function
def create_journal(storage_path: Optional[str] = None, 
                   models_path: Optional[str] = None) -> DecisionJournal:
    """Create a decision journal with optional paths."""
    journal = DecisionJournal(storage_path)
    if models_path:
        journal.load_models(models_path)
    return journal


if __name__ == "__main__":
    # Demo usage
    from pathlib import Path
    
    # Create journal
    journal = DecisionJournal()
    
    # Create a sample decision
    decision = journal.create_decision(
        decision_type=DecisionType.INVESTMENT,
        title="Increase allocation to emerging markets",
        description="Considering increasing EM allocation from 10% to 20% due to valuation gap",
        decision_made="Increase EM allocation to 15% (halfway)",
        rationale="Valuations are attractive but uncertainty is high. Splitting the difference.",
        mental_models_used=[69, 68, 101, 41],  # Margin of safety, Mr. Market, etc.
        model_reasoning={
            69: "Margin of safety exists due to 40% discount to historical P/E",
            68: "Mr. Market is pessimistic on EM, creating opportunity",
            101: "Long-term compounding potential is significant",
            41: "Using probabilistic thinking - 60% chance of outperformance"
        },
        predicted_outcome="15-20% outperformance vs developed markets over 3 years",
        confidence=0.65,
        time_horizon="3 years",
        context=DecisionContext(
            market_conditions="Global uncertainty, EM underperformance",
            time_pressure="low",
            information_quality="adequate",
            emotional_state="calm",
            key_uncertainties=["China policy", "USD strength", "Global recession risk"],
            alternatives_considered=["Stay at 10%", "Go full 20%", "Wait for more clarity"]
        ),
        pre_mortem=PreMortem(
            failure_scenarios=[
                "Global recession hits EM harder than expected",
                "USD strengthens significantly",
                "China-Taiwan conflict"
            ],
            warning_signs=[
                "EM currencies weakening >10%",
                "Capital outflows accelerating",
                "Credit spreads widening"
            ],
            kill_criteria=[
                "20% drawdown from entry",
                "Fundamental thesis invalidated"
            ]
        ),
        tags=["allocation", "emerging_markets", "contrarian"]
    )
    
    print(f"Created decision: {decision.id}")
    print(f"Title: {decision.title}")
    print(f"Confidence: {decision.confidence:.0%}")
    
    # Generate insights
    insights = journal.generate_insights()
    print(f"\nInsights: {json.dumps(insights['summary'], indent=2)}")
