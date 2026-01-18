"""
Automated Improvement Suggestion Engine

Analyzes system usage, decision outcomes, and failure patterns to automatically
generate improvement suggestions for the Mental Models System.

This implements the continuous improvement snowball - the system gets better
at making itself better.

Architecture:
┌─────────────────────────────────────────────────────────────────────────┐
│                    IMPROVEMENT SUGGESTION ENGINE                         │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  DATA SOURCES                    ANALYSIS                    OUTPUT     │
│  ┌──────────────┐               ┌──────────────┐          ┌──────────┐ │
│  │ Decision     │               │ Pattern      │          │ Improve- │ │
│  │ Journal      │──────────────▶│ Detection    │──────────▶│ ment     │ │
│  │              │               │              │          │ Suggest- │ │
│  │ Failure      │               │ Gap          │          │ ions     │ │
│  │ Modes        │──────────────▶│ Analysis     │──────────▶│          │ │
│  │              │               │              │          │ Manus    │ │
│  │ Model        │               │ Effectiveness│          │ Export   │ │
│  │ Effectiveness│──────────────▶│ Tracking     │──────────▶│          │ │
│  └──────────────┘               └──────────────┘          └──────────┘ │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
"""

import json
from dataclasses import dataclass, field, asdict
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Tuple
from pathlib import Path
from collections import defaultdict
from enum import Enum
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class ImprovementCategory(Enum):
    """Categories of improvements."""
    NEW_MENTAL_MODEL = "new_mental_model"
    NEW_FAILURE_MODE = "new_failure_mode"
    MODEL_REFINEMENT = "model_refinement"
    SAFEGUARD_ENHANCEMENT = "safeguard_enhancement"
    CASE_STUDY_ADDITION = "case_study_addition"
    SYSTEM_OPTIMIZATION = "system_optimization"
    DATA_QUALITY = "data_quality"
    CONNECTOR_IMPROVEMENT = "connector_improvement"


class ImprovementPriority(Enum):
    """Priority levels for improvements."""
    LOW = 1
    MEDIUM = 2
    HIGH = 3
    CRITICAL = 4


@dataclass
class ImprovementSuggestion:
    """A suggested improvement to the system."""
    id: str
    category: ImprovementCategory
    priority: ImprovementPriority
    title: str
    description: str
    rationale: str
    evidence: List[str]
    implementation_steps: List[str]
    estimated_impact: float  # 0-1
    estimated_effort: str  # "low", "medium", "high"
    related_models: List[str] = field(default_factory=list)
    related_failure_modes: List[str] = field(default_factory=list)
    created_at: datetime = field(default_factory=datetime.now)
    status: str = "pending"  # pending, in_progress, completed, rejected
    
    def to_dict(self) -> Dict:
        return {
            **asdict(self),
            "category": self.category.value,
            "priority": self.priority.value,
            "created_at": self.created_at.isoformat()
        }
    
    def to_manus_format(self) -> str:
        """Format for Manus import."""
        return f"""## Improvement Suggestion: {self.title}

**Category:** {self.category.value}
**Priority:** {self.priority.name}
**Estimated Impact:** {self.estimated_impact:.0%}
**Estimated Effort:** {self.estimated_effort}

### Description
{self.description}

### Rationale
{self.rationale}

### Evidence
{chr(10).join(f'- {e}' for e in self.evidence)}

### Implementation Steps
{chr(10).join(f'{i+1}. {step}' for i, step in enumerate(self.implementation_steps))}

### Related Models
{', '.join(self.related_models) if self.related_models else 'None'}

### Related Failure Modes
{', '.join(self.related_failure_modes) if self.related_failure_modes else 'None'}
"""


@dataclass
class UsagePattern:
    """A detected usage pattern."""
    pattern_type: str
    frequency: int
    models_involved: List[str]
    outcomes: Dict[str, int]  # success, partial, failure counts
    avg_confidence: float
    time_range: Tuple[datetime, datetime]


class ImprovementSuggestionEngine:
    """
    Analyzes system data to generate improvement suggestions.
    
    Implements the improvement snowball:
    1. Track what works and what doesn't
    2. Identify patterns and gaps
    3. Generate actionable improvements
    4. Export for implementation
    """
    
    def __init__(self, data_dir: Optional[Path] = None):
        if data_dir is None:
            data_dir = Path(__file__).parent.parent.parent / 'data'
        self.data_dir = Path(data_dir)
        
        # Load system data
        self.mental_models = self._load_mental_models()
        self.failure_modes = self._load_failure_modes()
        self.decision_history = self._load_decision_history()
        self.effectiveness_data = self._load_effectiveness_data()
        
        # Suggestions storage
        self.suggestions: List[ImprovementSuggestion] = []
        self._load_existing_suggestions()
    
    def _load_mental_models(self) -> Dict:
        """Load mental models data."""
        path = self.data_dir / 'raw' / 'mental_models_complete.json'
        if path.exists():
            with open(path, 'r') as f:
                return json.load(f)
        return {"mental_models": [], "categories": []}
    
    def _load_failure_modes(self) -> Dict[str, List[Dict]]:
        """Load all failure modes."""
        modes = {}
        raw_dir = self.data_dir / 'raw'
        
        for filepath in raw_dir.glob('failure_modes*.json'):
            try:
                with open(filepath, 'r') as f:
                    data = json.load(f)
                for model in data.get('models', []):
                    model_name = model.get('name', '')
                    if model_name:
                        modes[model_name] = model.get('failure_modes', [])
            except Exception as e:
                logger.warning(f"Error loading {filepath}: {e}")
        
        return modes
    
    def _load_decision_history(self) -> List[Dict]:
        """Load decision journal history."""
        path = self.data_dir / 'decisions' / 'decision_history.json'
        if path.exists():
            with open(path, 'r') as f:
                return json.load(f)
        return []
    
    def _load_effectiveness_data(self) -> Dict:
        """Load model effectiveness tracking data."""
        path = self.data_dir / 'tracker' / 'effectiveness.json'
        if path.exists():
            with open(path, 'r') as f:
                return json.load(f)
        return {}
    
    def _load_existing_suggestions(self):
        """Load existing suggestions."""
        path = self.data_dir / 'improvements' / 'suggestions.json'
        if path.exists():
            with open(path, 'r') as f:
                data = json.load(f)
            for s in data:
                self.suggestions.append(ImprovementSuggestion(
                    id=s['id'],
                    category=ImprovementCategory(s['category']),
                    priority=ImprovementPriority(s['priority']),
                    title=s['title'],
                    description=s['description'],
                    rationale=s['rationale'],
                    evidence=s['evidence'],
                    implementation_steps=s['implementation_steps'],
                    estimated_impact=s['estimated_impact'],
                    estimated_effort=s['estimated_effort'],
                    related_models=s.get('related_models', []),
                    related_failure_modes=s.get('related_failure_modes', []),
                    created_at=datetime.fromisoformat(s['created_at']),
                    status=s.get('status', 'pending')
                ))
    
    def analyze_and_suggest(self) -> List[ImprovementSuggestion]:
        """
        Run all analysis and generate improvement suggestions.
        
        Returns:
            List of new improvement suggestions
        """
        new_suggestions = []
        
        # 1. Analyze failure mode coverage gaps
        new_suggestions.extend(self._analyze_failure_mode_gaps())
        
        # 2. Analyze model effectiveness patterns
        new_suggestions.extend(self._analyze_effectiveness_patterns())
        
        # 3. Analyze decision outcome patterns
        new_suggestions.extend(self._analyze_decision_patterns())
        
        # 4. Analyze model usage patterns
        new_suggestions.extend(self._analyze_usage_patterns())
        
        # 5. Analyze system performance
        new_suggestions.extend(self._analyze_system_performance())
        
        # 6. Generate cross-model insights
        new_suggestions.extend(self._analyze_cross_model_patterns())
        
        # Deduplicate and prioritize
        new_suggestions = self._deduplicate_suggestions(new_suggestions)
        new_suggestions = self._prioritize_suggestions(new_suggestions)
        
        # Add to storage
        self.suggestions.extend(new_suggestions)
        self._save_suggestions()
        
        return new_suggestions
    
    def _analyze_failure_mode_gaps(self) -> List[ImprovementSuggestion]:
        """Identify models with insufficient failure mode coverage."""
        suggestions = []
        
        model_names = [m['name'] for m in self.mental_models.get('mental_models', [])]
        
        for model_name in model_names:
            modes = self.failure_modes.get(model_name, [])
            
            if len(modes) < 3:
                suggestions.append(ImprovementSuggestion(
                    id=f"fm_gap_{model_name.replace(' ', '_').lower()}",
                    category=ImprovementCategory.NEW_FAILURE_MODE,
                    priority=ImprovementPriority.HIGH if len(modes) == 0 else ImprovementPriority.MEDIUM,
                    title=f"Add failure modes for {model_name}",
                    description=f"The mental model '{model_name}' has only {len(modes)} failure modes. "
                               f"Target is at least 5 failure modes per model for comprehensive coverage.",
                    rationale="Insufficient failure mode coverage increases risk of misapplying the model. "
                             "Each model should have documented failure modes with real-world cases.",
                    evidence=[
                        f"Current failure mode count: {len(modes)}",
                        f"Target failure mode count: 5",
                        f"Gap: {5 - len(modes)} failure modes needed"
                    ],
                    implementation_steps=[
                        f"Research common misapplications of {model_name}",
                        "Document real-world cases where the model was misapplied",
                        "Identify warning signs for each failure mode",
                        "Define safeguards to prevent each failure mode",
                        "Add to failure_modes JSON files"
                    ],
                    estimated_impact=0.7,
                    estimated_effort="medium",
                    related_models=[model_name]
                ))
        
        return suggestions
    
    def _analyze_effectiveness_patterns(self) -> List[ImprovementSuggestion]:
        """Analyze model effectiveness data for improvement opportunities."""
        suggestions = []
        
        if not self.effectiveness_data:
            suggestions.append(ImprovementSuggestion(
                id="effectiveness_tracking_setup",
                category=ImprovementCategory.SYSTEM_OPTIMIZATION,
                priority=ImprovementPriority.HIGH,
                title="Enable effectiveness tracking",
                description="Model effectiveness tracking is not yet enabled. "
                           "This prevents learning from decision outcomes.",
                rationale="Without effectiveness tracking, the system cannot learn which model "
                         "combinations work best in different contexts.",
                evidence=["No effectiveness data found"],
                implementation_steps=[
                    "Integrate EffectivenessTracker into decision workflow",
                    "Record model combinations used for each decision",
                    "Track decision outcomes",
                    "Run periodic effectiveness analysis"
                ],
                estimated_impact=0.9,
                estimated_effort="medium"
            ))
            return suggestions
        
        # Analyze low-performing models
        for model_name, data in self.effectiveness_data.items():
            if isinstance(data, dict):
                success_rate = data.get('success_rate', 0)
                usage_count = data.get('usage_count', 0)
                
                if usage_count >= 10 and success_rate < 0.5:
                    suggestions.append(ImprovementSuggestion(
                        id=f"low_perf_{model_name.replace(' ', '_').lower()}",
                        category=ImprovementCategory.MODEL_REFINEMENT,
                        priority=ImprovementPriority.HIGH,
                        title=f"Investigate low performance of {model_name}",
                        description=f"The model '{model_name}' has a success rate of {success_rate:.0%} "
                                   f"across {usage_count} uses. This is below the 50% threshold.",
                        rationale="Low-performing models may need refinement, additional context, "
                                 "or better application guidelines.",
                        evidence=[
                            f"Success rate: {success_rate:.0%}",
                            f"Usage count: {usage_count}",
                            f"Expected success rate: >50%"
                        ],
                        implementation_steps=[
                            "Review failed applications of this model",
                            "Identify common patterns in failures",
                            "Update model description with clearer application guidelines",
                            "Add failure modes for identified patterns",
                            "Consider adding prerequisite models"
                        ],
                        estimated_impact=0.8,
                        estimated_effort="high",
                        related_models=[model_name]
                    ))
        
        return suggestions
    
    def _analyze_decision_patterns(self) -> List[ImprovementSuggestion]:
        """Analyze decision history for patterns."""
        suggestions = []
        
        if not self.decision_history:
            return suggestions
        
        # Analyze decisions with poor outcomes
        poor_outcomes = [d for d in self.decision_history 
                        if d.get('outcome', {}).get('success_score', 1) < 0.5]
        
        if len(poor_outcomes) >= 3:
            # Find common models in poor decisions
            model_counts = defaultdict(int)
            for decision in poor_outcomes:
                for model in decision.get('models_applied', []):
                    model_counts[model] += 1
            
            # Models appearing in multiple poor decisions
            problematic_models = [(m, c) for m, c in model_counts.items() if c >= 2]
            
            for model, count in problematic_models:
                suggestions.append(ImprovementSuggestion(
                    id=f"decision_pattern_{model.replace(' ', '_').lower()}",
                    category=ImprovementCategory.SAFEGUARD_ENHANCEMENT,
                    priority=ImprovementPriority.HIGH,
                    title=f"Review application of {model} in decisions",
                    description=f"The model '{model}' appears in {count} decisions with poor outcomes. "
                               f"This suggests the model may be frequently misapplied.",
                    rationale="Repeated poor outcomes with the same model indicate a systematic issue "
                             "that needs investigation.",
                    evidence=[
                        f"Appearances in poor decisions: {count}",
                        f"Total poor decisions analyzed: {len(poor_outcomes)}"
                    ],
                    implementation_steps=[
                        "Review the specific decisions where this model was applied",
                        "Identify what context was missing or misunderstood",
                        "Add warning signs to failure modes",
                        "Create checklist for model application",
                        "Consider adding prerequisite checks"
                    ],
                    estimated_impact=0.75,
                    estimated_effort="medium",
                    related_models=[model]
                ))
        
        return suggestions
    
    def _analyze_usage_patterns(self) -> List[ImprovementSuggestion]:
        """Analyze how models are being used."""
        suggestions = []
        
        # Check for underutilized models
        model_names = set(m['name'] for m in self.mental_models.get('mental_models', []))
        used_models = set()
        
        for decision in self.decision_history:
            used_models.update(decision.get('models_applied', []))
        
        unused_models = model_names - used_models
        
        if len(unused_models) > len(model_names) * 0.3:  # More than 30% unused
            suggestions.append(ImprovementSuggestion(
                id="underutilized_models",
                category=ImprovementCategory.DATA_QUALITY,
                priority=ImprovementPriority.MEDIUM,
                title="Improve discoverability of underutilized models",
                description=f"{len(unused_models)} models ({len(unused_models)/len(model_names):.0%}) "
                           f"have never been used in decisions.",
                rationale="Underutilized models may lack clear application guidelines or may not "
                         "be surfacing in relevant contexts.",
                evidence=[
                    f"Total models: {len(model_names)}",
                    f"Used models: {len(used_models)}",
                    f"Unused models: {len(unused_models)}",
                    f"Examples: {', '.join(list(unused_models)[:5])}"
                ],
                implementation_steps=[
                    "Review keywords and descriptions of unused models",
                    "Add more specific application examples",
                    "Improve search indexing for these models",
                    "Consider adding to related_models for frequently used models"
                ],
                estimated_impact=0.5,
                estimated_effort="low",
                related_models=list(unused_models)[:10]
            ))
        
        return suggestions
    
    def _analyze_system_performance(self) -> List[ImprovementSuggestion]:
        """Analyze system performance metrics."""
        suggestions = []
        
        # Check failure mode coverage
        total_models = len(self.mental_models.get('mental_models', []))
        models_with_modes = len(self.failure_modes)
        coverage = models_with_modes / total_models if total_models > 0 else 0
        
        if coverage < 0.9:
            suggestions.append(ImprovementSuggestion(
                id="failure_mode_coverage",
                category=ImprovementCategory.DATA_QUALITY,
                priority=ImprovementPriority.HIGH,
                title="Improve failure mode coverage",
                description=f"Only {coverage:.0%} of models have documented failure modes. "
                           f"Target is 100% coverage.",
                rationale="Complete failure mode coverage is essential for risk management.",
                evidence=[
                    f"Models with failure modes: {models_with_modes}",
                    f"Total models: {total_models}",
                    f"Coverage: {coverage:.0%}"
                ],
                implementation_steps=[
                    "Identify models without failure modes",
                    "Prioritize high-usage models",
                    "Research and document failure modes",
                    "Add real-world case studies"
                ],
                estimated_impact=0.85,
                estimated_effort="high"
            ))
        
        return suggestions
    
    def _analyze_cross_model_patterns(self) -> List[ImprovementSuggestion]:
        """Analyze patterns across multiple models."""
        suggestions = []
        
        # Find frequently co-occurring models
        if len(self.decision_history) >= 5:
            co_occurrences = defaultdict(int)
            
            for decision in self.decision_history:
                models = decision.get('models_applied', [])
                for i, m1 in enumerate(models):
                    for m2 in models[i+1:]:
                        pair = tuple(sorted([m1, m2]))
                        co_occurrences[pair] += 1
            
            # Find pairs that frequently co-occur
            frequent_pairs = [(pair, count) for pair, count in co_occurrences.items() 
                            if count >= 3]
            
            for (m1, m2), count in frequent_pairs:
                suggestions.append(ImprovementSuggestion(
                    id=f"synergy_{m1.replace(' ', '_').lower()}_{m2.replace(' ', '_').lower()}",
                    category=ImprovementCategory.CASE_STUDY_ADDITION,
                    priority=ImprovementPriority.LOW,
                    title=f"Document synergy between {m1} and {m2}",
                    description=f"The models '{m1}' and '{m2}' are frequently used together "
                               f"({count} times). Consider documenting their synergy.",
                    rationale="Frequently co-occurring models may have synergistic effects "
                             "worth documenting for future reference.",
                    evidence=[
                        f"Co-occurrence count: {count}",
                        f"Total decisions analyzed: {len(self.decision_history)}"
                    ],
                    implementation_steps=[
                        "Review decisions where both models were applied",
                        "Identify synergistic effects",
                        "Document as a case study",
                        "Add to related_models for each model"
                    ],
                    estimated_impact=0.4,
                    estimated_effort="low",
                    related_models=[m1, m2]
                ))
        
        return suggestions
    
    def _deduplicate_suggestions(self, suggestions: List[ImprovementSuggestion]) -> List[ImprovementSuggestion]:
        """Remove duplicate suggestions."""
        seen_ids = set(s.id for s in self.suggestions)
        return [s for s in suggestions if s.id not in seen_ids]
    
    def _prioritize_suggestions(self, suggestions: List[ImprovementSuggestion]) -> List[ImprovementSuggestion]:
        """Sort suggestions by priority and impact."""
        return sorted(
            suggestions,
            key=lambda s: (s.priority.value, s.estimated_impact),
            reverse=True
        )
    
    def _save_suggestions(self):
        """Save suggestions to file."""
        path = self.data_dir / 'improvements'
        path.mkdir(parents=True, exist_ok=True)
        
        with open(path / 'suggestions.json', 'w') as f:
            json.dump([s.to_dict() for s in self.suggestions], f, indent=2)
    
    def get_pending_suggestions(self) -> List[ImprovementSuggestion]:
        """Get all pending suggestions."""
        return [s for s in self.suggestions if s.status == 'pending']
    
    def get_high_priority_suggestions(self) -> List[ImprovementSuggestion]:
        """Get high and critical priority suggestions."""
        return [s for s in self.suggestions 
                if s.priority.value >= ImprovementPriority.HIGH.value 
                and s.status == 'pending']
    
    def export_for_manus(self, output_path: Optional[Path] = None) -> str:
        """
        Export suggestions in Manus-compatible format.
        
        Returns:
            Path to the exported file
        """
        if output_path is None:
            output_path = self.data_dir / 'improvements' / 'manus_import.md'
        
        output_path.parent.mkdir(parents=True, exist_ok=True)
        
        pending = self.get_pending_suggestions()
        
        content = f"""# Improvement Suggestions for Mental Models System

Generated: {datetime.now().isoformat()}
Total Suggestions: {len(pending)}

## Summary

| Priority | Count |
|----------|-------|
| Critical | {len([s for s in pending if s.priority == ImprovementPriority.CRITICAL])} |
| High | {len([s for s in pending if s.priority == ImprovementPriority.HIGH])} |
| Medium | {len([s for s in pending if s.priority == ImprovementPriority.MEDIUM])} |
| Low | {len([s for s in pending if s.priority == ImprovementPriority.LOW])} |

---

"""
        
        for suggestion in pending:
            content += suggestion.to_manus_format() + "\n---\n\n"
        
        with open(output_path, 'w') as f:
            f.write(content)
        
        return str(output_path)
    
    def mark_completed(self, suggestion_id: str):
        """Mark a suggestion as completed."""
        for s in self.suggestions:
            if s.id == suggestion_id:
                s.status = 'completed'
                break
        self._save_suggestions()
    
    def mark_rejected(self, suggestion_id: str, reason: str = ""):
        """Mark a suggestion as rejected."""
        for s in self.suggestions:
            if s.id == suggestion_id:
                s.status = 'rejected'
                break
        self._save_suggestions()


# Convenience functions
def generate_suggestions() -> List[ImprovementSuggestion]:
    """Generate improvement suggestions."""
    engine = ImprovementSuggestionEngine()
    return engine.analyze_and_suggest()


def export_suggestions_for_manus() -> str:
    """Export suggestions for Manus."""
    engine = ImprovementSuggestionEngine()
    return engine.export_for_manus()


def get_high_priority_improvements() -> List[ImprovementSuggestion]:
    """Get high priority improvements."""
    engine = ImprovementSuggestionEngine()
    return engine.get_high_priority_suggestions()


if __name__ == "__main__":
    # Run improvement analysis
    print("=" * 60)
    print("IMPROVEMENT SUGGESTION ENGINE")
    print("=" * 60)
    
    engine = ImprovementSuggestionEngine()
    suggestions = engine.analyze_and_suggest()
    
    print(f"\nGenerated {len(suggestions)} new suggestions")
    
    print(f"\nPending suggestions by priority:")
    pending = engine.get_pending_suggestions()
    for priority in ImprovementPriority:
        count = len([s for s in pending if s.priority == priority])
        print(f"  {priority.name}: {count}")
    
    print(f"\nTop 5 high-priority suggestions:")
    for s in engine.get_high_priority_suggestions()[:5]:
        print(f"\n  [{s.priority.name}] {s.title}")
        print(f"    Impact: {s.estimated_impact:.0%}, Effort: {s.estimated_effort}")
        print(f"    {s.description[:100]}...")
    
    # Export for Manus
    export_path = engine.export_for_manus()
    print(f"\n✅ Exported to: {export_path}")
