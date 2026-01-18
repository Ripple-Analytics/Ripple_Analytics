"""
Predictive Model Effectiveness Tracker

Tracks which mental model combinations actually predict outcomes.
The Jim Simons approach - let data reveal what works.

Features:
1. Tracks every prediction with model combinations used
2. Measures outcomes vs predictions
3. Calculates effectiveness using Bayesian updating
4. Identifies synergistic model combinations
5. Provides calibration metrics

Architecture:
┌─────────────────────────────────────────────────────────────────────────┐
│                    PREDICTIVE EFFECTIVENESS TRACKER                      │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  DECISION                      TRACKING                    INSIGHTS     │
│  ┌──────────────┐             ┌──────────────┐           ┌──────────┐  │
│  │ Decision     │             │ Outcome      │           │ Model    │  │
│  │ Made         │─────────────│ Recorded     │───────────│ Rankings │  │
│  │              │             │              │           │          │  │
│  │ Models: [5,  │             │ Result: +23% │           │ Synergy  │  │
│  │   12, 45]    │             │ vs Pred: +20%│           │ Matrix   │  │
│  │ Confidence:  │             │              │           │          │  │
│  │   0.75       │             │ Calibration: │           │ Calibra- │  │
│  └──────────────┘             │   0.92       │           │ tion     │  │
│                               └──────────────┘           └──────────┘  │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
"""

import json
import math
import statistics
from dataclasses import dataclass, field, asdict
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple, Any, Set
from enum import Enum
from pathlib import Path
from collections import defaultdict
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# =============================================================================
# DATA STRUCTURES
# =============================================================================

class OutcomeType(Enum):
    """Types of outcomes."""
    PENDING = "pending"
    CORRECT = "correct"
    PARTIALLY_CORRECT = "partially_correct"
    INCORRECT = "incorrect"
    UNKNOWN = "unknown"


@dataclass
class Prediction:
    """A prediction made using mental models."""
    id: str
    created_at: datetime
    
    # Context
    title: str
    description: str
    domain: str  # investment, business, personal, etc.
    
    # Models used
    models_used: List[int]  # Model IDs
    
    # Prediction details
    prediction: str
    predicted_outcome: float  # Quantitative prediction if applicable
    confidence: float  # 0-1
    time_horizon: str  # e.g., "6 months", "1 year"
    
    # Optional fields with defaults
    model_weights: Dict[int, float] = field(default_factory=dict)  # How much each model influenced
    expected_resolution: datetime = None
    
    # Outcome tracking
    outcome_type: OutcomeType = OutcomeType.PENDING
    actual_outcome: float = None
    outcome_description: str = ""
    outcome_recorded_at: datetime = None
    
    # Metadata
    tags: List[str] = field(default_factory=list)
    notes: str = ""
    
    def to_dict(self) -> Dict:
        d = asdict(self)
        d['created_at'] = self.created_at.isoformat()
        d['outcome_type'] = self.outcome_type.value
        if self.expected_resolution:
            d['expected_resolution'] = self.expected_resolution.isoformat()
        if self.outcome_recorded_at:
            d['outcome_recorded_at'] = self.outcome_recorded_at.isoformat()
        return d
    
    @classmethod
    def from_dict(cls, d: Dict) -> 'Prediction':
        d['created_at'] = datetime.fromisoformat(d['created_at'])
        d['outcome_type'] = OutcomeType(d['outcome_type'])
        if d.get('expected_resolution'):
            d['expected_resolution'] = datetime.fromisoformat(d['expected_resolution'])
        if d.get('outcome_recorded_at'):
            d['outcome_recorded_at'] = datetime.fromisoformat(d['outcome_recorded_at'])
        return cls(**d)


@dataclass
class ModelEffectiveness:
    """Effectiveness metrics for a single model."""
    model_id: int
    model_name: str
    
    # Usage stats
    times_used: int = 0
    times_correct: int = 0
    times_partially_correct: int = 0
    times_incorrect: int = 0
    
    # Accuracy metrics
    accuracy: float = 0.0  # Simple accuracy
    weighted_accuracy: float = 0.0  # Weighted by confidence
    
    # Calibration
    calibration_error: float = 0.0  # Difference between confidence and accuracy
    
    # Bayesian prior (starts at 0.5, updates with evidence)
    prior_effectiveness: float = 0.5
    posterior_effectiveness: float = 0.5
    
    # Domain-specific performance
    domain_accuracy: Dict[str, float] = field(default_factory=dict)
    
    def update(self, outcome: OutcomeType, confidence: float, domain: str):
        """Update effectiveness based on new outcome."""
        self.times_used += 1
        
        if outcome == OutcomeType.CORRECT:
            self.times_correct += 1
            success = 1.0
        elif outcome == OutcomeType.PARTIALLY_CORRECT:
            self.times_partially_correct += 1
            success = 0.5
        elif outcome == OutcomeType.INCORRECT:
            self.times_incorrect += 1
            success = 0.0
        else:
            return  # Don't update for pending/unknown
        
        # Update accuracy
        total_resolved = self.times_correct + self.times_partially_correct + self.times_incorrect
        self.accuracy = (self.times_correct + 0.5 * self.times_partially_correct) / total_resolved
        
        # Update weighted accuracy
        # (simplified - in production would track all confidences)
        self.weighted_accuracy = 0.9 * self.weighted_accuracy + 0.1 * success
        
        # Update calibration error
        self.calibration_error = abs(confidence - success)
        
        # Bayesian update
        # P(effective|success) = P(success|effective) * P(effective) / P(success)
        likelihood = 0.8 if success > 0.5 else 0.2
        self.posterior_effectiveness = (
            likelihood * self.prior_effectiveness / 
            (likelihood * self.prior_effectiveness + (1 - likelihood) * (1 - self.prior_effectiveness))
        )
        self.prior_effectiveness = self.posterior_effectiveness
        
        # Update domain accuracy
        if domain not in self.domain_accuracy:
            self.domain_accuracy[domain] = 0.5
        self.domain_accuracy[domain] = 0.9 * self.domain_accuracy[domain] + 0.1 * success


@dataclass
class ModelSynergy:
    """Synergy between two or more models."""
    model_ids: Tuple[int, ...]
    model_names: Tuple[str, ...]
    
    # Usage stats
    times_used_together: int = 0
    times_correct: int = 0
    times_incorrect: int = 0
    
    # Synergy metrics
    combined_accuracy: float = 0.0
    synergy_score: float = 0.0  # How much better than individual models
    
    # Average return when used together (if applicable)
    avg_return: float = 0.0
    
    def update(self, outcome: OutcomeType, return_value: float = None):
        """Update synergy based on new outcome."""
        self.times_used_together += 1
        
        if outcome == OutcomeType.CORRECT:
            self.times_correct += 1
        elif outcome == OutcomeType.INCORRECT:
            self.times_incorrect += 1
        
        total = self.times_correct + self.times_incorrect
        if total > 0:
            self.combined_accuracy = self.times_correct / total
        
        if return_value is not None:
            self.avg_return = (
                (self.avg_return * (self.times_used_together - 1) + return_value) / 
                self.times_used_together
            )


# =============================================================================
# EFFECTIVENESS TRACKER
# =============================================================================

class EffectivenessTracker:
    """
    Main tracker for model effectiveness and synergies.
    """
    
    def __init__(self, storage_path: str = None):
        self.storage_path = Path(storage_path) if storage_path else None
        
        # Data stores
        self.predictions: Dict[str, Prediction] = {}
        self.model_effectiveness: Dict[int, ModelEffectiveness] = {}
        self.model_synergies: Dict[Tuple[int, ...], ModelSynergy] = {}
        
        # Model data
        self.models_data: Dict[int, Dict] = {}
        
        # Load existing data
        if self.storage_path and self.storage_path.exists():
            self._load()
        
        # Load models
        self._load_models()
    
    def _load_models(self):
        """Load mental models data."""
        models_path = Path(__file__).parent.parent.parent / "data" / "raw" / "mental_models_complete.json"
        
        if models_path.exists():
            with open(models_path, 'r') as f:
                data = json.load(f)
            
            categories = {c["id"]: c["name"] for c in data.get("categories", [])}
            
            for m in data.get("mental_models", []):
                m["category_name"] = categories.get(m.get("category_id"), "")
                self.models_data[m["id"]] = m
                
                # Initialize effectiveness if not exists
                if m["id"] not in self.model_effectiveness:
                    self.model_effectiveness[m["id"]] = ModelEffectiveness(
                        model_id=m["id"],
                        model_name=m["name"]
                    )
            
            logger.info(f"Loaded {len(self.models_data)} models")
    
    def _load(self):
        """Load tracker data from storage."""
        try:
            with open(self.storage_path, 'r') as f:
                data = json.load(f)
            
            # Load predictions
            for p_data in data.get('predictions', []):
                pred = Prediction.from_dict(p_data)
                self.predictions[pred.id] = pred
            
            # Load effectiveness
            for e_data in data.get('effectiveness', []):
                eff = ModelEffectiveness(**e_data)
                self.model_effectiveness[eff.model_id] = eff
            
            # Load synergies
            for s_data in data.get('synergies', []):
                s_data['model_ids'] = tuple(s_data['model_ids'])
                s_data['model_names'] = tuple(s_data['model_names'])
                syn = ModelSynergy(**s_data)
                self.model_synergies[syn.model_ids] = syn
            
            logger.info(f"Loaded {len(self.predictions)} predictions, {len(self.model_synergies)} synergies")
        except Exception as e:
            logger.error(f"Error loading tracker data: {e}")
    
    def _save(self):
        """Save tracker data to storage."""
        if not self.storage_path:
            return
        
        try:
            self.storage_path.parent.mkdir(parents=True, exist_ok=True)
            
            data = {
                'predictions': [p.to_dict() for p in self.predictions.values()],
                'effectiveness': [asdict(e) for e in self.model_effectiveness.values()],
                'synergies': [
                    {**asdict(s), 'model_ids': list(s.model_ids), 'model_names': list(s.model_names)}
                    for s in self.model_synergies.values()
                ],
                'metadata': {
                    'last_updated': datetime.now().isoformat(),
                    'total_predictions': len(self.predictions)
                }
            }
            
            with open(self.storage_path, 'w') as f:
                json.dump(data, f, indent=2)
            
            logger.info("Tracker data saved")
        except Exception as e:
            logger.error(f"Error saving tracker data: {e}")
    
    # =========================================================================
    # PREDICTION MANAGEMENT
    # =========================================================================
    
    def create_prediction(self,
                         title: str,
                         description: str,
                         domain: str,
                         models_used: List[int],
                         prediction: str,
                         predicted_outcome: float,
                         confidence: float,
                         time_horizon: str,
                         model_weights: Dict[int, float] = None,
                         tags: List[str] = None) -> Prediction:
        """Create a new prediction."""
        import uuid
        
        pred_id = str(uuid.uuid4())[:8]
        
        pred = Prediction(
            id=pred_id,
            created_at=datetime.now(),
            title=title,
            description=description,
            domain=domain,
            models_used=models_used,
            model_weights=model_weights or {m: 1.0/len(models_used) for m in models_used},
            prediction=prediction,
            predicted_outcome=predicted_outcome,
            confidence=confidence,
            time_horizon=time_horizon,
            tags=tags or []
        )
        
        self.predictions[pred_id] = pred
        self._save()
        
        logger.info(f"Created prediction {pred_id}: {title}")
        return pred
    
    def record_outcome(self,
                      prediction_id: str,
                      outcome_type: OutcomeType,
                      actual_outcome: float = None,
                      outcome_description: str = "") -> Prediction:
        """Record the outcome of a prediction."""
        if prediction_id not in self.predictions:
            raise ValueError(f"Prediction {prediction_id} not found")
        
        pred = self.predictions[prediction_id]
        pred.outcome_type = outcome_type
        pred.actual_outcome = actual_outcome
        pred.outcome_description = outcome_description
        pred.outcome_recorded_at = datetime.now()
        
        # Update model effectiveness
        for model_id in pred.models_used:
            if model_id in self.model_effectiveness:
                self.model_effectiveness[model_id].update(
                    outcome_type, 
                    pred.confidence,
                    pred.domain
                )
        
        # Update synergies
        if len(pred.models_used) >= 2:
            self._update_synergies(pred)
        
        self._save()
        
        logger.info(f"Recorded outcome for {prediction_id}: {outcome_type.value}")
        return pred
    
    def _update_synergies(self, pred: Prediction):
        """Update synergy data for model combinations."""
        from itertools import combinations
        
        # Calculate return if applicable
        return_value = None
        if pred.predicted_outcome and pred.actual_outcome:
            return_value = pred.actual_outcome
        
        # Update pairs
        for pair in combinations(sorted(pred.models_used), 2):
            if pair not in self.model_synergies:
                names = tuple(
                    self.models_data.get(m, {}).get("name", f"Model {m}")
                    for m in pair
                )
                self.model_synergies[pair] = ModelSynergy(
                    model_ids=pair,
                    model_names=names
                )
            
            self.model_synergies[pair].update(pred.outcome_type, return_value)
        
        # Update triplets if 3+ models
        if len(pred.models_used) >= 3:
            for triplet in combinations(sorted(pred.models_used), 3):
                if triplet not in self.model_synergies:
                    names = tuple(
                        self.models_data.get(m, {}).get("name", f"Model {m}")
                        for m in triplet
                    )
                    self.model_synergies[triplet] = ModelSynergy(
                        model_ids=triplet,
                        model_names=names
                    )
                
                self.model_synergies[triplet].update(pred.outcome_type, return_value)
    
    # =========================================================================
    # ANALYSIS
    # =========================================================================
    
    def get_model_rankings(self, min_uses: int = 5) -> List[Dict]:
        """Get models ranked by effectiveness."""
        rankings = []
        
        for model_id, eff in self.model_effectiveness.items():
            if eff.times_used >= min_uses:
                rankings.append({
                    "model_id": model_id,
                    "model_name": eff.model_name,
                    "times_used": eff.times_used,
                    "accuracy": eff.accuracy,
                    "weighted_accuracy": eff.weighted_accuracy,
                    "posterior_effectiveness": eff.posterior_effectiveness,
                    "calibration_error": eff.calibration_error
                })
        
        # Sort by posterior effectiveness
        rankings.sort(key=lambda x: x["posterior_effectiveness"], reverse=True)
        
        return rankings
    
    def get_synergy_rankings(self, min_uses: int = 3) -> List[Dict]:
        """Get model combinations ranked by synergy."""
        rankings = []
        
        for key, syn in self.model_synergies.items():
            if syn.times_used_together >= min_uses:
                # Calculate synergy score
                # Compare combined accuracy to individual model accuracies
                individual_accs = []
                for model_id in syn.model_ids:
                    if model_id in self.model_effectiveness:
                        individual_accs.append(self.model_effectiveness[model_id].accuracy)
                
                if individual_accs:
                    avg_individual = statistics.mean(individual_accs)
                    syn.synergy_score = syn.combined_accuracy - avg_individual
                
                rankings.append({
                    "model_ids": list(syn.model_ids),
                    "model_names": list(syn.model_names),
                    "times_used": syn.times_used_together,
                    "combined_accuracy": syn.combined_accuracy,
                    "synergy_score": syn.synergy_score,
                    "avg_return": syn.avg_return
                })
        
        # Sort by synergy score
        rankings.sort(key=lambda x: x["synergy_score"], reverse=True)
        
        return rankings
    
    def get_calibration_report(self) -> Dict:
        """Get calibration report showing confidence vs accuracy."""
        # Bucket predictions by confidence
        buckets = defaultdict(list)
        
        for pred in self.predictions.values():
            if pred.outcome_type not in [OutcomeType.PENDING, OutcomeType.UNKNOWN]:
                bucket = round(pred.confidence, 1)
                success = 1.0 if pred.outcome_type == OutcomeType.CORRECT else (
                    0.5 if pred.outcome_type == OutcomeType.PARTIALLY_CORRECT else 0.0
                )
                buckets[bucket].append(success)
        
        # Calculate actual accuracy per bucket
        calibration = {}
        for bucket, outcomes in sorted(buckets.items()):
            calibration[bucket] = {
                "expected": bucket,
                "actual": statistics.mean(outcomes),
                "count": len(outcomes),
                "error": abs(bucket - statistics.mean(outcomes))
            }
        
        # Calculate overall Brier score
        brier_scores = []
        for pred in self.predictions.values():
            if pred.outcome_type not in [OutcomeType.PENDING, OutcomeType.UNKNOWN]:
                actual = 1.0 if pred.outcome_type == OutcomeType.CORRECT else 0.0
                brier_scores.append((pred.confidence - actual) ** 2)
        
        brier_score = statistics.mean(brier_scores) if brier_scores else None
        
        return {
            "buckets": calibration,
            "brier_score": brier_score,
            "interpretation": self._interpret_brier(brier_score)
        }
    
    def _interpret_brier(self, score: float) -> str:
        """Interpret Brier score."""
        if score is None:
            return "Insufficient data"
        elif score < 0.1:
            return "Excellent calibration"
        elif score < 0.2:
            return "Good calibration"
        elif score < 0.3:
            return "Fair calibration"
        else:
            return "Poor calibration - review confidence estimates"
    
    def get_domain_analysis(self) -> Dict[str, Dict]:
        """Get effectiveness analysis by domain."""
        domain_stats = defaultdict(lambda: {
            "predictions": 0,
            "correct": 0,
            "top_models": defaultdict(int)
        })
        
        for pred in self.predictions.values():
            domain = pred.domain
            domain_stats[domain]["predictions"] += 1
            
            if pred.outcome_type == OutcomeType.CORRECT:
                domain_stats[domain]["correct"] += 1
                for model_id in pred.models_used:
                    domain_stats[domain]["top_models"][model_id] += 1
        
        # Calculate accuracy and find top models per domain
        result = {}
        for domain, stats in domain_stats.items():
            if stats["predictions"] > 0:
                accuracy = stats["correct"] / stats["predictions"]
                
                # Get top 5 models for this domain
                top_models = sorted(
                    stats["top_models"].items(),
                    key=lambda x: x[1],
                    reverse=True
                )[:5]
                
                result[domain] = {
                    "predictions": stats["predictions"],
                    "accuracy": accuracy,
                    "top_models": [
                        {
                            "model_id": m[0],
                            "model_name": self.models_data.get(m[0], {}).get("name", f"Model {m[0]}"),
                            "times_used_correctly": m[1]
                        }
                        for m in top_models
                    ]
                }
        
        return result
    
    def suggest_models(self, domain: str, context: str = "") -> List[Dict]:
        """Suggest best models for a given domain and context."""
        suggestions = []
        
        # Get domain-specific performance
        for model_id, eff in self.model_effectiveness.items():
            domain_acc = eff.domain_accuracy.get(domain, eff.accuracy)
            
            if eff.times_used >= 3:  # Minimum usage threshold
                suggestions.append({
                    "model_id": model_id,
                    "model_name": eff.model_name,
                    "domain_accuracy": domain_acc,
                    "overall_accuracy": eff.accuracy,
                    "posterior_effectiveness": eff.posterior_effectiveness,
                    "recommended_confidence": min(0.9, domain_acc + 0.1)
                })
        
        # Sort by domain accuracy
        suggestions.sort(key=lambda x: x["domain_accuracy"], reverse=True)
        
        return suggestions[:10]
    
    def get_summary(self) -> Dict:
        """Get overall tracker summary."""
        resolved = [
            p for p in self.predictions.values()
            if p.outcome_type not in [OutcomeType.PENDING, OutcomeType.UNKNOWN]
        ]
        
        correct = len([p for p in resolved if p.outcome_type == OutcomeType.CORRECT])
        
        return {
            "total_predictions": len(self.predictions),
            "resolved_predictions": len(resolved),
            "pending_predictions": len(self.predictions) - len(resolved),
            "overall_accuracy": correct / len(resolved) if resolved else 0,
            "models_tracked": len(self.model_effectiveness),
            "synergies_tracked": len(self.model_synergies),
            "top_models": self.get_model_rankings()[:5],
            "top_synergies": self.get_synergy_rankings()[:5]
        }


# =============================================================================
# CLI INTERFACE
# =============================================================================

def main():
    """CLI entry point."""
    import argparse
    
    parser = argparse.ArgumentParser(description="Model Effectiveness Tracker")
    parser.add_argument("command", choices=["summary", "rankings", "synergies", "calibration", "domains"])
    parser.add_argument("--storage", default="./data/tracker.json", help="Storage file path")
    parser.add_argument("--min-uses", type=int, default=3, help="Minimum uses for rankings")
    
    args = parser.parse_args()
    
    tracker = EffectivenessTracker(args.storage)
    
    if args.command == "summary":
        print(json.dumps(tracker.get_summary(), indent=2))
    elif args.command == "rankings":
        print(json.dumps(tracker.get_model_rankings(args.min_uses), indent=2))
    elif args.command == "synergies":
        print(json.dumps(tracker.get_synergy_rankings(args.min_uses), indent=2))
    elif args.command == "calibration":
        print(json.dumps(tracker.get_calibration_report(), indent=2))
    elif args.command == "domains":
        print(json.dumps(tracker.get_domain_analysis(), indent=2))


if __name__ == "__main__":
    main()
