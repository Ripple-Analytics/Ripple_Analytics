"""
Safeguard Engine for Mental Models System

Detects and prevents failure modes in mental model application.
Implements active safeguards to ensure robust decision-making.

"Knowing what you don't know is more useful than being brilliant."
- Charlie Munger
"""

import logging
from typing import Dict, List, Optional, Set, Tuple
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum

from .failure_modes_loader import FailureModesLoader
from ..exceptions import SafeguardViolationError, FailureModeDetectedError

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class SafeguardLevel(Enum):
    """Severity level of safeguards."""
    INFO = "info"
    WARNING = "warning"
    ERROR = "error"
    CRITICAL = "critical"


@dataclass
class SafeguardAlert:
    """An alert from the safeguard system."""
    level: SafeguardLevel
    model_name: str
    failure_mode: str
    description: str
    detection_signals: List[str]
    recommended_actions: List[str]
    timestamp: datetime = field(default_factory=datetime.now)
    
    def to_dict(self) -> Dict:
        return {
            "level": self.level.value,
            "model_name": self.model_name,
            "failure_mode": self.failure_mode,
            "description": self.description,
            "detection_signals": self.detection_signals,
            "recommended_actions": self.recommended_actions,
            "timestamp": self.timestamp.isoformat()
        }


class SafeguardEngine:
    """
    Engine for detecting and preventing mental model failure modes.
    
    Usage:
        engine = SafeguardEngine()
        
        # Check for failure modes
        alerts = engine.check_models(
            models_used=["Network Effects", "First-Mover Advantage"],
            context="We're scaling rapidly to capture market share"
        )
        
        # Handle alerts
        for alert in alerts:
            if alert.level == SafeguardLevel.CRITICAL:
                print(f"CRITICAL: {alert.description}")
    """
    
    def __init__(self):
        self.loader = FailureModesLoader()
        self.alert_history: List[SafeguardAlert] = []
    
    def check_models(
        self,
        models_used: List[str],
        context: str = "",
        raise_on_critical: bool = False
    ) -> List[SafeguardAlert]:
        """
        Check mental models for potential failure modes.
        
        Args:
            models_used: List of mental model names being used
            context: Context or situation description
            raise_on_critical: Whether to raise exception on critical failures
        
        Returns:
            List of safeguard alerts
        """
        alerts = []
        
        for model_name in models_used:
            # Get failure modes for this model
            failure_modes = self.loader.get_failure_modes_for_model(model_name)
            
            for failure_mode in failure_modes:
                # Check if failure mode might apply
                if self._check_failure_mode_applies(failure_mode, context):
                    # Determine severity level
                    level = self._map_severity_to_level(failure_mode.severity)
                    
                    # Get safeguards
                    safeguards = self.loader.get_safeguards_for_model(model_name)
                    recommended_actions = [s.action for s in safeguards if s.failure_mode == failure_mode.name]
                    
                    # Create alert
                    alert = SafeguardAlert(
                        level=level,
                        model_name=model_name,
                        failure_mode=failure_mode.name,
                        description=failure_mode.description,
                        detection_signals=failure_mode.detection_signals,
                        recommended_actions=recommended_actions
                    )
                    
                    alerts.append(alert)
                    self.alert_history.append(alert)
                    
                    # Log alert
                    logger.warning(
                        f"[{level.value.upper()}] Failure mode detected: "
                        f"{model_name} - {failure_mode.name}"
                    )
                    
                    # Raise exception if critical and requested
                    if raise_on_critical and level == SafeguardLevel.CRITICAL:
                        raise FailureModeDetectedError(
                            model_name=model_name,
                            failure_mode=failure_mode.name,
                            severity=failure_mode.severity
                        )
        
        return alerts
    
    def _check_failure_mode_applies(self, failure_mode, context: str) -> bool:
        """
        Check if a failure mode might apply to the given context.
        
        This is a simple heuristic check. In a production system,
        this could use NLP or ML to better detect applicability.
        """
        if not context:
            # If no context provided, assume all failure modes might apply
            return True
        
        context_lower = context.lower()
        
        # Check if any detection signals appear in context
        for signal in failure_mode.detection_signals:
            signal_keywords = signal.lower().split()
            if any(keyword in context_lower for keyword in signal_keywords):
                return True
        
        # Check failure mode name and description
        failure_keywords = failure_mode.name.lower().split()
        if any(keyword in context_lower for keyword in failure_keywords):
            return True
        
        return False
    
    def _map_severity_to_level(self, severity: str) -> SafeguardLevel:
        """Map failure mode severity to safeguard level."""
        severity_lower = severity.lower()
        
        if severity_lower == "critical":
            return SafeguardLevel.CRITICAL
        elif severity_lower == "high":
            return SafeguardLevel.ERROR
        elif severity_lower == "medium":
            return SafeguardLevel.WARNING
        else:
            return SafeguardLevel.INFO
    
    def check_lollapalooza_risks(
        self,
        models: List[str],
        context: str = ""
    ) -> List[SafeguardAlert]:
        """
        Check for risks when multiple models interact (Lollapalooza effects).
        
        When multiple mental models reinforce each other, both positive
        and negative effects can be amplified. This checks for potential
        negative amplification.
        """
        alerts = []
        
        if len(models) < 2:
            return alerts
        
        # Check each model individually first
        individual_alerts = self.check_models(models, context)
        
        # Check for dangerous combinations
        dangerous_combinations = [
            ({"Confirmation Bias", "Authority Bias"}, 
             "Extreme susceptibility to authoritative misinformation"),
            ({"Sunk Cost Fallacy", "Commitment Bias"},
             "Inability to abandon failing projects"),
            ({"Overconfidence", "Availability Bias"},
             "Severe miscalculation of risks"),
            ({"Network Effects", "Winner-Take-All"},
             "Potential for monopolistic behavior"),
            ({"Economies of Scale", "First-Mover Advantage"},
             "Over-aggressive expansion risks")
        ]
        
        models_set = set(models)
        
        for combo_models, risk_description in dangerous_combinations:
            if combo_models.issubset(models_set):
                alert = SafeguardAlert(
                    level=SafeguardLevel.ERROR,
                    model_name=", ".join(combo_models),
                    failure_mode="Dangerous Model Combination",
                    description=risk_description,
                    detection_signals=[f"Using {len(combo_models)} interacting models"],
                    recommended_actions=[
                        "Seek external review",
                        "Apply extra scrutiny to assumptions",
                        "Consider contrary evidence",
                        "Implement staged rollout"
                    ]
                )
                alerts.append(alert)
                self.alert_history.append(alert)
        
        return alerts + individual_alerts
    
    def get_alert_history(
        self,
        level: Optional[SafeguardLevel] = None,
        model_name: Optional[str] = None
    ) -> List[Dict]:
        """
        Get history of alerts.
        
        Args:
            level: Filter by severity level
            model_name: Filter by model name
        
        Returns:
            List of alert dictionaries
        """
        filtered_alerts = self.alert_history
        
        if level:
            filtered_alerts = [a for a in filtered_alerts if a.level == level]
        
        if model_name:
            filtered_alerts = [a for a in filtered_alerts if model_name in a.model_name]
        
        return [alert.to_dict() for alert in filtered_alerts]
    
    def get_statistics(self) -> Dict:
        """Get statistics about alerts."""
        if not self.alert_history:
            return {
                "total_alerts": 0,
                "by_level": {},
                "by_model": {},
                "most_common_failures": []
            }
        
        # Count by level
        by_level = {}
        for alert in self.alert_history:
            level = alert.level.value
            by_level[level] = by_level.get(level, 0) + 1
        
        # Count by model
        by_model = {}
        for alert in self.alert_history:
            model = alert.model_name
            by_model[model] = by_model.get(model, 0) + 1
        
        # Most common failure modes
        failure_counts = {}
        for alert in self.alert_history:
            failure = alert.failure_mode
            failure_counts[failure] = failure_counts.get(failure, 0) + 1
        
        most_common = sorted(
            failure_counts.items(),
            key=lambda x: x[1],
            reverse=True
        )[:10]
        
        return {
            "total_alerts": len(self.alert_history),
            "by_level": by_level,
            "by_model": dict(sorted(by_model.items(), key=lambda x: x[1], reverse=True)[:10]),
            "most_common_failures": most_common
        }
    
    def clear_history(self):
        """Clear alert history."""
        self.alert_history = []
        logger.info("Cleared safeguard alert history")


def demonstrate_safeguard_engine():
    """Demonstrate the safeguard engine with examples."""
    print("=== Safeguard Engine Demonstration ===\n")
    
    engine = SafeguardEngine()
    
    # Example 1: Rapid scaling
    print("Example 1: Rapid Scaling Decision")
    print("-" * 50)
    context = """
    We're seeing 50% month-over-month growth. Network effects are kicking in.
    We should scale as fast as possible to capture the market before competitors.
    Let's hire 500 people this quarter and expand to 20 cities.
    """
    
    alerts = engine.check_models(
        models_used=["Network Effects", "First-Mover Advantage", "Economies of Scale"],
        context=context
    )
    
    print(f"Found {len(alerts)} potential issues:\n")
    for alert in alerts:
        print(f"[{alert.level.value.upper()}] {alert.model_name}")
        print(f"  Failure Mode: {alert.failure_mode}")
        print(f"  Description: {alert.description}")
        print(f"  Recommended Actions:")
        for action in alert.recommended_actions[:3]:
            print(f"    - {action}")
        print()
    
    # Example 2: Investment decision
    print("\nExample 2: Investment Decision")
    print("-" * 50)
    context = """
    This investment looks great. My mentor (a successful investor) recommended it,
    and I've seen similar deals work out well recently. I'm very confident this
    will be a winner.
    """
    
    alerts = engine.check_models(
        models_used=["Authority Bias", "Availability Bias", "Overconfidence"],
        context=context
    )
    
    print(f"Found {len(alerts)} potential issues:\n")
    for alert in alerts[:3]:  # Show first 3
        print(f"[{alert.level.value.upper()}] {alert.model_name}")
        print(f"  Failure Mode: {alert.failure_mode}")
        print()
    
    # Show statistics
    print("\nSafeguard Statistics:")
    print("-" * 50)
    stats = engine.get_statistics()
    print(f"Total alerts: {stats['total_alerts']}")
    print(f"By level: {stats['by_level']}")


if __name__ == "__main__":
    demonstrate_safeguard_engine()
