"""
Lollapalooza Analysis Module

Wrapper around the detection engine for analysis workflows.
Provides a simplified interface for detecting Lollapalooza effects from mental model signals.

"When multiple tendencies combine, you get extreme consequences."
- Charlie Munger
"""

from dataclasses import dataclass, field
from datetime import datetime
from typing import List, Dict, Any
from ..detection.lollapalooza_engine import (
    LollapaloozaDetectionEngine,
    Signal,
    SignalType,
    ModelActivation,
    LollapaloozaEvent
)


@dataclass
class LollapaloozaEffect:
    """Simplified representation of a Lollapalooza effect for analysis."""
    strength: float  # 0.0 to 1.0
    contributing_models: List[str]
    description: str
    confidence: float
    timestamp: datetime = field(default_factory=datetime.now)
    recommendations: List[str] = field(default_factory=list)


class LollapaloozaDetector:
    """
    Detector for Lollapalooza effects in mental model analysis.
    
    Usage:
        detector = LollapaloozaDetector()
        effects = detector.detect_from_signals(signals)
    """
    
    def __init__(self):
        """Initialize the detector."""
        self.engine = LollapaloozaDetectionEngine()
    
    def detect_from_signals(self, signals: List[Any]) -> List[LollapaloozaEffect]:
        """
        Detect Lollapalooza effects from a list of mental model signals.
        
        Args:
            signals: List of mental model signals (from MentalModelAnalyzer)
            
        Returns:
            List of detected Lollapalooza effects
        """
        effects = []
        
        # Group signals by strength and check for convergence
        if len(signals) < 3:
            return effects
        
        # Calculate convergence strength
        model_names = [s.model_name if hasattr(s, 'model_name') else str(s) for s in signals]
        avg_confidence = sum(s.confidence if hasattr(s, 'confidence') else 0.7 for s in signals) / len(signals)
        
        # Determine strength based on number of models and their confidence
        # Use a more reasonable formula: base strength from model count + confidence boost
        base_strength = min(0.7, len(signals) / 5.0)  # 3 models = 0.6, 5 models = 1.0 (capped at 0.7)
        strength = min(1.0, base_strength + (avg_confidence * 0.3))  # Add confidence boost
        
        # Create effect if strength is significant
        if strength >= 0.5:
            effect = LollapaloozaEffect(
                strength=strength,
                contributing_models=model_names,
                description=f"Detected convergence of {len(signals)} mental models",
                confidence=avg_confidence,
                recommendations=self._generate_recommendations(model_names)
            )
            effects.append(effect)
        
        return effects
    
    def detect_from_text(self, text: str) -> List[LollapaloozaEffect]:
        """
        Detect Lollapalooza effects directly from text.
        
        Args:
            text: Text to analyze
            
        Returns:
            List of detected Lollapalooza effects
        """
        from .model_analyzer import MentalModelAnalyzer
        
        # Analyze text for mental model signals
        analyzer = MentalModelAnalyzer()
        signals = analyzer.analyze_text(text)
        
        # Detect Lollapalooza effects from signals
        return self.detect_from_signals(signals)
    
    def _generate_recommendations(self, model_names: List[str]) -> List[str]:
        """Generate recommendations based on detected models."""
        recommendations = [
            "Monitor for reinforcing feedback loops between models",
            "Consider safeguards for each individual model",
            "Assess cumulative risk of multiple models converging",
            "Document decision rationale given multiple factors"
        ]
        
        # Add specific recommendations based on model types
        if any("Network Effects" in name for name in model_names):
            recommendations.append("Track network growth metrics and tipping points")
        
        if any("Scale" in name for name in model_names):
            recommendations.append("Monitor unit economics at different scales")
        
        if any("Switching Costs" in name for name in model_names):
            recommendations.append("Measure customer retention and churn patterns")
        
        return recommendations
    
    def analyze_interaction_strength(self, model1: str, model2: str) -> float:
        """
        Analyze the interaction strength between two mental models.
        
        Args:
            model1: First model name
            model2: Second model name
            
        Returns:
            Interaction strength (0.0 to 1.0)
        """
        # Known strong interactions
        strong_pairs = {
            ("Network Effects", "Economies of Scale"): 0.9,
            ("Network Effects", "Switching Costs"): 0.85,
            ("Economies of Scale", "Switching Costs"): 0.8,
            ("Brand", "Network Effects"): 0.75,
            ("Data Advantage", "Network Effects"): 0.85,
        }
        
        # Check both orderings
        pair1 = (model1, model2)
        pair2 = (model2, model1)
        
        if pair1 in strong_pairs:
            return strong_pairs[pair1]
        elif pair2 in strong_pairs:
            return strong_pairs[pair2]
        else:
            # Default moderate interaction
            return 0.5
