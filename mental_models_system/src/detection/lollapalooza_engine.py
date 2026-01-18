"""
Real-time Lollapalooza Detection Engine

Detects when multiple mental models converge to create extreme outcomes.
Based on Charlie Munger's concept of Lollapalooza effects where multiple
psychological tendencies act in concert.

This engine:
1. Monitors incoming signals/events in real-time
2. Identifies which mental models are being triggered
3. Calculates confluence scores when multiple models activate
4. Alerts when Lollapalooza conditions are detected
5. Tracks historical patterns for learning

"When multiple tendencies combine, you get extreme consequences."
- Charlie Munger
"""

import json
import logging
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple, Callable
from collections import defaultdict
import threading
import queue
import time

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class SignalType(Enum):
    """Types of signals that can trigger mental model activation."""
    MARKET = "market"
    BEHAVIORAL = "behavioral"
    ORGANIZATIONAL = "organizational"
    ECONOMIC = "economic"
    PSYCHOLOGICAL = "psychological"
    TECHNICAL = "technical"
    NEWS = "news"
    SENTIMENT = "sentiment"


class AlertLevel(Enum):
    """Alert levels for Lollapalooza detection."""
    LOW = "low"           # 2-3 models converging
    MEDIUM = "medium"     # 4-5 models converging
    HIGH = "high"         # 6-7 models converging
    CRITICAL = "critical" # 8+ models converging (true Lollapalooza)


@dataclass
class Signal:
    """Represents an incoming signal/event to be analyzed."""
    id: str
    signal_type: SignalType
    source: str
    data: Dict
    timestamp: datetime = field(default_factory=datetime.now)
    metadata: Dict = field(default_factory=dict)


@dataclass
class ModelActivation:
    """Represents a mental model being activated by a signal."""
    model_id: int
    model_name: str
    category: str
    activation_strength: float  # 0.0 to 1.0
    signal_id: str
    timestamp: datetime = field(default_factory=datetime.now)
    reasoning: str = ""


@dataclass
class LollapaloozaEvent:
    """Represents a detected Lollapalooza event."""
    id: str
    timestamp: datetime
    alert_level: AlertLevel
    confluence_score: float
    activated_models: List[ModelActivation]
    signals: List[Signal]
    category_breakdown: Dict[str, int]
    prediction: str
    confidence: float
    recommendations: List[str]


class ModelActivationRules:
    """
    Rules for determining when signals activate mental models.
    
    Each rule maps signal patterns to mental model activations.
    Based on Munger's framework of recognizing psychological patterns.
    """
    
    def __init__(self):
        self.rules: Dict[int, List[Callable]] = defaultdict(list)
        self._initialize_default_rules()
    
    def _initialize_default_rules(self):
        """Initialize default activation rules for key mental models."""
        
        # Model 1: Reward and Punishment Superresponse
        self.add_rule(1, lambda s: (
            s.signal_type == SignalType.BEHAVIORAL and
            any(k in s.data for k in ['incentive', 'bonus', 'penalty', 'reward'])
        ))
        
        # Model 14: Deprival-Superreaction (Loss Aversion)
        self.add_rule(14, lambda s: (
            s.signal_type in [SignalType.MARKET, SignalType.ECONOMIC] and
            s.data.get('change_percent', 0) < -5
        ))
        
        # Model 15: Social-Proof Tendency
        self.add_rule(15, lambda s: (
            s.signal_type in [SignalType.BEHAVIORAL, SignalType.SENTIMENT] and
            any(k in s.data for k in ['crowd', 'consensus', 'majority', 'trending'])
        ))
        
        # Model 25: Lollapalooza Effect (meta - activates when others combine)
        # This is handled specially in the engine
        
        # Model 26: Confirmation Bias
        self.add_rule(26, lambda s: (
            s.signal_type == SignalType.BEHAVIORAL and
            any(k in s.data for k in ['selective', 'ignore_contrary', 'echo_chamber'])
        ))
        
        # Model 30: Incentive-Caused Bias
        self.add_rule(30, lambda s: (
            s.signal_type in [SignalType.ORGANIZATIONAL, SignalType.ECONOMIC] and
            any(k in s.data for k in ['commission', 'fee', 'conflict_of_interest'])
        ))
        
        # Model 68: Mr. Market
        self.add_rule(68, lambda s: (
            s.signal_type == SignalType.MARKET and
            abs(s.data.get('volatility', 0)) > 2.0
        ))
        
        # Model 73: Economic Moats
        self.add_rule(73, lambda s: (
            s.signal_type in [SignalType.ECONOMIC, SignalType.TECHNICAL] and
            any(k in s.data for k in ['competitive_advantage', 'barrier', 'moat'])
        ))
        
        # Model 77: Network Effects
        self.add_rule(77, lambda s: (
            s.signal_type == SignalType.TECHNICAL and
            any(k in s.data for k in ['users', 'network', 'viral', 'adoption'])
        ))
        
        # Model 101: Compounding
        self.add_rule(101, lambda s: (
            s.signal_type in [SignalType.ECONOMIC, SignalType.MARKET] and
            any(k in s.data for k in ['growth_rate', 'compound', 'reinvest'])
        ))
        
        # Model 110: Positive Feedback Loops
        self.add_rule(110, lambda s: (
            any(k in s.data for k in ['feedback', 'reinforcing', 'spiral', 'momentum'])
        ))
    
    def add_rule(self, model_id: int, rule: Callable[[Signal], bool]):
        """Add an activation rule for a mental model."""
        self.rules[model_id].append(rule)
    
    def check_activation(self, model_id: int, signal: Signal) -> bool:
        """Check if a signal activates a specific mental model."""
        if model_id not in self.rules:
            return False
        return any(rule(signal) for rule in self.rules[model_id])


class LollapaloozaDetectionEngine:
    """
    Real-time engine for detecting Lollapalooza effects.
    
    Monitors signals, tracks model activations, and alerts when
    multiple models converge to create potential extreme outcomes.
    """
    
    # Lollapalooza patterns - known combinations that create extreme effects
    KNOWN_PATTERNS = {
        "bubble_formation": {
            "models": [15, 13, 25, 110, 18, 129],  # Social proof, overoptimism, etc.
            "description": "Classic bubble pattern with social proof and positive feedback",
            "typical_outcome": "Extreme price appreciation followed by crash"
        },
        "corporate_fraud": {
            "models": [30, 60, 22, 125, 11, 25],  # Incentive bias, agency problems, etc.
            "description": "Conditions enabling corporate fraud",
            "typical_outcome": "Accounting fraud, eventual collapse"
        },
        "monopoly_building": {
            "models": [77, 78, 82, 76, 80],  # Network effects, scale, winner-take-all
            "description": "Conditions for monopoly formation",
            "typical_outcome": "Market dominance, high returns"
        },
        "value_investing_opportunity": {
            "models": [69, 73, 68, 101, 41],  # Margin of safety, moat, Mr. Market
            "description": "Classic value investing setup",
            "typical_outcome": "Significant long-term returns"
        },
        "organizational_decay": {
            "models": [90, 124, 125, 5, 34],  # Bureaucracy, inertia, status quo
            "description": "Conditions for organizational decline",
            "typical_outcome": "Loss of competitiveness, disruption vulnerability"
        }
    }
    
    def __init__(self, 
                 models_data_path: Optional[str] = None,
                 window_seconds: int = 300,
                 min_confluence_threshold: float = 0.6):
        """
        Initialize the Lollapalooza detection engine.
        
        Args:
            models_data_path: Path to mental models JSON data
            window_seconds: Time window for considering related signals
            min_confluence_threshold: Minimum score to trigger alert
        """
        self.window_seconds = window_seconds
        self.min_confluence_threshold = min_confluence_threshold
        
        # Load mental models data
        self.models = {}
        self.categories = {}
        if models_data_path:
            self._load_models(models_data_path)
        
        # Activation rules
        self.activation_rules = ModelActivationRules()
        
        # State tracking
        self.recent_activations: List[ModelActivation] = []
        self.recent_signals: List[Signal] = []
        self.detected_events: List[LollapaloozaEvent] = []
        
        # Threading for real-time processing
        self.signal_queue: queue.Queue = queue.Queue()
        self.running = False
        self.processor_thread: Optional[threading.Thread] = None
        
        # Callbacks for alerts
        self.alert_callbacks: List[Callable[[LollapaloozaEvent], None]] = []
        
        # Metrics
        self.metrics = {
            "signals_processed": 0,
            "activations_detected": 0,
            "lollapalooza_events": 0,
            "false_positives": 0,
            "true_positives": 0
        }
    
    def _load_models(self, path: str):
        """Load mental models from JSON file."""
        try:
            with open(path, 'r') as f:
                data = json.load(f)
            
            for model in data.get('mental_models', []):
                self.models[model['id']] = model
            
            for cat in data.get('categories', []):
                self.categories[cat['id']] = cat
            
            logger.info(f"Loaded {len(self.models)} mental models")
        except Exception as e:
            logger.error(f"Error loading models: {e}")
    
    def start(self):
        """Start the real-time detection engine."""
        if self.running:
            return
        
        self.running = True
        self.processor_thread = threading.Thread(target=self._process_loop, daemon=True)
        self.processor_thread.start()
        logger.info("Lollapalooza detection engine started")
    
    def stop(self):
        """Stop the detection engine."""
        self.running = False
        if self.processor_thread:
            self.processor_thread.join(timeout=5)
        logger.info("Lollapalooza detection engine stopped")
    
    def submit_signal(self, signal: Signal):
        """Submit a signal for processing."""
        self.signal_queue.put(signal)
    
    def register_alert_callback(self, callback: Callable[[LollapaloozaEvent], None]):
        """Register a callback to be called when Lollapalooza is detected."""
        self.alert_callbacks.append(callback)
    
    def _process_loop(self):
        """Main processing loop for real-time detection."""
        while self.running:
            try:
                # Get signal with timeout to allow checking running flag
                signal = self.signal_queue.get(timeout=1.0)
                self._process_signal(signal)
            except queue.Empty:
                continue
            except Exception as e:
                logger.error(f"Error processing signal: {e}")
    
    def _process_signal(self, signal: Signal):
        """Process a single signal."""
        self.metrics["signals_processed"] += 1
        
        # Clean old data outside window
        self._cleanup_old_data()
        
        # Store signal
        self.recent_signals.append(signal)
        
        # Check which models are activated
        activations = self._detect_activations(signal)
        self.recent_activations.extend(activations)
        
        # Check for Lollapalooza conditions
        if activations:
            self._check_lollapalooza()
    
    def _cleanup_old_data(self):
        """Remove data outside the analysis window."""
        cutoff = datetime.now() - timedelta(seconds=self.window_seconds)
        
        self.recent_signals = [
            s for s in self.recent_signals if s.timestamp > cutoff
        ]
        self.recent_activations = [
            a for a in self.recent_activations if a.timestamp > cutoff
        ]
    
    def _detect_activations(self, signal: Signal) -> List[ModelActivation]:
        """Detect which mental models are activated by a signal."""
        activations = []
        
        for model_id, model in self.models.items():
            if self.activation_rules.check_activation(model_id, signal):
                activation = ModelActivation(
                    model_id=model_id,
                    model_name=model['name'],
                    category=self.categories.get(model['category_id'], {}).get('name', 'Unknown'),
                    activation_strength=model.get('applicability', 0.8),
                    signal_id=signal.id,
                    reasoning=f"Signal {signal.signal_type.value} triggered {model['name']}"
                )
                activations.append(activation)
                self.metrics["activations_detected"] += 1
                logger.debug(f"Model activated: {model['name']}")
        
        return activations
    
    def _check_lollapalooza(self):
        """Check if current activations constitute a Lollapalooza event."""
        if len(self.recent_activations) < 3:
            return
        
        # Get unique models activated in window
        active_model_ids = set(a.model_id for a in self.recent_activations)
        
        # Calculate confluence score
        confluence_score = self._calculate_confluence_score(active_model_ids)
        
        if confluence_score < self.min_confluence_threshold:
            return
        
        # Determine alert level
        alert_level = self._determine_alert_level(len(active_model_ids), confluence_score)
        
        # Check for known patterns
        pattern_match = self._match_known_patterns(active_model_ids)
        
        # Create Lollapalooza event
        event = self._create_event(
            active_model_ids, 
            confluence_score, 
            alert_level,
            pattern_match
        )
        
        self.detected_events.append(event)
        self.metrics["lollapalooza_events"] += 1
        
        # Trigger alerts
        self._trigger_alerts(event)
        
        logger.warning(f"LOLLAPALOOZA DETECTED: {alert_level.value} - Score: {confluence_score:.2f}")
    
    def _calculate_confluence_score(self, active_model_ids: Set[int]) -> float:
        """
        Calculate confluence score based on activated models.
        
        Considers:
        - Number of models activated
        - Diversity of categories
        - Model applicability scores
        - Known pattern matches
        """
        if not active_model_ids:
            return 0.0
        
        # Base score from number of models
        num_models = len(active_model_ids)
        base_score = min(num_models / 10.0, 1.0)  # Max at 10 models
        
        # Category diversity bonus
        categories = set()
        total_applicability = 0
        for model_id in active_model_ids:
            model = self.models.get(model_id, {})
            categories.add(model.get('category_id'))
            total_applicability += model.get('applicability', 0.5)
        
        diversity_bonus = len(categories) / 8.0  # 8 total categories
        
        # Average applicability
        avg_applicability = total_applicability / num_models if num_models > 0 else 0
        
        # Pattern match bonus
        pattern_bonus = 0.2 if self._match_known_patterns(active_model_ids) else 0
        
        # Weighted combination
        confluence_score = (
            base_score * 0.4 +
            diversity_bonus * 0.2 +
            avg_applicability * 0.3 +
            pattern_bonus
        )
        
        return min(confluence_score, 1.0)
    
    def _determine_alert_level(self, num_models: int, confluence_score: float) -> AlertLevel:
        """Determine alert level based on activation count and score."""
        if num_models >= 8 or confluence_score >= 0.9:
            return AlertLevel.CRITICAL
        elif num_models >= 6 or confluence_score >= 0.8:
            return AlertLevel.HIGH
        elif num_models >= 4 or confluence_score >= 0.7:
            return AlertLevel.MEDIUM
        else:
            return AlertLevel.LOW
    
    def _match_known_patterns(self, active_model_ids: Set[int]) -> Optional[str]:
        """Check if activated models match a known Lollapalooza pattern."""
        best_match = None
        best_overlap = 0
        
        for pattern_name, pattern in self.KNOWN_PATTERNS.items():
            pattern_models = set(pattern['models'])
            overlap = len(active_model_ids & pattern_models)
            overlap_ratio = overlap / len(pattern_models)
            
            if overlap_ratio >= 0.5 and overlap > best_overlap:
                best_match = pattern_name
                best_overlap = overlap
        
        return best_match
    
    def _create_event(self, 
                      active_model_ids: Set[int],
                      confluence_score: float,
                      alert_level: AlertLevel,
                      pattern_match: Optional[str]) -> LollapaloozaEvent:
        """Create a Lollapalooza event object."""
        # Get activations for active models
        activations = [
            a for a in self.recent_activations 
            if a.model_id in active_model_ids
        ]
        
        # Category breakdown
        category_breakdown = defaultdict(int)
        for a in activations:
            category_breakdown[a.category] += 1
        
        # Generate prediction and recommendations
        prediction, confidence, recommendations = self._generate_insights(
            active_model_ids, pattern_match, confluence_score
        )
        
        return LollapaloozaEvent(
            id=f"lolla_{datetime.now().strftime('%Y%m%d_%H%M%S')}",
            timestamp=datetime.now(),
            alert_level=alert_level,
            confluence_score=confluence_score,
            activated_models=activations,
            signals=self.recent_signals.copy(),
            category_breakdown=dict(category_breakdown),
            prediction=prediction,
            confidence=confidence,
            recommendations=recommendations
        )
    
    def _generate_insights(self, 
                          active_model_ids: Set[int],
                          pattern_match: Optional[str],
                          confluence_score: float) -> Tuple[str, float, List[str]]:
        """Generate prediction and recommendations based on detected pattern."""
        
        if pattern_match:
            pattern = self.KNOWN_PATTERNS[pattern_match]
            prediction = f"Pattern: {pattern_match.replace('_', ' ').title()}. {pattern['typical_outcome']}"
            confidence = min(confluence_score + 0.1, 0.95)
            
            recommendations = [
                f"Review historical cases of {pattern_match.replace('_', ' ')}",
                "Apply inversion: What could make this analysis wrong?",
                "Check for disconfirming evidence",
                "Consider second-order effects"
            ]
        else:
            prediction = f"Novel pattern with {len(active_model_ids)} converging models. Extreme outcome likely."
            confidence = confluence_score * 0.8
            
            recommendations = [
                "Document this pattern for future reference",
                "Seek additional data points",
                "Apply two-track analysis (rational + psychological)",
                "Consider position sizing carefully"
            ]
        
        # Add model-specific recommendations
        if 14 in active_model_ids:  # Deprival-Superreaction
            recommendations.append("Watch for loss aversion driving irrational decisions")
        if 15 in active_model_ids:  # Social Proof
            recommendations.append("Be contrarian - crowd may be wrong")
        if 110 in active_model_ids:  # Positive Feedback
            recommendations.append("Monitor for acceleration or reversal of feedback loop")
        
        return prediction, confidence, recommendations
    
    def _trigger_alerts(self, event: LollapaloozaEvent):
        """Trigger registered alert callbacks."""
        for callback in self.alert_callbacks:
            try:
                callback(event)
            except Exception as e:
                logger.error(f"Error in alert callback: {e}")
    
    def get_status(self) -> Dict:
        """Get current engine status and metrics."""
        return {
            "running": self.running,
            "window_seconds": self.window_seconds,
            "recent_signals_count": len(self.recent_signals),
            "recent_activations_count": len(self.recent_activations),
            "total_events_detected": len(self.detected_events),
            "metrics": self.metrics,
            "active_models": list(set(a.model_id for a in self.recent_activations))
        }
    
    def get_recent_events(self, limit: int = 10) -> List[Dict]:
        """Get recent Lollapalooza events."""
        events = self.detected_events[-limit:]
        return [
            {
                "id": e.id,
                "timestamp": e.timestamp.isoformat(),
                "alert_level": e.alert_level.value,
                "confluence_score": e.confluence_score,
                "num_models": len(e.activated_models),
                "prediction": e.prediction,
                "confidence": e.confidence,
                "recommendations": e.recommendations
            }
            for e in reversed(events)
        ]


# Convenience function for creating engine with default data
def create_engine(data_dir: Optional[str] = None) -> LollapaloozaDetectionEngine:
    """Create a Lollapalooza detection engine with default configuration."""
    if data_dir is None:
        data_dir = Path(__file__).parent.parent.parent / "data" / "raw"
    
    models_path = Path(data_dir) / "mental_models_complete.json"
    
    engine = LollapaloozaDetectionEngine(
        models_data_path=str(models_path) if models_path.exists() else None,
        window_seconds=300,
        min_confluence_threshold=0.6
    )
    
    return engine


if __name__ == "__main__":
    # Demo usage
    engine = create_engine()
    
    # Register alert callback
    def alert_handler(event: LollapaloozaEvent):
        print(f"\n{'='*60}")
        print(f"🚨 LOLLAPALOOZA ALERT: {event.alert_level.value.upper()}")
        print(f"Score: {event.confluence_score:.2f}")
        print(f"Prediction: {event.prediction}")
        print(f"Confidence: {event.confidence:.2%}")
        print(f"Models activated: {len(event.activated_models)}")
        print(f"Recommendations:")
        for rec in event.recommendations:
            print(f"  • {rec}")
        print(f"{'='*60}\n")
    
    engine.register_alert_callback(alert_handler)
    engine.start()
    
    # Simulate some signals
    test_signals = [
        Signal(
            id="sig_001",
            signal_type=SignalType.MARKET,
            source="market_data",
            data={"change_percent": -8.5, "volatility": 3.2}
        ),
        Signal(
            id="sig_002",
            signal_type=SignalType.SENTIMENT,
            source="social_media",
            data={"trending": True, "crowd": "panic_selling"}
        ),
        Signal(
            id="sig_003",
            signal_type=SignalType.BEHAVIORAL,
            source="trading_desk",
            data={"selective": True, "echo_chamber": True}
        ),
        Signal(
            id="sig_004",
            signal_type=SignalType.ECONOMIC,
            source="analyst",
            data={"feedback": "reinforcing", "spiral": True}
        ),
    ]
    
    print("Submitting test signals...")
    for signal in test_signals:
        engine.submit_signal(signal)
        time.sleep(0.5)
    
    time.sleep(2)
    
    print("\nEngine Status:")
    print(json.dumps(engine.get_status(), indent=2))
    
    print("\nRecent Events:")
    print(json.dumps(engine.get_recent_events(), indent=2))
    
    engine.stop()
