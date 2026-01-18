from dataclasses import dataclass, field
from typing import List, Dict, Any, Optional, Callable
from enum import Enum
from datetime import datetime
import logging

logger = logging.getLogger(__name__)


class FailureSeverity(Enum):
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    CRITICAL = "critical"


class FailureCategory(Enum):
    DATA_BIAS = "data_bias"
    REASONING_ERROR = "reasoning_error"
    INCOMPLETE_ANALYSIS = "incomplete_analysis"
    OVERCONFIDENCE = "overconfidence"
    CONTEXT_BLINDNESS = "context_blindness"
    TEMPORAL_ERROR = "temporal_error"
    SCALE_MISMATCH = "scale_mismatch"
    FEEDBACK_LOOP = "feedback_loop"
    EDGE_CASE = "edge_case"
    INTEGRATION_FAILURE = "integration_failure"


@dataclass
class FailureMode:
    id: str
    model_id: int
    model_name: str
    name: str
    description: str
    category: FailureCategory
    severity: FailureSeverity
    detection_signals: List[str]
    prevention_strategies: List[str]
    example_scenario: str
    safeguard_function: Optional[str] = None
    metadata: dict = field(default_factory=dict)


@dataclass
class DetectionResult:
    failure_mode: FailureMode
    detected: bool
    confidence: float
    signals_found: List[str]
    recommendations: List[str]
    timestamp: datetime = field(default_factory=datetime.now)


@dataclass
class Safeguard:
    id: str
    name: str
    description: str
    applicable_failures: List[str]
    implementation: Callable
    priority: int = 1


class FailureModeDetector:
    def __init__(self):
        self._failure_modes: Dict[str, FailureMode] = {}
        self._safeguards: Dict[str, Safeguard] = {}
        self._detection_history: List[DetectionResult] = []

    def register_failure_mode(self, failure_mode: FailureMode) -> None:
        self._failure_modes[failure_mode.id] = failure_mode

    def register_safeguard(self, safeguard: Safeguard) -> None:
        self._safeguards[safeguard.id] = safeguard

    def detect_failures(
        self,
        context: Dict[str, Any],
        model_ids: Optional[List[int]] = None,
    ) -> List[DetectionResult]:
        results = []
        
        for failure_id, failure_mode in self._failure_modes.items():
            if model_ids and failure_mode.model_id not in model_ids:
                continue
            
            result = self._check_failure_mode(failure_mode, context)
            results.append(result)
            self._detection_history.append(result)
        
        return [r for r in results if r.detected]

    def _check_failure_mode(
        self,
        failure_mode: FailureMode,
        context: Dict[str, Any],
    ) -> DetectionResult:
        signals_found = []
        confidence = 0.0
        
        for signal in failure_mode.detection_signals:
            if self._check_signal(signal, context):
                signals_found.append(signal)
        
        if signals_found:
            confidence = len(signals_found) / len(failure_mode.detection_signals)
        
        detected = confidence >= 0.4
        
        recommendations = []
        if detected:
            recommendations = failure_mode.prevention_strategies[:3]
        
        return DetectionResult(
            failure_mode=failure_mode,
            detected=detected,
            confidence=confidence,
            signals_found=signals_found,
            recommendations=recommendations,
        )

    def _check_signal(self, signal: str, context: Dict[str, Any]) -> bool:
        signal_lower = signal.lower()
        
        if "single_source" in signal_lower or "one source" in signal_lower:
            sources = context.get("sources", [])
            return len(sources) <= 1
        
        if "no_disconfirming" in signal_lower or "no contradicting" in signal_lower:
            has_contradictions = context.get("has_contradictions", False)
            return not has_contradictions
        
        if "high_confidence" in signal_lower or "overconfident" in signal_lower:
            confidence = context.get("confidence", 0)
            return confidence > 0.95
        
        if "recent_data_only" in signal_lower or "recency" in signal_lower:
            data_age = context.get("data_age_days", 0)
            return data_age < 30
        
        if "small_sample" in signal_lower:
            sample_size = context.get("sample_size", 0)
            return sample_size < 30
        
        if "no_base_rate" in signal_lower:
            has_base_rate = context.get("has_base_rate", False)
            return not has_base_rate
        
        if "extreme_outcome" in signal_lower:
            outcome_magnitude = context.get("outcome_magnitude", 0)
            return abs(outcome_magnitude) > 3
        
        if "unanimous" in signal_lower or "no_dissent" in signal_lower:
            dissent_count = context.get("dissent_count", 0)
            return dissent_count == 0
        
        if "short_timeframe" in signal_lower:
            timeframe_days = context.get("timeframe_days", 365)
            return timeframe_days < 90
        
        if "single_model" in signal_lower or "one_model" in signal_lower:
            models_used = context.get("models_used", [])
            return len(models_used) <= 1
        
        return False

    def get_safeguards_for_failure(self, failure_id: str) -> List[Safeguard]:
        safeguards = []
        for safeguard in self._safeguards.values():
            if failure_id in safeguard.applicable_failures:
                safeguards.append(safeguard)
        return sorted(safeguards, key=lambda s: s.priority, reverse=True)

    def apply_safeguards(
        self,
        context: Dict[str, Any],
        detected_failures: List[DetectionResult],
    ) -> Dict[str, Any]:
        modified_context = context.copy()
        applied_safeguards = []
        
        for result in detected_failures:
            safeguards = self.get_safeguards_for_failure(result.failure_mode.id)
            for safeguard in safeguards:
                try:
                    modified_context = safeguard.implementation(modified_context)
                    applied_safeguards.append(safeguard.name)
                except Exception as e:
                    logger.warning(f"Safeguard {safeguard.name} failed: {e}")
        
        modified_context["applied_safeguards"] = applied_safeguards
        return modified_context

    def get_failure_modes_for_model(self, model_id: int) -> List[FailureMode]:
        return [
            fm for fm in self._failure_modes.values()
            if fm.model_id == model_id
        ]

    def get_all_failure_modes(self) -> List[FailureMode]:
        return list(self._failure_modes.values())

    def get_detection_history(
        self,
        limit: int = 100,
        only_detected: bool = False,
    ) -> List[DetectionResult]:
        history = self._detection_history[-limit:]
        if only_detected:
            history = [r for r in history if r.detected]
        return history

    def get_stats(self) -> Dict[str, Any]:
        total_detections = len(self._detection_history)
        detected_count = sum(1 for r in self._detection_history if r.detected)
        
        by_category = {}
        by_severity = {}
        
        for result in self._detection_history:
            if result.detected:
                cat = result.failure_mode.category.value
                sev = result.failure_mode.severity.value
                by_category[cat] = by_category.get(cat, 0) + 1
                by_severity[sev] = by_severity.get(sev, 0) + 1
        
        return {
            "total_failure_modes": len(self._failure_modes),
            "total_safeguards": len(self._safeguards),
            "total_detections": total_detections,
            "detected_failures": detected_count,
            "detection_rate": detected_count / total_detections if total_detections > 0 else 0,
            "by_category": by_category,
            "by_severity": by_severity,
        }
