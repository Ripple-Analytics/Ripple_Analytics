from typing import Dict, Any, List, Callable, Optional
from dataclasses import dataclass, field
from .detector import FailureMode, DetectionResult, Safeguard, FailureCategory, FailureSeverity
from .registry import FailureModeRegistry


@dataclass
class SafeguardResult:
    safeguard_name: str
    applied: bool
    modifications: Dict[str, Any]
    message: str


class SafeguardEngine:
    def __init__(self):
        self._safeguards: Dict[str, Safeguard] = {}
        self._register_default_safeguards()

    def _register_default_safeguards(self) -> None:
        self.register_safeguard(Safeguard(
            id="sg_require_disconfirming",
            name="Require Disconfirming Evidence",
            description="Ensures analysis includes search for disconfirming evidence",
            applicable_failures=["fm_5_2", "fm_26_1", "fm_26_2"],
            implementation=self._safeguard_require_disconfirming,
            priority=10,
        ))
        
        self.register_safeguard(Safeguard(
            id="sg_multiple_sources",
            name="Require Multiple Sources",
            description="Ensures analysis uses multiple independent sources",
            applicable_failures=["fm_1_1", "fm_10_1", "fm_18_2"],
            implementation=self._safeguard_multiple_sources,
            priority=9,
        ))
        
        self.register_safeguard(Safeguard(
            id="sg_confidence_calibration",
            name="Confidence Calibration",
            description="Adjusts overconfident assessments",
            applicable_failures=["fm_4_2", "fm_12_1", "fm_13_1"],
            implementation=self._safeguard_confidence_calibration,
            priority=8,
        ))
        
        self.register_safeguard(Safeguard(
            id="sg_base_rate_check",
            name="Base Rate Check",
            description="Ensures base rates are considered",
            applicable_failures=["fm_13_3", "fm_33_1"],
            implementation=self._safeguard_base_rate_check,
            priority=8,
        ))
        
        self.register_safeguard(Safeguard(
            id="sg_inversion_analysis",
            name="Inversion Analysis",
            description="Applies inversion to consider what could go wrong",
            applicable_failures=["fm_13_2", "fm_25_1"],
            implementation=self._safeguard_inversion_analysis,
            priority=7,
        ))
        
        self.register_safeguard(Safeguard(
            id="sg_temporal_check",
            name="Temporal Context Check",
            description="Ensures temporal context is considered",
            applicable_failures=["fm_18_1", "fm_21_2"],
            implementation=self._safeguard_temporal_check,
            priority=6,
        ))
        
        self.register_safeguard(Safeguard(
            id="sg_stakeholder_mapping",
            name="Stakeholder Incentive Mapping",
            description="Maps stakeholder incentives",
            applicable_failures=["fm_1_1", "fm_1_2", "fm_30_1"],
            implementation=self._safeguard_stakeholder_mapping,
            priority=7,
        ))
        
        self.register_safeguard(Safeguard(
            id="sg_sunk_cost_check",
            name="Sunk Cost Check",
            description="Flags potential sunk cost fallacy",
            applicable_failures=["fm_5_1", "fm_5_4", "fm_11_3"],
            implementation=self._safeguard_sunk_cost_check,
            priority=9,
        ))

    def register_safeguard(self, safeguard: Safeguard) -> None:
        self._safeguards[safeguard.id] = safeguard

    def get_safeguard(self, safeguard_id: str) -> Optional[Safeguard]:
        return self._safeguards.get(safeguard_id)

    def get_all_safeguards(self) -> List[Safeguard]:
        return list(self._safeguards.values())

    def get_safeguards_for_failure(self, failure_id: str) -> List[Safeguard]:
        applicable = []
        for safeguard in self._safeguards.values():
            if failure_id in safeguard.applicable_failures:
                applicable.append(safeguard)
        return sorted(applicable, key=lambda s: s.priority, reverse=True)

    def apply_safeguards(
        self,
        context: Dict[str, Any],
        detected_failures: List[DetectionResult],
    ) -> Dict[str, Any]:
        modified_context = context.copy()
        applied_safeguards = []
        safeguard_results = []
        
        for result in detected_failures:
            if not result.detected:
                continue
            
            safeguards = self.get_safeguards_for_failure(result.failure_mode.id)
            for safeguard in safeguards:
                if safeguard.id in applied_safeguards:
                    continue
                
                try:
                    modified_context = safeguard.implementation(modified_context)
                    applied_safeguards.append(safeguard.id)
                    safeguard_results.append(SafeguardResult(
                        safeguard_name=safeguard.name,
                        applied=True,
                        modifications={},
                        message=f"Applied {safeguard.name}",
                    ))
                except Exception as e:
                    safeguard_results.append(SafeguardResult(
                        safeguard_name=safeguard.name,
                        applied=False,
                        modifications={},
                        message=f"Failed to apply: {str(e)}",
                    ))
        
        modified_context["applied_safeguards"] = applied_safeguards
        modified_context["safeguard_results"] = [
            {"name": r.safeguard_name, "applied": r.applied, "message": r.message}
            for r in safeguard_results
        ]
        
        return modified_context

    def _safeguard_require_disconfirming(self, context: Dict[str, Any]) -> Dict[str, Any]:
        context["requires_disconfirming_evidence"] = True
        context["disconfirming_evidence_prompt"] = "What evidence would disprove this thesis?"
        return context

    def _safeguard_multiple_sources(self, context: Dict[str, Any]) -> Dict[str, Any]:
        context["requires_multiple_sources"] = True
        context["minimum_sources"] = 3
        context["source_diversity_prompt"] = "Ensure sources are independent and diverse"
        return context

    def _safeguard_confidence_calibration(self, context: Dict[str, Any]) -> Dict[str, Any]:
        if "confidence" in context and context["confidence"] > 0.8:
            context["original_confidence"] = context["confidence"]
            context["confidence"] = context["confidence"] * 0.8
            context["confidence_adjusted"] = True
            context["confidence_warning"] = "Confidence reduced due to overconfidence risk"
        return context

    def _safeguard_base_rate_check(self, context: Dict[str, Any]) -> Dict[str, Any]:
        context["requires_base_rate"] = True
        context["base_rate_prompt"] = "What is the base rate for this type of outcome?"
        return context

    def _safeguard_inversion_analysis(self, context: Dict[str, Any]) -> Dict[str, Any]:
        context["requires_inversion"] = True
        context["inversion_prompt"] = "What could go wrong? How could this fail?"
        return context

    def _safeguard_temporal_check(self, context: Dict[str, Any]) -> Dict[str, Any]:
        context["requires_temporal_analysis"] = True
        context["temporal_prompt"] = "Consider long-term historical context, not just recent data"
        return context

    def _safeguard_stakeholder_mapping(self, context: Dict[str, Any]) -> Dict[str, Any]:
        context["requires_stakeholder_mapping"] = True
        context["stakeholder_prompt"] = "Map all stakeholders and their incentives"
        return context

    def _safeguard_sunk_cost_check(self, context: Dict[str, Any]) -> Dict[str, Any]:
        context["sunk_cost_warning"] = True
        context["sunk_cost_prompt"] = "Evaluate this decision as if starting fresh - ignore past investments"
        return context


def apply_safeguards(
    context: Dict[str, Any],
    detected_failures: List[DetectionResult],
) -> Dict[str, Any]:
    engine = SafeguardEngine()
    return engine.apply_safeguards(context, detected_failures)
