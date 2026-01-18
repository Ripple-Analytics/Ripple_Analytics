"""
Lisp (Hy) Integration Module

This module provides a bridge between Python and the Hy-based mental models system.
Hy is a Lisp dialect that compiles to Python, giving us:
- Homoiconicity (code as data) for self-modifying systems
- Powerful macros for domain-specific abstractions
- REPL-driven development for rapid iteration
- Functional programming paradigms

The system is now primarily written in Lisp (Hy) for faster feature development.
Python serves as a thin wrapper for FastAPI endpoints and external integrations.

Modules:
- mental_models_dsl.hy: Core DSL with defmodel, defrule, deffailure macros
- mental_models_library.hy: Complete library of 129+ mental models
- analysis_engine.hy: Latticework, lollapalooza, inversion, two-track analysis
- statistical_engine.hy: Correlations, regression, factor analysis
- data_processing.hy: Text chunking, pattern matching, document analysis

Usage:
    from mental_models_system.src.lisp import LispBridge
    
    bridge = LispBridge()
    result = bridge.analyze_context({"situation": "Investment decision"})
    stats = bridge.statistical_analysis({"var1": [1,2,3], "var2": [4,5,6]})
"""

import os
import sys
from typing import Dict, List, Any, Optional
from pathlib import Path
from dataclasses import dataclass, field
from enum import Enum
from datetime import datetime
import json

HY_AVAILABLE = False
HY_MODULES = {}

try:
    import hy
    import hy.importer
    HY_AVAILABLE = True
except ImportError:
    pass


class LispCategory(Enum):
    ECONOMICS = "economics"
    PSYCHOLOGY = "psychology"
    PHYSICS = "physics"
    BIOLOGY = "biology"
    MATHEMATICS = "mathematics"
    SYSTEMS = "systems"
    DECISION_MAKING = "decision_making"
    STRATEGY = "strategy"


class LispSeverity(Enum):
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    CRITICAL = "critical"


@dataclass
class LispModel:
    name: str
    category: LispCategory
    originator: str
    description: str
    rules: List[Dict[str, Any]] = field(default_factory=list)
    failure_modes: List[Dict[str, Any]] = field(default_factory=list)


@dataclass
class LispAnalysisResult:
    models_applied: int
    individual_results: List[Dict[str, Any]]
    combined_confidence: float
    lollapalooza_potential: bool
    failure_analysis: List[Dict[str, Any]]
    two_track_analysis: Dict[str, Any]
    timestamp: str


class LispBridge:
    """Bridge between Python and Hy-based mental models DSL."""
    
    def __init__(self):
        self.hy_available = HY_AVAILABLE
        self.dsl_path = Path(__file__).parent / "mental_models_dsl.hy"
        self._hy_module = None
        self._model_registry: Dict[str, LispModel] = {}
        
        if self.hy_available and self.dsl_path.exists():
            self._load_hy_module()
        else:
            self._init_fallback_models()
    
    def _load_hy_module(self):
        """Load the Hy DSL module."""
        try:
            import hy.importer
            self._hy_module = hy.importer.runhy.run_path(str(self.dsl_path))
        except Exception as e:
            print(f"Warning: Could not load Hy module: {e}")
            self._init_fallback_models()
    
    def _init_fallback_models(self):
        """Initialize fallback models when Hy is not available."""
        self._model_registry = {
            "circle-of-competence": LispModel(
                name="circle-of-competence",
                category=LispCategory.DECISION_MAKING,
                originator="Warren Buffett",
                description="Know the boundaries of your knowledge and stay within them",
                rules=[
                    {"name": "stay-inside", "confidence": 0.9},
                    {"name": "avoid-outside", "confidence": 0.85}
                ],
                failure_modes=[
                    {"name": "overconfidence", "severity": LispSeverity.HIGH,
                     "description": "Believing your circle is larger than it is"},
                    {"name": "underconfidence", "severity": LispSeverity.MEDIUM,
                     "description": "Not acting within your actual competence"}
                ]
            ),
            "margin-of-safety": LispModel(
                name="margin-of-safety",
                category=LispCategory.DECISION_MAKING,
                originator="Benjamin Graham",
                description="Always leave room for error in your calculations",
                rules=[{"name": "require-margin", "confidence": 0.95}],
                failure_modes=[
                    {"name": "insufficient-margin", "severity": LispSeverity.CRITICAL,
                     "description": "Not leaving enough buffer for unexpected events"}
                ]
            ),
            "second-order-thinking": LispModel(
                name="second-order-thinking",
                category=LispCategory.SYSTEMS,
                originator="Howard Marks",
                description="Think about the consequences of the consequences",
                rules=[{"name": "ask-then-what", "confidence": 0.9}],
                failure_modes=[
                    {"name": "first-order-only", "severity": LispSeverity.HIGH,
                     "description": "Only considering immediate effects"}
                ]
            ),
            "inversion": LispModel(
                name="inversion",
                category=LispCategory.DECISION_MAKING,
                originator="Carl Jacobi / Charlie Munger",
                description="Invert, always invert - think about what to avoid",
                rules=[{"name": "invert-problem", "confidence": 0.95}],
                failure_modes=[
                    {"name": "forward-only-thinking", "severity": LispSeverity.MEDIUM,
                     "description": "Only thinking about how to succeed, not how to fail"}
                ]
            ),
            "incentives": LispModel(
                name="incentives",
                category=LispCategory.PSYCHOLOGY,
                originator="Charlie Munger",
                description="Never think about anything else when you should be thinking about incentives",
                rules=[{"name": "follow-incentives", "confidence": 0.95}],
                failure_modes=[
                    {"name": "ignoring-incentives", "severity": LispSeverity.CRITICAL,
                     "description": "Not considering what motivates behavior"}
                ]
            )
        }
    
    def get_all_models(self) -> List[Dict[str, Any]]:
        """Get all registered mental models."""
        if self._hy_module and hasattr(self._hy_module, 'get_all_models'):
            return self._hy_module.get_all_models()
        
        return [
            {
                "name": m.name,
                "category": m.category.value,
                "originator": m.originator,
                "description": m.description,
                "rules": m.rules,
                "failure_modes": [
                    {"name": fm["name"], "severity": fm["severity"].value, 
                     "description": fm["description"]}
                    for fm in m.failure_modes
                ]
            }
            for m in self._model_registry.values()
        ]
    
    def get_model_names(self) -> List[str]:
        """Get all registered model names."""
        if self._hy_module and hasattr(self._hy_module, 'get_model_names'):
            return self._hy_module.get_model_names()
        return list(self._model_registry.keys())
    
    def analyze_context(self, context: Dict[str, Any]) -> LispAnalysisResult:
        """Analyze a context using all registered mental models."""
        if self._hy_module and hasattr(self._hy_module, 'analyze_with_all_models'):
            result = self._hy_module.analyze_with_all_models(context)
            return LispAnalysisResult(
                models_applied=result.get("total_models", 0),
                individual_results=result.get("composition", {}).get("individual_results", []),
                combined_confidence=result.get("composition", {}).get("combined_confidence", 0.0),
                lollapalooza_potential=result.get("lollapalooza", {}).get("is_lollapalooza", False),
                failure_analysis=result.get("failure_analysis", []),
                two_track_analysis=result.get("two_track_analysis", {}),
                timestamp=result.get("timestamp", str(datetime.now()))
            )
        
        return self._fallback_analyze(context)
    
    def _fallback_analyze(self, context: Dict[str, Any]) -> LispAnalysisResult:
        """Fallback analysis when Hy is not available."""
        models = list(self._model_registry.values())
        individual_results = []
        
        for model in models:
            avg_confidence = sum(r.get("confidence", 0.5) for r in model.rules) / max(len(model.rules), 1)
            individual_results.append({
                "model": model.name,
                "applicable_rules": len(model.rules),
                "recommendations": [f"Apply {model.name}: {model.description}"],
                "confidence": avg_confidence
            })
        
        combined_confidence = sum(r["confidence"] for r in individual_results) / max(len(individual_results), 1)
        high_confidence_count = sum(1 for r in individual_results if r["confidence"] > 0.7)
        
        failure_analysis = []
        for model in models:
            failure_analysis.append({
                "model": model.name,
                "active_failures": [],
                "risk_level": 0
            })
        
        two_track = {
            "situation": context.get("situation", "Unknown"),
            "track_1_rational": {
                "economic_incentives": "Who benefits financially?",
                "opportunity_costs": "What are we giving up?",
                "second_order_effects": "What happens next?",
                "margin_of_safety": "What's our buffer for error?"
            },
            "track_2_psychological": {
                "social_proof": "Are we following the crowd?",
                "commitment_bias": "Are we doubling down on past decisions?",
                "incentive_bias": "Are incentives distorting our view?",
                "availability_bias": "Are we overweighting recent/vivid events?"
            }
        }
        
        return LispAnalysisResult(
            models_applied=len(models),
            individual_results=individual_results,
            combined_confidence=combined_confidence,
            lollapalooza_potential=high_confidence_count >= 3,
            failure_analysis=failure_analysis,
            two_track_analysis=two_track,
            timestamp=str(datetime.now())
        )
    
    def invert(self, problem: str) -> Dict[str, Any]:
        """Apply Munger's inversion technique to a problem."""
        if self._hy_module and hasattr(self._hy_module, 'invert'):
            return self._hy_module.invert(problem)
        
        return {
            "original_problem": problem,
            "inverted_questions": [
                f"What would guarantee failure in: {problem}?",
                f"What should I definitely NOT do regarding: {problem}?",
                f"What assumptions am I making about: {problem}?",
                f"Who has failed at: {problem} and why?"
            ],
            "approach": "Answer these questions first, then avoid those pitfalls"
        }
    
    def two_track_analysis(self, situation: str) -> Dict[str, Any]:
        """Perform Munger's two-track analysis (rational + psychological)."""
        if self._hy_module and hasattr(self._hy_module, 'two_track_analysis'):
            return self._hy_module.two_track_analysis(situation)
        
        return {
            "situation": situation,
            "track_1_rational": {
                "economic_incentives": "Who benefits financially?",
                "opportunity_costs": "What are we giving up?",
                "second_order_effects": "What happens next?",
                "margin_of_safety": "What's our buffer for error?"
            },
            "track_2_psychological": {
                "social_proof": "Are we following the crowd?",
                "commitment_bias": "Are we doubling down on past decisions?",
                "incentive_bias": "Are incentives distorting our view?",
                "availability_bias": "Are we overweighting recent/vivid events?",
                "confirmation_bias": "Are we seeking confirming evidence?",
                "loss_aversion": "Are we irrationally avoiding losses?"
            },
            "recommendation": "Analyze both tracks before deciding"
        }
    
    def detect_lollapalooza(self, context: Dict[str, Any]) -> Dict[str, Any]:
        """Detect lollapalooza effect (multiple models reinforcing each other)."""
        if self._hy_module and hasattr(self._hy_module, 'detect_lollapalooza'):
            models = self._hy_module.get_all_models() if hasattr(self._hy_module, 'get_all_models') else []
            return self._hy_module.detect_lollapalooza(models, context)
        
        result = self.analyze_context(context)
        high_confidence = [r for r in result.individual_results if r.get("confidence", 0) > 0.7]
        
        return {
            "is_lollapalooza": len(high_confidence) >= 3,
            "models_aligned": len(high_confidence),
            "aligned_recommendations": [r.get("recommendations", [])[0] for r in high_confidence if r.get("recommendations")],
            "strength": sum(r.get("confidence", 0) for r in high_confidence) / max(len(high_confidence), 1)
        }
    
    def to_json(self, obj: Any) -> str:
        """Convert analysis results to JSON."""
        if isinstance(obj, LispAnalysisResult):
            return json.dumps({
                "models_applied": obj.models_applied,
                "individual_results": obj.individual_results,
                "combined_confidence": obj.combined_confidence,
                "lollapalooza_potential": obj.lollapalooza_potential,
                "failure_analysis": obj.failure_analysis,
                "two_track_analysis": obj.two_track_analysis,
                "timestamp": obj.timestamp
            }, indent=2)
        return json.dumps(obj, indent=2, default=str)


def get_lisp_bridge() -> LispBridge:
    """Get a singleton instance of the Lisp bridge."""
    if not hasattr(get_lisp_bridge, "_instance"):
        get_lisp_bridge._instance = LispBridge()
    return get_lisp_bridge._instance


__all__ = [
    "LispBridge",
    "LispModel", 
    "LispCategory",
    "LispSeverity",
    "LispAnalysisResult",
    "get_lisp_bridge",
    "HY_AVAILABLE"
]
