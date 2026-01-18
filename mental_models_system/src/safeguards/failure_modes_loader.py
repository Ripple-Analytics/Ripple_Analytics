"""
Failure Modes Loader - Loads and provides access to deep failure mode analysis.

Integrates with:
- Mental model analyzer
- Decision journal
- Risk assessment
- Lollapalooza detection
"""

import json
from pathlib import Path
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, field
from enum import Enum


class SafeguardType(Enum):
    STRUCTURAL = "structural"
    COGNITIVE = "cognitive"
    SOCIAL = "social"
    PROCEDURAL = "procedural"


@dataclass
class CaseStudy:
    """Real-world case study of a failure mode."""
    name: str
    what_happened: str
    incentive_structure: Optional[str] = None
    failure_point: str = ""
    outcome: str = ""
    lesson: str = ""
    
    @classmethod
    def from_dict(cls, data: dict) -> "CaseStudy":
        return cls(
            name=data.get("name", ""),
            what_happened=data.get("what_happened", ""),
            incentive_structure=data.get("incentive_structure"),
            failure_point=data.get("failure_point", ""),
            outcome=data.get("outcome", ""),
            lesson=data.get("lesson", "")
        )


@dataclass
class Safeguard:
    """A safeguard against a failure mode."""
    type: SafeguardType
    action: str
    implementation: str
    
    @classmethod
    def from_dict(cls, data: dict) -> "Safeguard":
        return cls(
            type=SafeguardType(data.get("type", "cognitive")),
            action=data.get("action", ""),
            implementation=data.get("implementation", "")
        )


@dataclass
class QuantitativeThresholds:
    """Quantitative warning signs and critical thresholds."""
    warning_signs: List[str] = field(default_factory=list)
    critical_thresholds: List[str] = field(default_factory=list)
    
    @classmethod
    def from_dict(cls, data: dict) -> "QuantitativeThresholds":
        return cls(
            warning_signs=data.get("warning_signs", []),
            critical_thresholds=data.get("critical_thresholds", [])
        )


@dataclass
class FailureMode:
    """A detailed failure mode for a mental model."""
    id: str
    name: str
    description: str
    mechanism: str = ""
    psychological_root: str = ""
    case_studies: List[CaseStudy] = field(default_factory=list)
    quantitative_thresholds: Optional[QuantitativeThresholds] = None
    behavioral_signals: List[str] = field(default_factory=list)
    safeguards: List[Safeguard] = field(default_factory=list)
    
    @classmethod
    def from_dict(cls, data: dict) -> "FailureMode":
        case_studies = [
            CaseStudy.from_dict(cs) 
            for cs in data.get("real_world_cases", [])
        ]
        
        safeguards = [
            Safeguard.from_dict(sg)
            for sg in data.get("safeguards", [])
        ]
        
        thresholds = None
        if "quantitative_thresholds" in data:
            thresholds = QuantitativeThresholds.from_dict(
                data["quantitative_thresholds"]
            )
        
        return cls(
            id=data.get("id", ""),
            name=data.get("name", ""),
            description=data.get("description", ""),
            mechanism=data.get("mechanism", ""),
            psychological_root=data.get("psychological_root", ""),
            case_studies=case_studies,
            quantitative_thresholds=thresholds,
            behavioral_signals=data.get("behavioral_signals", []),
            safeguards=safeguards
        )


@dataclass
class ModelFailureModes:
    """All failure modes for a single mental model."""
    model_id: int
    model_name: str
    category: str
    failure_modes: List[FailureMode]
    
    @classmethod
    def from_dict(cls, model_id: str, data: dict) -> "ModelFailureModes":
        failure_modes = [
            FailureMode.from_dict(fm)
            for fm in data.get("failure_modes", [])
        ]
        
        return cls(
            model_id=int(model_id),
            model_name=data.get("model_name", ""),
            category=data.get("category", ""),
            failure_modes=failure_modes
        )


class FailureModesLoader:
    """
    Loads and provides access to deep failure mode analysis.
    
    Usage:
        loader = FailureModesLoader()
        
        # Get failure modes for a specific model
        modes = loader.get_failure_modes(1)  # Incentive-Caused Bias
        
        # Get all case studies
        cases = loader.get_all_case_studies()
        
        # Get safeguards for a decision
        safeguards = loader.get_safeguards_for_models([1, 2, 3])
        
        # Check for warning signs
        warnings = loader.check_warning_signs(decision_context)
    """
    
    def __init__(self, data_path: Optional[Path] = None):
        if data_path is None:
            data_path = Path(__file__).parent.parent.parent / "data" / "raw"
        
        self.data_dir = data_path if data_path.is_dir() else data_path.parent
        self.data_path = data_path
        self._data: Dict[str, ModelFailureModes] = {}
        self._simple_data: Dict[str, List[Dict]] = {}  # For simple format files
        self._load_all_data()
    
    def _load_all_data(self):
        """Load failure modes from all JSON files."""
        # Load from deep format file
        deep_file = self.data_dir / "failure_modes_deep.json"
        if deep_file.exists():
            self._load_data(deep_file)
        
        # Load from simple format files
        simple_files = [
            'failure_modes_economics.json',
            'failure_modes_economics_2.json',
            'failure_modes_moats.json',
            'failure_modes_thinking.json',
            'failure_modes_thinking_2.json',
            'failure_modes_thinking_3.json',
            'failure_modes_psychology.json',
            'failure_modes_psychology_2.json',
            'failure_modes_psychology_3.json',
            'failure_modes_math.json',
            'failure_modes_physics.json',
            'failure_modes_biology.json',
            'failure_modes_math_physics_bio.json',
            'failure_modes_organizational.json',
            'failure_modes_economics_3.json',
            'failure_modes_moats_2.json',
            'failure_modes_final.json'
        ]
        
        for filename in simple_files:
            filepath = self.data_dir / filename
            if filepath.exists():
                self._load_simple_format(filepath)
    
    def _load_data(self, filepath: Path = None):
        """Load failure modes from deep format JSON file."""
        if filepath is None:
            filepath = self.data_path
        
        if not filepath.exists():
            return
        
        with open(filepath) as f:
            raw_data = json.load(f)
        
        failure_modes_data = raw_data.get("failure_modes", {})
        
        for model_id, model_data in failure_modes_data.items():
            self._data[model_id] = ModelFailureModes.from_dict(model_id, model_data)
    
    def _load_simple_format(self, filepath: Path):
        """Load failure modes from simple format JSON files."""
        try:
            with open(filepath) as f:
                data = json.load(f)
            
            if 'models' in data:
                for model in data['models']:
                    model_name = model.get('name', '')
                    model_id = str(model.get('id', ''))
                    if model_name and 'failure_modes' in model:
                        self._simple_data[model_name] = model['failure_modes']
                        # Also add by ID for lookup
                        if model_id:
                            self._simple_data[model_id] = model['failure_modes']
        except Exception as e:
            print(f"Warning: Could not load {filepath}: {e}")
    
    def get_failure_modes(self, model_id: int) -> Optional[ModelFailureModes]:
        """Get all failure modes for a specific mental model."""
        return self._data.get(str(model_id))
    
    def get_failure_mode_by_id(self, failure_mode_id: str) -> Optional[FailureMode]:
        """Get a specific failure mode by its ID (e.g., '1_F1')."""
        model_id = failure_mode_id.split("_")[0]
        model_modes = self._data.get(model_id)
        
        if model_modes:
            for fm in model_modes.failure_modes:
                if fm.id == failure_mode_id:
                    return fm
        
        return None
    
    def get_all_case_studies(self) -> List[Dict[str, Any]]:
        """Get all case studies across all failure modes."""
        case_studies = []
        
        for model_modes in self._data.values():
            for fm in model_modes.failure_modes:
                for cs in fm.case_studies:
                    case_studies.append({
                        "model_id": model_modes.model_id,
                        "model_name": model_modes.model_name,
                        "failure_mode_id": fm.id,
                        "failure_mode_name": fm.name,
                        "case_study": cs
                    })
        
        return case_studies
    
    def get_safeguards_for_models(self, model_ids: List[int]) -> List[Dict[str, Any]]:
        """Get all safeguards for a list of mental models."""
        safeguards = []
        
        for model_id in model_ids:
            model_modes = self._data.get(str(model_id))
            if model_modes:
                for fm in model_modes.failure_modes:
                    for sg in fm.safeguards:
                        safeguards.append({
                            "model_id": model_id,
                            "model_name": model_modes.model_name,
                            "failure_mode": fm.name,
                            "safeguard": sg
                        })
        
        return safeguards
    
    def get_warning_signs_for_models(self, model_ids: List[int]) -> Dict[str, List[str]]:
        """Get all warning signs for a list of mental models."""
        warning_signs = {
            "warning": [],
            "critical": []
        }
        
        for model_id in model_ids:
            model_modes = self._data.get(str(model_id))
            if model_modes:
                for fm in model_modes.failure_modes:
                    if fm.quantitative_thresholds:
                        warning_signs["warning"].extend(
                            fm.quantitative_thresholds.warning_signs
                        )
                        warning_signs["critical"].extend(
                            fm.quantitative_thresholds.critical_thresholds
                        )
        
        return warning_signs
    
    def check_decision_context(
        self, 
        context: Dict[str, Any],
        model_ids: List[int]
    ) -> Dict[str, Any]:
        """
        Check a decision context against failure modes.
        
        Args:
            context: Dictionary describing the decision context
            model_ids: List of mental model IDs being applied
            
        Returns:
            Dictionary with warnings, risks, and recommended safeguards
        """
        result = {
            "risk_level": "low",
            "warnings": [],
            "applicable_failure_modes": [],
            "recommended_safeguards": [],
            "behavioral_signals_to_watch": []
        }
        
        for model_id in model_ids:
            model_modes = self._data.get(str(model_id))
            if not model_modes:
                continue
            
            for fm in model_modes.failure_modes:
                # Check if failure mode is applicable
                relevance = self._assess_failure_mode_relevance(context, fm)
                
                if relevance > 0.5:
                    result["applicable_failure_modes"].append({
                        "id": fm.id,
                        "name": fm.name,
                        "description": fm.description,
                        "relevance": relevance
                    })
                    
                    # Add safeguards
                    for sg in fm.safeguards:
                        result["recommended_safeguards"].append({
                            "type": sg.type.value,
                            "action": sg.action,
                            "implementation": sg.implementation
                        })
                    
                    # Add behavioral signals
                    result["behavioral_signals_to_watch"].extend(
                        fm.behavioral_signals
                    )
        
        # Determine risk level
        num_applicable = len(result["applicable_failure_modes"])
        if num_applicable >= 5:
            result["risk_level"] = "critical"
        elif num_applicable >= 3:
            result["risk_level"] = "high"
        elif num_applicable >= 1:
            result["risk_level"] = "medium"
        
        # Deduplicate
        result["recommended_safeguards"] = self._deduplicate_safeguards(
            result["recommended_safeguards"]
        )
        result["behavioral_signals_to_watch"] = list(set(
            result["behavioral_signals_to_watch"]
        ))
        
        return result
    
    def _assess_failure_mode_relevance(
        self, 
        context: Dict[str, Any], 
        failure_mode: FailureMode
    ) -> float:
        """Assess how relevant a failure mode is to a given context."""
        relevance = 0.0
        
        # Check for keyword matches in context
        context_text = json.dumps(context).lower()
        
        keywords = [
            failure_mode.name.lower(),
            failure_mode.description.lower(),
            failure_mode.mechanism.lower()
        ]
        
        for keyword in keywords:
            if keyword and any(word in context_text for word in keyword.split()):
                relevance += 0.3
        
        # Check behavioral signals
        for signal in failure_mode.behavioral_signals:
            if signal.lower() in context_text:
                relevance += 0.2
        
        return min(relevance, 1.0)
    
    def _deduplicate_safeguards(
        self, 
        safeguards: List[Dict[str, Any]]
    ) -> List[Dict[str, Any]]:
        """Remove duplicate safeguards."""
        seen = set()
        unique = []
        
        for sg in safeguards:
            key = (sg["type"], sg["action"])
            if key not in seen:
                seen.add(key)
                unique.append(sg)
        
        return unique
    
    def generate_pre_mortem(self, model_ids: List[int]) -> str:
        """
        Generate a pre-mortem analysis for a decision using given models.
        
        Returns a formatted string with potential failure scenarios.
        """
        lines = ["# Pre-Mortem Analysis", ""]
        lines.append("## Potential Failure Scenarios")
        lines.append("")
        
        for model_id in model_ids:
            model_modes = self._data.get(str(model_id))
            if not model_modes:
                continue
            
            lines.append(f"### {model_modes.model_name}")
            lines.append("")
            
            for fm in model_modes.failure_modes:
                lines.append(f"**{fm.name}**: {fm.description}")
                
                if fm.case_studies:
                    cs = fm.case_studies[0]
                    lines.append(f"- Historical example: {cs.name}")
                    lines.append(f"- Lesson: {cs.lesson}")
                
                lines.append("")
        
        lines.append("## Recommended Safeguards")
        lines.append("")
        
        safeguards = self.get_safeguards_for_models(model_ids)
        for sg in safeguards[:10]:  # Top 10
            lines.append(f"- [{sg['safeguard'].type.value.upper()}] {sg['safeguard'].action}")
        
        return "\n".join(lines)
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get statistics about loaded failure modes."""
        total_modes = 0
        total_case_studies = 0
        total_safeguards = 0
        categories = {}
        
        for model_modes in self._data.values():
            total_modes += len(model_modes.failure_modes)
            
            cat = model_modes.category
            categories[cat] = categories.get(cat, 0) + 1
            
            for fm in model_modes.failure_modes:
                total_case_studies += len(fm.case_studies)
                total_safeguards += len(fm.safeguards)
        
        return {
            "models_with_failure_modes": len(self._data),
            "total_failure_modes": total_modes,
            "total_case_studies": total_case_studies,
            "total_safeguards": total_safeguards,
            "categories": categories
        }
    
    def get_safeguards_for_model(self, model_name: str) -> List[Dict[str, Any]]:
        """
        Get all safeguards for a specific mental model by name.
        
        Args:
            model_name: Name of the mental model
            
        Returns:
            List of safeguards for the model
        """
        safeguards = []
        
        # Check simple data format first
        if model_name in self._simple_data:
            for fm in self._simple_data[model_name]:
                if 'safeguards' in fm:
                    for sg in fm['safeguards']:
                        safeguards.append({
                            "model_name": model_name,
                            "failure_mode": fm.get('name', ''),
                            "safeguard": sg
                        })
        
        # Check deep data format
        for model_modes in self._data.values():
            if model_modes.model_name == model_name:
                for fm in model_modes.failure_modes:
                    for sg in fm.safeguards:
                        safeguards.append({
                            "model_id": model_modes.model_id,
                            "model_name": model_modes.model_name,
                            "failure_mode": fm.name,
                            "safeguard": sg
                        })
        
        return safeguards


# Convenience function
def load_failure_modes(data_path: Optional[Path] = None) -> FailureModesLoader:
    """Load failure modes from the default or specified path."""
    return FailureModesLoader(data_path)


if __name__ == "__main__":
    # Test the loader
    loader = FailureModesLoader()
    
    print("Failure Modes Statistics:")
    stats = loader.get_statistics()
    for key, value in stats.items():
        print(f"  {key}: {value}")
    
    print("\nCase Studies:")
    for cs in loader.get_all_case_studies()[:5]:
        print(f"  - {cs['case_study'].name} ({cs['model_name']})")
    
    print("\nPre-mortem for models 1, 2, 3:")
    print(loader.generate_pre_mortem([1, 2, 3])[:500] + "...")
