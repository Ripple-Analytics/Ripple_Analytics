import json
from pathlib import Path
from typing import List, Dict, Optional, Any
from .detector import FailureMode, FailureSeverity, FailureCategory


class FailureModeRegistry:
    _instance = None
    _failure_modes: Dict[str, List[FailureMode]] = {}
    _loaded = False

    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
        return cls._instance

    def __init__(self):
        if not self._loaded:
            self._load_failure_modes()
            self._loaded = True

    def _load_failure_modes(self) -> None:
        json_path = Path(__file__).parent.parent.parent / "data" / "raw" / "failure_modes_complete.json"
        
        if not json_path.exists():
            return
        
        try:
            with open(json_path, "r") as f:
                data = json.load(f)
            
            for model_data in data.get("failure_modes", []):
                model_id = model_data["model_id"]
                model_name = model_data["model_name"]
                
                failure_modes = []
                for fm_data in model_data.get("failures", []):
                    try:
                        category = FailureCategory(fm_data["category"])
                    except ValueError:
                        category = FailureCategory.REASONING_ERROR
                    
                    try:
                        severity = FailureSeverity(fm_data["severity"])
                    except ValueError:
                        severity = FailureSeverity.MEDIUM
                    
                    fm = FailureMode(
                        id=fm_data["id"],
                        model_id=model_id,
                        model_name=model_name,
                        name=fm_data["name"],
                        description=fm_data["description"],
                        category=category,
                        severity=severity,
                        detection_signals=fm_data.get("detection_signals", []),
                        prevention_strategies=fm_data.get("prevention_strategies", []),
                        example_scenario=fm_data.get("example_scenario", ""),
                    )
                    failure_modes.append(fm)
                
                self._failure_modes[str(model_id)] = failure_modes
        except Exception as e:
            print(f"Warning: Failed to load failure modes: {e}")

    def get_failure_modes(self, model_id: int) -> List[FailureMode]:
        return self._failure_modes.get(str(model_id), [])

    def get_all_failure_modes(self) -> Dict[str, List[FailureMode]]:
        return self._failure_modes

    def get_failure_mode_by_id(self, failure_id: str) -> Optional[FailureMode]:
        for model_failures in self._failure_modes.values():
            for fm in model_failures:
                if fm.id == failure_id:
                    return fm
        return None

    def get_failure_modes_by_category(self, category: FailureCategory) -> List[FailureMode]:
        result = []
        for model_failures in self._failure_modes.values():
            for fm in model_failures:
                if fm.category == category:
                    result.append(fm)
        return result

    def get_failure_modes_by_severity(self, severity: FailureSeverity) -> List[FailureMode]:
        result = []
        for model_failures in self._failure_modes.values():
            for fm in model_failures:
                if fm.severity == severity:
                    result.append(fm)
        return result

    def get_critical_failure_modes(self) -> List[FailureMode]:
        return self.get_failure_modes_by_severity(FailureSeverity.CRITICAL)

    def get_stats(self) -> Dict[str, Any]:
        total_modes = sum(len(fms) for fms in self._failure_modes.values())
        by_category = {}
        by_severity = {}
        
        for model_failures in self._failure_modes.values():
            for fm in model_failures:
                cat = fm.category.value
                sev = fm.severity.value
                by_category[cat] = by_category.get(cat, 0) + 1
                by_severity[sev] = by_severity.get(sev, 0) + 1
        
        return {
            "total_models_with_failure_modes": len(self._failure_modes),
            "total_failure_modes": total_modes,
            "by_category": by_category,
            "by_severity": by_severity,
        }


def get_failure_modes_for_model(model_id: int) -> List[FailureMode]:
    registry = FailureModeRegistry()
    return registry.get_failure_modes(model_id)
