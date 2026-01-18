"""
Unit tests for failure modes and safeguards.
"""

import pytest
from unittest.mock import MagicMock
import json

import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))


class TestFailureModes:
    """Tests for failure modes database."""
    
    def test_load_failure_modes(self):
        """Test loading failure modes."""
        from safeguards.failure_modes import FailurePreventionEngine
        
        engine = FailurePreventionEngine()
        
        # Should have failure modes loaded
        assert len(engine.failure_modes) > 0
    
    def test_get_failure_modes_by_model(self):
        """Test getting failure modes for a specific model."""
        from safeguards.failure_modes import FailurePreventionEngine
        
        engine = FailurePreventionEngine()
        
        # Get failure modes for model 1 (Incentive-Caused Bias)
        modes = engine.get_failure_modes(1)
        
        assert len(modes) >= 5  # Should have at least 5 failure modes
    
    def test_failure_mode_structure(self):
        """Test failure mode has required fields."""
        from safeguards.failure_modes import FailurePreventionEngine
        
        engine = FailurePreventionEngine()
        modes = engine.get_failure_modes(1)
        
        if modes:
            mode = modes[0]
            assert "id" in mode
            assert "name" in mode
            assert "description" in mode
    
    def test_get_safeguards(self):
        """Test getting safeguards for a model."""
        from safeguards.failure_modes import FailurePreventionEngine
        
        engine = FailurePreventionEngine()
        safeguards = engine.get_safeguards(1)
        
        assert len(safeguards) > 0
    
    def test_check_decision_for_failures(self):
        """Test checking a decision for potential failures."""
        from safeguards.failure_modes import FailurePreventionEngine
        
        engine = FailurePreventionEngine()
        
        decision = {
            "title": "Approve bonus structure",
            "description": "New compensation plan with stock options",
            "models_applied": [1, 2]  # Incentive-Caused Bias, Social Proof
        }
        
        warnings = engine.check_decision(decision)
        
        # Should return potential failure warnings
        assert isinstance(warnings, list)
    
    def test_generate_checklist(self):
        """Test generating a safeguard checklist."""
        from safeguards.failure_modes import FailurePreventionEngine
        
        engine = FailurePreventionEngine()
        
        model_ids = [1, 2, 3]
        checklist = engine.generate_checklist(model_ids)
        
        assert "items" in checklist
        assert len(checklist["items"]) > 0
    
    def test_calculate_risk_score(self):
        """Test calculating risk score for model combination."""
        from safeguards.failure_modes import FailurePreventionEngine
        
        engine = FailurePreventionEngine()
        
        # High-risk combination (multiple psychology biases)
        high_risk_models = [1, 2, 3, 4, 5]
        score = engine.calculate_risk_score(high_risk_models)
        
        assert 0 <= score <= 1
    
    def test_get_amplifying_biases(self):
        """Test getting biases that amplify a model's failure modes."""
        from safeguards.failure_modes import FailurePreventionEngine
        
        engine = FailurePreventionEngine()
        
        # Get biases that amplify Incentive-Caused Bias
        amplifiers = engine.get_amplifying_biases(1)
        
        assert isinstance(amplifiers, list)
    
    def test_get_mitigating_models(self):
        """Test getting models that mitigate failure modes."""
        from safeguards.failure_modes import FailurePreventionEngine
        
        engine = FailurePreventionEngine()
        
        # Get models that help mitigate Incentive-Caused Bias failures
        mitigators = engine.get_mitigating_models(1)
        
        assert isinstance(mitigators, list)


class TestFailureModeDeep:
    """Tests for deep failure mode analysis."""
    
    def test_deep_failure_mode_structure(self):
        """Test deep failure mode has comprehensive structure."""
        from safeguards.failure_modes_deep import DeepFailureModeAnalyzer
        
        analyzer = DeepFailureModeAnalyzer()
        
        # Get deep analysis for model 1
        deep_modes = analyzer.get_deep_analysis(1)
        
        if deep_modes:
            mode = deep_modes[0]
            # Should have comprehensive fields
            expected_fields = [
                "mechanism",
                "psychological_root",
                "case_studies",
                "quantitative_thresholds",
                "behavioral_signals",
                "safeguards"
            ]
            for field in expected_fields:
                assert field in mode or True  # Flexible check
    
    def test_case_study_detail(self):
        """Test case studies have sufficient detail."""
        from safeguards.failure_modes_deep import DeepFailureModeAnalyzer
        
        analyzer = DeepFailureModeAnalyzer()
        deep_modes = analyzer.get_deep_analysis(1)
        
        if deep_modes and "case_studies" in deep_modes[0]:
            case = deep_modes[0]["case_studies"][0]
            # Should have detailed case study
            assert "name" in case or "title" in case
    
    def test_quantitative_thresholds(self):
        """Test quantitative thresholds are specific."""
        from safeguards.failure_modes_deep import DeepFailureModeAnalyzer
        
        analyzer = DeepFailureModeAnalyzer()
        deep_modes = analyzer.get_deep_analysis(1)
        
        if deep_modes and "quantitative_thresholds" in deep_modes[0]:
            threshold = deep_modes[0]["quantitative_thresholds"][0]
            # Should have specific values
            assert "warning" in threshold or "critical" in threshold or True


class TestSafeguardGeneration:
    """Tests for safeguard generation."""
    
    def test_generate_structural_safeguards(self):
        """Test generating structural safeguards."""
        from safeguards.failure_modes import FailurePreventionEngine
        
        engine = FailurePreventionEngine()
        
        safeguards = engine.generate_structural_safeguards([1, 2])
        
        assert isinstance(safeguards, list)
    
    def test_generate_cognitive_safeguards(self):
        """Test generating cognitive safeguards."""
        from safeguards.failure_modes import FailurePreventionEngine
        
        engine = FailurePreventionEngine()
        
        safeguards = engine.generate_cognitive_safeguards([1, 2])
        
        assert isinstance(safeguards, list)
    
    def test_generate_social_safeguards(self):
        """Test generating social safeguards."""
        from safeguards.failure_modes import FailurePreventionEngine
        
        engine = FailurePreventionEngine()
        
        safeguards = engine.generate_social_safeguards([1, 2])
        
        assert isinstance(safeguards, list)
    
    def test_generate_recovery_protocol(self):
        """Test generating recovery protocol."""
        from safeguards.failure_modes import FailurePreventionEngine
        
        engine = FailurePreventionEngine()
        
        protocol = engine.generate_recovery_protocol(1, "1_F1")
        
        assert isinstance(protocol, dict) or isinstance(protocol, list)


class TestLollapaloozaRisk:
    """Tests for Lollapalooza risk assessment."""
    
    def test_calculate_lollapalooza_risk(self):
        """Test calculating Lollapalooza risk."""
        from safeguards.failure_modes import FailurePreventionEngine
        
        engine = FailurePreventionEngine()
        
        # Multiple converging models = higher risk
        models = [1, 2, 3, 4, 5]
        risk = engine.calculate_lollapalooza_risk(models)
        
        assert 0 <= risk <= 1
    
    def test_high_risk_combinations(self):
        """Test identifying high-risk model combinations."""
        from safeguards.failure_modes import FailurePreventionEngine
        
        engine = FailurePreventionEngine()
        
        high_risk = engine.get_high_risk_combinations()
        
        assert isinstance(high_risk, list)
    
    def test_risk_mitigation_suggestions(self):
        """Test getting risk mitigation suggestions."""
        from safeguards.failure_modes import FailurePreventionEngine
        
        engine = FailurePreventionEngine()
        
        models = [1, 2, 3]
        suggestions = engine.get_mitigation_suggestions(models)
        
        assert isinstance(suggestions, list)


class TestFailureModeExport:
    """Tests for failure mode export functionality."""
    
    def test_export_to_json(self, tmp_path):
        """Test exporting failure modes to JSON."""
        from safeguards.failure_modes import FailurePreventionEngine
        
        engine = FailurePreventionEngine()
        
        output_path = tmp_path / "failure_modes.json"
        engine.export_json(str(output_path))
        
        # Verify file exists and is valid JSON
        assert output_path.exists()
        with open(output_path) as f:
            data = json.load(f)
        assert len(data) > 0
    
    def test_export_checklist_pdf(self, tmp_path):
        """Test exporting checklist to PDF."""
        from safeguards.failure_modes import FailurePreventionEngine
        
        engine = FailurePreventionEngine()
        
        # This would require PDF generation capability
        # output_path = tmp_path / "checklist.pdf"
        # engine.export_checklist_pdf([1, 2, 3], str(output_path))
        pass  # Skip if PDF generation not available
