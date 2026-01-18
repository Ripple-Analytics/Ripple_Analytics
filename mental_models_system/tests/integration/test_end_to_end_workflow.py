"""
End-to-End Integration Tests for Mental Models System

Tests complete workflows from signal detection through Lollapalooza analysis,
demonstrating the full power of the system in real-world scenarios.

"The big money is not in the buying and selling, but in the waiting."
- Charlie Munger
"""

import pytest
import json
import tempfile
from pathlib import Path
from datetime import datetime
from typing import List, Dict, Any


class TestEndToEndWorkflows:
    """Test complete workflows through the system."""
    
    def test_signal_to_lollapalooza_workflow(self):
        """
        Test the complete workflow:
        1. Detect mental model signals in text
        2. Analyze model interactions
        3. Identify Lollapalooza effects
        4. Generate safeguards
        """
        from src.analysis.model_analyzer import MentalModelAnalyzer
        from src.analysis.lollapalooza import LollapaloozaDetector
        from src.safeguards.failure_modes_loader import FailureModesLoader
        
        # Sample text with multiple interacting mental models
        text = """
        Amazon's dominance in e-commerce demonstrates a powerful Lollapalooza effect:
        
        1. Network Effects: More sellers attract more buyers, more buyers attract more sellers.
        2. Economies of Scale: Massive volume allows lower prices and faster delivery.
        3. Switching Costs: Prime membership and ecosystem lock-in make it hard to leave.
        4. Brand: Trust and reliability create a moat.
        5. Data Advantage: Customer data enables better recommendations and inventory.
        
        These models reinforce each other, creating an almost unassailable competitive position.
        """
        
        # Step 1: Detect signals
        analyzer = MentalModelAnalyzer()
        signals = analyzer.analyze_text(text)
        
        assert len(signals) >= 3, "Should detect multiple mental model signals"
        assert any("Network Effects" in s.model_name for s in signals), "Should detect Network Effects"
        assert any("Economies of Scale" in s.model_name for s in signals), "Should detect Economies of Scale"
        
        # Step 2: Detect Lollapalooza
        detector = LollapaloozaDetector()
        lollapalooza_effects = detector.detect_from_signals(signals)
        
        assert len(lollapalooza_effects) > 0, "Should detect Lollapalooza effect"
        effect = lollapalooza_effects[0]
        assert effect.strength >= 0.7, "Should be a strong Lollapalooza effect"
        assert len(effect.contributing_models) >= 3, "Should have multiple contributing models"
        
        # Step 3: Generate safeguards
        loader = FailureModesLoader()
        safeguards = []
        for model_name in effect.contributing_models:
            model_safeguards = loader.get_safeguards_for_model(model_name)
            safeguards.extend(model_safeguards)
        
        assert len(safeguards) > 0, "Should generate safeguards"
    
    def test_decision_tracking_workflow(self):
        """
        Test decision tracking workflow:
        1. Create decision with mental models
        2. Track implementation
        3. Record outcome
        4. Learn from results
        """
        from src.analysis.decision_tracker import DecisionTracker
        
        tracker = DecisionTracker()
        
        # Step 1: Create decision
        decision_id = tracker.create_decision(
            title="Expand to New Market",
            description="Should we enter the European market?",
            mental_models_used=["First-Mover Advantage", "Economies of Scale", "Regulatory Capture"],
            decision_made="Yes, enter within 6 months",
            rationale="Strong first-mover opportunity with favorable regulations",
            confidence=0.75
        )
        
        assert decision_id is not None, "Should create decision"
        
        # Step 2: Track implementation
        tracker.update_decision_status(decision_id, "in_progress")
        decision = tracker.get_decision(decision_id)
        assert decision["status"] == "in_progress"
        
        # Step 3: Record outcome
        tracker.record_outcome(
            decision_id,
            outcome="Successfully launched in 3 countries",
            success=True,
            lessons_learned="Regulatory research was critical; should have allocated more resources"
        )
        
        decision = tracker.get_decision(decision_id)
        assert decision["outcome"] is not None
        assert decision["success"] is True
    
    def test_regime_detection_workflow(self):
        """
        Test regime detection workflow:
        1. Analyze time series data
        2. Detect regime changes
        3. Identify applicable mental models per regime
        4. Generate regime-specific strategies
        """
        from src.analysis.hmm_regime import HMMRegimeDetector
        from src.analysis.model_analyzer import MentalModelLoader
        import numpy as np
        
        # Generate sample market data with regime changes
        np.random.seed(42)
        
        # Bull market regime (high returns, low volatility)
        bull_returns = np.random.normal(0.001, 0.01, 100)
        
        # Bear market regime (negative returns, high volatility)
        bear_returns = np.random.normal(-0.002, 0.03, 100)
        
        # Sideways regime (near-zero returns, medium volatility)
        sideways_returns = np.random.normal(0.0, 0.015, 100)
        
        # Combine into time series with regime changes
        returns = np.concatenate([bull_returns, bear_returns, sideways_returns])
        
        # Step 1: Detect regimes
        detector = HMMRegimeDetector(n_regimes=3)
        detector.fit(returns)
        
        current_regime = detector.predict_current_regime(returns[-20:])
        assert current_regime is not None, "Should detect current regime"
        
        # Step 2: Get regime characteristics
        regime_stats = detector.get_regime_statistics()
        assert len(regime_stats) == 3, "Should have 3 regimes"
        
        # Step 3: Identify applicable mental models
        loader = MentalModelLoader()
        
        # Different mental models apply in different regimes
        if current_regime.mean_return > 0.0005:
            # Bull market - momentum, trend following
            applicable_models = loader.search_models("momentum")
            assert len(applicable_models) > 0
        elif current_regime.mean_return < -0.0005:
            # Bear market - mean reversion, contrarian
            applicable_models = loader.search_models("reversion")
        else:
            # Sideways - range trading, volatility
            applicable_models = loader.search_models("volatility")
    
    def test_knowledge_graph_workflow(self):
        """
        Test knowledge graph workflow:
        1. Build graph from mental models
        2. Find related models
        3. Discover non-obvious connections
        4. Generate insights
        """
        from src.analysis.knowledge_graph import KnowledgeGraph
        from src.analysis.model_analyzer import MentalModelLoader
        
        # Step 1: Build graph
        loader = MentalModelLoader()
        graph = KnowledgeGraph()
        
        models = loader.get_all_models()
        for model in models[:50]:  # Use subset for speed
            graph.add_model(model)
        
        # Step 2: Find related models
        related = graph.find_related_models("Network Effects", max_distance=2)
        assert len(related) > 0, "Should find related models"
        
        # Step 3: Find shortest path between models
        path = graph.find_shortest_path("Network Effects", "Economies of Scale")
        if path:  # Path may not exist in subset
            assert len(path) >= 2, "Path should have at least 2 nodes"
        
        # Step 4: Get central models (most connected)
        central_models = graph.get_most_central_models(top_n=10)
        assert len(central_models) > 0, "Should identify central models"
    
    def test_failure_mode_detection_workflow(self):
        """
        Test failure mode detection workflow:
        1. Analyze decision/situation
        2. Identify mental models being used
        3. Check for failure modes
        4. Generate warnings and safeguards
        """
        from src.analysis.model_analyzer import MentalModelAnalyzer
        from src.safeguards.failure_modes_loader import FailureModesLoader
        from src.safeguards.safeguard_engine import SafeguardEngine
        
        # Situation with potential failure modes
        situation = """
        We're seeing incredible growth - 50% month-over-month for 6 months straight.
        Everyone wants our product. We should scale as fast as possible and capture
        the entire market before competitors arrive. The network effects will make us
        unbeatable. We need to hire 500 people this quarter and expand to 20 cities.
        """
        
        # Step 1: Detect mental models
        analyzer = MentalModelAnalyzer()
        signals = analyzer.analyze_text(situation)
        
        # Step 2: Load failure modes
        loader = FailureModesLoader()
        
        # Step 3: Check for failure modes
        safeguard_engine = SafeguardEngine()
        warnings = []
        
        for signal in signals:
            failure_modes = loader.get_failure_modes_for_model(signal.model_name)
            for failure_mode in failure_modes:
                # Check if failure mode applies
                if failure_mode.severity in ["high", "critical"]:
                    warnings.append({
                        "model": signal.model_name,
                        "failure_mode": failure_mode.name,
                        "description": failure_mode.description,
                        "detection_signals": failure_mode.detection_signals
                    })
        
        # Should detect warnings about overconfidence, scaling too fast, etc.
        assert len(warnings) > 0, "Should detect potential failure modes"
    
    def test_bayesian_update_workflow(self):
        """
        Test Bayesian updating workflow:
        1. Start with prior belief
        2. Observe evidence
        3. Update belief
        4. Make decision based on posterior
        """
        from src.analysis.bayesian_updater import BayesianUpdater
        
        updater = BayesianUpdater()
        
        # Hypothesis: New product will succeed
        prior_success_prob = 0.3  # 30% prior belief
        
        # Evidence 1: Positive customer feedback
        posterior1 = updater.update(
            prior=prior_success_prob,
            likelihood_if_true=0.8,  # 80% chance of positive feedback if product will succeed
            likelihood_if_false=0.2   # 20% chance of positive feedback if product will fail
        )
        
        assert posterior1 > prior_success_prob, "Positive evidence should increase belief"
        
        # Evidence 2: Strong pre-orders
        posterior2 = updater.update(
            prior=posterior1,
            likelihood_if_true=0.9,
            likelihood_if_false=0.1
        )
        
        assert posterior2 > posterior1, "More positive evidence should further increase belief"
        
        # Evidence 3: Competitor launches similar product
        posterior3 = updater.update(
            prior=posterior2,
            likelihood_if_true=0.4,  # Competitor launch is somewhat negative
            likelihood_if_false=0.6
        )
        
        assert posterior3 < posterior2, "Negative evidence should decrease belief"
    
    def test_api_integration_workflow(self):
        """
        Test API integration workflow:
        1. Submit analysis request via API
        2. Process asynchronously
        3. Retrieve results
        4. Export in multiple formats
        """
        from src.api.main import app
        from fastapi.testclient import TestClient
        
        client = TestClient(app)
        
        # Step 1: Submit analysis request
        response = client.post("/api/v1/analyze", json={
            "text": "Tesla demonstrates network effects and economies of scale.",
            "include_lollapalooza": True,
            "include_safeguards": True
        })
        
        assert response.status_code == 200
        result = response.json()
        
        # Step 2: Verify results
        assert "signals" in result
        assert "lollapalooza_effects" in result
        assert len(result["signals"]) > 0
        
        # Step 3: Export results
        export_response = client.post("/api/v1/export", json={
            "analysis_id": result.get("id", "test"),
            "format": "json"
        })
        
        assert export_response.status_code == 200


class TestErrorHandlingWorkflows:
    """Test error handling in workflows."""
    
    def test_invalid_input_handling(self):
        """Test that invalid inputs are handled gracefully."""
        from src.analysis.model_analyzer import MentalModelAnalyzer
        
        analyzer = MentalModelAnalyzer()
        
        # Empty text
        signals = analyzer.analyze_text("")
        assert signals == [], "Empty text should return empty list"
        
        # Very short text
        signals = analyzer.analyze_text("Hi")
        assert isinstance(signals, list), "Should return list even for short text"
    
    def test_missing_data_handling(self):
        """Test handling of missing data."""
        from src.analysis.decision_tracker import DecisionTracker
        
        tracker = DecisionTracker()
        
        # Try to get non-existent decision
        decision = tracker.get_decision("nonexistent_id")
        assert decision is None, "Should return None for non-existent decision"
    
    def test_concurrent_access_handling(self):
        """Test handling of concurrent access."""
        from src.analysis.decision_tracker import DecisionTracker
        import threading
        
        tracker = DecisionTracker()
        results = []
        
        def create_decision(i):
            decision_id = tracker.create_decision(
                title=f"Decision {i}",
                description=f"Test decision {i}",
                mental_models_used=["Test Model"],
                decision_made="Test",
                rationale="Test",
                confidence=0.5
            )
            results.append(decision_id)
        
        # Create multiple decisions concurrently
        threads = [threading.Thread(target=create_decision, args=(i,)) for i in range(10)]
        for t in threads:
            t.start()
        for t in threads:
            t.join()
        
        # All decisions should be created successfully
        assert len(results) == 10, "All concurrent operations should succeed"
        assert len(set(results)) == 10, "All decision IDs should be unique"


class TestPerformanceWorkflows:
    """Test performance of workflows."""
    
    def test_large_text_analysis_performance(self):
        """Test that large text analysis completes in reasonable time."""
        from src.analysis.model_analyzer import MentalModelAnalyzer
        import time
        
        analyzer = MentalModelAnalyzer()
        
        # Generate large text (10,000 words)
        large_text = " ".join(["This is a test of network effects and economies of scale."] * 1000)
        
        start_time = time.time()
        signals = analyzer.analyze_text(large_text)
        elapsed_time = time.time() - start_time
        
        assert elapsed_time < 10.0, f"Large text analysis should complete in < 10s, took {elapsed_time:.2f}s"
        assert len(signals) > 0, "Should detect signals in large text"
    
    def test_batch_analysis_performance(self):
        """Test batch analysis performance."""
        from src.analysis.model_analyzer import MentalModelAnalyzer
        import time
        
        analyzer = MentalModelAnalyzer()
        
        texts = [
            "Network effects create moats.",
            "Economies of scale reduce costs.",
            "Switching costs lock in customers.",
            "Brand creates trust and loyalty.",
            "First-mover advantage is powerful."
        ] * 20  # 100 texts
        
        start_time = time.time()
        results = [analyzer.analyze_text(text) for text in texts]
        elapsed_time = time.time() - start_time
        
        assert elapsed_time < 30.0, f"Batch analysis should complete in < 30s, took {elapsed_time:.2f}s"
        assert len(results) == 100, "Should analyze all texts"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
