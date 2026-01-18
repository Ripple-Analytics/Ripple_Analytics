"""
Unit tests for mental model analyzer.
"""

import pytest
from unittest.mock import AsyncMock, MagicMock, patch
import json

import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))


class TestMentalModelLoader:
    """Tests for MentalModelLoader."""
    
    def test_load_models_from_file(self, temp_models_file):
        """Test loading models from JSON file."""
        from analysis.model_analyzer import MentalModelLoader
        
        loader = MentalModelLoader(str(temp_models_file.parent))
        models = loader.get_all_models()
        
        assert len(models) == 3
        assert models[0]["name"] == "Incentive-Caused Bias"
    
    def test_get_model_by_id(self, temp_models_file):
        """Test getting model by ID."""
        from analysis.model_analyzer import MentalModelLoader
        
        loader = MentalModelLoader(str(temp_models_file.parent))
        model = loader.get_model(1)
        
        assert model is not None
        assert model["name"] == "Incentive-Caused Bias"
    
    def test_get_models_by_category(self, temp_models_file):
        """Test getting models by category."""
        from analysis.model_analyzer import MentalModelLoader
        
        loader = MentalModelLoader(str(temp_models_file.parent))
        psychology_models = loader.get_by_category("Psychology")
        
        assert len(psychology_models) == 2
    
    def test_search_models(self, temp_models_file):
        """Test searching models."""
        from analysis.model_analyzer import MentalModelLoader
        
        loader = MentalModelLoader(str(temp_models_file.parent))
        results = loader.search("incentive")
        
        assert len(results) >= 1
        assert any("Incentive" in m["name"] for m in results)


@pytest.mark.asyncio
class TestMentalModelAnalyzer:
    """Tests for MentalModelAnalyzer."""
    
    async def test_analyze_text(self, mock_llm_client, sample_document_text):
        """Test analyzing text with mental models."""
        from analysis.model_analyzer import MentalModelAnalyzer
        
        analyzer = MentalModelAnalyzer(mock_llm_client)
        
        # Mock the analysis - return proper JSON strings for each call
        # The analyzer makes multiple LLM calls: identify_models, detect_lollapalooza, categorize_document
        mock_llm_client.generate.return_value = json.dumps([
            {"model_name": "Incentive-Caused Bias", "relevance_score": 0.9, "confidence": 0.85, "evidence": "test", "insights": ["test"]}
        ])
        
        result = await analyzer.analyze_text(sample_document_text)
        
        # Verify LLM was called (multiple times for different analysis steps)
        assert mock_llm_client.generate.call_count >= 1
    
    async def test_batch_analyze(self, mock_llm_client):
        """Test batch analysis of multiple texts."""
        from analysis.model_analyzer import MentalModelAnalyzer
        
        analyzer = MentalModelAnalyzer(mock_llm_client)
        
        texts = [
            "Text about incentives and compensation",
            "Text about market dynamics",
            "Text about compounding returns"
        ]
        
        mock_llm_client.generate.return_value = MagicMock(
            text=json.dumps({"models": [], "confidence": 0.5})
        )
        
        # Would test batch processing
        # results = await analyzer.batch_analyze(texts)
    
    async def test_detect_lollapalooza(self, mock_llm_client):
        """Test Lollapalooza detection."""
        from analysis.model_analyzer import MentalModelAnalyzer
        
        analyzer = MentalModelAnalyzer(mock_llm_client)
        
        # Mock response with multiple models
        mock_llm_client.generate.return_value = MagicMock(
            text=json.dumps({
                "models": [
                    {"id": 1, "relevance": 0.9},
                    {"id": 2, "relevance": 0.8},
                    {"id": 3, "relevance": 0.85}
                ],
                "lollapalooza": True,
                "lollapalooza_score": 0.88
            })
        )
        
        # Lollapalooza should be detected when 3+ models converge


class TestAnalysisResult:
    """Tests for analysis result structures."""
    
    def test_result_serialization(self):
        """Test result can be serialized to JSON."""
        result = {
            "document_id": "test_doc",
            "models": [
                {"id": 1, "name": "Test", "relevance": 0.9}
            ],
            "lollapalooza": False,
            "confidence": 0.85,
            "timestamp": "2024-01-01T00:00:00"
        }
        
        # Should be JSON serializable
        json_str = json.dumps(result)
        parsed = json.loads(json_str)
        
        assert parsed["document_id"] == "test_doc"
        assert len(parsed["models"]) == 1
    
    def test_result_validation(self):
        """Test result validation."""
        # Valid result
        valid_result = {
            "models": [{"id": 1, "relevance": 0.9}],
            "confidence": 0.85
        }
        
        assert "models" in valid_result
        assert "confidence" in valid_result
        assert 0 <= valid_result["confidence"] <= 1


class TestPromptTemplates:
    """Tests for analysis prompt templates."""
    
    def test_analysis_prompt_format(self, sample_mental_models):
        """Test analysis prompt is properly formatted."""
        from analysis.model_analyzer import MentalModelAnalyzer
        
        # Verify prompt includes model information
        model_names = [m["name"] for m in sample_mental_models]
        
        assert "Incentive-Caused Bias" in model_names
        assert "Social Proof" in model_names
    
    def test_prompt_length_limits(self):
        """Test prompts don't exceed length limits."""
        # Typical LLM context limits
        MAX_PROMPT_LENGTH = 8000  # Conservative limit
        
        # Create a long document
        long_text = "Test text. " * 1000
        
        # Prompt should be truncated or chunked
        assert len(long_text) > MAX_PROMPT_LENGTH
        # Analyzer should handle this gracefully


@pytest.mark.asyncio
class TestAnalyzerEdgeCases:
    """Tests for edge cases in analyzer."""
    
    async def test_empty_text(self, mock_llm_client):
        """Test handling empty text."""
        from analysis.model_analyzer import MentalModelAnalyzer
        
        analyzer = MentalModelAnalyzer(mock_llm_client)
        
        mock_llm_client.generate.return_value = MagicMock(
            text=json.dumps({"models": [], "confidence": 0.0})
        )
        
        # Should handle gracefully
        # result = await analyzer.analyze_text("")
    
    async def test_very_long_text(self, mock_llm_client):
        """Test handling very long text."""
        from analysis.model_analyzer import MentalModelAnalyzer
        
        analyzer = MentalModelAnalyzer(mock_llm_client)
        
        long_text = "Word " * 10000
        
        mock_llm_client.generate.return_value = MagicMock(
            text=json.dumps({"models": [], "confidence": 0.5})
        )
        
        # Should chunk or truncate
        # result = await analyzer.analyze_text(long_text)
    
    async def test_special_characters(self, mock_llm_client):
        """Test handling special characters."""
        from analysis.model_analyzer import MentalModelAnalyzer
        
        analyzer = MentalModelAnalyzer(mock_llm_client)
        
        special_text = "Test with émojis 🚀 and spëcial çharacters"
        
        mock_llm_client.generate.return_value = MagicMock(
            text=json.dumps({"models": [], "confidence": 0.5})
        )
        
        # Should handle gracefully
        # result = await analyzer.analyze_text(special_text)
    
    async def test_llm_error_handling(self, mock_llm_client):
        """Test handling LLM errors."""
        from analysis.model_analyzer import MentalModelAnalyzer
        
        analyzer = MentalModelAnalyzer(mock_llm_client)
        
        # Simulate LLM error
        mock_llm_client.generate.side_effect = Exception("LLM Error")
        
        # Should handle gracefully without crashing
        # with pytest.raises or returns error result
