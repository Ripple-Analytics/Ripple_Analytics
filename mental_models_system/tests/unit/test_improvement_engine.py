"""
Unit tests for improvement suggestion engine.
"""
import pytest
import json
import tempfile
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from analysis.improvement_engine import (
    ImprovementEngine, ImprovementSuggestion,
    ImprovementCategory, Priority, Status,
    get_improvement_engine, run_improvement_analysis, get_top_improvements
)


class TestImprovementSuggestion:
    """Tests for ImprovementSuggestion dataclass."""
    
    def test_suggestion_creation(self):
        """Test creating a suggestion."""
        suggestion = ImprovementSuggestion(
            id="test_123",
            title="Test Suggestion",
            description="A test suggestion",
            category=ImprovementCategory.FEATURE,
            priority=Priority.HIGH,
            impact_score=0.8,
            effort_estimate="medium",
            affected_components=["src/test.py"],
            implementation_steps=["Step 1", "Step 2"],
            acceptance_criteria=["Criteria 1"]
        )
        
        assert suggestion.id == "test_123"
        assert suggestion.title == "Test Suggestion"
        assert suggestion.category == ImprovementCategory.FEATURE
        assert suggestion.priority == Priority.HIGH
        assert suggestion.status == Status.SUGGESTED
    
    def test_suggestion_to_dict(self):
        """Test serialization to dict."""
        suggestion = ImprovementSuggestion(
            id="test_123",
            title="Test Suggestion",
            description="A test suggestion",
            category=ImprovementCategory.CONTENT,
            priority=Priority.MEDIUM,
            impact_score=0.7,
            effort_estimate="small",
            affected_components=["src/test.py"],
            implementation_steps=["Step 1"],
            acceptance_criteria=["Criteria 1"]
        )
        
        data = suggestion.to_dict()
        
        assert data["id"] == "test_123"
        assert data["category"] == "content"
        assert data["priority"] == 3  # MEDIUM = 3
        assert "created_at" in data


class TestImprovementCategory:
    """Tests for ImprovementCategory enum."""
    
    def test_categories_exist(self):
        """Test all categories exist."""
        assert ImprovementCategory.CONTENT.value == "content"
        assert ImprovementCategory.FEATURE.value == "feature"
        assert ImprovementCategory.QUALITY.value == "quality"
        assert ImprovementCategory.PERFORMANCE.value == "performance"
        assert ImprovementCategory.DOCUMENTATION.value == "documentation"
        assert ImprovementCategory.TESTING.value == "testing"
        assert ImprovementCategory.INTEGRATION.value == "integration"


class TestPriority:
    """Tests for Priority enum."""
    
    def test_priority_ordering(self):
        """Test priority values are ordered correctly."""
        assert Priority.CRITICAL.value < Priority.HIGH.value
        assert Priority.HIGH.value < Priority.MEDIUM.value
        assert Priority.MEDIUM.value < Priority.LOW.value


class TestStatus:
    """Tests for Status enum."""
    
    def test_status_values(self):
        """Test all status values exist."""
        assert Status.SUGGESTED.value == "suggested"
        assert Status.APPROVED.value == "approved"
        assert Status.IN_PROGRESS.value == "in_progress"
        assert Status.COMPLETED.value == "completed"
        assert Status.REJECTED.value == "rejected"


class TestImprovementEngine:
    """Tests for ImprovementEngine."""
    
    @pytest.fixture
    def temp_dir(self):
        """Create temporary directory for tests."""
        with tempfile.TemporaryDirectory() as tmpdir:
            yield tmpdir
    
    @pytest.fixture
    def engine(self, temp_dir):
        """Create engine with temp directory."""
        return ImprovementEngine(data_dir=temp_dir)
    
    def test_engine_creation(self, engine):
        """Test engine creation."""
        assert engine is not None
        assert engine.suggestions == {}
    
    def test_analyze_and_suggest(self, engine):
        """Test running analysis."""
        suggestions = engine.analyze_and_suggest()
        
        assert len(suggestions) > 0
        assert all(isinstance(s, ImprovementSuggestion) for s in suggestions)
    
    def test_suggestions_saved(self, temp_dir):
        """Test suggestions are saved to disk."""
        engine = ImprovementEngine(data_dir=temp_dir)
        engine.analyze_and_suggest()
        
        # Check file exists
        suggestions_file = Path(temp_dir) / "improvement_suggestions.json"
        assert suggestions_file.exists()
        
        # Check content
        with open(suggestions_file) as f:
            data = json.load(f)
        assert len(data) > 0
    
    def test_suggestions_loaded(self, temp_dir):
        """Test suggestions are loaded from disk."""
        # Create and save suggestions
        engine1 = ImprovementEngine(data_dir=temp_dir)
        engine1.analyze_and_suggest()
        count = len(engine1.suggestions)
        
        # Create new engine and verify loaded
        engine2 = ImprovementEngine(data_dir=temp_dir)
        assert len(engine2.suggestions) == count
    
    def test_get_prioritized_suggestions(self, engine):
        """Test getting prioritized suggestions."""
        engine.analyze_and_suggest()
        
        prioritized = engine.get_prioritized_suggestions()
        
        # Should be sorted by priority then impact
        for i in range(len(prioritized) - 1):
            current = prioritized[i]
            next_item = prioritized[i + 1]
            
            # Lower priority value = higher priority
            if current.priority.value == next_item.priority.value:
                assert current.impact_score >= next_item.impact_score
            else:
                assert current.priority.value <= next_item.priority.value
    
    def test_filter_by_status(self, engine):
        """Test filtering by status."""
        engine.analyze_and_suggest()
        
        suggested = engine.get_prioritized_suggestions(status=Status.SUGGESTED)
        assert all(s.status == Status.SUGGESTED for s in suggested)
    
    def test_filter_by_category(self, engine):
        """Test filtering by category."""
        engine.analyze_and_suggest()
        
        content = engine.get_prioritized_suggestions(category=ImprovementCategory.CONTENT)
        assert all(s.category == ImprovementCategory.CONTENT for s in content)
    
    def test_limit_results(self, engine):
        """Test limiting results."""
        engine.analyze_and_suggest()
        
        limited = engine.get_prioritized_suggestions(limit=3)
        assert len(limited) <= 3
    
    def test_add_suggestion(self, engine):
        """Test adding a custom suggestion."""
        suggestion = ImprovementSuggestion(
            id="custom_123",
            title="Custom Suggestion",
            description="A custom suggestion",
            category=ImprovementCategory.FEATURE,
            priority=Priority.HIGH,
            impact_score=0.9,
            effort_estimate="small",
            affected_components=["src/custom.py"],
            implementation_steps=["Step 1"],
            acceptance_criteria=["Criteria 1"],
            source="user"
        )
        
        engine.add_suggestion(suggestion)
        
        assert "custom_123" in engine.suggestions
        assert engine.suggestions["custom_123"].source == "user"
    
    def test_update_status(self, engine):
        """Test updating suggestion status."""
        engine.analyze_and_suggest()
        
        # Get first suggestion
        suggestion_id = list(engine.suggestions.keys())[0]
        
        engine.update_status(suggestion_id, Status.APPROVED)
        
        assert engine.suggestions[suggestion_id].status == Status.APPROVED
    
    def test_mark_implemented(self, engine):
        """Test marking suggestion as implemented."""
        engine.analyze_and_suggest()
        
        suggestion_id = list(engine.suggestions.keys())[0]
        
        engine.mark_implemented(suggestion_id, "Successfully implemented")
        
        suggestion = engine.suggestions[suggestion_id]
        assert suggestion.status == Status.COMPLETED
        assert suggestion.implemented_at is not None
        assert suggestion.outcome_notes == "Successfully implemented"
    
    def test_get_statistics(self, engine):
        """Test getting statistics."""
        engine.analyze_and_suggest()
        
        stats = engine.get_statistics()
        
        assert "total" in stats
        assert "by_status" in stats
        assert "by_category" in stats
        assert "by_priority" in stats
        assert "completion_rate" in stats
        assert "average_impact" in stats
        
        assert stats["total"] > 0
    
    def test_generate_report(self, engine):
        """Test generating markdown report."""
        engine.analyze_and_suggest()
        
        report = engine.generate_report()
        
        assert "# Improvement Suggestions Report" in report
        assert "## Summary" in report
        assert "## By Priority" in report
        assert "## Top Priority Items" in report
    
    def test_empty_statistics(self, engine):
        """Test statistics with no suggestions."""
        stats = engine.get_statistics()
        
        assert stats["total"] == 0


class TestConvenienceFunctions:
    """Tests for convenience functions."""
    
    def test_get_improvement_engine(self):
        """Test getting engine instance."""
        engine = get_improvement_engine()
        assert engine is not None
        assert isinstance(engine, ImprovementEngine)
    
    def test_run_improvement_analysis(self, tmp_path):
        """Test running analysis via convenience function."""
        # This uses default path, so just check it runs
        suggestions = run_improvement_analysis()
        assert isinstance(suggestions, list)
    
    def test_get_top_improvements(self, tmp_path):
        """Test getting top improvements."""
        improvements = get_top_improvements(limit=3)
        assert len(improvements) <= 3


class TestSuggestionContent:
    """Tests for suggestion content quality."""
    
    @pytest.fixture
    def engine(self, tmp_path):
        """Create engine with temp directory."""
        return ImprovementEngine(data_dir=str(tmp_path))
    
    def test_content_gap_suggestions(self, engine):
        """Test content gap analysis produces valid suggestions."""
        suggestions = engine._analyze_content_gaps()
        
        assert len(suggestions) > 0
        for s in suggestions:
            assert s.category == ImprovementCategory.CONTENT
            assert len(s.implementation_steps) > 0
            assert len(s.acceptance_criteria) > 0
    
    def test_test_coverage_suggestions(self, engine):
        """Test test coverage analysis produces valid suggestions."""
        suggestions = engine._analyze_test_coverage()
        
        assert len(suggestions) > 0
        for s in suggestions:
            assert s.category == ImprovementCategory.TESTING
    
    def test_documentation_suggestions(self, engine):
        """Test documentation analysis produces valid suggestions."""
        suggestions = engine._analyze_documentation()
        
        assert len(suggestions) > 0
        for s in suggestions:
            assert s.category == ImprovementCategory.DOCUMENTATION
    
    def test_code_quality_suggestions(self, engine):
        """Test code quality analysis produces valid suggestions."""
        suggestions = engine._analyze_code_quality()
        
        assert len(suggestions) > 0
        for s in suggestions:
            assert s.category == ImprovementCategory.QUALITY
    
    def test_feature_suggestions(self, engine):
        """Test feature analysis produces valid suggestions."""
        suggestions = engine._analyze_feature_opportunities()
        
        assert len(suggestions) > 0
        for s in suggestions:
            assert s.category == ImprovementCategory.FEATURE
    
    def test_integration_suggestions(self, engine):
        """Test integration analysis produces valid suggestions."""
        suggestions = engine._analyze_integration_opportunities()
        
        assert len(suggestions) > 0
        for s in suggestions:
            assert s.category == ImprovementCategory.INTEGRATION
