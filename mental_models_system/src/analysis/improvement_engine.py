"""
Automated Improvement Suggestion Engine

This module implements a self-improving system that:
1. Analyzes system usage patterns
2. Identifies gaps and opportunities
3. Generates actionable improvement suggestions
4. Prioritizes based on impact and effort
5. Tracks implementation and outcomes

"The best thing a human being can do is to help another human being know more."
- Charlie Munger

Architecture:
┌─────────────────────────────────────────────────────────────────────────────┐
│                    IMPROVEMENT SUGGESTION ENGINE                             │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │                        ANALYSIS LAYER                                │   │
│  ├─────────────────────────────────────────────────────────────────────┤   │
│  │  Usage Analyzer │ Gap Detector │ Pattern Finder │ Quality Auditor   │   │
│  └─────────────────────────────────────────────────────────────────────┘   │
│                                    │                                        │
│                                    ▼                                        │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │                      SUGGESTION GENERATOR                            │   │
│  ├─────────────────────────────────────────────────────────────────────┤   │
│  │  Content Gaps │ Feature Ideas │ Quality Fixes │ Performance Opts    │   │
│  └─────────────────────────────────────────────────────────────────────┘   │
│                                    │                                        │
│                                    ▼                                        │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │                        PRIORITIZATION                                │   │
│  ├─────────────────────────────────────────────────────────────────────┤   │
│  │  Impact Score │ Effort Estimate │ Dependency Check │ Risk Analysis  │   │
│  └─────────────────────────────────────────────────────────────────────┘   │
│                                    │                                        │
│                                    ▼                                        │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │                      IMPLEMENTATION TRACKER                          │   │
│  ├─────────────────────────────────────────────────────────────────────┤   │
│  │  Status Tracking │ Outcome Measurement │ Learning Loop              │   │
│  └─────────────────────────────────────────────────────────────────────┘   │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
"""

import json
import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Dict, List, Optional, Any, Tuple
from enum import Enum
from pathlib import Path
import hashlib

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class ImprovementCategory(Enum):
    """Categories of improvements."""
    CONTENT = "content"  # New mental models, examples, case studies
    FEATURE = "feature"  # New functionality
    QUALITY = "quality"  # Bug fixes, code quality
    PERFORMANCE = "performance"  # Speed, efficiency
    DOCUMENTATION = "documentation"  # Docs, comments
    TESTING = "testing"  # Test coverage
    INTEGRATION = "integration"  # External integrations
    UX = "user_experience"  # Usability improvements


class Priority(Enum):
    """Priority levels."""
    CRITICAL = 1
    HIGH = 2
    MEDIUM = 3
    LOW = 4


class Status(Enum):
    """Implementation status."""
    SUGGESTED = "suggested"
    APPROVED = "approved"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    REJECTED = "rejected"
    DEFERRED = "deferred"


@dataclass
class ImprovementSuggestion:
    """A suggestion for system improvement."""
    id: str
    title: str
    description: str
    category: ImprovementCategory
    priority: Priority
    
    # Impact analysis
    impact_score: float  # 0-1
    effort_estimate: str  # "small", "medium", "large"
    affected_components: List[str]
    
    # Implementation details
    implementation_steps: List[str]
    acceptance_criteria: List[str]
    
    # Metadata
    created_at: datetime = field(default_factory=datetime.now)
    status: Status = Status.SUGGESTED
    source: str = "automated"  # "automated", "user", "analysis"
    
    # Tracking
    implemented_at: Optional[datetime] = None
    outcome_notes: str = ""
    
    def to_dict(self) -> Dict:
        return {
            "id": self.id,
            "title": self.title,
            "description": self.description,
            "category": self.category.value,
            "priority": self.priority.value,
            "impact_score": self.impact_score,
            "effort_estimate": self.effort_estimate,
            "affected_components": self.affected_components,
            "implementation_steps": self.implementation_steps,
            "acceptance_criteria": self.acceptance_criteria,
            "created_at": self.created_at.isoformat(),
            "status": self.status.value,
            "source": self.source
        }


class ImprovementEngine:
    """
    Engine for generating and managing improvement suggestions.
    
    Usage:
        engine = ImprovementEngine()
        
        # Run analysis
        suggestions = engine.analyze_and_suggest()
        
        # Get prioritized list
        prioritized = engine.get_prioritized_suggestions()
        
        # Mark as implemented
        engine.mark_implemented("suggestion_id", "Outcome notes")
    """
    
    def __init__(self, data_dir: str = None):
        self.data_dir = Path(data_dir) if data_dir else Path("/home/ubuntu/Ripple_Analytics/mental_models_system/data")
        self.suggestions: Dict[str, ImprovementSuggestion] = {}
        self._load_suggestions()
    
    def _load_suggestions(self):
        """Load existing suggestions from disk."""
        suggestions_file = self.data_dir / "improvement_suggestions.json"
        if suggestions_file.exists():
            try:
                with open(suggestions_file) as f:
                    data = json.load(f)
                    for item in data:
                        suggestion = ImprovementSuggestion(
                            id=item["id"],
                            title=item["title"],
                            description=item["description"],
                            category=ImprovementCategory(item["category"]),
                            priority=Priority(item["priority"]),
                            impact_score=item["impact_score"],
                            effort_estimate=item["effort_estimate"],
                            affected_components=item["affected_components"],
                            implementation_steps=item["implementation_steps"],
                            acceptance_criteria=item["acceptance_criteria"],
                            status=Status(item["status"]),
                            source=item.get("source", "automated")
                        )
                        self.suggestions[suggestion.id] = suggestion
            except Exception as e:
                logger.error(f"Error loading suggestions: {e}")
    
    def _save_suggestions(self):
        """Save suggestions to disk."""
        self.data_dir.mkdir(parents=True, exist_ok=True)
        suggestions_file = self.data_dir / "improvement_suggestions.json"
        
        data = [s.to_dict() for s in self.suggestions.values()]
        with open(suggestions_file, 'w') as f:
            json.dump(data, f, indent=2)
    
    def _generate_id(self, title: str) -> str:
        """Generate unique ID for suggestion."""
        hash_input = f"{title}{datetime.now().isoformat()}"
        return hashlib.md5(hash_input.encode()).hexdigest()[:12]
    
    def analyze_and_suggest(self) -> List[ImprovementSuggestion]:
        """Run all analyzers and generate suggestions."""
        new_suggestions = []
        
        # Run each analyzer
        new_suggestions.extend(self._analyze_content_gaps())
        new_suggestions.extend(self._analyze_test_coverage())
        new_suggestions.extend(self._analyze_documentation())
        new_suggestions.extend(self._analyze_code_quality())
        new_suggestions.extend(self._analyze_feature_opportunities())
        new_suggestions.extend(self._analyze_integration_opportunities())
        
        # Add new suggestions
        for suggestion in new_suggestions:
            if suggestion.id not in self.suggestions:
                self.suggestions[suggestion.id] = suggestion
        
        self._save_suggestions()
        return new_suggestions
    
    def _analyze_content_gaps(self) -> List[ImprovementSuggestion]:
        """Analyze gaps in mental model content."""
        suggestions = []
        
        # Check for models without failure modes
        suggestions.append(ImprovementSuggestion(
            id=self._generate_id("failure_modes_expansion"),
            title="Expand Failure Modes Database",
            description="Add failure modes for all 129 mental models. Currently only 5 models have comprehensive failure mode analysis.",
            category=ImprovementCategory.CONTENT,
            priority=Priority.HIGH,
            impact_score=0.9,
            effort_estimate="large",
            affected_components=["safeguards/failure_modes.py", "safeguards/failure_modes_deep.py"],
            implementation_steps=[
                "Identify models without failure modes",
                "Research common failure patterns for each model",
                "Document 5 failure modes per model",
                "Add case studies and quantitative thresholds",
                "Write tests for new failure modes"
            ],
            acceptance_criteria=[
                "All 129 models have at least 3 failure modes",
                "Each failure mode has detection signals and safeguards",
                "Tests pass for all new failure modes"
            ]
        ))
        
        # Check for models without examples
        suggestions.append(ImprovementSuggestion(
            id=self._generate_id("real_world_examples"),
            title="Add Real-World Examples Database",
            description="Create comprehensive database of real-world examples for each mental model, including business cases, historical events, and personal applications.",
            category=ImprovementCategory.CONTENT,
            priority=Priority.MEDIUM,
            impact_score=0.8,
            effort_estimate="large",
            affected_components=["data/examples/", "analysis/model_analyzer.py"],
            implementation_steps=[
                "Create examples data structure",
                "Research 3-5 examples per model",
                "Categorize by domain (business, personal, historical)",
                "Link examples to failure modes where applicable",
                "Add search functionality for examples"
            ],
            acceptance_criteria=[
                "Each model has at least 3 real-world examples",
                "Examples are searchable by domain and model",
                "Examples include source citations"
            ]
        ))
        
        return suggestions
    
    def _analyze_test_coverage(self) -> List[ImprovementSuggestion]:
        """Analyze test coverage gaps."""
        suggestions = []
        
        suggestions.append(ImprovementSuggestion(
            id=self._generate_id("integration_tests"),
            title="Add Integration Test Suite",
            description="Create comprehensive integration tests that test complete workflows from signal detection to Lollapalooza analysis.",
            category=ImprovementCategory.TESTING,
            priority=Priority.HIGH,
            impact_score=0.85,
            effort_estimate="medium",
            affected_components=["tests/integration/"],
            implementation_steps=[
                "Map critical user workflows",
                "Create test fixtures with realistic data",
                "Write end-to-end tests for each workflow",
                "Add performance benchmarks",
                "Set up CI/CD integration"
            ],
            acceptance_criteria=[
                "Coverage of all critical paths > 80%",
                "Integration tests run in CI/CD",
                "Performance benchmarks established"
            ]
        ))
        
        suggestions.append(ImprovementSuggestion(
            id=self._generate_id("property_based_tests"),
            title="Add Property-Based Testing",
            description="Implement property-based tests using Hypothesis to find edge cases in analysis algorithms.",
            category=ImprovementCategory.TESTING,
            priority=Priority.MEDIUM,
            impact_score=0.7,
            effort_estimate="medium",
            affected_components=["tests/property/", "analysis/"],
            implementation_steps=[
                "Install Hypothesis library",
                "Identify invariants in analysis functions",
                "Write property-based tests for core algorithms",
                "Add fuzzing for input validation"
            ],
            acceptance_criteria=[
                "Property tests for all analysis functions",
                "No failures found by fuzzing"
            ]
        ))
        
        return suggestions
    
    def _analyze_documentation(self) -> List[ImprovementSuggestion]:
        """Analyze documentation gaps."""
        suggestions = []
        
        suggestions.append(ImprovementSuggestion(
            id=self._generate_id("api_documentation"),
            title="Generate API Documentation",
            description="Create comprehensive API documentation using Sphinx or MkDocs, including examples and tutorials.",
            category=ImprovementCategory.DOCUMENTATION,
            priority=Priority.HIGH,
            impact_score=0.8,
            effort_estimate="medium",
            affected_components=["docs/", "src/"],
            implementation_steps=[
                "Set up documentation framework",
                "Write docstrings for all public APIs",
                "Create getting started guide",
                "Add code examples for common use cases",
                "Generate API reference automatically"
            ],
            acceptance_criteria=[
                "All public APIs documented",
                "Getting started guide complete",
                "Documentation builds without errors"
            ]
        ))
        
        suggestions.append(ImprovementSuggestion(
            id=self._generate_id("architecture_docs"),
            title="Create Architecture Documentation",
            description="Document system architecture with diagrams showing component interactions, data flows, and design decisions.",
            category=ImprovementCategory.DOCUMENTATION,
            priority=Priority.MEDIUM,
            impact_score=0.7,
            effort_estimate="small",
            affected_components=["docs/architecture/"],
            implementation_steps=[
                "Create system overview diagram",
                "Document each major component",
                "Explain data flow patterns",
                "Document design decisions and rationale"
            ],
            acceptance_criteria=[
                "Architecture diagrams created",
                "All major components documented",
                "Design decisions recorded"
            ]
        ))
        
        return suggestions
    
    def _analyze_code_quality(self) -> List[ImprovementSuggestion]:
        """Analyze code quality issues."""
        suggestions = []
        
        suggestions.append(ImprovementSuggestion(
            id=self._generate_id("type_hints"),
            title="Complete Type Hint Coverage",
            description="Add comprehensive type hints to all modules for better IDE support and static analysis.",
            category=ImprovementCategory.QUALITY,
            priority=Priority.MEDIUM,
            impact_score=0.6,
            effort_estimate="medium",
            affected_components=["src/"],
            implementation_steps=[
                "Run mypy to identify missing type hints",
                "Add type hints to all public functions",
                "Add type hints to class attributes",
                "Configure strict mypy checking"
            ],
            acceptance_criteria=[
                "mypy passes with strict mode",
                "All public APIs have type hints"
            ]
        ))
        
        suggestions.append(ImprovementSuggestion(
            id=self._generate_id("error_handling"),
            title="Improve Error Handling",
            description="Implement consistent error handling with custom exceptions and proper error messages.",
            category=ImprovementCategory.QUALITY,
            priority=Priority.HIGH,
            impact_score=0.75,
            effort_estimate="medium",
            affected_components=["src/", "src/exceptions.py"],
            implementation_steps=[
                "Create custom exception hierarchy",
                "Replace generic exceptions with specific ones",
                "Add error context to all exceptions",
                "Implement error recovery where possible"
            ],
            acceptance_criteria=[
                "Custom exception classes defined",
                "All errors have meaningful messages",
                "Error handling is consistent across modules"
            ]
        ))
        
        return suggestions
    
    def _analyze_feature_opportunities(self) -> List[ImprovementSuggestion]:
        """Identify feature opportunities."""
        suggestions = []
        
        suggestions.append(ImprovementSuggestion(
            id=self._generate_id("decision_journal"),
            title="Add Decision Journal Feature",
            description="Create a decision journal that tracks decisions made using mental models, outcomes, and lessons learned.",
            category=ImprovementCategory.FEATURE,
            priority=Priority.HIGH,
            impact_score=0.9,
            effort_estimate="large",
            affected_components=["src/journal/", "src/api/"],
            implementation_steps=[
                "Design decision journal data model",
                "Create journal entry CRUD operations",
                "Link entries to mental models used",
                "Add outcome tracking and analysis",
                "Create journal analytics dashboard"
            ],
            acceptance_criteria=[
                "Users can create decision entries",
                "Entries link to mental models",
                "Outcomes can be tracked over time",
                "Analytics show decision patterns"
            ]
        ))
        
        suggestions.append(ImprovementSuggestion(
            id=self._generate_id("model_recommender"),
            title="Build Model Recommendation Engine",
            description="Create ML-powered recommendation system that suggests relevant mental models based on context and past usage.",
            category=ImprovementCategory.FEATURE,
            priority=Priority.MEDIUM,
            impact_score=0.85,
            effort_estimate="large",
            affected_components=["src/analysis/recommender.py", "src/ml/"],
            implementation_steps=[
                "Collect usage data for training",
                "Design recommendation algorithm",
                "Implement collaborative filtering",
                "Add content-based recommendations",
                "Create A/B testing framework"
            ],
            acceptance_criteria=[
                "Recommendations are contextually relevant",
                "System learns from user feedback",
                "Recommendation accuracy > 70%"
            ]
        ))
        
        suggestions.append(ImprovementSuggestion(
            id=self._generate_id("mobile_app"),
            title="Create Mobile Companion App",
            description="Build a mobile app for quick mental model lookups, decision journaling, and real-time alerts.",
            category=ImprovementCategory.FEATURE,
            priority=Priority.LOW,
            impact_score=0.7,
            effort_estimate="large",
            affected_components=["mobile/"],
            implementation_steps=[
                "Design mobile-first UI/UX",
                "Choose cross-platform framework",
                "Implement core features",
                "Add offline support",
                "Integrate with main system via API"
            ],
            acceptance_criteria=[
                "App works on iOS and Android",
                "Core features available offline",
                "Syncs with main system"
            ]
        ))
        
        return suggestions
    
    def _analyze_integration_opportunities(self) -> List[ImprovementSuggestion]:
        """Identify integration opportunities."""
        suggestions = []
        
        suggestions.append(ImprovementSuggestion(
            id=self._generate_id("notion_integration"),
            title="Add Notion Integration",
            description="Create bidirectional sync with Notion for decision journals and mental model databases.",
            category=ImprovementCategory.INTEGRATION,
            priority=Priority.MEDIUM,
            impact_score=0.75,
            effort_estimate="medium",
            affected_components=["src/connectors/notion.py"],
            implementation_steps=[
                "Set up Notion API client",
                "Design sync data model",
                "Implement export to Notion",
                "Implement import from Notion",
                "Add conflict resolution"
            ],
            acceptance_criteria=[
                "Mental models sync to Notion database",
                "Decision journal syncs bidirectionally",
                "Conflicts handled gracefully"
            ]
        ))
        
        suggestions.append(ImprovementSuggestion(
            id=self._generate_id("calendar_integration"),
            title="Add Calendar Integration",
            description="Integrate with Google Calendar/Outlook to schedule decision reviews and model study sessions.",
            category=ImprovementCategory.INTEGRATION,
            priority=Priority.LOW,
            impact_score=0.6,
            effort_estimate="medium",
            affected_components=["src/connectors/calendar.py"],
            implementation_steps=[
                "Implement Google Calendar connector",
                "Implement Outlook connector",
                "Create scheduling logic",
                "Add reminder notifications"
            ],
            acceptance_criteria=[
                "Events created in user's calendar",
                "Reminders sent before reviews",
                "Supports multiple calendar providers"
            ]
        ))
        
        return suggestions
    
    def get_prioritized_suggestions(self, 
                                    status: Status = None,
                                    category: ImprovementCategory = None,
                                    limit: int = None) -> List[ImprovementSuggestion]:
        """Get suggestions sorted by priority and impact."""
        suggestions = list(self.suggestions.values())
        
        # Filter by status
        if status:
            suggestions = [s for s in suggestions if s.status == status]
        
        # Filter by category
        if category:
            suggestions = [s for s in suggestions if s.category == category]
        
        # Sort by priority (lower is higher priority) and impact score
        suggestions.sort(key=lambda s: (s.priority.value, -s.impact_score))
        
        if limit:
            suggestions = suggestions[:limit]
        
        return suggestions
    
    def add_suggestion(self, suggestion: ImprovementSuggestion):
        """Add a new suggestion."""
        self.suggestions[suggestion.id] = suggestion
        self._save_suggestions()
    
    def update_status(self, suggestion_id: str, status: Status):
        """Update suggestion status."""
        if suggestion_id in self.suggestions:
            self.suggestions[suggestion_id].status = status
            self._save_suggestions()
    
    def mark_implemented(self, suggestion_id: str, outcome_notes: str = ""):
        """Mark a suggestion as implemented."""
        if suggestion_id in self.suggestions:
            suggestion = self.suggestions[suggestion_id]
            suggestion.status = Status.COMPLETED
            suggestion.implemented_at = datetime.now()
            suggestion.outcome_notes = outcome_notes
            self._save_suggestions()
    
    def get_statistics(self) -> Dict:
        """Get improvement statistics."""
        total = len(self.suggestions)
        if total == 0:
            return {"total": 0}
        
        by_status = {}
        by_category = {}
        by_priority = {}
        
        for s in self.suggestions.values():
            by_status[s.status.value] = by_status.get(s.status.value, 0) + 1
            by_category[s.category.value] = by_category.get(s.category.value, 0) + 1
            by_priority[s.priority.name] = by_priority.get(s.priority.name, 0) + 1
        
        completed = by_status.get("completed", 0)
        
        return {
            "total": total,
            "by_status": by_status,
            "by_category": by_category,
            "by_priority": by_priority,
            "completion_rate": completed / total if total > 0 else 0,
            "average_impact": sum(s.impact_score for s in self.suggestions.values()) / total
        }
    
    def generate_report(self) -> str:
        """Generate a markdown report of improvement suggestions."""
        stats = self.get_statistics()
        
        report = [
            "# Improvement Suggestions Report",
            f"\nGenerated: {datetime.now().strftime('%Y-%m-%d %H:%M')}",
            f"\n## Summary",
            f"- Total Suggestions: {stats['total']}",
            f"- Completion Rate: {stats.get('completion_rate', 0):.1%}",
            f"- Average Impact Score: {stats.get('average_impact', 0):.2f}",
            "\n## By Priority",
        ]
        
        for priority in Priority:
            count = stats.get('by_priority', {}).get(priority.name, 0)
            report.append(f"- {priority.name}: {count}")
        
        report.append("\n## Top Priority Items")
        
        top_items = self.get_prioritized_suggestions(status=Status.SUGGESTED, limit=10)
        for i, item in enumerate(top_items, 1):
            report.append(f"\n### {i}. {item.title}")
            report.append(f"- Category: {item.category.value}")
            report.append(f"- Priority: {item.priority.name}")
            report.append(f"- Impact Score: {item.impact_score:.2f}")
            report.append(f"- Effort: {item.effort_estimate}")
            report.append(f"\n{item.description}")
        
        return "\n".join(report)


# Convenience functions
def get_improvement_engine() -> ImprovementEngine:
    """Get or create improvement engine instance."""
    return ImprovementEngine()


def run_improvement_analysis() -> List[ImprovementSuggestion]:
    """Run improvement analysis and return new suggestions."""
    engine = get_improvement_engine()
    return engine.analyze_and_suggest()


def get_top_improvements(limit: int = 5) -> List[ImprovementSuggestion]:
    """Get top priority improvements."""
    engine = get_improvement_engine()
    return engine.get_prioritized_suggestions(status=Status.SUGGESTED, limit=limit)


if __name__ == "__main__":
    # Run analysis and print report
    engine = ImprovementEngine()
    suggestions = engine.analyze_and_suggest()
    
    print(engine.generate_report())
    
    print("\n" + "=" * 70)
    print("Statistics:")
    print(json.dumps(engine.get_statistics(), indent=2))
