"""
Manus Integration API

This module provides the bridge between your local LLM processing and Manus,
enabling the automated improvement cycle:

┌─────────────────────────────────────────────────────────────────────────────┐
│                    AUTOMATED IMPROVEMENT SNOWBALL                            │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌─────────────────┐                      ┌─────────────────┐               │
│  │  Your Terabytes │                      │     Manus       │               │
│  │   of Documents  │                      │   (This Agent)  │               │
│  └────────┬────────┘                      └────────┬────────┘               │
│           │                                        │                         │
│           ▼                                        │                         │
│  ┌─────────────────┐                              │                         │
│  │  Local LLMs     │                              │                         │
│  │  (Ollama/vLLM)  │                              │                         │
│  └────────┬────────┘                              │                         │
│           │                                        │                         │
│           ▼                                        │                         │
│  ┌─────────────────┐    Export JSON     ┌────────┴────────┐                │
│  │   Knowledge     │ ──────────────────▶│  Import Queue   │                │
│  │   Extraction    │                    └────────┬────────┘                │
│  └─────────────────┘                             │                         │
│                                                   ▼                         │
│                                          ┌─────────────────┐               │
│                                          │  Review & Merge │               │
│                                          └────────┬────────┘               │
│                                                   │                         │
│                                                   ▼                         │
│  ┌─────────────────┐    Feedback        ┌─────────────────┐               │
│  │   Improvement   │ ◀──────────────────│  GitHub Push    │               │
│  │   Suggestions   │                    └─────────────────┘               │
│  └─────────────────┘                                                       │
│                                                                              │
│  The cycle repeats continuously, each iteration improving the system        │
└─────────────────────────────────────────────────────────────────────────────┘

Usage:
    1. Run local LLM extraction on your documents
    2. Export results using ManusExporter
    3. Manus imports and reviews the suggestions
    4. Approved items are merged into the system
    5. Feedback is logged for improving extraction
"""

import os
import json
import hashlib
from datetime import datetime
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Any, Callable
from enum import Enum
from pathlib import Path
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# =============================================================================
# DATA STRUCTURES
# =============================================================================

class ImprovementType(Enum):
    """Types of improvements that can be suggested."""
    NEW_MENTAL_MODEL = "new_mental_model"
    NEW_FAILURE_MODE = "new_failure_mode"
    NEW_CASE_STUDY = "new_case_study"
    NEW_INSIGHT = "new_insight"
    NEW_CONNECTION = "new_connection"
    UPDATE_EXISTING = "update_existing"
    FIX_ERROR = "fix_error"
    ENHANCE_DESCRIPTION = "enhance_description"


class ImprovementStatus(Enum):
    """Status of an improvement suggestion."""
    PENDING = "pending"
    APPROVED = "approved"
    REJECTED = "rejected"
    IMPLEMENTED = "implemented"
    DEFERRED = "deferred"


class Priority(Enum):
    """Priority levels for improvements."""
    CRITICAL = 1  # Must implement immediately
    HIGH = 2      # Implement this session
    MEDIUM = 3    # Implement when convenient
    LOW = 4       # Nice to have
    FUTURE = 5    # Consider for later


@dataclass
class Improvement:
    """A single improvement suggestion."""
    id: str
    type: ImprovementType
    priority: Priority
    title: str
    description: str
    implementation: str
    expected_impact: str
    source_document: Optional[str] = None
    source_chunk: Optional[str] = None
    confidence: float = 0.8
    status: ImprovementStatus = ImprovementStatus.PENDING
    created_at: datetime = field(default_factory=datetime.now)
    reviewed_at: Optional[datetime] = None
    implemented_at: Optional[datetime] = None
    reviewer_notes: Optional[str] = None
    
    def to_dict(self) -> Dict:
        return {
            "id": self.id,
            "type": self.type.value,
            "priority": self.priority.value,
            "title": self.title,
            "description": self.description,
            "implementation": self.implementation,
            "expected_impact": self.expected_impact,
            "source_document": self.source_document,
            "source_chunk": self.source_chunk,
            "confidence": self.confidence,
            "status": self.status.value,
            "created_at": self.created_at.isoformat(),
            "reviewed_at": self.reviewed_at.isoformat() if self.reviewed_at else None,
            "implemented_at": self.implemented_at.isoformat() if self.implemented_at else None,
            "reviewer_notes": self.reviewer_notes
        }
    
    @classmethod
    def from_dict(cls, data: Dict) -> 'Improvement':
        return cls(
            id=data["id"],
            type=ImprovementType(data["type"]),
            priority=Priority(data["priority"]),
            title=data["title"],
            description=data["description"],
            implementation=data["implementation"],
            expected_impact=data["expected_impact"],
            source_document=data.get("source_document"),
            source_chunk=data.get("source_chunk"),
            confidence=data.get("confidence", 0.8),
            status=ImprovementStatus(data.get("status", "pending")),
            created_at=datetime.fromisoformat(data["created_at"]) if data.get("created_at") else datetime.now(),
            reviewed_at=datetime.fromisoformat(data["reviewed_at"]) if data.get("reviewed_at") else None,
            implemented_at=datetime.fromisoformat(data["implemented_at"]) if data.get("implemented_at") else None,
            reviewer_notes=data.get("reviewer_notes")
        )


@dataclass
class ImportBatch:
    """A batch of improvements to import."""
    batch_id: str
    created_at: datetime
    source: str  # "local_llm", "manual", "devin", etc.
    improvements: List[Improvement]
    metadata: Dict = field(default_factory=dict)
    
    def to_dict(self) -> Dict:
        return {
            "batch_id": self.batch_id,
            "created_at": self.created_at.isoformat(),
            "source": self.source,
            "improvements": [i.to_dict() for i in self.improvements],
            "metadata": self.metadata,
            "statistics": {
                "total": len(self.improvements),
                "by_type": self._count_by_type(),
                "by_priority": self._count_by_priority()
            }
        }
    
    def _count_by_type(self) -> Dict[str, int]:
        counts = {}
        for imp in self.improvements:
            key = imp.type.value
            counts[key] = counts.get(key, 0) + 1
        return counts
    
    def _count_by_priority(self) -> Dict[int, int]:
        counts = {}
        for imp in self.improvements:
            key = imp.priority.value
            counts[key] = counts.get(key, 0) + 1
        return counts


# =============================================================================
# MANUS EXPORTER
# =============================================================================

class ManusExporter:
    """
    Export extracted knowledge in a format optimized for Manus import.
    
    This creates JSON files that Manus can easily parse and act upon.
    """
    
    def __init__(self, output_dir: str = "./manus_exports"):
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)
        self.export_history: List[Dict] = []
    
    def _generate_id(self, content: str) -> str:
        """Generate a unique ID for an improvement."""
        return hashlib.md5(f"{content}{datetime.now().isoformat()}".encode()).hexdigest()[:12]
    
    def create_improvement(self,
                          type: ImprovementType,
                          title: str,
                          description: str,
                          implementation: str,
                          expected_impact: str,
                          priority: Priority = Priority.MEDIUM,
                          source_document: str = None,
                          confidence: float = 0.8) -> Improvement:
        """Create a new improvement suggestion."""
        return Improvement(
            id=self._generate_id(title + description),
            type=type,
            priority=priority,
            title=title,
            description=description,
            implementation=implementation,
            expected_impact=expected_impact,
            source_document=source_document,
            confidence=confidence
        )
    
    def create_batch_from_extractions(self, 
                                      extractions: Dict,
                                      source: str = "local_llm") -> ImportBatch:
        """
        Convert raw extractions from local LLM into an import batch.
        
        Args:
            extractions: Dict with keys like 'mental_models', 'failure_modes', etc.
            source: Source identifier
        
        Returns:
            ImportBatch ready for export
        """
        improvements = []
        
        # Process mental models
        for model in extractions.get("mental_models", []):
            imp = self.create_improvement(
                type=ImprovementType.NEW_MENTAL_MODEL,
                title=model.get("name", "Unnamed Model"),
                description=model.get("description", ""),
                implementation=json.dumps({
                    "category": model.get("category", "general"),
                    "how_to_apply": model.get("how_to_apply", ""),
                    "failure_modes": model.get("failure_modes", []),
                    "related_models": model.get("related_models", [])
                }, indent=2),
                expected_impact="Adds new mental model to the latticework",
                priority=Priority.MEDIUM,
                source_document=model.get("_source_document"),
                confidence=model.get("confidence", 0.7)
            )
            improvements.append(imp)
        
        # Process failure modes
        for fm in extractions.get("failure_modes", []):
            imp = self.create_improvement(
                type=ImprovementType.NEW_FAILURE_MODE,
                title=fm.get("name", "Unnamed Failure Mode"),
                description=fm.get("description", ""),
                implementation=json.dumps({
                    "related_model": fm.get("related_model", ""),
                    "warning_signs": fm.get("warning_signs", []),
                    "prevention": fm.get("prevention", []),
                    "example": fm.get("example", "")
                }, indent=2),
                expected_impact="Improves failure detection for mental model application",
                priority=Priority.HIGH,  # Failure modes are high priority
                source_document=fm.get("_source_document"),
                confidence=fm.get("confidence", 0.7)
            )
            improvements.append(imp)
        
        # Process case studies
        for cs in extractions.get("case_studies", []):
            imp = self.create_improvement(
                type=ImprovementType.NEW_CASE_STUDY,
                title=cs.get("title", "Unnamed Case Study"),
                description=cs.get("summary", ""),
                implementation=json.dumps({
                    "year": cs.get("year"),
                    "entity": cs.get("entity", ""),
                    "mental_models": cs.get("mental_models", []),
                    "lessons": cs.get("lessons", []),
                    "outcomes": cs.get("outcomes", "")
                }, indent=2),
                expected_impact="Adds real-world example for mental model application",
                priority=Priority.MEDIUM,
                source_document=cs.get("_source_document"),
                confidence=cs.get("confidence", 0.7)
            )
            improvements.append(imp)
        
        # Process insights
        for insight in extractions.get("insights", []):
            imp = self.create_improvement(
                type=ImprovementType.NEW_INSIGHT,
                title=insight.get("insight", "")[:100],
                description=insight.get("insight", ""),
                implementation=json.dumps({
                    "source": insight.get("source", ""),
                    "category": insight.get("category", ""),
                    "how_to_apply": insight.get("how_to_apply", ""),
                    "pitfalls": insight.get("pitfalls", [])
                }, indent=2),
                expected_impact="Adds wisdom to the knowledge base",
                priority=Priority.LOW,
                source_document=insight.get("_source_document"),
                confidence=insight.get("confidence", 0.6)
            )
            improvements.append(imp)
        
        # Create batch
        batch = ImportBatch(
            batch_id=self._generate_id(str(datetime.now())),
            created_at=datetime.now(),
            source=source,
            improvements=improvements,
            metadata={
                "extraction_stats": {
                    "mental_models": len(extractions.get("mental_models", [])),
                    "failure_modes": len(extractions.get("failure_modes", [])),
                    "case_studies": len(extractions.get("case_studies", [])),
                    "insights": len(extractions.get("insights", []))
                }
            }
        )
        
        return batch
    
    def export_batch(self, batch: ImportBatch, filename: str = None) -> str:
        """Export a batch to JSON file."""
        if filename is None:
            filename = f"import_batch_{batch.batch_id}.json"
        
        output_path = self.output_dir / filename
        
        export_data = {
            "format_version": "1.0",
            "exported_at": datetime.now().isoformat(),
            "instructions": self._get_instructions(),
            "batch": batch.to_dict()
        }
        
        with open(output_path, 'w') as f:
            json.dump(export_data, f, indent=2, default=str)
        
        self.export_history.append({
            "batch_id": batch.batch_id,
            "path": str(output_path),
            "exported_at": datetime.now().isoformat(),
            "num_improvements": len(batch.improvements)
        })
        
        logger.info(f"Exported batch {batch.batch_id} with {len(batch.improvements)} improvements to {output_path}")
        return str(output_path)
    
    def _get_instructions(self) -> str:
        return """
MANUS IMPORT INSTRUCTIONS
=========================

This file contains improvement suggestions extracted from your document collection
by local LLMs. To process:

1. REVIEW each improvement:
   - Check the title and description for relevance
   - Verify the confidence score (higher = more reliable)
   - Review the implementation details

2. For APPROVED items:
   - Update status to "approved"
   - Add reviewer_notes if needed
   - Implement the changes described in 'implementation'

3. For REJECTED items:
   - Update status to "rejected"
   - Add reviewer_notes explaining why

4. PRIORITY GUIDE:
   1 = CRITICAL - Implement immediately
   2 = HIGH - Implement this session
   3 = MEDIUM - Implement when convenient
   4 = LOW - Nice to have
   5 = FUTURE - Consider for later

5. After processing:
   - Push changes to GitHub
   - Export feedback for improving extraction quality
"""
    
    def export_for_devin(self, batch: ImportBatch, filename: str = None) -> str:
        """Export batch in format optimized for Devin to implement."""
        if filename is None:
            filename = f"devin_tasks_{batch.batch_id}.json"
        
        output_path = self.output_dir / filename
        
        # Group by type for easier task assignment
        tasks_by_type = {}
        for imp in batch.improvements:
            type_key = imp.type.value
            if type_key not in tasks_by_type:
                tasks_by_type[type_key] = []
            tasks_by_type[type_key].append(imp.to_dict())
        
        export_data = {
            "format_version": "1.0",
            "exported_at": datetime.now().isoformat(),
            "instructions": """
DEVIN TASK INSTRUCTIONS
=======================

These tasks were generated from document analysis. For each task:

1. Review the improvement details
2. Implement the changes described
3. Add appropriate tests
4. Create a PR with clear description
5. Mark task as complete

Work through tasks in priority order (1 = highest priority).
""",
            "batch_id": batch.batch_id,
            "total_tasks": len(batch.improvements),
            "tasks_by_type": tasks_by_type
        }
        
        with open(output_path, 'w') as f:
            json.dump(export_data, f, indent=2, default=str)
        
        return str(output_path)


# =============================================================================
# MANUS IMPORTER
# =============================================================================

class ManusImporter:
    """
    Import and process improvement batches.
    
    This is used by Manus to process the exported suggestions.
    """
    
    def __init__(self, system_data_path: str = None):
        self.system_data_path = system_data_path
        self.processed_batches: List[str] = []
        self.statistics = {
            "total_imported": 0,
            "approved": 0,
            "rejected": 0,
            "implemented": 0
        }
    
    def load_batch(self, file_path: str) -> ImportBatch:
        """Load an import batch from file."""
        with open(file_path, 'r') as f:
            data = json.load(f)
        
        batch_data = data["batch"]
        improvements = [Improvement.from_dict(i) for i in batch_data["improvements"]]
        
        return ImportBatch(
            batch_id=batch_data["batch_id"],
            created_at=datetime.fromisoformat(batch_data["created_at"]),
            source=batch_data["source"],
            improvements=improvements,
            metadata=batch_data.get("metadata", {})
        )
    
    def review_batch(self, batch: ImportBatch, 
                    auto_approve_threshold: float = 0.9) -> Dict:
        """
        Review a batch and auto-approve high-confidence items.
        
        Returns summary of review results.
        """
        results = {
            "auto_approved": [],
            "needs_review": [],
            "low_confidence": []
        }
        
        for imp in batch.improvements:
            if imp.confidence >= auto_approve_threshold:
                imp.status = ImprovementStatus.APPROVED
                imp.reviewed_at = datetime.now()
                imp.reviewer_notes = "Auto-approved (high confidence)"
                results["auto_approved"].append(imp.id)
            elif imp.confidence >= 0.6:
                results["needs_review"].append(imp.id)
            else:
                results["low_confidence"].append(imp.id)
        
        return results
    
    def get_pending_improvements(self, batch: ImportBatch, 
                                 type_filter: ImprovementType = None,
                                 priority_filter: Priority = None) -> List[Improvement]:
        """Get pending improvements with optional filtering."""
        pending = [i for i in batch.improvements if i.status == ImprovementStatus.PENDING]
        
        if type_filter:
            pending = [i for i in pending if i.type == type_filter]
        
        if priority_filter:
            pending = [i for i in pending if i.priority == priority_filter]
        
        return sorted(pending, key=lambda x: x.priority.value)
    
    def approve_improvement(self, batch: ImportBatch, improvement_id: str, 
                           notes: str = None) -> bool:
        """Approve an improvement."""
        for imp in batch.improvements:
            if imp.id == improvement_id:
                imp.status = ImprovementStatus.APPROVED
                imp.reviewed_at = datetime.now()
                imp.reviewer_notes = notes
                self.statistics["approved"] += 1
                return True
        return False
    
    def reject_improvement(self, batch: ImportBatch, improvement_id: str,
                          reason: str) -> bool:
        """Reject an improvement."""
        for imp in batch.improvements:
            if imp.id == improvement_id:
                imp.status = ImprovementStatus.REJECTED
                imp.reviewed_at = datetime.now()
                imp.reviewer_notes = reason
                self.statistics["rejected"] += 1
                return True
        return False
    
    def mark_implemented(self, batch: ImportBatch, improvement_id: str) -> bool:
        """Mark an improvement as implemented."""
        for imp in batch.improvements:
            if imp.id == improvement_id:
                imp.status = ImprovementStatus.IMPLEMENTED
                imp.implemented_at = datetime.now()
                self.statistics["implemented"] += 1
                return True
        return False
    
    def generate_implementation_report(self, batch: ImportBatch) -> Dict:
        """Generate a report of what needs to be implemented."""
        approved = [i for i in batch.improvements if i.status == ImprovementStatus.APPROVED]
        
        report = {
            "batch_id": batch.batch_id,
            "generated_at": datetime.now().isoformat(),
            "total_approved": len(approved),
            "by_type": {},
            "implementation_steps": []
        }
        
        for imp in sorted(approved, key=lambda x: x.priority.value):
            type_key = imp.type.value
            if type_key not in report["by_type"]:
                report["by_type"][type_key] = []
            report["by_type"][type_key].append({
                "id": imp.id,
                "title": imp.title,
                "priority": imp.priority.value,
                "implementation": imp.implementation
            })
            
            report["implementation_steps"].append({
                "step": len(report["implementation_steps"]) + 1,
                "type": type_key,
                "title": imp.title,
                "action": imp.implementation,
                "expected_impact": imp.expected_impact
            })
        
        return report


# =============================================================================
# FEEDBACK LOOP
# =============================================================================

class FeedbackCollector:
    """
    Collect feedback on improvement quality to improve extraction.
    
    This closes the loop - feedback from Manus improves local LLM extraction.
    """
    
    def __init__(self, feedback_dir: str = "./feedback"):
        self.feedback_dir = Path(feedback_dir)
        self.feedback_dir.mkdir(parents=True, exist_ok=True)
        self.feedback_log: List[Dict] = []
    
    def record_feedback(self, 
                       improvement_id: str,
                       was_useful: bool,
                       quality_score: float,  # 0-1
                       notes: str = None,
                       improvement_type: str = None):
        """Record feedback on an improvement."""
        feedback = {
            "improvement_id": improvement_id,
            "was_useful": was_useful,
            "quality_score": quality_score,
            "notes": notes,
            "improvement_type": improvement_type,
            "recorded_at": datetime.now().isoformat()
        }
        self.feedback_log.append(feedback)
    
    def export_feedback(self, filename: str = None) -> str:
        """Export feedback for improving extraction."""
        if filename is None:
            filename = f"feedback_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        
        output_path = self.feedback_dir / filename
        
        # Aggregate statistics
        total = len(self.feedback_log)
        useful = sum(1 for f in self.feedback_log if f["was_useful"])
        avg_quality = sum(f["quality_score"] for f in self.feedback_log) / total if total > 0 else 0
        
        export_data = {
            "exported_at": datetime.now().isoformat(),
            "statistics": {
                "total_feedback": total,
                "useful_count": useful,
                "useful_rate": useful / total if total > 0 else 0,
                "average_quality": avg_quality
            },
            "feedback": self.feedback_log,
            "recommendations": self._generate_recommendations()
        }
        
        with open(output_path, 'w') as f:
            json.dump(export_data, f, indent=2)
        
        return str(output_path)
    
    def _generate_recommendations(self) -> List[str]:
        """Generate recommendations for improving extraction."""
        recommendations = []
        
        if not self.feedback_log:
            return ["No feedback collected yet"]
        
        # Analyze by type
        by_type = {}
        for f in self.feedback_log:
            t = f.get("improvement_type", "unknown")
            if t not in by_type:
                by_type[t] = {"useful": 0, "total": 0, "quality_sum": 0}
            by_type[t]["total"] += 1
            by_type[t]["quality_sum"] += f["quality_score"]
            if f["was_useful"]:
                by_type[t]["useful"] += 1
        
        for type_name, stats in by_type.items():
            useful_rate = stats["useful"] / stats["total"] if stats["total"] > 0 else 0
            avg_quality = stats["quality_sum"] / stats["total"] if stats["total"] > 0 else 0
            
            if useful_rate < 0.5:
                recommendations.append(
                    f"Improve extraction for '{type_name}' - only {useful_rate:.0%} useful"
                )
            if avg_quality < 0.6:
                recommendations.append(
                    f"Quality for '{type_name}' is low ({avg_quality:.2f}) - refine prompts"
                )
        
        return recommendations if recommendations else ["Extraction quality is good - continue current approach"]


# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

def create_exporter(output_dir: str = "./manus_exports") -> ManusExporter:
    """Create a ManusExporter with default settings."""
    return ManusExporter(output_dir)


def create_importer(system_data_path: str = None) -> ManusImporter:
    """Create a ManusImporter with default settings."""
    return ManusImporter(system_data_path)


def quick_export(extractions: Dict, output_dir: str = "./manus_exports") -> str:
    """Quick export of extractions to Manus format."""
    exporter = ManusExporter(output_dir)
    batch = exporter.create_batch_from_extractions(extractions)
    return exporter.export_batch(batch)


# =============================================================================
# CLI
# =============================================================================

if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(description="Manus Integration API")
    parser.add_argument("--import", dest="import_file", help="Import a batch file")
    parser.add_argument("--review", action="store_true", help="Review pending improvements")
    parser.add_argument("--report", action="store_true", help="Generate implementation report")
    parser.add_argument("--output", default="./manus_exports", help="Output directory")
    
    args = parser.parse_args()
    
    if args.import_file:
        importer = ManusImporter()
        batch = importer.load_batch(args.import_file)
        print(f"Loaded batch {batch.batch_id} with {len(batch.improvements)} improvements")
        
        if args.review:
            results = importer.review_batch(batch)
            print(f"Auto-approved: {len(results['auto_approved'])}")
            print(f"Needs review: {len(results['needs_review'])}")
            print(f"Low confidence: {len(results['low_confidence'])}")
        
        if args.report:
            report = importer.generate_implementation_report(batch)
            print(json.dumps(report, indent=2))
    else:
        print("Manus Integration API")
        print("=" * 50)
        print("Use --import <file> to import a batch")
        print("Use --review to auto-review improvements")
        print("Use --report to generate implementation report")
