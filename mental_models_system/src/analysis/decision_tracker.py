"""
Decision Tracker for Mental Models System

Tracks decisions made using mental models, their outcomes, and lessons learned.
Enables learning from past decisions and improving future decision-making.

"In my whole life, I have known no wise people who didn't read all the time - none, zero."
- Charlie Munger
"""

import json
import logging
from dataclasses import dataclass, field, asdict
from datetime import datetime
from typing import Dict, List, Optional, Any
from pathlib import Path
import hashlib
from enum import Enum

from ..exceptions import (
    DecisionNotFoundError,
    InvalidDecisionStateError,
    InvalidInputError,
    validate_input
)

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class DecisionStatus(Enum):
    """Status of a decision."""
    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    ABANDONED = "abandoned"


@dataclass
class Decision:
    """A decision tracked in the system."""
    id: str
    title: str
    description: str
    mental_models_used: List[str]
    decision_made: str
    rationale: str
    confidence: float
    
    # Metadata
    created_at: datetime = field(default_factory=datetime.now)
    status: str = DecisionStatus.PENDING.value
    
    # Outcome tracking
    outcome: Optional[str] = None
    success: Optional[bool] = None
    lessons_learned: Optional[str] = None
    completed_at: Optional[datetime] = None
    
    # Additional context
    tags: List[str] = field(default_factory=list)
    related_decisions: List[str] = field(default_factory=list)
    
    def to_dict(self) -> Dict:
        """Convert to dictionary."""
        data = asdict(self)
        data['created_at'] = self.created_at.isoformat()
        if self.completed_at:
            data['completed_at'] = self.completed_at.isoformat()
        return data


class DecisionTracker:
    """
    Tracks decisions and their outcomes.
    
    Usage:
        tracker = DecisionTracker()
        
        # Create decision
        decision_id = tracker.create_decision(
            title="Expand to Europe",
            description="Should we expand?",
            mental_models_used=["First-Mover Advantage", "Economies of Scale"],
            decision_made="Yes, expand in Q2",
            rationale="Strong market opportunity",
            confidence=0.8
        )
        
        # Update status
        tracker.update_decision_status(decision_id, "in_progress")
        
        # Record outcome
        tracker.record_outcome(
            decision_id,
            outcome="Successfully launched in 3 countries",
            success=True,
            lessons_learned="Regulatory research was critical"
        )
    """
    
    def __init__(self, data_dir: str = None):
        self.data_dir = Path(data_dir) if data_dir else Path("/home/ubuntu/Ripple_Analytics/mental_models_system/data")
        self.decisions: Dict[str, Decision] = {}
        self._load_decisions()
    
    def _load_decisions(self):
        """Load decisions from disk."""
        decisions_file = self.data_dir / "decisions.json"
        if decisions_file.exists():
            try:
                with open(decisions_file) as f:
                    data = json.load(f)
                    for item in data:
                        decision = Decision(
                            id=item["id"],
                            title=item["title"],
                            description=item["description"],
                            mental_models_used=item["mental_models_used"],
                            decision_made=item["decision_made"],
                            rationale=item["rationale"],
                            confidence=item["confidence"],
                            status=item.get("status", DecisionStatus.PENDING.value),
                            outcome=item.get("outcome"),
                            success=item.get("success"),
                            lessons_learned=item.get("lessons_learned"),
                            tags=item.get("tags", []),
                            related_decisions=item.get("related_decisions", [])
                        )
                        if "created_at" in item:
                            decision.created_at = datetime.fromisoformat(item["created_at"])
                        if "completed_at" in item and item["completed_at"]:
                            decision.completed_at = datetime.fromisoformat(item["completed_at"])
                        
                        self.decisions[decision.id] = decision
            except Exception as e:
                logger.error(f"Error loading decisions: {e}")
    
    def _save_decisions(self):
        """Save decisions to disk."""
        self.data_dir.mkdir(parents=True, exist_ok=True)
        decisions_file = self.data_dir / "decisions.json"
        
        data = [d.to_dict() for d in self.decisions.values()]
        with open(decisions_file, 'w') as f:
            json.dump(data, f, indent=2)
    
    def _generate_id(self, title: str) -> str:
        """Generate unique ID for decision."""
        hash_input = f"{title}{datetime.now().isoformat()}"
        return hashlib.md5(hash_input.encode()).hexdigest()[:12]
    
    def create_decision(
        self,
        title: str,
        description: str,
        mental_models_used: List[str],
        decision_made: str,
        rationale: str,
        confidence: float,
        tags: List[str] = None
    ) -> str:
        """
        Create a new decision.
        
        Args:
            title: Short title for the decision
            description: Detailed description of the situation
            mental_models_used: List of mental model names used
            decision_made: The actual decision made
            rationale: Reasoning behind the decision
            confidence: Confidence level (0-1)
            tags: Optional tags for categorization
        
        Returns:
            Decision ID
        """
        validate_input(len(title) > 0, "Title cannot be empty")
        validate_input(0 <= confidence <= 1, "Confidence must be between 0 and 1")
        validate_input(len(mental_models_used) > 0, "Must specify at least one mental model")
        
        decision_id = self._generate_id(title)
        
        decision = Decision(
            id=decision_id,
            title=title,
            description=description,
            mental_models_used=mental_models_used,
            decision_made=decision_made,
            rationale=rationale,
            confidence=confidence,
            tags=tags or []
        )
        
        self.decisions[decision_id] = decision
        self._save_decisions()
        
        logger.info(f"Created decision: {decision_id} - {title}")
        return decision_id
    
    def get_decision(self, decision_id: str) -> Optional[Dict]:
        """Get a decision by ID."""
        decision = self.decisions.get(decision_id)
        return decision.to_dict() if decision else None
    
    def update_decision_status(self, decision_id: str, status: str):
        """Update the status of a decision."""
        if decision_id not in self.decisions:
            raise DecisionNotFoundError(decision_id)
        
        # Validate status
        try:
            DecisionStatus(status)
        except ValueError:
            raise InvalidInputError(
                f"Invalid status: {status}",
                {"valid_statuses": [s.value for s in DecisionStatus]}
            )
        
        self.decisions[decision_id].status = status
        self._save_decisions()
        
        logger.info(f"Updated decision {decision_id} status to {status}")
    
    def record_outcome(
        self,
        decision_id: str,
        outcome: str,
        success: bool,
        lessons_learned: str = ""
    ):
        """
        Record the outcome of a decision.
        
        Args:
            decision_id: ID of the decision
            outcome: Description of what happened
            success: Whether the decision was successful
            lessons_learned: Lessons learned from the outcome
        """
        if decision_id not in self.decisions:
            raise DecisionNotFoundError(decision_id)
        
        decision = self.decisions[decision_id]
        decision.outcome = outcome
        decision.success = success
        decision.lessons_learned = lessons_learned
        decision.completed_at = datetime.now()
        decision.status = DecisionStatus.COMPLETED.value
        
        self._save_decisions()
        
        logger.info(f"Recorded outcome for decision {decision_id}: {'Success' if success else 'Failure'}")
    
    def get_decisions_by_model(self, model_name: str) -> List[Dict]:
        """Get all decisions that used a specific mental model."""
        results = []
        for decision in self.decisions.values():
            if model_name in decision.mental_models_used:
                results.append(decision.to_dict())
        return results
    
    def get_decisions_by_tag(self, tag: str) -> List[Dict]:
        """Get all decisions with a specific tag."""
        results = []
        for decision in self.decisions.values():
            if tag in decision.tags:
                results.append(decision.to_dict())
        return results
    
    def get_success_rate_by_model(self, model_name: str) -> Optional[float]:
        """
        Calculate success rate for decisions using a specific model.
        
        Returns:
            Success rate (0-1) or None if no completed decisions
        """
        decisions = self.get_decisions_by_model(model_name)
        completed = [d for d in decisions if d["success"] is not None]
        
        if not completed:
            return None
        
        successes = sum(1 for d in completed if d["success"])
        return successes / len(completed)
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get statistics about tracked decisions."""
        total = len(self.decisions)
        if total == 0:
            return {
                "total_decisions": 0,
                "by_status": {},
                "success_rate": None,
                "most_used_models": []
            }
        
        # Count by status
        by_status = {}
        for decision in self.decisions.values():
            by_status[decision.status] = by_status.get(decision.status, 0) + 1
        
        # Calculate success rate
        completed = [d for d in self.decisions.values() if d.success is not None]
        success_rate = None
        if completed:
            successes = sum(1 for d in completed if d.success)
            success_rate = successes / len(completed)
        
        # Most used models
        model_counts = {}
        for decision in self.decisions.values():
            for model in decision.mental_models_used:
                model_counts[model] = model_counts.get(model, 0) + 1
        
        most_used = sorted(model_counts.items(), key=lambda x: x[1], reverse=True)[:10]
        
        return {
            "total_decisions": total,
            "by_status": by_status,
            "success_rate": success_rate,
            "most_used_models": most_used,
            "completed_decisions": len(completed)
        }
