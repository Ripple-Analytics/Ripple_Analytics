"""
Comprehensive Audit Logging System.

Tracks all system operations for:
- Compliance and accountability
- Debugging and troubleshooting
- Performance analysis
- Security monitoring
"""

import json
import logging
import hashlib
from datetime import datetime, timedelta
from typing import Dict, List, Any, Optional
from dataclasses import dataclass, asdict
from enum import Enum
from pathlib import Path
import gzip
import threading
from collections import defaultdict


class AuditEventType(Enum):
    """Types of audit events."""
    # Analysis events
    ANALYSIS_STARTED = "analysis.started"
    ANALYSIS_COMPLETED = "analysis.completed"
    ANALYSIS_FAILED = "analysis.failed"
    
    # Decision events
    DECISION_CREATED = "decision.created"
    DECISION_UPDATED = "decision.updated"
    DECISION_OUTCOME_RECORDED = "decision.outcome_recorded"
    
    # Model events
    MODEL_APPLIED = "model.applied"
    MODEL_EFFECTIVENESS_UPDATED = "model.effectiveness_updated"
    LOLLAPALOOZA_DETECTED = "lollapalooza.detected"
    
    # Failure mode events
    FAILURE_MODE_TRIGGERED = "failure_mode.triggered"
    SAFEGUARD_RECOMMENDED = "safeguard.recommended"
    RISK_ASSESSMENT_COMPLETED = "risk_assessment.completed"
    
    # Signal events
    SIGNAL_HARVESTED = "signal.harvested"
    SIGNAL_PROCESSED = "signal.processed"
    ALERT_SENT = "alert.sent"
    
    # API events
    API_REQUEST = "api.request"
    API_RESPONSE = "api.response"
    API_ERROR = "api.error"
    RATE_LIMIT_EXCEEDED = "api.rate_limit_exceeded"
    
    # Authentication events
    AUTH_SUCCESS = "auth.success"
    AUTH_FAILURE = "auth.failure"
    API_KEY_CREATED = "auth.api_key_created"
    API_KEY_REVOKED = "auth.api_key_revoked"
    
    # Data events
    DATA_EXPORTED = "data.exported"
    DATA_IMPORTED = "data.imported"
    DATA_MODIFIED = "data.modified"
    
    # System events
    SYSTEM_STARTED = "system.started"
    SYSTEM_STOPPED = "system.stopped"
    JOB_SCHEDULED = "job.scheduled"
    JOB_EXECUTED = "job.executed"
    JOB_FAILED = "job.failed"
    
    # Improvement events
    IMPROVEMENT_SUGGESTED = "improvement.suggested"
    IMPROVEMENT_IMPLEMENTED = "improvement.implemented"


@dataclass
class AuditEvent:
    """An audit event record."""
    id: str
    timestamp: datetime
    event_type: str
    actor: str  # User, API key, system
    action: str
    resource_type: str
    resource_id: Optional[str]
    details: Dict[str, Any]
    outcome: str  # success, failure, pending
    duration_ms: Optional[int]
    ip_address: Optional[str]
    user_agent: Optional[str]
    correlation_id: Optional[str]  # For tracking related events
    
    def to_dict(self) -> Dict[str, Any]:
        data = asdict(self)
        data["timestamp"] = self.timestamp.isoformat()
        return data
    
    def to_json(self) -> str:
        return json.dumps(self.to_dict())


class AuditLogger:
    """
    Comprehensive audit logging system.
    
    Features:
    - Structured logging with JSON format
    - Log rotation and compression
    - Query and search capabilities
    - Statistics and reporting
    - Real-time streaming
    """
    
    def __init__(
        self,
        log_dir: str = "./data/audit",
        max_file_size_mb: int = 100,
        retention_days: int = 90,
        enable_console: bool = False
    ):
        self.log_dir = Path(log_dir)
        self.log_dir.mkdir(parents=True, exist_ok=True)
        
        self.max_file_size = max_file_size_mb * 1024 * 1024
        self.retention_days = retention_days
        self.enable_console = enable_console
        
        # Current log file
        self.current_file = self.log_dir / f"audit_{datetime.now().strftime('%Y%m%d')}.jsonl"
        
        # In-memory buffer for recent events
        self.buffer: List[AuditEvent] = []
        self.buffer_max_size = 1000
        
        # Statistics
        self.stats = defaultdict(int)
        
        # Lock for thread safety
        self._lock = threading.Lock()
        
        # Set up Python logging
        self.logger = logging.getLogger("audit")
        self.logger.setLevel(logging.INFO)
        
        # File handler
        handler = logging.FileHandler(self.current_file)
        handler.setFormatter(logging.Formatter("%(message)s"))
        self.logger.addHandler(handler)
        
        if enable_console:
            console = logging.StreamHandler()
            console.setFormatter(logging.Formatter("%(message)s"))
            self.logger.addHandler(console)
        
        # Log system start
        self.log(
            event_type=AuditEventType.SYSTEM_STARTED,
            actor="system",
            action="initialize",
            resource_type="audit_logger",
            details={"log_dir": str(self.log_dir), "retention_days": retention_days}
        )
    
    def log(
        self,
        event_type: AuditEventType,
        actor: str,
        action: str,
        resource_type: str,
        resource_id: str = None,
        details: Dict[str, Any] = None,
        outcome: str = "success",
        duration_ms: int = None,
        ip_address: str = None,
        user_agent: str = None,
        correlation_id: str = None
    ) -> AuditEvent:
        """
        Log an audit event.
        
        Args:
            event_type: Type of event
            actor: Who performed the action
            action: What action was performed
            resource_type: Type of resource affected
            resource_id: ID of the resource
            details: Additional event details
            outcome: success, failure, or pending
            duration_ms: Duration of the operation
            ip_address: Client IP address
            user_agent: Client user agent
            correlation_id: ID for tracking related events
            
        Returns:
            The created AuditEvent
        """
        event_id = hashlib.sha256(
            f"{datetime.now().isoformat()}{actor}{action}{resource_type}".encode()
        ).hexdigest()[:16]
        
        event = AuditEvent(
            id=event_id,
            timestamp=datetime.now(),
            event_type=event_type.value if isinstance(event_type, AuditEventType) else event_type,
            actor=actor,
            action=action,
            resource_type=resource_type,
            resource_id=resource_id,
            details=details or {},
            outcome=outcome,
            duration_ms=duration_ms,
            ip_address=ip_address,
            user_agent=user_agent,
            correlation_id=correlation_id
        )
        
        with self._lock:
            # Write to log file
            self.logger.info(event.to_json())
            
            # Add to buffer
            self.buffer.append(event)
            if len(self.buffer) > self.buffer_max_size:
                self.buffer.pop(0)
            
            # Update statistics
            self.stats[event.event_type] += 1
            self.stats[f"outcome.{outcome}"] += 1
            
            # Check for log rotation
            self._check_rotation()
        
        return event
    
    def log_api_request(
        self,
        method: str,
        path: str,
        actor: str,
        ip_address: str = None,
        user_agent: str = None,
        correlation_id: str = None
    ) -> str:
        """Log an API request and return correlation ID."""
        if not correlation_id:
            correlation_id = hashlib.sha256(
                f"{datetime.now().isoformat()}{path}".encode()
            ).hexdigest()[:16]
        
        self.log(
            event_type=AuditEventType.API_REQUEST,
            actor=actor,
            action=method,
            resource_type="api",
            resource_id=path,
            details={"method": method, "path": path},
            outcome="pending",
            ip_address=ip_address,
            user_agent=user_agent,
            correlation_id=correlation_id
        )
        
        return correlation_id
    
    def log_api_response(
        self,
        correlation_id: str,
        status_code: int,
        duration_ms: int,
        actor: str
    ):
        """Log an API response."""
        outcome = "success" if status_code < 400 else "failure"
        
        self.log(
            event_type=AuditEventType.API_RESPONSE,
            actor=actor,
            action="response",
            resource_type="api",
            details={"status_code": status_code},
            outcome=outcome,
            duration_ms=duration_ms,
            correlation_id=correlation_id
        )
    
    def log_analysis(
        self,
        actor: str,
        text_length: int,
        models_applied: List[str],
        lollapalooza_score: float,
        duration_ms: int,
        correlation_id: str = None
    ):
        """Log a mental model analysis."""
        self.log(
            event_type=AuditEventType.ANALYSIS_COMPLETED,
            actor=actor,
            action="analyze",
            resource_type="text",
            details={
                "text_length": text_length,
                "models_applied": models_applied,
                "model_count": len(models_applied),
                "lollapalooza_score": lollapalooza_score
            },
            outcome="success",
            duration_ms=duration_ms,
            correlation_id=correlation_id
        )
        
        if lollapalooza_score >= 0.7:
            self.log(
                event_type=AuditEventType.LOLLAPALOOZA_DETECTED,
                actor=actor,
                action="detect",
                resource_type="lollapalooza",
                details={
                    "score": lollapalooza_score,
                    "models": models_applied
                },
                correlation_id=correlation_id
            )
    
    def log_decision(
        self,
        actor: str,
        decision_id: str,
        action: str,
        models_used: List[str] = None,
        risk_score: float = None
    ):
        """Log a decision journal event."""
        event_type = {
            "create": AuditEventType.DECISION_CREATED,
            "update": AuditEventType.DECISION_UPDATED,
            "outcome": AuditEventType.DECISION_OUTCOME_RECORDED
        }.get(action, AuditEventType.DECISION_UPDATED)
        
        self.log(
            event_type=event_type,
            actor=actor,
            action=action,
            resource_type="decision",
            resource_id=decision_id,
            details={
                "models_used": models_used or [],
                "risk_score": risk_score
            }
        )
    
    def log_failure_mode(
        self,
        actor: str,
        model_name: str,
        failure_mode: str,
        context: str,
        risk_level: str
    ):
        """Log a failure mode trigger."""
        self.log(
            event_type=AuditEventType.FAILURE_MODE_TRIGGERED,
            actor=actor,
            action="trigger",
            resource_type="failure_mode",
            resource_id=model_name,
            details={
                "failure_mode": failure_mode,
                "context": context[:200],
                "risk_level": risk_level
            }
        )
    
    def query(
        self,
        event_type: str = None,
        actor: str = None,
        resource_type: str = None,
        start_time: datetime = None,
        end_time: datetime = None,
        outcome: str = None,
        limit: int = 100
    ) -> List[AuditEvent]:
        """
        Query audit events.
        
        Args:
            event_type: Filter by event type
            actor: Filter by actor
            resource_type: Filter by resource type
            start_time: Start of time range
            end_time: End of time range
            outcome: Filter by outcome
            limit: Maximum results
            
        Returns:
            List of matching AuditEvents
        """
        results = []
        
        # Search buffer first (most recent)
        for event in reversed(self.buffer):
            if self._matches_query(event, event_type, actor, resource_type, start_time, end_time, outcome):
                results.append(event)
                if len(results) >= limit:
                    return results
        
        # Search log files if needed
        if len(results) < limit:
            for log_file in sorted(self.log_dir.glob("audit_*.jsonl*"), reverse=True):
                if len(results) >= limit:
                    break
                
                events = self._read_log_file(log_file)
                for event in reversed(events):
                    if self._matches_query(event, event_type, actor, resource_type, start_time, end_time, outcome):
                        results.append(event)
                        if len(results) >= limit:
                            break
        
        return results
    
    def _matches_query(
        self,
        event: AuditEvent,
        event_type: str,
        actor: str,
        resource_type: str,
        start_time: datetime,
        end_time: datetime,
        outcome: str
    ) -> bool:
        """Check if event matches query criteria."""
        if event_type and event.event_type != event_type:
            return False
        if actor and event.actor != actor:
            return False
        if resource_type and event.resource_type != resource_type:
            return False
        if start_time and event.timestamp < start_time:
            return False
        if end_time and event.timestamp > end_time:
            return False
        if outcome and event.outcome != outcome:
            return False
        return True
    
    def _read_log_file(self, path: Path) -> List[AuditEvent]:
        """Read events from a log file."""
        events = []
        
        try:
            if path.suffix == ".gz":
                with gzip.open(path, 'rt') as f:
                    for line in f:
                        events.append(self._parse_event(line))
            else:
                with open(path) as f:
                    for line in f:
                        events.append(self._parse_event(line))
        except Exception:
            pass
        
        return events
    
    def _parse_event(self, line: str) -> AuditEvent:
        """Parse a JSON line into an AuditEvent."""
        data = json.loads(line)
        data["timestamp"] = datetime.fromisoformat(data["timestamp"])
        return AuditEvent(**data)
    
    def _check_rotation(self):
        """Check if log rotation is needed."""
        if self.current_file.exists():
            if self.current_file.stat().st_size > self.max_file_size:
                self._rotate_log()
        
        # Check date change
        today = datetime.now().strftime('%Y%m%d')
        if not self.current_file.name.endswith(f"{today}.jsonl"):
            self._rotate_log()
    
    def _rotate_log(self):
        """Rotate the current log file."""
        if self.current_file.exists():
            # Compress old file
            compressed = self.current_file.with_suffix(".jsonl.gz")
            with open(self.current_file, 'rb') as f_in:
                with gzip.open(compressed, 'wb') as f_out:
                    f_out.writelines(f_in)
            self.current_file.unlink()
        
        # Create new file
        self.current_file = self.log_dir / f"audit_{datetime.now().strftime('%Y%m%d_%H%M%S')}.jsonl"
        
        # Update handler
        for handler in self.logger.handlers[:]:
            if isinstance(handler, logging.FileHandler):
                handler.close()
                self.logger.removeHandler(handler)
        
        handler = logging.FileHandler(self.current_file)
        handler.setFormatter(logging.Formatter("%(message)s"))
        self.logger.addHandler(handler)
        
        # Clean old files
        self._cleanup_old_files()
    
    def _cleanup_old_files(self):
        """Remove log files older than retention period."""
        cutoff = datetime.now() - timedelta(days=self.retention_days)
        
        for log_file in self.log_dir.glob("audit_*.jsonl*"):
            try:
                # Parse date from filename
                date_str = log_file.stem.split("_")[1][:8]
                file_date = datetime.strptime(date_str, "%Y%m%d")
                
                if file_date < cutoff:
                    log_file.unlink()
            except Exception:
                pass
    
    def get_stats(self) -> Dict[str, Any]:
        """Get audit statistics."""
        return {
            "total_events": sum(v for k, v in self.stats.items() if not k.startswith("outcome.")),
            "events_by_type": {k: v for k, v in self.stats.items() if not k.startswith("outcome.")},
            "outcomes": {k.replace("outcome.", ""): v for k, v in self.stats.items() if k.startswith("outcome.")},
            "buffer_size": len(self.buffer),
            "log_file": str(self.current_file),
            "retention_days": self.retention_days
        }
    
    def get_recent_events(self, count: int = 50) -> List[Dict]:
        """Get most recent events."""
        return [e.to_dict() for e in self.buffer[-count:]]


# Global audit logger instance
audit_logger = AuditLogger()


def log_event(
    event_type: AuditEventType,
    actor: str,
    action: str,
    resource_type: str,
    **kwargs
) -> AuditEvent:
    """Convenience function for logging events."""
    return audit_logger.log(event_type, actor, action, resource_type, **kwargs)
