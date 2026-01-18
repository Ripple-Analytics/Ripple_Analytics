"""
Event Sourcing System

Complete audit trail and event sourcing for the Mental Models System:
- Immutable event log
- Event replay for state reconstruction
- Snapshots for performance
- Event projections
- CQRS pattern support
"""

import asyncio
import hashlib
import json
import os
import time
import uuid
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Callable, Dict, Generic, List, Optional, TypeVar


class EventType(Enum):
    """Types of events in the system."""
    # Analysis events
    ANALYSIS_STARTED = "analysis.started"
    ANALYSIS_COMPLETED = "analysis.completed"
    ANALYSIS_FAILED = "analysis.failed"
    
    # Model events
    MODEL_APPLIED = "model.applied"
    MODEL_EFFECTIVENESS_UPDATED = "model.effectiveness_updated"
    
    # Decision events
    DECISION_CREATED = "decision.created"
    DECISION_UPDATED = "decision.updated"
    DECISION_OUTCOME_RECORDED = "decision.outcome_recorded"
    
    # Signal events
    SIGNAL_DETECTED = "signal.detected"
    SIGNAL_PROCESSED = "signal.processed"
    LOLLAPALOOZA_DETECTED = "signal.lollapalooza"
    
    # Failure mode events
    FAILURE_MODE_TRIGGERED = "failure_mode.triggered"
    SAFEGUARD_APPLIED = "failure_mode.safeguard_applied"
    
    # System events
    SYSTEM_STARTED = "system.started"
    SYSTEM_STOPPED = "system.stopped"
    CONFIG_CHANGED = "system.config_changed"
    
    # User events
    USER_ACTION = "user.action"
    API_REQUEST = "user.api_request"


@dataclass
class Event:
    """
    Immutable event record.
    
    Events are the source of truth in an event-sourced system.
    """
    id: str
    type: EventType
    timestamp: datetime
    aggregate_id: str
    aggregate_type: str
    data: Dict[str, Any]
    metadata: Dict[str, Any] = field(default_factory=dict)
    version: int = 1
    
    def __post_init__(self):
        # Generate hash for integrity verification
        self._hash = self._compute_hash()
    
    def _compute_hash(self) -> str:
        """Compute hash for event integrity."""
        content = json.dumps({
            "id": self.id,
            "type": self.type.value,
            "timestamp": self.timestamp.isoformat(),
            "aggregate_id": self.aggregate_id,
            "data": self.data
        }, sort_keys=True)
        return hashlib.sha256(content.encode()).hexdigest()
    
    @property
    def hash(self) -> str:
        return self._hash
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "id": self.id,
            "type": self.type.value,
            "timestamp": self.timestamp.isoformat(),
            "aggregate_id": self.aggregate_id,
            "aggregate_type": self.aggregate_type,
            "data": self.data,
            "metadata": self.metadata,
            "version": self.version,
            "hash": self.hash
        }
    
    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "Event":
        return cls(
            id=data["id"],
            type=EventType(data["type"]),
            timestamp=datetime.fromisoformat(data["timestamp"]),
            aggregate_id=data["aggregate_id"],
            aggregate_type=data["aggregate_type"],
            data=data["data"],
            metadata=data.get("metadata", {}),
            version=data.get("version", 1)
        )


@dataclass
class Snapshot:
    """Snapshot of aggregate state for performance optimization."""
    aggregate_id: str
    aggregate_type: str
    state: Dict[str, Any]
    version: int
    timestamp: datetime
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "aggregate_id": self.aggregate_id,
            "aggregate_type": self.aggregate_type,
            "state": self.state,
            "version": self.version,
            "timestamp": self.timestamp.isoformat()
        }


class EventStore(ABC):
    """Abstract event store interface."""
    
    @abstractmethod
    async def append(self, event: Event) -> None:
        """Append an event to the store."""
        pass
    
    @abstractmethod
    async def get_events(
        self,
        aggregate_id: str,
        from_version: int = 0
    ) -> List[Event]:
        """Get events for an aggregate."""
        pass
    
    @abstractmethod
    async def get_all_events(
        self,
        from_timestamp: Optional[datetime] = None,
        event_types: Optional[List[EventType]] = None
    ) -> List[Event]:
        """Get all events, optionally filtered."""
        pass
    
    @abstractmethod
    async def save_snapshot(self, snapshot: Snapshot) -> None:
        """Save a snapshot."""
        pass
    
    @abstractmethod
    async def get_snapshot(self, aggregate_id: str) -> Optional[Snapshot]:
        """Get the latest snapshot for an aggregate."""
        pass


class InMemoryEventStore(EventStore):
    """In-memory event store for development and testing."""
    
    def __init__(self):
        self._events: List[Event] = []
        self._snapshots: Dict[str, Snapshot] = {}
        self._lock = asyncio.Lock()
    
    async def append(self, event: Event) -> None:
        async with self._lock:
            self._events.append(event)
    
    async def get_events(
        self,
        aggregate_id: str,
        from_version: int = 0
    ) -> List[Event]:
        return [
            e for e in self._events
            if e.aggregate_id == aggregate_id and e.version >= from_version
        ]
    
    async def get_all_events(
        self,
        from_timestamp: Optional[datetime] = None,
        event_types: Optional[List[EventType]] = None
    ) -> List[Event]:
        events = self._events
        
        if from_timestamp:
            events = [e for e in events if e.timestamp >= from_timestamp]
        
        if event_types:
            events = [e for e in events if e.type in event_types]
        
        return events
    
    async def save_snapshot(self, snapshot: Snapshot) -> None:
        async with self._lock:
            self._snapshots[snapshot.aggregate_id] = snapshot
    
    async def get_snapshot(self, aggregate_id: str) -> Optional[Snapshot]:
        return self._snapshots.get(aggregate_id)
    
    def get_stats(self) -> Dict[str, Any]:
        """Get store statistics."""
        return {
            "total_events": len(self._events),
            "total_snapshots": len(self._snapshots),
            "events_by_type": {
                t.value: len([e for e in self._events if e.type == t])
                for t in EventType
                if any(e.type == t for e in self._events)
            }
        }


class FileEventStore(EventStore):
    """File-based event store for persistence."""
    
    def __init__(self, base_path: str = "./data/events"):
        self.base_path = base_path
        self.events_file = os.path.join(base_path, "events.jsonl")
        self.snapshots_dir = os.path.join(base_path, "snapshots")
        os.makedirs(base_path, exist_ok=True)
        os.makedirs(self.snapshots_dir, exist_ok=True)
        self._lock = asyncio.Lock()
    
    async def append(self, event: Event) -> None:
        async with self._lock:
            with open(self.events_file, "a") as f:
                f.write(json.dumps(event.to_dict()) + "\n")
    
    async def get_events(
        self,
        aggregate_id: str,
        from_version: int = 0
    ) -> List[Event]:
        events = []
        if os.path.exists(self.events_file):
            with open(self.events_file, "r") as f:
                for line in f:
                    if line.strip():
                        event = Event.from_dict(json.loads(line))
                        if event.aggregate_id == aggregate_id and event.version >= from_version:
                            events.append(event)
        return events
    
    async def get_all_events(
        self,
        from_timestamp: Optional[datetime] = None,
        event_types: Optional[List[EventType]] = None
    ) -> List[Event]:
        events = []
        if os.path.exists(self.events_file):
            with open(self.events_file, "r") as f:
                for line in f:
                    if line.strip():
                        event = Event.from_dict(json.loads(line))
                        
                        if from_timestamp and event.timestamp < from_timestamp:
                            continue
                        
                        if event_types and event.type not in event_types:
                            continue
                        
                        events.append(event)
        return events
    
    async def save_snapshot(self, snapshot: Snapshot) -> None:
        snapshot_file = os.path.join(
            self.snapshots_dir,
            f"{snapshot.aggregate_id}.json"
        )
        async with self._lock:
            with open(snapshot_file, "w") as f:
                json.dump(snapshot.to_dict(), f)
    
    async def get_snapshot(self, aggregate_id: str) -> Optional[Snapshot]:
        snapshot_file = os.path.join(self.snapshots_dir, f"{aggregate_id}.json")
        if os.path.exists(snapshot_file):
            with open(snapshot_file, "r") as f:
                data = json.load(f)
                return Snapshot(
                    aggregate_id=data["aggregate_id"],
                    aggregate_type=data["aggregate_type"],
                    state=data["state"],
                    version=data["version"],
                    timestamp=datetime.fromisoformat(data["timestamp"])
                )
        return None


T = TypeVar("T")


class EventProjection(Generic[T], ABC):
    """Base class for event projections."""
    
    @abstractmethod
    def apply(self, event: Event, state: T) -> T:
        """Apply an event to the current state."""
        pass
    
    @abstractmethod
    def initial_state(self) -> T:
        """Return the initial state."""
        pass


class EventBus:
    """Event bus for publishing and subscribing to events."""
    
    def __init__(self):
        self._handlers: Dict[EventType, List[Callable]] = {}
        self._all_handlers: List[Callable] = []
    
    def subscribe(self, event_type: EventType, handler: Callable):
        """Subscribe to a specific event type."""
        if event_type not in self._handlers:
            self._handlers[event_type] = []
        self._handlers[event_type].append(handler)
    
    def subscribe_all(self, handler: Callable):
        """Subscribe to all events."""
        self._all_handlers.append(handler)
    
    async def publish(self, event: Event):
        """Publish an event to all subscribers."""
        # Notify specific handlers
        if event.type in self._handlers:
            for handler in self._handlers[event.type]:
                try:
                    if asyncio.iscoroutinefunction(handler):
                        await handler(event)
                    else:
                        handler(event)
                except Exception as e:
                    print(f"Error in event handler: {e}")
        
        # Notify all-event handlers
        for handler in self._all_handlers:
            try:
                if asyncio.iscoroutinefunction(handler):
                    await handler(event)
                else:
                    handler(event)
            except Exception as e:
                print(f"Error in event handler: {e}")


class EventSourcingSystem:
    """
    Main event sourcing system.
    
    Provides:
    - Event creation and storage
    - Event replay
    - Projections
    - Snapshots
    - CQRS support
    """
    
    def __init__(
        self,
        store: Optional[EventStore] = None,
        snapshot_threshold: int = 100
    ):
        self.store = store or InMemoryEventStore()
        self.bus = EventBus()
        self.snapshot_threshold = snapshot_threshold
        self._projections: Dict[str, EventProjection] = {}
        self._event_count: Dict[str, int] = {}
    
    def create_event(
        self,
        event_type: EventType,
        aggregate_id: str,
        aggregate_type: str,
        data: Dict[str, Any],
        metadata: Optional[Dict[str, Any]] = None
    ) -> Event:
        """Create a new event."""
        return Event(
            id=str(uuid.uuid4()),
            type=event_type,
            timestamp=datetime.now(),
            aggregate_id=aggregate_id,
            aggregate_type=aggregate_type,
            data=data,
            metadata=metadata or {},
            version=self._event_count.get(aggregate_id, 0) + 1
        )
    
    async def emit(
        self,
        event_type: EventType,
        aggregate_id: str,
        aggregate_type: str,
        data: Dict[str, Any],
        metadata: Optional[Dict[str, Any]] = None
    ) -> Event:
        """Create, store, and publish an event."""
        event = self.create_event(
            event_type=event_type,
            aggregate_id=aggregate_id,
            aggregate_type=aggregate_type,
            data=data,
            metadata=metadata
        )
        
        # Store event
        await self.store.append(event)
        
        # Update count
        self._event_count[aggregate_id] = event.version
        
        # Publish to bus
        await self.bus.publish(event)
        
        # Check if snapshot needed
        if event.version % self.snapshot_threshold == 0:
            await self._create_snapshot(aggregate_id, aggregate_type)
        
        return event
    
    async def _create_snapshot(self, aggregate_id: str, aggregate_type: str):
        """Create a snapshot for an aggregate."""
        events = await self.store.get_events(aggregate_id)
        if not events:
            return
        
        # Build state from events
        state = {}
        for event in events:
            state = self._apply_event_to_state(event, state)
        
        snapshot = Snapshot(
            aggregate_id=aggregate_id,
            aggregate_type=aggregate_type,
            state=state,
            version=events[-1].version,
            timestamp=datetime.now()
        )
        
        await self.store.save_snapshot(snapshot)
    
    def _apply_event_to_state(self, event: Event, state: Dict[str, Any]) -> Dict[str, Any]:
        """Apply an event to a state dictionary."""
        # Generic state builder
        state["last_event_id"] = event.id
        state["last_event_type"] = event.type.value
        state["last_updated"] = event.timestamp.isoformat()
        state["version"] = event.version
        
        # Merge event data
        if "events" not in state:
            state["events"] = []
        state["events"].append({
            "type": event.type.value,
            "timestamp": event.timestamp.isoformat(),
            "data_keys": list(event.data.keys())
        })
        
        return state
    
    async def get_aggregate_state(self, aggregate_id: str) -> Dict[str, Any]:
        """Get the current state of an aggregate by replaying events."""
        # Try to get snapshot first
        snapshot = await self.store.get_snapshot(aggregate_id)
        
        if snapshot:
            state = snapshot.state
            from_version = snapshot.version + 1
        else:
            state = {}
            from_version = 0
        
        # Replay events since snapshot
        events = await self.store.get_events(aggregate_id, from_version)
        for event in events:
            state = self._apply_event_to_state(event, state)
        
        return state
    
    async def replay_events(
        self,
        from_timestamp: Optional[datetime] = None,
        event_types: Optional[List[EventType]] = None,
        handler: Optional[Callable] = None
    ) -> List[Event]:
        """Replay events for rebuilding state or projections."""
        events = await self.store.get_all_events(from_timestamp, event_types)
        
        if handler:
            for event in events:
                if asyncio.iscoroutinefunction(handler):
                    await handler(event)
                else:
                    handler(event)
        
        return events
    
    def register_projection(self, name: str, projection: EventProjection):
        """Register an event projection."""
        self._projections[name] = projection
    
    async def get_projection_state(self, name: str, aggregate_id: str) -> Any:
        """Get the state of a projection for an aggregate."""
        if name not in self._projections:
            raise ValueError(f"Unknown projection: {name}")
        
        projection = self._projections[name]
        state = projection.initial_state()
        
        events = await self.store.get_events(aggregate_id)
        for event in events:
            state = projection.apply(event, state)
        
        return state
    
    def get_stats(self) -> Dict[str, Any]:
        """Get system statistics."""
        if isinstance(self.store, InMemoryEventStore):
            store_stats = self.store.get_stats()
        else:
            store_stats = {"type": type(self.store).__name__}
        
        return {
            "store": store_stats,
            "aggregates_tracked": len(self._event_count),
            "total_events_emitted": sum(self._event_count.values()),
            "projections_registered": list(self._projections.keys())
        }


# Convenience functions for common events
async def emit_analysis_event(
    system: EventSourcingSystem,
    analysis_id: str,
    event_type: EventType,
    data: Dict[str, Any]
) -> Event:
    """Emit an analysis-related event."""
    return await system.emit(
        event_type=event_type,
        aggregate_id=analysis_id,
        aggregate_type="analysis",
        data=data,
        metadata={"source": "analysis_engine"}
    )


async def emit_decision_event(
    system: EventSourcingSystem,
    decision_id: str,
    event_type: EventType,
    data: Dict[str, Any]
) -> Event:
    """Emit a decision-related event."""
    return await system.emit(
        event_type=event_type,
        aggregate_id=decision_id,
        aggregate_type="decision",
        data=data,
        metadata={"source": "decision_journal"}
    )


async def emit_signal_event(
    system: EventSourcingSystem,
    signal_id: str,
    event_type: EventType,
    data: Dict[str, Any]
) -> Event:
    """Emit a signal-related event."""
    return await system.emit(
        event_type=event_type,
        aggregate_id=signal_id,
        aggregate_type="signal",
        data=data,
        metadata={"source": "signal_harvester"}
    )


if __name__ == "__main__":
    import asyncio
    
    async def test():
        # Create event sourcing system
        system = EventSourcingSystem()
        
        # Subscribe to events
        def on_analysis(event):
            print(f"Analysis event: {event.type.value}")
        
        system.bus.subscribe(EventType.ANALYSIS_STARTED, on_analysis)
        system.bus.subscribe(EventType.ANALYSIS_COMPLETED, on_analysis)
        
        # Emit some events
        analysis_id = str(uuid.uuid4())
        
        await system.emit(
            EventType.ANALYSIS_STARTED,
            analysis_id,
            "analysis",
            {"document": "test.pdf", "models": ["network_effects", "moats"]}
        )
        
        await system.emit(
            EventType.MODEL_APPLIED,
            analysis_id,
            "analysis",
            {"model": "network_effects", "score": 0.85}
        )
        
        await system.emit(
            EventType.ANALYSIS_COMPLETED,
            analysis_id,
            "analysis",
            {"total_models": 2, "lollapalooza_score": 0.72}
        )
        
        # Get aggregate state
        state = await system.get_aggregate_state(analysis_id)
        print(f"\nAggregate state: {json.dumps(state, indent=2)}")
        
        # Get stats
        print(f"\nSystem stats: {system.get_stats()}")
    
    asyncio.run(test())
