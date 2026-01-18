"""
WebSocket Server for Real-Time Updates

Provides real-time streaming of:
- Signal harvester alerts
- Lollapalooza detections
- Document analysis progress
- System metrics
- Failure mode warnings

Architecture:
┌─────────────────────────────────────────────────────────────────────────────┐
│                         WEBSOCKET SERVER                                     │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌─────────────┐     ┌─────────────┐     ┌─────────────┐                   │
│  │   Clients   │◀───▶│   Channels  │◀───▶│   Events    │                   │
│  └─────────────┘     └─────────────┘     └─────────────┘                   │
│         │                  │                    │                           │
│         │                  │                    │                           │
│         ▼                  ▼                    ▼                           │
│  ┌─────────────┐     ┌─────────────┐     ┌─────────────┐                   │
│  │   Auth      │     │   Rooms     │     │   Pub/Sub   │                   │
│  └─────────────┘     └─────────────┘     └─────────────┘                   │
│                                                                              │
│  Channels:                                                                   │
│  - signals: Real-time signal alerts                                         │
│  - lollapalooza: Convergence detections                                     │
│  - analysis: Document analysis progress                                     │
│  - metrics: System metrics stream                                           │
│  - failures: Failure mode warnings                                          │
│  - all: All events (for dashboards)                                         │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
"""

import asyncio
import json
import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Dict, List, Optional, Set, Any, Callable
from enum import Enum
import uuid

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class EventType(Enum):
    """Types of WebSocket events."""
    SIGNAL = "signal"
    LOLLAPALOOZA = "lollapalooza"
    ANALYSIS_START = "analysis_start"
    ANALYSIS_PROGRESS = "analysis_progress"
    ANALYSIS_COMPLETE = "analysis_complete"
    METRIC = "metric"
    FAILURE_WARNING = "failure_warning"
    SYSTEM_STATUS = "system_status"
    HEARTBEAT = "heartbeat"


class Channel(Enum):
    """WebSocket channels for subscription."""
    SIGNALS = "signals"
    LOLLAPALOOZA = "lollapalooza"
    ANALYSIS = "analysis"
    METRICS = "metrics"
    FAILURES = "failures"
    ALL = "all"


@dataclass
class WebSocketEvent:
    """A WebSocket event to broadcast."""
    event_type: EventType
    channel: Channel
    data: Dict[str, Any]
    timestamp: datetime = field(default_factory=datetime.now)
    event_id: str = field(default_factory=lambda: str(uuid.uuid4()))
    
    def to_json(self) -> str:
        return json.dumps({
            "event_id": self.event_id,
            "type": self.event_type.value,
            "channel": self.channel.value,
            "data": self.data,
            "timestamp": self.timestamp.isoformat()
        })


@dataclass
class WebSocketClient:
    """A connected WebSocket client."""
    client_id: str
    websocket: Any  # WebSocket connection object
    subscriptions: Set[Channel] = field(default_factory=set)
    connected_at: datetime = field(default_factory=datetime.now)
    last_heartbeat: datetime = field(default_factory=datetime.now)
    metadata: Dict[str, Any] = field(default_factory=dict)


class WebSocketManager:
    """
    Manages WebSocket connections and event broadcasting.
    
    Usage:
        manager = WebSocketManager()
        
        # In your WebSocket endpoint
        @app.websocket("/ws")
        async def websocket_endpoint(websocket):
            client_id = await manager.connect(websocket)
            try:
                while True:
                    data = await websocket.receive_text()
                    await manager.handle_message(client_id, data)
            except:
                manager.disconnect(client_id)
        
        # Broadcast events
        await manager.broadcast_signal({
            "source": "SEC",
            "content": "New 13F filing",
            "models": ["Network Effects", "Moat"]
        })
    """
    
    def __init__(self):
        self._clients: Dict[str, WebSocketClient] = {}
        self._event_history: List[WebSocketEvent] = []
        self._max_history = 1000
        self._event_handlers: Dict[str, List[Callable]] = {}
        self._heartbeat_interval = 30  # seconds
        self._running = False
    
    async def connect(self, websocket: Any, metadata: Dict = None) -> str:
        """Connect a new WebSocket client."""
        client_id = str(uuid.uuid4())
        
        client = WebSocketClient(
            client_id=client_id,
            websocket=websocket,
            subscriptions={Channel.ALL},  # Default to all
            metadata=metadata or {}
        )
        
        self._clients[client_id] = client
        logger.info(f"WebSocket client connected: {client_id}")
        
        # Send welcome message
        await self._send_to_client(client_id, {
            "type": "connected",
            "client_id": client_id,
            "available_channels": [c.value for c in Channel],
            "timestamp": datetime.now().isoformat()
        })
        
        return client_id
    
    def disconnect(self, client_id: str):
        """Disconnect a WebSocket client."""
        if client_id in self._clients:
            del self._clients[client_id]
            logger.info(f"WebSocket client disconnected: {client_id}")
    
    async def handle_message(self, client_id: str, message: str):
        """Handle incoming message from client."""
        try:
            data = json.loads(message)
            action = data.get("action")
            
            if action == "subscribe":
                channels = data.get("channels", [])
                await self._subscribe(client_id, channels)
            
            elif action == "unsubscribe":
                channels = data.get("channels", [])
                await self._unsubscribe(client_id, channels)
            
            elif action == "ping":
                await self._send_to_client(client_id, {"type": "pong"})
                if client_id in self._clients:
                    self._clients[client_id].last_heartbeat = datetime.now()
            
            elif action == "get_history":
                channel = data.get("channel")
                limit = data.get("limit", 50)
                await self._send_history(client_id, channel, limit)
            
            else:
                await self._send_to_client(client_id, {
                    "type": "error",
                    "message": f"Unknown action: {action}"
                })
        
        except json.JSONDecodeError:
            await self._send_to_client(client_id, {
                "type": "error",
                "message": "Invalid JSON"
            })
    
    async def _subscribe(self, client_id: str, channels: List[str]):
        """Subscribe client to channels."""
        if client_id not in self._clients:
            return
        
        client = self._clients[client_id]
        for channel_name in channels:
            try:
                channel = Channel(channel_name)
                client.subscriptions.add(channel)
            except ValueError:
                pass
        
        await self._send_to_client(client_id, {
            "type": "subscribed",
            "channels": [c.value for c in client.subscriptions]
        })
    
    async def _unsubscribe(self, client_id: str, channels: List[str]):
        """Unsubscribe client from channels."""
        if client_id not in self._clients:
            return
        
        client = self._clients[client_id]
        for channel_name in channels:
            try:
                channel = Channel(channel_name)
                client.subscriptions.discard(channel)
            except ValueError:
                pass
        
        await self._send_to_client(client_id, {
            "type": "unsubscribed",
            "channels": [c.value for c in client.subscriptions]
        })
    
    async def _send_history(self, client_id: str, channel: str, limit: int):
        """Send event history to client."""
        if channel:
            try:
                channel_enum = Channel(channel)
                events = [
                    e for e in self._event_history
                    if e.channel == channel_enum
                ][-limit:]
            except ValueError:
                events = []
        else:
            events = self._event_history[-limit:]
        
        await self._send_to_client(client_id, {
            "type": "history",
            "events": [json.loads(e.to_json()) for e in events]
        })
    
    async def _send_to_client(self, client_id: str, data: Dict):
        """Send data to a specific client."""
        if client_id not in self._clients:
            return
        
        client = self._clients[client_id]
        try:
            await client.websocket.send_text(json.dumps(data))
        except Exception as e:
            logger.error(f"Error sending to client {client_id}: {e}")
            self.disconnect(client_id)
    
    async def broadcast(self, event: WebSocketEvent):
        """Broadcast event to all subscribed clients."""
        # Store in history
        self._event_history.append(event)
        if len(self._event_history) > self._max_history:
            self._event_history = self._event_history[-self._max_history:]
        
        # Broadcast to subscribed clients
        message = event.to_json()
        disconnected = []
        
        for client_id, client in self._clients.items():
            if event.channel in client.subscriptions or Channel.ALL in client.subscriptions:
                try:
                    await client.websocket.send_text(message)
                except Exception as e:
                    logger.error(f"Error broadcasting to {client_id}: {e}")
                    disconnected.append(client_id)
        
        # Clean up disconnected clients
        for client_id in disconnected:
            self.disconnect(client_id)
        
        # Trigger event handlers
        for handler in self._event_handlers.get(event.event_type.value, []):
            try:
                await handler(event)
            except Exception as e:
                logger.error(f"Event handler error: {e}")
    
    # Convenience methods for broadcasting specific event types
    
    async def broadcast_signal(self, data: Dict):
        """Broadcast a signal alert."""
        event = WebSocketEvent(
            event_type=EventType.SIGNAL,
            channel=Channel.SIGNALS,
            data=data
        )
        await self.broadcast(event)
    
    async def broadcast_lollapalooza(self, data: Dict):
        """Broadcast a Lollapalooza detection."""
        event = WebSocketEvent(
            event_type=EventType.LOLLAPALOOZA,
            channel=Channel.LOLLAPALOOZA,
            data=data
        )
        await self.broadcast(event)
    
    async def broadcast_analysis_start(self, document: str, total_chunks: int):
        """Broadcast analysis start."""
        event = WebSocketEvent(
            event_type=EventType.ANALYSIS_START,
            channel=Channel.ANALYSIS,
            data={
                "document": document,
                "total_chunks": total_chunks,
                "status": "started"
            }
        )
        await self.broadcast(event)
    
    async def broadcast_analysis_progress(self, document: str, current: int, total: int, models_found: List[str] = None):
        """Broadcast analysis progress."""
        event = WebSocketEvent(
            event_type=EventType.ANALYSIS_PROGRESS,
            channel=Channel.ANALYSIS,
            data={
                "document": document,
                "current": current,
                "total": total,
                "progress": current / total if total > 0 else 0,
                "models_found": models_found or []
            }
        )
        await self.broadcast(event)
    
    async def broadcast_analysis_complete(self, document: str, result: Dict):
        """Broadcast analysis completion."""
        event = WebSocketEvent(
            event_type=EventType.ANALYSIS_COMPLETE,
            channel=Channel.ANALYSIS,
            data={
                "document": document,
                "status": "complete",
                "result": result
            }
        )
        await self.broadcast(event)
    
    async def broadcast_metric(self, metric_name: str, value: float, labels: Dict = None):
        """Broadcast a system metric."""
        event = WebSocketEvent(
            event_type=EventType.METRIC,
            channel=Channel.METRICS,
            data={
                "metric": metric_name,
                "value": value,
                "labels": labels or {}
            }
        )
        await self.broadcast(event)
    
    async def broadcast_failure_warning(self, model_id: int, failure_mode: str, context: str, severity: str = "medium"):
        """Broadcast a failure mode warning."""
        event = WebSocketEvent(
            event_type=EventType.FAILURE_WARNING,
            channel=Channel.FAILURES,
            data={
                "model_id": model_id,
                "failure_mode": failure_mode,
                "context": context,
                "severity": severity
            }
        )
        await self.broadcast(event)
    
    async def broadcast_system_status(self, status: Dict):
        """Broadcast system status update."""
        event = WebSocketEvent(
            event_type=EventType.SYSTEM_STATUS,
            channel=Channel.ALL,
            data=status
        )
        await self.broadcast(event)
    
    def on_event(self, event_type: str):
        """Decorator to register event handlers."""
        def decorator(func):
            if event_type not in self._event_handlers:
                self._event_handlers[event_type] = []
            self._event_handlers[event_type].append(func)
            return func
        return decorator
    
    async def start_heartbeat(self):
        """Start heartbeat loop."""
        self._running = True
        while self._running:
            await asyncio.sleep(self._heartbeat_interval)
            await self._send_heartbeat()
    
    async def _send_heartbeat(self):
        """Send heartbeat to all clients."""
        event = WebSocketEvent(
            event_type=EventType.HEARTBEAT,
            channel=Channel.ALL,
            data={
                "connected_clients": len(self._clients),
                "uptime": "running"
            }
        )
        await self.broadcast(event)
    
    def stop_heartbeat(self):
        """Stop heartbeat loop."""
        self._running = False
    
    def get_stats(self) -> Dict:
        """Get WebSocket server statistics."""
        return {
            "connected_clients": len(self._clients),
            "event_history_size": len(self._event_history),
            "channels": {
                channel.value: sum(
                    1 for c in self._clients.values()
                    if channel in c.subscriptions
                )
                for channel in Channel
            }
        }


# FastAPI integration
def create_websocket_router(manager: WebSocketManager):
    """Create FastAPI WebSocket router."""
    try:
        from fastapi import APIRouter, WebSocket, WebSocketDisconnect
        
        router = APIRouter()
        
        @router.websocket("/ws")
        async def websocket_endpoint(websocket: WebSocket):
            await websocket.accept()
            client_id = await manager.connect(websocket)
            
            try:
                while True:
                    data = await websocket.receive_text()
                    await manager.handle_message(client_id, data)
            except WebSocketDisconnect:
                manager.disconnect(client_id)
            except Exception as e:
                logger.error(f"WebSocket error: {e}")
                manager.disconnect(client_id)
        
        @router.get("/ws/stats")
        async def get_websocket_stats():
            return manager.get_stats()
        
        return router
    
    except ImportError:
        logger.warning("FastAPI not available, WebSocket router not created")
        return None


# Global manager instance
_manager: Optional[WebSocketManager] = None


def get_websocket_manager() -> WebSocketManager:
    """Get or create the global WebSocket manager."""
    global _manager
    if _manager is None:
        _manager = WebSocketManager()
    return _manager


# Example usage and integration with signal harvester
async def integrate_with_harvester():
    """Example integration with signal harvester."""
    manager = get_websocket_manager()
    
    # This would be called from the signal harvester when a signal is detected
    async def on_signal_detected(signal: Dict):
        await manager.broadcast_signal({
            "source": signal.get("source"),
            "content": signal.get("content"),
            "models": signal.get("models_detected", []),
            "lollapalooza_score": signal.get("lollapalooza_score", 0),
            "priority": signal.get("priority", "normal")
        })
        
        # Check for Lollapalooza
        if signal.get("lollapalooza_score", 0) >= 0.7:
            await manager.broadcast_lollapalooza({
                "source": signal.get("source"),
                "models": signal.get("models_detected", []),
                "score": signal.get("lollapalooza_score"),
                "description": f"Lollapalooza detected: {len(signal.get('models_detected', []))} models converging"
            })


if __name__ == "__main__":
    # Test the WebSocket manager
    import asyncio
    
    async def test():
        manager = WebSocketManager()
        
        print("WebSocket Manager Stats:")
        print(json.dumps(manager.get_stats(), indent=2))
        
        # Test event creation
        event = WebSocketEvent(
            event_type=EventType.SIGNAL,
            channel=Channel.SIGNALS,
            data={"test": "data"}
        )
        print(f"\nTest Event JSON:\n{event.to_json()}")
    
    asyncio.run(test())
