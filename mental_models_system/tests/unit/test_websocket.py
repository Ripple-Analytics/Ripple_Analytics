"""
Unit tests for WebSocket server.
"""
import pytest
import asyncio
import json
from unittest.mock import AsyncMock, MagicMock
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from api.websocket_server import (
    WebSocketManager, WebSocketEvent, WebSocketClient,
    EventType, Channel, get_websocket_manager
)


class TestWebSocketEvent:
    """Tests for WebSocketEvent."""
    
    def test_event_creation(self):
        """Test creating a WebSocket event."""
        event = WebSocketEvent(
            event_type=EventType.SIGNAL,
            channel=Channel.SIGNALS,
            data={"test": "data"}
        )
        
        assert event.event_type == EventType.SIGNAL
        assert event.channel == Channel.SIGNALS
        assert event.data == {"test": "data"}
        assert event.event_id is not None
    
    def test_event_to_json(self):
        """Test event serialization to JSON."""
        event = WebSocketEvent(
            event_type=EventType.LOLLAPALOOZA,
            channel=Channel.LOLLAPALOOZA,
            data={"models": ["Model1", "Model2"]}
        )
        
        json_str = event.to_json()
        parsed = json.loads(json_str)
        
        assert parsed["type"] == "lollapalooza"
        assert parsed["channel"] == "lollapalooza"
        assert parsed["data"]["models"] == ["Model1", "Model2"]
        assert "timestamp" in parsed
        assert "event_id" in parsed


class TestEventType:
    """Tests for EventType enum."""
    
    def test_event_types(self):
        """Test all event types exist."""
        assert EventType.SIGNAL.value == "signal"
        assert EventType.LOLLAPALOOZA.value == "lollapalooza"
        assert EventType.ANALYSIS_START.value == "analysis_start"
        assert EventType.ANALYSIS_PROGRESS.value == "analysis_progress"
        assert EventType.ANALYSIS_COMPLETE.value == "analysis_complete"
        assert EventType.METRIC.value == "metric"
        assert EventType.FAILURE_WARNING.value == "failure_warning"


class TestChannel:
    """Tests for Channel enum."""
    
    def test_channels(self):
        """Test all channels exist."""
        assert Channel.SIGNALS.value == "signals"
        assert Channel.LOLLAPALOOZA.value == "lollapalooza"
        assert Channel.ANALYSIS.value == "analysis"
        assert Channel.METRICS.value == "metrics"
        assert Channel.FAILURES.value == "failures"
        assert Channel.ALL.value == "all"


class TestWebSocketManager:
    """Tests for WebSocketManager."""
    
    @pytest.fixture
    def manager(self):
        """Create a fresh manager for each test."""
        return WebSocketManager()
    
    @pytest.fixture
    def mock_websocket(self):
        """Create a mock WebSocket connection."""
        ws = AsyncMock()
        ws.send_text = AsyncMock()
        return ws
    
    @pytest.mark.asyncio
    async def test_connect_client(self, manager, mock_websocket):
        """Test connecting a client."""
        client_id = await manager.connect(mock_websocket)
        
        assert client_id is not None
        assert len(client_id) > 0
        assert client_id in manager._clients
        
        # Should have sent welcome message
        mock_websocket.send_text.assert_called_once()
        welcome = json.loads(mock_websocket.send_text.call_args[0][0])
        assert welcome["type"] == "connected"
        assert welcome["client_id"] == client_id
    
    @pytest.mark.asyncio
    async def test_disconnect_client(self, manager, mock_websocket):
        """Test disconnecting a client."""
        client_id = await manager.connect(mock_websocket)
        assert client_id in manager._clients
        
        manager.disconnect(client_id)
        assert client_id not in manager._clients
    
    @pytest.mark.asyncio
    async def test_subscribe_to_channel(self, manager, mock_websocket):
        """Test subscribing to a channel."""
        client_id = await manager.connect(mock_websocket)
        
        await manager.handle_message(client_id, json.dumps({
            "action": "subscribe",
            "channels": ["signals", "lollapalooza"]
        }))
        
        client = manager._clients[client_id]
        assert Channel.SIGNALS in client.subscriptions
        assert Channel.LOLLAPALOOZA in client.subscriptions
    
    @pytest.mark.asyncio
    async def test_unsubscribe_from_channel(self, manager, mock_websocket):
        """Test unsubscribing from a channel."""
        client_id = await manager.connect(mock_websocket)
        
        # Subscribe first
        await manager.handle_message(client_id, json.dumps({
            "action": "subscribe",
            "channels": ["signals", "lollapalooza"]
        }))
        
        # Then unsubscribe
        await manager.handle_message(client_id, json.dumps({
            "action": "unsubscribe",
            "channels": ["signals"]
        }))
        
        client = manager._clients[client_id]
        assert Channel.SIGNALS not in client.subscriptions
        assert Channel.LOLLAPALOOZA in client.subscriptions
    
    @pytest.mark.asyncio
    async def test_ping_pong(self, manager, mock_websocket):
        """Test ping/pong heartbeat."""
        client_id = await manager.connect(mock_websocket)
        mock_websocket.send_text.reset_mock()
        
        await manager.handle_message(client_id, json.dumps({
            "action": "ping"
        }))
        
        # Should have sent pong
        mock_websocket.send_text.assert_called_once()
        response = json.loads(mock_websocket.send_text.call_args[0][0])
        assert response["type"] == "pong"
    
    @pytest.mark.asyncio
    async def test_broadcast_signal(self, manager, mock_websocket):
        """Test broadcasting a signal."""
        client_id = await manager.connect(mock_websocket)
        mock_websocket.send_text.reset_mock()
        
        await manager.broadcast_signal({
            "source": "SEC",
            "content": "New filing"
        })
        
        # Should have received the signal
        mock_websocket.send_text.assert_called_once()
        event = json.loads(mock_websocket.send_text.call_args[0][0])
        assert event["type"] == "signal"
        assert event["data"]["source"] == "SEC"
    
    @pytest.mark.asyncio
    async def test_broadcast_lollapalooza(self, manager, mock_websocket):
        """Test broadcasting a Lollapalooza detection."""
        client_id = await manager.connect(mock_websocket)
        mock_websocket.send_text.reset_mock()
        
        await manager.broadcast_lollapalooza({
            "models": ["Model1", "Model2", "Model3"],
            "score": 0.85
        })
        
        mock_websocket.send_text.assert_called_once()
        event = json.loads(mock_websocket.send_text.call_args[0][0])
        assert event["type"] == "lollapalooza"
        assert len(event["data"]["models"]) == 3
    
    @pytest.mark.asyncio
    async def test_broadcast_analysis_progress(self, manager, mock_websocket):
        """Test broadcasting analysis progress."""
        client_id = await manager.connect(mock_websocket)
        mock_websocket.send_text.reset_mock()
        
        await manager.broadcast_analysis_progress(
            document="test.pdf",
            current=5,
            total=10,
            models_found=["Model1"]
        )
        
        mock_websocket.send_text.assert_called_once()
        event = json.loads(mock_websocket.send_text.call_args[0][0])
        assert event["type"] == "analysis_progress"
        assert event["data"]["progress"] == 0.5
    
    @pytest.mark.asyncio
    async def test_broadcast_failure_warning(self, manager, mock_websocket):
        """Test broadcasting a failure warning."""
        client_id = await manager.connect(mock_websocket)
        mock_websocket.send_text.reset_mock()
        
        await manager.broadcast_failure_warning(
            model_id=1,
            failure_mode="Confirmation Bias",
            context="Investment analysis",
            severity="high"
        )
        
        mock_websocket.send_text.assert_called_once()
        event = json.loads(mock_websocket.send_text.call_args[0][0])
        assert event["type"] == "failure_warning"
        assert event["data"]["severity"] == "high"
    
    @pytest.mark.asyncio
    async def test_channel_filtering(self, manager, mock_websocket):
        """Test that clients only receive events for subscribed channels."""
        client_id = await manager.connect(mock_websocket)
        
        # Subscribe only to signals
        await manager.handle_message(client_id, json.dumps({
            "action": "subscribe",
            "channels": ["signals"]
        }))
        
        # Unsubscribe from all (default)
        manager._clients[client_id].subscriptions = {Channel.SIGNALS}
        
        mock_websocket.send_text.reset_mock()
        
        # Broadcast to lollapalooza channel - should not receive
        event = WebSocketEvent(
            event_type=EventType.LOLLAPALOOZA,
            channel=Channel.LOLLAPALOOZA,
            data={"test": "data"}
        )
        await manager.broadcast(event)
        
        # Should not have received (not subscribed to lollapalooza)
        mock_websocket.send_text.assert_not_called()
        
        # Broadcast to signals channel - should receive
        event = WebSocketEvent(
            event_type=EventType.SIGNAL,
            channel=Channel.SIGNALS,
            data={"test": "data"}
        )
        await manager.broadcast(event)
        
        mock_websocket.send_text.assert_called_once()
    
    @pytest.mark.asyncio
    async def test_event_history(self, manager, mock_websocket):
        """Test event history is maintained."""
        # Broadcast some events
        for i in range(5):
            await manager.broadcast_signal({"index": i})
        
        assert len(manager._event_history) == 5
    
    @pytest.mark.asyncio
    async def test_get_history(self, manager, mock_websocket):
        """Test getting event history."""
        client_id = await manager.connect(mock_websocket)
        
        # Broadcast some events
        for i in range(5):
            await manager.broadcast_signal({"index": i})
        
        mock_websocket.send_text.reset_mock()
        
        await manager.handle_message(client_id, json.dumps({
            "action": "get_history",
            "channel": "signals",
            "limit": 3
        }))
        
        mock_websocket.send_text.assert_called_once()
        response = json.loads(mock_websocket.send_text.call_args[0][0])
        assert response["type"] == "history"
        assert len(response["events"]) == 3
    
    def test_get_stats(self, manager):
        """Test getting server statistics."""
        stats = manager.get_stats()
        
        assert "connected_clients" in stats
        assert "event_history_size" in stats
        assert "channels" in stats
        assert stats["connected_clients"] == 0
    
    @pytest.mark.asyncio
    async def test_stats_with_clients(self, manager, mock_websocket):
        """Test stats with connected clients."""
        await manager.connect(mock_websocket)
        
        stats = manager.get_stats()
        assert stats["connected_clients"] == 1
    
    @pytest.mark.asyncio
    async def test_invalid_json_message(self, manager, mock_websocket):
        """Test handling invalid JSON message."""
        client_id = await manager.connect(mock_websocket)
        mock_websocket.send_text.reset_mock()
        
        await manager.handle_message(client_id, "not valid json")
        
        mock_websocket.send_text.assert_called_once()
        response = json.loads(mock_websocket.send_text.call_args[0][0])
        assert response["type"] == "error"
        assert "Invalid JSON" in response["message"]
    
    @pytest.mark.asyncio
    async def test_unknown_action(self, manager, mock_websocket):
        """Test handling unknown action."""
        client_id = await manager.connect(mock_websocket)
        mock_websocket.send_text.reset_mock()
        
        await manager.handle_message(client_id, json.dumps({
            "action": "unknown_action"
        }))
        
        mock_websocket.send_text.assert_called_once()
        response = json.loads(mock_websocket.send_text.call_args[0][0])
        assert response["type"] == "error"


class TestGlobalManager:
    """Tests for global manager instance."""
    
    def test_get_websocket_manager(self):
        """Test getting global manager."""
        manager1 = get_websocket_manager()
        manager2 = get_websocket_manager()
        
        # Should return same instance
        assert manager1 is manager2


class TestEventHandlers:
    """Tests for event handlers."""
    
    @pytest.mark.asyncio
    async def test_event_handler_decorator(self):
        """Test registering event handlers."""
        manager = WebSocketManager()
        handler_called = False
        
        @manager.on_event("signal")
        async def handle_signal(event):
            nonlocal handler_called
            handler_called = True
        
        await manager.broadcast_signal({"test": "data"})
        
        assert handler_called
    
    @pytest.mark.asyncio
    async def test_multiple_handlers(self):
        """Test multiple handlers for same event."""
        manager = WebSocketManager()
        call_count = 0
        
        @manager.on_event("signal")
        async def handler1(event):
            nonlocal call_count
            call_count += 1
        
        @manager.on_event("signal")
        async def handler2(event):
            nonlocal call_count
            call_count += 1
        
        await manager.broadcast_signal({"test": "data"})
        
        assert call_count == 2
