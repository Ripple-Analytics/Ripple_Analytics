"""
Webhook Manager for Mental Models System.

Provides real-time notifications via:
- Slack webhooks
- Discord webhooks
- Generic HTTP webhooks
- Email notifications (via webhook)

Features:
- Multiple webhook targets
- Event filtering
- Retry with exponential backoff
- Rate limiting
- Payload templating
"""

import asyncio
import json
import os
import hashlib
import hmac
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import List, Dict, Optional, Any, Callable
from enum import Enum
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class WebhookType(Enum):
    """Types of webhook targets."""
    SLACK = "slack"
    DISCORD = "discord"
    GENERIC = "generic"
    MATTERMOST = "mattermost"
    TEAMS = "teams"


class EventType(Enum):
    """Types of events that can trigger webhooks."""
    LOLLAPALOOZA_DETECTED = "lollapalooza_detected"
    HIGH_RISK_DECISION = "high_risk_decision"
    SIGNAL_HARVESTED = "signal_harvested"
    IMPROVEMENT_GENERATED = "improvement_generated"
    MODEL_EFFECTIVENESS_UPDATED = "model_effectiveness_updated"
    JOB_COMPLETED = "job_completed"
    JOB_FAILED = "job_failed"
    SYSTEM_ALERT = "system_alert"
    CUSTOM = "custom"


class EventPriority(Enum):
    """Event priority levels."""
    LOW = 1
    MEDIUM = 2
    HIGH = 3
    CRITICAL = 4


@dataclass
class WebhookTarget:
    """A webhook target configuration."""
    id: str
    name: str
    type: WebhookType
    url: str
    enabled: bool = True
    events: List[EventType] = field(default_factory=list)  # Empty = all events
    min_priority: EventPriority = EventPriority.LOW
    secret: Optional[str] = None  # For signature verification
    headers: Dict[str, str] = field(default_factory=dict)
    rate_limit_per_minute: int = 60
    retry_count: int = 3
    metadata: Dict = field(default_factory=dict)
    
    # Rate limiting state
    _request_times: List[datetime] = field(default_factory=list)
    
    def to_dict(self) -> Dict:
        return {
            "id": self.id,
            "name": self.name,
            "type": self.type.value,
            "url": self.url[:50] + "..." if len(self.url) > 50 else self.url,
            "enabled": self.enabled,
            "events": [e.value for e in self.events],
            "min_priority": self.min_priority.value,
            "rate_limit_per_minute": self.rate_limit_per_minute,
            "retry_count": self.retry_count
        }


@dataclass
class WebhookEvent:
    """An event to be sent via webhook."""
    type: EventType
    priority: EventPriority
    title: str
    message: str
    data: Dict = field(default_factory=dict)
    timestamp: datetime = field(default_factory=datetime.now)
    source: str = "mental_models_system"
    
    def to_dict(self) -> Dict:
        return {
            "type": self.type.value,
            "priority": self.priority.value,
            "title": self.title,
            "message": self.message,
            "data": self.data,
            "timestamp": self.timestamp.isoformat(),
            "source": self.source
        }


@dataclass
class WebhookDelivery:
    """Record of a webhook delivery attempt."""
    event_id: str
    target_id: str
    status: str  # "success", "failed", "pending"
    attempts: int
    response_code: Optional[int]
    response_body: Optional[str]
    error: Optional[str]
    delivered_at: Optional[datetime]
    
    def to_dict(self) -> Dict:
        return {
            "event_id": self.event_id,
            "target_id": self.target_id,
            "status": self.status,
            "attempts": self.attempts,
            "response_code": self.response_code,
            "delivered_at": self.delivered_at.isoformat() if self.delivered_at else None,
            "error": self.error
        }


class WebhookManager:
    """
    Manages webhook targets and event delivery.
    
    Usage:
        manager = WebhookManager()
        
        # Add a Slack webhook
        manager.add_target(WebhookTarget(
            id="slack_alerts",
            name="Slack Alerts",
            type=WebhookType.SLACK,
            url="https://hooks.slack.com/services/...",
            events=[EventType.LOLLAPALOOZA_DETECTED, EventType.HIGH_RISK_DECISION]
        ))
        
        # Send an event
        await manager.send_event(WebhookEvent(
            type=EventType.LOLLAPALOOZA_DETECTED,
            priority=EventPriority.HIGH,
            title="Lollapalooza Effect Detected",
            message="Multiple mental models converging on AAPL",
            data={"ticker": "AAPL", "score": 0.92}
        ))
    """
    
    def __init__(self, data_dir: str = None):
        self.data_dir = data_dir or os.path.join(
            os.path.dirname(__file__),
            "../../data/webhooks"
        )
        os.makedirs(self.data_dir, exist_ok=True)
        
        self.targets: Dict[str, WebhookTarget] = {}
        self.delivery_history: List[WebhookDelivery] = []
        
        # Load persisted targets
        self._load_targets()
    
    def _load_targets(self):
        """Load webhook targets from persistence."""
        targets_file = os.path.join(self.data_dir, "targets.json")
        if os.path.exists(targets_file):
            try:
                with open(targets_file) as f:
                    data = json.load(f)
                    for target_data in data.get("targets", []):
                        target = WebhookTarget(
                            id=target_data["id"],
                            name=target_data["name"],
                            type=WebhookType(target_data["type"]),
                            url=target_data["url"],
                            enabled=target_data.get("enabled", True),
                            events=[EventType(e) for e in target_data.get("events", [])],
                            min_priority=EventPriority(target_data.get("min_priority", 1)),
                            secret=target_data.get("secret"),
                            headers=target_data.get("headers", {}),
                            rate_limit_per_minute=target_data.get("rate_limit_per_minute", 60),
                            retry_count=target_data.get("retry_count", 3)
                        )
                        self.targets[target.id] = target
            except Exception as e:
                logger.error(f"Error loading webhook targets: {e}")
    
    def _save_targets(self):
        """Save webhook targets to persistence."""
        targets_file = os.path.join(self.data_dir, "targets.json")
        data = {
            "targets": [
                {
                    "id": t.id,
                    "name": t.name,
                    "type": t.type.value,
                    "url": t.url,
                    "enabled": t.enabled,
                    "events": [e.value for e in t.events],
                    "min_priority": t.min_priority.value,
                    "secret": t.secret,
                    "headers": t.headers,
                    "rate_limit_per_minute": t.rate_limit_per_minute,
                    "retry_count": t.retry_count
                }
                for t in self.targets.values()
            ],
            "updated_at": datetime.now().isoformat()
        }
        with open(targets_file, 'w') as f:
            json.dump(data, f, indent=2)
    
    def add_target(self, target: WebhookTarget) -> bool:
        """Add a webhook target."""
        self.targets[target.id] = target
        self._save_targets()
        logger.info(f"Added webhook target: {target.name}")
        return True
    
    def remove_target(self, target_id: str) -> bool:
        """Remove a webhook target."""
        if target_id in self.targets:
            del self.targets[target_id]
            self._save_targets()
            logger.info(f"Removed webhook target: {target_id}")
            return True
        return False
    
    def enable_target(self, target_id: str) -> bool:
        """Enable a webhook target."""
        if target_id in self.targets:
            self.targets[target_id].enabled = True
            self._save_targets()
            return True
        return False
    
    def disable_target(self, target_id: str) -> bool:
        """Disable a webhook target."""
        if target_id in self.targets:
            self.targets[target_id].enabled = False
            self._save_targets()
            return True
        return False
    
    def list_targets(self) -> List[WebhookTarget]:
        """List all webhook targets."""
        return list(self.targets.values())
    
    def _should_send_to_target(self, target: WebhookTarget, event: WebhookEvent) -> bool:
        """Check if an event should be sent to a target."""
        if not target.enabled:
            return False
        
        # Check priority
        if event.priority.value < target.min_priority.value:
            return False
        
        # Check event type filter
        if target.events and event.type not in target.events:
            return False
        
        # Check rate limit
        now = datetime.now()
        target._request_times = [
            t for t in target._request_times
            if now - t < timedelta(minutes=1)
        ]
        if len(target._request_times) >= target.rate_limit_per_minute:
            logger.warning(f"Rate limit exceeded for target: {target.name}")
            return False
        
        return True
    
    def _format_payload(self, target: WebhookTarget, event: WebhookEvent) -> Dict:
        """Format the webhook payload based on target type."""
        if target.type == WebhookType.SLACK:
            return self._format_slack_payload(event)
        elif target.type == WebhookType.DISCORD:
            return self._format_discord_payload(event)
        elif target.type == WebhookType.TEAMS:
            return self._format_teams_payload(event)
        else:
            return event.to_dict()
    
    def _format_slack_payload(self, event: WebhookEvent) -> Dict:
        """Format payload for Slack webhook."""
        priority_emoji = {
            EventPriority.LOW: "ℹ️",
            EventPriority.MEDIUM: "⚠️",
            EventPriority.HIGH: "🔶",
            EventPriority.CRITICAL: "🔴"
        }
        
        emoji = priority_emoji.get(event.priority, "📢")
        
        blocks = [
            {
                "type": "header",
                "text": {
                    "type": "plain_text",
                    "text": f"{emoji} {event.title}"
                }
            },
            {
                "type": "section",
                "text": {
                    "type": "mrkdwn",
                    "text": event.message
                }
            }
        ]
        
        # Add data fields
        if event.data:
            fields = []
            for key, value in list(event.data.items())[:10]:
                fields.append({
                    "type": "mrkdwn",
                    "text": f"*{key}:* {value}"
                })
            
            if fields:
                blocks.append({
                    "type": "section",
                    "fields": fields[:10]
                })
        
        # Add footer
        blocks.append({
            "type": "context",
            "elements": [
                {
                    "type": "mrkdwn",
                    "text": f"Event: `{event.type.value}` | Priority: `{event.priority.name}` | {event.timestamp.strftime('%Y-%m-%d %H:%M:%S')}"
                }
            ]
        })
        
        return {"blocks": blocks}
    
    def _format_discord_payload(self, event: WebhookEvent) -> Dict:
        """Format payload for Discord webhook."""
        color_map = {
            EventPriority.LOW: 0x3498db,      # Blue
            EventPriority.MEDIUM: 0xf1c40f,   # Yellow
            EventPriority.HIGH: 0xe67e22,     # Orange
            EventPriority.CRITICAL: 0xe74c3c  # Red
        }
        
        embed = {
            "title": event.title,
            "description": event.message,
            "color": color_map.get(event.priority, 0x95a5a6),
            "timestamp": event.timestamp.isoformat(),
            "footer": {
                "text": f"Event: {event.type.value} | Priority: {event.priority.name}"
            }
        }
        
        if event.data:
            embed["fields"] = [
                {"name": k, "value": str(v), "inline": True}
                for k, v in list(event.data.items())[:25]
            ]
        
        return {"embeds": [embed]}
    
    def _format_teams_payload(self, event: WebhookEvent) -> Dict:
        """Format payload for Microsoft Teams webhook."""
        return {
            "@type": "MessageCard",
            "@context": "http://schema.org/extensions",
            "themeColor": "0076D7",
            "summary": event.title,
            "sections": [{
                "activityTitle": event.title,
                "activitySubtitle": event.timestamp.strftime('%Y-%m-%d %H:%M:%S'),
                "text": event.message,
                "facts": [
                    {"name": k, "value": str(v)}
                    for k, v in list(event.data.items())[:10]
                ]
            }]
        }
    
    def _sign_payload(self, payload: bytes, secret: str) -> str:
        """Create HMAC signature for payload."""
        return hmac.new(
            secret.encode(),
            payload,
            hashlib.sha256
        ).hexdigest()
    
    async def _deliver_to_target(
        self, 
        target: WebhookTarget, 
        event: WebhookEvent,
        event_id: str
    ) -> WebhookDelivery:
        """Deliver an event to a specific target."""
        import aiohttp
        
        payload = self._format_payload(target, event)
        payload_bytes = json.dumps(payload).encode()
        
        headers = {
            "Content-Type": "application/json",
            **target.headers
        }
        
        # Add signature if secret is configured
        if target.secret:
            headers["X-Webhook-Signature"] = self._sign_payload(payload_bytes, target.secret)
        
        delivery = WebhookDelivery(
            event_id=event_id,
            target_id=target.id,
            status="pending",
            attempts=0,
            response_code=None,
            response_body=None,
            error=None,
            delivered_at=None
        )
        
        # Retry with exponential backoff
        for attempt in range(target.retry_count):
            delivery.attempts = attempt + 1
            
            try:
                async with aiohttp.ClientSession() as session:
                    async with session.post(
                        target.url,
                        data=payload_bytes,
                        headers=headers,
                        timeout=aiohttp.ClientTimeout(total=30)
                    ) as resp:
                        delivery.response_code = resp.status
                        delivery.response_body = await resp.text()
                        
                        if resp.status in [200, 201, 202, 204]:
                            delivery.status = "success"
                            delivery.delivered_at = datetime.now()
                            target._request_times.append(datetime.now())
                            logger.info(f"Webhook delivered to {target.name}")
                            break
                        else:
                            delivery.error = f"HTTP {resp.status}"
                            
            except asyncio.TimeoutError:
                delivery.error = "Request timeout"
            except Exception as e:
                delivery.error = str(e)
            
            # Wait before retry (exponential backoff)
            if attempt < target.retry_count - 1:
                await asyncio.sleep(2 ** attempt)
        
        if delivery.status != "success":
            delivery.status = "failed"
            logger.error(f"Webhook delivery failed to {target.name}: {delivery.error}")
        
        return delivery
    
    async def send_event(self, event: WebhookEvent) -> List[WebhookDelivery]:
        """Send an event to all matching webhook targets."""
        event_id = hashlib.sha256(
            f"{event.type.value}:{event.timestamp.isoformat()}:{event.title}".encode()
        ).hexdigest()[:16]
        
        deliveries = []
        
        # Find matching targets
        matching_targets = [
            t for t in self.targets.values()
            if self._should_send_to_target(t, event)
        ]
        
        if not matching_targets:
            logger.debug(f"No matching targets for event: {event.type.value}")
            return deliveries
        
        # Deliver to all matching targets concurrently
        tasks = [
            self._deliver_to_target(target, event, event_id)
            for target in matching_targets
        ]
        
        deliveries = await asyncio.gather(*tasks)
        
        # Record delivery history
        self.delivery_history.extend(deliveries)
        
        # Trim history to last 1000 entries
        if len(self.delivery_history) > 1000:
            self.delivery_history = self.delivery_history[-1000:]
        
        return deliveries
    
    def get_stats(self) -> Dict:
        """Get webhook statistics."""
        successful = sum(1 for d in self.delivery_history if d.status == "success")
        failed = sum(1 for d in self.delivery_history if d.status == "failed")
        
        return {
            "total_targets": len(self.targets),
            "enabled_targets": sum(1 for t in self.targets.values() if t.enabled),
            "total_deliveries": len(self.delivery_history),
            "successful_deliveries": successful,
            "failed_deliveries": failed,
            "success_rate": f"{successful / len(self.delivery_history):.2%}" if self.delivery_history else "N/A",
            "recent_deliveries": [d.to_dict() for d in self.delivery_history[-10:]]
        }


# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

_webhook_manager: Optional[WebhookManager] = None


def get_webhook_manager() -> WebhookManager:
    """Get the global webhook manager instance."""
    global _webhook_manager
    if _webhook_manager is None:
        _webhook_manager = WebhookManager()
    return _webhook_manager


async def send_alert(
    title: str,
    message: str,
    priority: EventPriority = EventPriority.MEDIUM,
    event_type: EventType = EventType.SYSTEM_ALERT,
    data: Dict = None
) -> List[WebhookDelivery]:
    """Send a quick alert via webhooks."""
    manager = get_webhook_manager()
    
    event = WebhookEvent(
        type=event_type,
        priority=priority,
        title=title,
        message=message,
        data=data or {}
    )
    
    return await manager.send_event(event)


async def send_lollapalooza_alert(
    title: str,
    message: str,
    score: float,
    models: List[str],
    data: Dict = None
) -> List[WebhookDelivery]:
    """Send a Lollapalooza detection alert."""
    manager = get_webhook_manager()
    
    event = WebhookEvent(
        type=EventType.LOLLAPALOOZA_DETECTED,
        priority=EventPriority.HIGH if score > 0.8 else EventPriority.MEDIUM,
        title=title,
        message=message,
        data={
            "score": score,
            "models": models,
            **(data or {})
        }
    )
    
    return await manager.send_event(event)


if __name__ == "__main__":
    # Test the webhook manager
    async def test():
        manager = WebhookManager()
        
        # Add a test target (won't actually send)
        manager.add_target(WebhookTarget(
            id="test_slack",
            name="Test Slack",
            type=WebhookType.SLACK,
            url="https://hooks.slack.com/services/test",
            events=[EventType.LOLLAPALOOZA_DETECTED]
        ))
        
        print("Webhook Targets:")
        for target in manager.list_targets():
            print(f"  - {target.name}: {target.type.value}")
        
        print(f"\nStats: {manager.get_stats()}")
    
    asyncio.run(test())
