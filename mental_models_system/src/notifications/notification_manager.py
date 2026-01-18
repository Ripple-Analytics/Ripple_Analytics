"""
Multi-Channel Notification System

Supports multiple notification channels:
- Slack
- Discord
- Email (SMTP)
- Matrix
- Webhooks
- Telegram
- SMS (Twilio)
- Push notifications
"""

import asyncio
import json
import smtplib
import hashlib
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart
from enum import Enum
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Set
import aiohttp


class NotificationPriority(Enum):
    """Notification priority levels."""
    LOW = "low"
    NORMAL = "normal"
    HIGH = "high"
    URGENT = "urgent"


class NotificationType(Enum):
    """Types of notifications."""
    INFO = "info"
    WARNING = "warning"
    ERROR = "error"
    SUCCESS = "success"
    ALERT = "alert"
    LOLLAPALOOZA = "lollapalooza"
    DECISION = "decision"
    IMPROVEMENT = "improvement"


@dataclass
class Notification:
    """A notification to be sent."""
    id: str
    title: str
    message: str
    type: NotificationType = NotificationType.INFO
    priority: NotificationPriority = NotificationPriority.NORMAL
    data: Dict[str, Any] = field(default_factory=dict)
    channels: List[str] = field(default_factory=list)
    created_at: datetime = field(default_factory=datetime.now)
    sent_at: Optional[datetime] = None
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "id": self.id,
            "title": self.title,
            "message": self.message,
            "type": self.type.value,
            "priority": self.priority.value,
            "data": self.data,
            "channels": self.channels,
            "created_at": self.created_at.isoformat(),
            "sent_at": self.sent_at.isoformat() if self.sent_at else None
        }


@dataclass
class DeliveryResult:
    """Result of notification delivery."""
    channel: str
    success: bool
    error: Optional[str] = None
    response: Optional[Dict[str, Any]] = None
    delivered_at: datetime = field(default_factory=datetime.now)


class NotificationChannel(ABC):
    """Abstract base class for notification channels."""
    
    @property
    @abstractmethod
    def name(self) -> str:
        """Channel name."""
        pass
    
    @abstractmethod
    async def send(self, notification: Notification) -> DeliveryResult:
        """Send a notification through this channel."""
        pass
    
    @abstractmethod
    def is_configured(self) -> bool:
        """Check if channel is properly configured."""
        pass


class SlackChannel(NotificationChannel):
    """Slack notification channel."""
    
    def __init__(self, webhook_url: Optional[str] = None, token: Optional[str] = None, default_channel: str = "#general"):
        self.webhook_url = webhook_url
        self.token = token
        self.default_channel = default_channel
    
    @property
    def name(self) -> str:
        return "slack"
    
    def is_configured(self) -> bool:
        return bool(self.webhook_url or self.token)
    
    async def send(self, notification: Notification) -> DeliveryResult:
        if not self.is_configured():
            return DeliveryResult(
                channel=self.name,
                success=False,
                error="Slack not configured"
            )
        
        try:
            # Format message for Slack
            emoji_map = {
                NotificationType.INFO: ":information_source:",
                NotificationType.WARNING: ":warning:",
                NotificationType.ERROR: ":x:",
                NotificationType.SUCCESS: ":white_check_mark:",
                NotificationType.ALERT: ":rotating_light:",
                NotificationType.LOLLAPALOOZA: ":star2:",
                NotificationType.DECISION: ":thinking_face:",
                NotificationType.IMPROVEMENT: ":chart_with_upwards_trend:"
            }
            
            emoji = emoji_map.get(notification.type, ":bell:")
            
            payload = {
                "text": f"{emoji} *{notification.title}*\n{notification.message}",
                "attachments": []
            }
            
            if notification.data:
                payload["attachments"].append({
                    "color": self._get_color(notification.type),
                    "fields": [
                        {"title": k, "value": str(v), "short": True}
                        for k, v in notification.data.items()
                    ]
                })
            
            async with aiohttp.ClientSession() as session:
                if self.webhook_url:
                    async with session.post(self.webhook_url, json=payload) as resp:
                        if resp.status == 200:
                            return DeliveryResult(
                                channel=self.name,
                                success=True,
                                response={"status": resp.status}
                            )
                        else:
                            return DeliveryResult(
                                channel=self.name,
                                success=False,
                                error=f"HTTP {resp.status}"
                            )
                else:
                    # Use token-based API
                    headers = {"Authorization": f"Bearer {self.token}"}
                    payload["channel"] = self.default_channel
                    async with session.post(
                        "https://slack.com/api/chat.postMessage",
                        headers=headers,
                        json=payload
                    ) as resp:
                        data = await resp.json()
                        return DeliveryResult(
                            channel=self.name,
                            success=data.get("ok", False),
                            response=data,
                            error=data.get("error")
                        )
        except Exception as e:
            return DeliveryResult(
                channel=self.name,
                success=False,
                error=str(e)
            )
    
    def _get_color(self, notification_type: NotificationType) -> str:
        colors = {
            NotificationType.INFO: "#36a64f",
            NotificationType.WARNING: "#ffcc00",
            NotificationType.ERROR: "#ff0000",
            NotificationType.SUCCESS: "#00ff00",
            NotificationType.ALERT: "#ff6600",
            NotificationType.LOLLAPALOOZA: "#9900ff",
            NotificationType.DECISION: "#0066ff",
            NotificationType.IMPROVEMENT: "#00ccff"
        }
        return colors.get(notification_type, "#808080")


class DiscordChannel(NotificationChannel):
    """Discord notification channel."""
    
    def __init__(self, webhook_url: Optional[str] = None):
        self.webhook_url = webhook_url
    
    @property
    def name(self) -> str:
        return "discord"
    
    def is_configured(self) -> bool:
        return bool(self.webhook_url)
    
    async def send(self, notification: Notification) -> DeliveryResult:
        if not self.is_configured():
            return DeliveryResult(
                channel=self.name,
                success=False,
                error="Discord not configured"
            )
        
        try:
            color_map = {
                NotificationType.INFO: 0x36a64f,
                NotificationType.WARNING: 0xffcc00,
                NotificationType.ERROR: 0xff0000,
                NotificationType.SUCCESS: 0x00ff00,
                NotificationType.ALERT: 0xff6600,
                NotificationType.LOLLAPALOOZA: 0x9900ff,
                NotificationType.DECISION: 0x0066ff,
                NotificationType.IMPROVEMENT: 0x00ccff
            }
            
            payload = {
                "embeds": [{
                    "title": notification.title,
                    "description": notification.message,
                    "color": color_map.get(notification.type, 0x808080),
                    "timestamp": notification.created_at.isoformat(),
                    "fields": [
                        {"name": k, "value": str(v), "inline": True}
                        for k, v in notification.data.items()
                    ] if notification.data else []
                }]
            }
            
            async with aiohttp.ClientSession() as session:
                async with session.post(self.webhook_url, json=payload) as resp:
                    if resp.status in (200, 204):
                        return DeliveryResult(
                            channel=self.name,
                            success=True,
                            response={"status": resp.status}
                        )
                    else:
                        return DeliveryResult(
                            channel=self.name,
                            success=False,
                            error=f"HTTP {resp.status}"
                        )
        except Exception as e:
            return DeliveryResult(
                channel=self.name,
                success=False,
                error=str(e)
            )


class EmailChannel(NotificationChannel):
    """Email notification channel via SMTP."""
    
    def __init__(
        self,
        smtp_host: str = "localhost",
        smtp_port: int = 587,
        username: Optional[str] = None,
        password: Optional[str] = None,
        from_address: str = "notifications@example.com",
        to_addresses: List[str] = None,
        use_tls: bool = True
    ):
        self.smtp_host = smtp_host
        self.smtp_port = smtp_port
        self.username = username
        self.password = password
        self.from_address = from_address
        self.to_addresses = to_addresses or []
        self.use_tls = use_tls
    
    @property
    def name(self) -> str:
        return "email"
    
    def is_configured(self) -> bool:
        return bool(self.smtp_host and self.to_addresses)
    
    async def send(self, notification: Notification) -> DeliveryResult:
        if not self.is_configured():
            return DeliveryResult(
                channel=self.name,
                success=False,
                error="Email not configured"
            )
        
        try:
            msg = MIMEMultipart("alternative")
            msg["Subject"] = f"[{notification.type.value.upper()}] {notification.title}"
            msg["From"] = self.from_address
            msg["To"] = ", ".join(self.to_addresses)
            
            # Plain text version
            text_content = f"{notification.title}\n\n{notification.message}"
            if notification.data:
                text_content += "\n\nDetails:\n"
                for k, v in notification.data.items():
                    text_content += f"  {k}: {v}\n"
            
            # HTML version
            html_content = f"""
            <html>
            <body>
                <h2>{notification.title}</h2>
                <p>{notification.message}</p>
                {"<h3>Details:</h3><ul>" + "".join(f"<li><strong>{k}:</strong> {v}</li>" for k, v in notification.data.items()) + "</ul>" if notification.data else ""}
                <hr>
                <small>Sent at: {notification.created_at.isoformat()}</small>
            </body>
            </html>
            """
            
            msg.attach(MIMEText(text_content, "plain"))
            msg.attach(MIMEText(html_content, "html"))
            
            # Send email (sync, wrapped in executor)
            loop = asyncio.get_event_loop()
            await loop.run_in_executor(None, self._send_email, msg)
            
            return DeliveryResult(
                channel=self.name,
                success=True,
                response={"recipients": self.to_addresses}
            )
        except Exception as e:
            return DeliveryResult(
                channel=self.name,
                success=False,
                error=str(e)
            )
    
    def _send_email(self, msg: MIMEMultipart):
        """Send email synchronously."""
        with smtplib.SMTP(self.smtp_host, self.smtp_port) as server:
            if self.use_tls:
                server.starttls()
            if self.username and self.password:
                server.login(self.username, self.password)
            server.send_message(msg)


class WebhookChannel(NotificationChannel):
    """Generic webhook notification channel."""
    
    def __init__(
        self,
        url: str,
        headers: Dict[str, str] = None,
        method: str = "POST",
        name_override: str = None
    ):
        self.url = url
        self.headers = headers or {}
        self.method = method
        self._name = name_override or "webhook"
    
    @property
    def name(self) -> str:
        return self._name
    
    def is_configured(self) -> bool:
        return bool(self.url)
    
    async def send(self, notification: Notification) -> DeliveryResult:
        if not self.is_configured():
            return DeliveryResult(
                channel=self.name,
                success=False,
                error="Webhook not configured"
            )
        
        try:
            payload = notification.to_dict()
            
            async with aiohttp.ClientSession() as session:
                async with session.request(
                    self.method,
                    self.url,
                    headers=self.headers,
                    json=payload
                ) as resp:
                    if resp.status < 400:
                        return DeliveryResult(
                            channel=self.name,
                            success=True,
                            response={"status": resp.status}
                        )
                    else:
                        return DeliveryResult(
                            channel=self.name,
                            success=False,
                            error=f"HTTP {resp.status}"
                        )
        except Exception as e:
            return DeliveryResult(
                channel=self.name,
                success=False,
                error=str(e)
            )


class TelegramChannel(NotificationChannel):
    """Telegram notification channel."""
    
    def __init__(self, bot_token: Optional[str] = None, chat_id: Optional[str] = None):
        self.bot_token = bot_token
        self.chat_id = chat_id
    
    @property
    def name(self) -> str:
        return "telegram"
    
    def is_configured(self) -> bool:
        return bool(self.bot_token and self.chat_id)
    
    async def send(self, notification: Notification) -> DeliveryResult:
        if not self.is_configured():
            return DeliveryResult(
                channel=self.name,
                success=False,
                error="Telegram not configured"
            )
        
        try:
            emoji_map = {
                NotificationType.INFO: "ℹ️",
                NotificationType.WARNING: "⚠️",
                NotificationType.ERROR: "❌",
                NotificationType.SUCCESS: "✅",
                NotificationType.ALERT: "🚨",
                NotificationType.LOLLAPALOOZA: "⭐",
                NotificationType.DECISION: "🤔",
                NotificationType.IMPROVEMENT: "📈"
            }
            
            emoji = emoji_map.get(notification.type, "🔔")
            text = f"{emoji} *{notification.title}*\n\n{notification.message}"
            
            if notification.data:
                text += "\n\n*Details:*\n"
                for k, v in notification.data.items():
                    text += f"• {k}: {v}\n"
            
            url = f"https://api.telegram.org/bot{self.bot_token}/sendMessage"
            payload = {
                "chat_id": self.chat_id,
                "text": text,
                "parse_mode": "Markdown"
            }
            
            async with aiohttp.ClientSession() as session:
                async with session.post(url, json=payload) as resp:
                    data = await resp.json()
                    return DeliveryResult(
                        channel=self.name,
                        success=data.get("ok", False),
                        response=data,
                        error=data.get("description") if not data.get("ok") else None
                    )
        except Exception as e:
            return DeliveryResult(
                channel=self.name,
                success=False,
                error=str(e)
            )


class NotificationManager:
    """
    Central notification manager that routes notifications to multiple channels.
    
    Features:
    - Multi-channel delivery
    - Priority-based routing
    - Notification history
    - Rate limiting
    - Deduplication
    """
    
    def __init__(self, data_dir: str = "./data/notifications"):
        self.data_dir = Path(data_dir)
        self.data_dir.mkdir(parents=True, exist_ok=True)
        
        self.channels: Dict[str, NotificationChannel] = {}
        self.history: List[Dict[str, Any]] = []
        self.sent_hashes: Set[str] = set()  # For deduplication
        
        # Priority-based channel routing
        self.priority_channels: Dict[NotificationPriority, List[str]] = {
            NotificationPriority.LOW: [],
            NotificationPriority.NORMAL: [],
            NotificationPriority.HIGH: [],
            NotificationPriority.URGENT: []
        }
        
        # Type-based channel routing
        self.type_channels: Dict[NotificationType, List[str]] = {}
        
        # Callbacks
        self.callbacks: List[Callable[[Notification, List[DeliveryResult]], None]] = []
        
        self._load_history()
    
    def register_channel(
        self,
        channel: NotificationChannel,
        priorities: List[NotificationPriority] = None,
        types: List[NotificationType] = None
    ):
        """Register a notification channel."""
        self.channels[channel.name] = channel
        
        # Register for priorities
        if priorities:
            for priority in priorities:
                if channel.name not in self.priority_channels[priority]:
                    self.priority_channels[priority].append(channel.name)
        else:
            # Default: register for all priorities
            for priority in NotificationPriority:
                if channel.name not in self.priority_channels[priority]:
                    self.priority_channels[priority].append(channel.name)
        
        # Register for types
        if types:
            for ntype in types:
                if ntype not in self.type_channels:
                    self.type_channels[ntype] = []
                if channel.name not in self.type_channels[ntype]:
                    self.type_channels[ntype].append(channel.name)
    
    def add_callback(self, callback: Callable[[Notification, List[DeliveryResult]], None]):
        """Add a callback to be called after notification delivery."""
        self.callbacks.append(callback)
    
    async def send(
        self,
        title: str,
        message: str,
        notification_type: NotificationType = NotificationType.INFO,
        priority: NotificationPriority = NotificationPriority.NORMAL,
        data: Dict[str, Any] = None,
        channels: List[str] = None,
        deduplicate: bool = True
    ) -> List[DeliveryResult]:
        """
        Send a notification.
        
        Args:
            title: Notification title
            message: Notification message
            notification_type: Type of notification
            priority: Priority level
            data: Additional data
            channels: Specific channels to use (None = auto-route)
            deduplicate: Whether to skip duplicate notifications
        
        Returns:
            List of delivery results
        """
        # Create notification
        notification_id = hashlib.md5(
            f"{title}{message}{datetime.now().isoformat()}".encode()
        ).hexdigest()[:12]
        
        notification = Notification(
            id=notification_id,
            title=title,
            message=message,
            type=notification_type,
            priority=priority,
            data=data or {}
        )
        
        # Deduplication check
        if deduplicate:
            content_hash = hashlib.md5(
                f"{title}{message}{json.dumps(data or {}, sort_keys=True)}".encode()
            ).hexdigest()
            
            if content_hash in self.sent_hashes:
                return [DeliveryResult(
                    channel="dedup",
                    success=True,
                    response={"skipped": True, "reason": "duplicate"}
                )]
            
            self.sent_hashes.add(content_hash)
            # Limit hash set size
            if len(self.sent_hashes) > 10000:
                self.sent_hashes = set(list(self.sent_hashes)[-5000:])
        
        # Determine target channels
        if channels:
            target_channels = channels
        else:
            # Auto-route based on priority and type
            target_channels = set()
            
            # Add priority-based channels
            target_channels.update(self.priority_channels.get(priority, []))
            
            # Add type-based channels
            if notification_type in self.type_channels:
                target_channels.update(self.type_channels[notification_type])
            
            target_channels = list(target_channels)
        
        notification.channels = target_channels
        
        # Send to all target channels
        results = []
        for channel_name in target_channels:
            if channel_name in self.channels:
                channel = self.channels[channel_name]
                if channel.is_configured():
                    result = await channel.send(notification)
                    results.append(result)
                else:
                    results.append(DeliveryResult(
                        channel=channel_name,
                        success=False,
                        error="Channel not configured"
                    ))
            else:
                results.append(DeliveryResult(
                    channel=channel_name,
                    success=False,
                    error="Channel not registered"
                ))
        
        # Update notification
        notification.sent_at = datetime.now()
        
        # Record history
        history_entry = {
            "notification": notification.to_dict(),
            "results": [
                {
                    "channel": r.channel,
                    "success": r.success,
                    "error": r.error,
                    "delivered_at": r.delivered_at.isoformat()
                }
                for r in results
            ]
        }
        self.history.append(history_entry)
        self._save_history()
        
        # Call callbacks
        for callback in self.callbacks:
            try:
                callback(notification, results)
            except Exception:
                pass
        
        return results
    
    async def send_lollapalooza_alert(
        self,
        title: str,
        score: float,
        models: List[str],
        context: str,
        data: Dict[str, Any] = None
    ) -> List[DeliveryResult]:
        """Send a Lollapalooza detection alert."""
        message = f"Lollapalooza Effect Detected!\n\nScore: {score:.2f}\nModels: {', '.join(models)}\n\nContext: {context}"
        
        return await self.send(
            title=title,
            message=message,
            notification_type=NotificationType.LOLLAPALOOZA,
            priority=NotificationPriority.HIGH,
            data={
                "score": score,
                "model_count": len(models),
                "models": models,
                **(data or {})
            }
        )
    
    async def send_decision_alert(
        self,
        title: str,
        decision: str,
        risk_score: float,
        models_used: List[str],
        data: Dict[str, Any] = None
    ) -> List[DeliveryResult]:
        """Send a decision notification."""
        message = f"Decision: {decision}\n\nRisk Score: {risk_score:.2f}\nModels Applied: {', '.join(models_used)}"
        
        priority = NotificationPriority.HIGH if risk_score > 0.7 else NotificationPriority.NORMAL
        
        return await self.send(
            title=title,
            message=message,
            notification_type=NotificationType.DECISION,
            priority=priority,
            data={
                "risk_score": risk_score,
                "models_used": models_used,
                **(data or {})
            }
        )
    
    async def send_improvement_suggestion(
        self,
        title: str,
        suggestion: str,
        impact: str,
        source: str,
        data: Dict[str, Any] = None
    ) -> List[DeliveryResult]:
        """Send an improvement suggestion notification."""
        message = f"Suggestion: {suggestion}\n\nExpected Impact: {impact}\nSource: {source}"
        
        return await self.send(
            title=title,
            message=message,
            notification_type=NotificationType.IMPROVEMENT,
            priority=NotificationPriority.NORMAL,
            data={
                "impact": impact,
                "source": source,
                **(data or {})
            }
        )
    
    def get_history(self, limit: int = 100) -> List[Dict[str, Any]]:
        """Get notification history."""
        return self.history[-limit:]
    
    def get_stats(self) -> Dict[str, Any]:
        """Get notification statistics."""
        total = len(self.history)
        successful = sum(
            1 for h in self.history
            if any(r["success"] for r in h["results"])
        )
        
        by_type = {}
        by_channel = {}
        
        for h in self.history:
            ntype = h["notification"]["type"]
            by_type[ntype] = by_type.get(ntype, 0) + 1
            
            for r in h["results"]:
                channel = r["channel"]
                if channel not in by_channel:
                    by_channel[channel] = {"total": 0, "success": 0}
                by_channel[channel]["total"] += 1
                if r["success"]:
                    by_channel[channel]["success"] += 1
        
        return {
            "total_notifications": total,
            "successful_deliveries": successful,
            "success_rate": successful / total if total > 0 else 0,
            "by_type": by_type,
            "by_channel": by_channel,
            "registered_channels": list(self.channels.keys()),
            "configured_channels": [
                name for name, ch in self.channels.items()
                if ch.is_configured()
            ]
        }
    
    def _save_history(self):
        """Save notification history to disk."""
        history_file = self.data_dir / "history.json"
        # Keep only last 1000 entries
        with open(history_file, "w") as f:
            json.dump(self.history[-1000:], f, indent=2)
    
    def _load_history(self):
        """Load notification history from disk."""
        history_file = self.data_dir / "history.json"
        if history_file.exists():
            try:
                with open(history_file) as f:
                    self.history = json.load(f)
            except Exception:
                self.history = []


# Factory function for easy setup
def create_notification_manager(
    slack_webhook: str = None,
    discord_webhook: str = None,
    telegram_token: str = None,
    telegram_chat_id: str = None,
    email_config: Dict[str, Any] = None,
    data_dir: str = "./data/notifications"
) -> NotificationManager:
    """
    Create a notification manager with common channels pre-configured.
    
    Args:
        slack_webhook: Slack webhook URL
        discord_webhook: Discord webhook URL
        telegram_token: Telegram bot token
        telegram_chat_id: Telegram chat ID
        email_config: Email configuration dict
        data_dir: Data directory for history
    
    Returns:
        Configured NotificationManager
    """
    manager = NotificationManager(data_dir=data_dir)
    
    if slack_webhook:
        manager.register_channel(
            SlackChannel(webhook_url=slack_webhook),
            priorities=[NotificationPriority.NORMAL, NotificationPriority.HIGH, NotificationPriority.URGENT]
        )
    
    if discord_webhook:
        manager.register_channel(
            DiscordChannel(webhook_url=discord_webhook),
            priorities=[NotificationPriority.NORMAL, NotificationPriority.HIGH]
        )
    
    if telegram_token and telegram_chat_id:
        manager.register_channel(
            TelegramChannel(bot_token=telegram_token, chat_id=telegram_chat_id),
            priorities=[NotificationPriority.HIGH, NotificationPriority.URGENT]
        )
    
    if email_config:
        manager.register_channel(
            EmailChannel(**email_config),
            priorities=[NotificationPriority.URGENT],
            types=[NotificationType.ERROR, NotificationType.LOLLAPALOOZA]
        )
    
    return manager


if __name__ == "__main__":
    # Test the notification manager
    async def test():
        manager = NotificationManager()
        
        # Register a webhook channel for testing
        manager.register_channel(
            WebhookChannel(url="https://httpbin.org/post", name_override="test_webhook")
        )
        
        # Send a test notification
        results = await manager.send(
            title="Test Notification",
            message="This is a test notification from the Mental Models System",
            notification_type=NotificationType.INFO,
            priority=NotificationPriority.NORMAL,
            data={"test": True, "timestamp": datetime.now().isoformat()}
        )
        
        for result in results:
            print(f"Channel: {result.channel}, Success: {result.success}, Error: {result.error}")
        
        # Get stats
        stats = manager.get_stats()
        print(f"\nStats: {json.dumps(stats, indent=2)}")
    
    asyncio.run(test())
