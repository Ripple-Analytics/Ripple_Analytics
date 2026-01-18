"""Multi-channel notification system."""

from .notification_manager import (
    NotificationManager,
    NotificationChannel,
    Notification,
    NotificationType,
    NotificationPriority,
    DeliveryResult,
    SlackChannel,
    DiscordChannel,
    EmailChannel,
    WebhookChannel,
    TelegramChannel,
    create_notification_manager
)

__all__ = [
    "NotificationManager",
    "NotificationChannel",
    "Notification",
    "NotificationType",
    "NotificationPriority",
    "DeliveryResult",
    "SlackChannel",
    "DiscordChannel",
    "EmailChannel",
    "WebhookChannel",
    "TelegramChannel",
    "create_notification_manager"
]
