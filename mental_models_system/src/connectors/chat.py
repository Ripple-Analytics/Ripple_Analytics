"""
Chat & Communication Connectors

Multiple chat platform integrations:
- Slack (primary)
- Discord (open source alternative)
- Matrix (fully open source, self-hosted)
- Mattermost (open source Slack alternative)
- Telegram (popular, good bot API)
"""

import os
import json
import asyncio
import logging
import aiohttp
from dataclasses import dataclass
from datetime import datetime
from typing import Dict, List, Optional, Any, Callable
from abc import abstractmethod

from .base import BaseConnector, ConnectorConfig, ConnectorType, ConnectorStatus

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


@dataclass
class Message:
    """A chat message."""
    id: str
    channel: str
    user: str
    text: str
    timestamp: datetime
    thread_id: str = None
    attachments: List[Dict] = None
    
    def to_dict(self) -> Dict:
        return {
            "id": self.id,
            "channel": self.channel,
            "user": self.user,
            "text": self.text,
            "timestamp": self.timestamp.isoformat(),
            "thread_id": self.thread_id
        }


class BaseChatConnector(BaseConnector):
    """Base class for chat connectors."""
    
    TYPE = ConnectorType.CHAT
    
    @abstractmethod
    async def send_message(self, channel: str, text: str, thread_id: str = None) -> bool:
        """Send a message to a channel."""
        pass
    
    @abstractmethod
    async def read_messages(self, channel: str, limit: int = 100) -> List[Message]:
        """Read messages from a channel."""
        pass
    
    @abstractmethod
    async def register_handler(self, event_type: str, handler: Callable):
        """Register an event handler."""
        pass


# =============================================================================
# SLACK CONNECTOR
# =============================================================================

class SlackConnector(BaseChatConnector):
    """
    Slack integration using Bolt SDK or webhooks.
    
    Supports:
    - Send/receive messages
    - Thread conversations
    - Slash commands
    - Interactive components
    - Webhooks for simple notifications
    """
    
    NAME = "slack"
    DESCRIPTION = "Slack workspace integration"
    REQUIRED_CREDENTIALS = []  # Can work with just webhook
    OPTIONAL_CREDENTIALS = ["bot_token", "signing_secret", "app_token", "webhook_url"]
    OPEN_SOURCE = False  # Slack itself is not open source
    
    def __init__(self, config: ConnectorConfig = None):
        super().__init__(config)
        self._client = None
        self._webhook_url = os.environ.get("SLACK_WEBHOOK_URL")
    
    async def connect(self) -> bool:
        """Connect to Slack."""
        # Try Bolt SDK first
        try:
            from slack_sdk import WebClient
            
            token = self.config.credentials.get("bot_token") or os.environ.get("SLACK_BOT_TOKEN")
            if token:
                self._client = WebClient(token=token)
                # Test connection
                self._client.auth_test()
                self.status = ConnectorStatus.CONNECTED
                return True
        except Exception as e:
            logger.warning(f"Slack SDK connection failed: {e}")
        
        # Fall back to webhook
        if self._webhook_url:
            self.status = ConnectorStatus.CONNECTED
            return True
        
        self.status = ConnectorStatus.ERROR
        return False
    
    async def disconnect(self) -> bool:
        self._client = None
        self.status = ConnectorStatus.DISCONNECTED
        return True
    
    async def health_check(self) -> bool:
        if self._client:
            try:
                self._client.auth_test()
                return True
            except:
                return False
        return self._webhook_url is not None
    
    async def send_message(self, channel: str, text: str, thread_id: str = None) -> bool:
        """Send a message to Slack."""
        # Try SDK first
        if self._client:
            try:
                self._client.chat_postMessage(
                    channel=channel,
                    text=text,
                    thread_ts=thread_id
                )
                return True
            except Exception as e:
                logger.error(f"Slack SDK send failed: {e}")
        
        # Fall back to webhook
        if self._webhook_url:
            async with aiohttp.ClientSession() as session:
                payload = {"text": text}
                if channel:
                    payload["channel"] = channel
                
                async with session.post(self._webhook_url, json=payload) as resp:
                    return resp.status == 200
        
        return False
    
    async def read_messages(self, channel: str, limit: int = 100) -> List[Message]:
        """Read messages from a channel."""
        if not self._client:
            logger.warning("SDK required for reading messages")
            return []
        
        try:
            result = self._client.conversations_history(channel=channel, limit=limit)
            
            messages = []
            for msg in result.get("messages", []):
                messages.append(Message(
                    id=msg.get("ts"),
                    channel=channel,
                    user=msg.get("user", ""),
                    text=msg.get("text", ""),
                    timestamp=datetime.fromtimestamp(float(msg.get("ts", 0))),
                    thread_id=msg.get("thread_ts")
                ))
            
            return messages
        except Exception as e:
            logger.error(f"Failed to read messages: {e}")
            return []
    
    async def register_handler(self, event_type: str, handler: Callable):
        """Register event handler (requires Bolt app)."""
        logger.warning("Event handlers require Bolt app setup")
    
    async def send_blocks(self, channel: str, blocks: List[Dict], text: str = "") -> bool:
        """Send rich message with blocks."""
        if not self._client:
            return await self.send_message(channel, text)
        
        try:
            self._client.chat_postMessage(
                channel=channel,
                text=text,
                blocks=blocks
            )
            return True
        except Exception as e:
            logger.error(f"Failed to send blocks: {e}")
            return False


# =============================================================================
# DISCORD CONNECTOR
# =============================================================================

class DiscordConnector(BaseChatConnector):
    """
    Discord integration using discord.py or webhooks.
    
    Open source alternative to Slack with:
    - Free unlimited history
    - Voice channels
    - Rich embeds
    - Bot API
    """
    
    NAME = "discord"
    DESCRIPTION = "Discord server integration"
    REQUIRED_CREDENTIALS = []
    OPTIONAL_CREDENTIALS = ["bot_token", "webhook_url"]
    OPEN_SOURCE = False  # Discord itself is not open source, but free
    
    def __init__(self, config: ConnectorConfig = None):
        super().__init__(config)
        self._client = None
        self._webhook_url = os.environ.get("DISCORD_WEBHOOK_URL")
    
    async def connect(self) -> bool:
        """Connect to Discord."""
        # Webhook is simplest
        if self._webhook_url:
            self.status = ConnectorStatus.CONNECTED
            return True
        
        # Try bot token
        try:
            import discord
            
            token = self.config.credentials.get("bot_token") or os.environ.get("DISCORD_BOT_TOKEN")
            if token:
                intents = discord.Intents.default()
                intents.message_content = True
                self._client = discord.Client(intents=intents)
                self.status = ConnectorStatus.CONNECTED
                return True
        except ImportError:
            logger.warning("discord.py not installed. Run: pip install discord.py")
        
        self.status = ConnectorStatus.ERROR
        return False
    
    async def disconnect(self) -> bool:
        if self._client:
            await self._client.close()
        self.status = ConnectorStatus.DISCONNECTED
        return True
    
    async def health_check(self) -> bool:
        return self._webhook_url is not None or self._client is not None
    
    async def send_message(self, channel: str, text: str, thread_id: str = None) -> bool:
        """Send a message to Discord."""
        if self._webhook_url:
            async with aiohttp.ClientSession() as session:
                payload = {"content": text}
                async with session.post(self._webhook_url, json=payload) as resp:
                    return resp.status in [200, 204]
        
        if self._client:
            try:
                ch = self._client.get_channel(int(channel))
                if ch:
                    await ch.send(text)
                    return True
            except Exception as e:
                logger.error(f"Discord send failed: {e}")
        
        return False
    
    async def read_messages(self, channel: str, limit: int = 100) -> List[Message]:
        """Read messages from a channel."""
        if not self._client:
            return []
        
        try:
            ch = self._client.get_channel(int(channel))
            if not ch:
                return []
            
            messages = []
            async for msg in ch.history(limit=limit):
                messages.append(Message(
                    id=str(msg.id),
                    channel=channel,
                    user=str(msg.author),
                    text=msg.content,
                    timestamp=msg.created_at
                ))
            
            return messages
        except Exception as e:
            logger.error(f"Failed to read Discord messages: {e}")
            return []
    
    async def register_handler(self, event_type: str, handler: Callable):
        """Register event handler."""
        if self._client:
            self._client.event(handler)
    
    async def send_embed(self, channel: str, title: str, description: str,
                        color: int = 0x00ff00, fields: List[Dict] = None) -> bool:
        """Send a rich embed message."""
        if self._webhook_url:
            embed = {
                "title": title,
                "description": description,
                "color": color,
                "fields": fields or []
            }
            
            async with aiohttp.ClientSession() as session:
                payload = {"embeds": [embed]}
                async with session.post(self._webhook_url, json=payload) as resp:
                    return resp.status in [200, 204]
        
        return False


# =============================================================================
# MATRIX CONNECTOR (Fully Open Source)
# =============================================================================

class MatrixConnector(BaseChatConnector):
    """
    Matrix protocol integration.
    
    Fully open source and self-hostable:
    - Decentralized
    - End-to-end encryption
    - Bridges to other platforms
    - Self-hosted with Synapse
    """
    
    NAME = "matrix"
    DESCRIPTION = "Matrix protocol (fully open source, self-hosted)"
    REQUIRED_CREDENTIALS = ["homeserver", "access_token"]
    OPTIONAL_CREDENTIALS = ["user_id"]
    OPEN_SOURCE = True
    
    def __init__(self, config: ConnectorConfig = None):
        super().__init__(config)
        self._client = None
        self._homeserver = os.environ.get("MATRIX_HOMESERVER", "https://matrix.org")
        self._token = os.environ.get("MATRIX_ACCESS_TOKEN")
    
    async def connect(self) -> bool:
        """Connect to Matrix homeserver."""
        try:
            from nio import AsyncClient
            
            homeserver = self.config.credentials.get("homeserver") or self._homeserver
            token = self.config.credentials.get("access_token") or self._token
            
            if not token:
                logger.error("Matrix access token required")
                self.status = ConnectorStatus.ERROR
                return False
            
            self._client = AsyncClient(homeserver)
            self._client.access_token = token
            
            # Verify connection
            response = await self._client.whoami()
            if response:
                self.status = ConnectorStatus.CONNECTED
                return True
            
        except ImportError:
            logger.error("matrix-nio not installed. Run: pip install matrix-nio")
        except Exception as e:
            logger.error(f"Matrix connection failed: {e}")
        
        self.status = ConnectorStatus.ERROR
        return False
    
    async def disconnect(self) -> bool:
        if self._client:
            await self._client.close()
        self.status = ConnectorStatus.DISCONNECTED
        return True
    
    async def health_check(self) -> bool:
        if self._client:
            try:
                response = await self._client.whoami()
                return response is not None
            except:
                return False
        return False
    
    async def send_message(self, channel: str, text: str, thread_id: str = None) -> bool:
        """Send a message to a Matrix room."""
        if not self._client:
            return False
        
        try:
            from nio import RoomMessageText
            
            content = {
                "msgtype": "m.text",
                "body": text
            }
            
            if thread_id:
                content["m.relates_to"] = {
                    "rel_type": "m.thread",
                    "event_id": thread_id
                }
            
            response = await self._client.room_send(
                room_id=channel,
                message_type="m.room.message",
                content=content
            )
            
            return response is not None
        except Exception as e:
            logger.error(f"Matrix send failed: {e}")
            return False
    
    async def read_messages(self, channel: str, limit: int = 100) -> List[Message]:
        """Read messages from a Matrix room."""
        if not self._client:
            return []
        
        try:
            response = await self._client.room_messages(
                room_id=channel,
                limit=limit
            )
            
            messages = []
            for event in response.chunk:
                if hasattr(event, 'body'):
                    messages.append(Message(
                        id=event.event_id,
                        channel=channel,
                        user=event.sender,
                        text=event.body,
                        timestamp=datetime.fromtimestamp(event.server_timestamp / 1000)
                    ))
            
            return messages
        except Exception as e:
            logger.error(f"Failed to read Matrix messages: {e}")
            return []
    
    async def register_handler(self, event_type: str, handler: Callable):
        """Register event handler."""
        if self._client:
            self._client.add_event_callback(handler, event_type)


# =============================================================================
# GENERIC WEBHOOK CONNECTOR
# =============================================================================

class WebhookConnector(BaseChatConnector):
    """
    Generic webhook connector for any service.
    
    Works with:
    - Custom webhooks
    - Zapier
    - IFTTT
    - n8n
    - Any HTTP endpoint
    """
    
    NAME = "webhook"
    DESCRIPTION = "Generic webhook connector"
    REQUIRED_CREDENTIALS = ["url"]
    OPEN_SOURCE = True
    
    def __init__(self, config: ConnectorConfig = None):
        super().__init__(config)
        self._url = None
    
    async def connect(self) -> bool:
        self._url = self.config.credentials.get("url") or os.environ.get("WEBHOOK_URL")
        if self._url:
            self.status = ConnectorStatus.CONNECTED
            return True
        self.status = ConnectorStatus.ERROR
        return False
    
    async def disconnect(self) -> bool:
        self.status = ConnectorStatus.DISCONNECTED
        return True
    
    async def health_check(self) -> bool:
        return self._url is not None
    
    async def send_message(self, channel: str, text: str, thread_id: str = None) -> bool:
        """Send a webhook payload."""
        if not self._url:
            return False
        
        async with aiohttp.ClientSession() as session:
            payload = {
                "text": text,
                "channel": channel,
                "timestamp": datetime.now().isoformat()
            }
            
            async with session.post(self._url, json=payload) as resp:
                return resp.status in [200, 201, 204]
    
    async def read_messages(self, channel: str, limit: int = 100) -> List[Message]:
        """Webhooks are send-only."""
        return []
    
    async def register_handler(self, event_type: str, handler: Callable):
        """Webhooks don't support handlers."""
        pass
    
    async def send_custom_payload(self, payload: Dict) -> bool:
        """Send a custom JSON payload."""
        if not self._url:
            return False
        
        async with aiohttp.ClientSession() as session:
            async with session.post(self._url, json=payload) as resp:
                return resp.status in [200, 201, 204]


# Register connectors
from .base import registry
registry.register_class(SlackConnector)
registry.register_class(DiscordConnector)
registry.register_class(MatrixConnector)
registry.register_class(WebhookConnector)
