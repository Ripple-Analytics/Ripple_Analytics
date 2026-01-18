"""
Slack connector for messages, channels, and bot integration.
"""

from typing import Any, Dict, List, Optional
import aiohttp
import logging
import json

from .base import BaseConnector, ConnectorConfig, ConnectorResult

logger = logging.getLogger(__name__)


class SlackConnector(BaseConnector):
    """Connector for Slack API integration."""
    
    BASE_URL = "https://slack.com/api"
    
    def __init__(self, config: ConnectorConfig):
        super().__init__(config)
        self._session: Optional[aiohttp.ClientSession] = None
        self._bot_token = config.credentials.get("bot_token", "")
        self._app_token = config.credentials.get("app_token", "")
    
    async def connect(self) -> bool:
        """Establish connection to Slack API."""
        try:
            headers = {
                "Authorization": f"Bearer {self._bot_token}",
                "Content-Type": "application/json"
            }
            
            self._session = aiohttp.ClientSession(headers=headers)
            
            # Test connection
            async with self._session.post(f"{self.BASE_URL}/auth.test") as resp:
                data = await resp.json()
                self._connected = data.get("ok", False)
                if self._connected:
                    logger.info(f"Connected to Slack as {data.get('user')}")
            
            return self._connected
        except Exception as e:
            logger.error(f"Failed to connect to Slack: {e}")
            return False
    
    async def disconnect(self) -> bool:
        """Close Slack API connection."""
        if self._session:
            await self._session.close()
            self._session = None
        self._connected = False
        return True
    
    async def fetch(self, query: Dict[str, Any]) -> ConnectorResult:
        """
        Fetch data from Slack.
        
        Query parameters:
        - resource: "channels", "messages", "users", "conversations"
        - channel: Channel ID (for messages)
        - limit: Number of results
        - cursor: Pagination cursor
        """
        if not self._session:
            return ConnectorResult(success=False, error="Not connected")
        
        try:
            resource = query.get("resource", "channels")
            
            if resource == "channels":
                url = f"{self.BASE_URL}/conversations.list"
                params = {"limit": query.get("limit", 100)}
            elif resource == "messages":
                url = f"{self.BASE_URL}/conversations.history"
                params = {
                    "channel": query.get("channel"),
                    "limit": query.get("limit", 100)
                }
            elif resource == "users":
                url = f"{self.BASE_URL}/users.list"
                params = {"limit": query.get("limit", 100)}
            elif resource == "conversations":
                url = f"{self.BASE_URL}/conversations.info"
                params = {"channel": query.get("channel")}
            else:
                return ConnectorResult(success=False, error=f"Unknown resource: {resource}")
            
            if "cursor" in query:
                params["cursor"] = query["cursor"]
            
            async with self._session.get(url, params=params) as resp:
                data = await resp.json()
                if data.get("ok"):
                    return ConnectorResult(
                        success=True,
                        data=data,
                        metadata={"url": url}
                    )
                else:
                    return ConnectorResult(
                        success=False,
                        error=f"Slack API error: {data.get('error')}"
                    )
        except Exception as e:
            logger.error(f"Slack fetch error: {e}")
            return ConnectorResult(success=False, error=str(e))
    
    async def push(self, data: Any) -> ConnectorResult:
        """
        Push data to Slack (send messages, etc.).
        
        Data parameters:
        - action: "send_message", "reply_thread", "react"
        - channel: Channel ID
        - text: Message text
        - thread_ts: Thread timestamp (for replies)
        - blocks: Block kit blocks (optional)
        """
        if not self._session:
            return ConnectorResult(success=False, error="Not connected")
        
        try:
            action = data.get("action", "send_message")
            
            if action in ["send_message", "reply_thread"]:
                url = f"{self.BASE_URL}/chat.postMessage"
                payload = {
                    "channel": data.get("channel"),
                    "text": data.get("text", "")
                }
                if data.get("thread_ts"):
                    payload["thread_ts"] = data["thread_ts"]
                if data.get("blocks"):
                    payload["blocks"] = data["blocks"]
            elif action == "react":
                url = f"{self.BASE_URL}/reactions.add"
                payload = {
                    "channel": data.get("channel"),
                    "timestamp": data.get("timestamp"),
                    "name": data.get("emoji", "thumbsup")
                }
            else:
                return ConnectorResult(success=False, error=f"Unknown action: {action}")
            
            async with self._session.post(url, json=payload) as resp:
                result = await resp.json()
                if result.get("ok"):
                    return ConnectorResult(success=True, data=result)
                else:
                    return ConnectorResult(
                        success=False,
                        error=f"Slack API error: {result.get('error')}"
                    )
        except Exception as e:
            logger.error(f"Slack push error: {e}")
            return ConnectorResult(success=False, error=str(e))
    
    async def send_message(self, channel: str, text: str, blocks: Optional[List] = None) -> ConnectorResult:
        """Send a message to a channel."""
        return await self.push({
            "action": "send_message",
            "channel": channel,
            "text": text,
            "blocks": blocks
        })
    
    async def reply_to_thread(self, channel: str, thread_ts: str, text: str) -> ConnectorResult:
        """Reply to a thread."""
        return await self.push({
            "action": "reply_thread",
            "channel": channel,
            "thread_ts": thread_ts,
            "text": text
        })
    
    async def list_channels(self, limit: int = 100) -> ConnectorResult:
        """List all channels."""
        return await self.fetch({"resource": "channels", "limit": limit})
    
    async def get_channel_history(self, channel: str, limit: int = 100) -> ConnectorResult:
        """Get channel message history."""
        return await self.fetch({
            "resource": "messages",
            "channel": channel,
            "limit": limit
        })


class SlackBot:
    """
    Slack bot for running the Mental Models System via Slack.
    
    This allows users to interact with the system through Slack commands.
    """
    
    def __init__(self, connector: SlackConnector):
        self.connector = connector
        self.commands = {
            "/analyze": self._handle_analyze,
            "/models": self._handle_models,
            "/failures": self._handle_failures,
            "/help": self._handle_help,
        }
    
    async def handle_event(self, event: Dict[str, Any]) -> Optional[ConnectorResult]:
        """Handle incoming Slack event."""
        event_type = event.get("type")
        
        if event_type == "message":
            return await self._handle_message(event)
        elif event_type == "app_mention":
            return await self._handle_mention(event)
        
        return None
    
    async def _handle_message(self, event: Dict[str, Any]) -> Optional[ConnectorResult]:
        """Handle direct message."""
        text = event.get("text", "")
        channel = event.get("channel")
        thread_ts = event.get("thread_ts") or event.get("ts")
        
        # Check for commands
        for cmd, handler in self.commands.items():
            if text.startswith(cmd):
                args = text[len(cmd):].strip()
                response = await handler(args)
                return await self.connector.reply_to_thread(channel, thread_ts, response)
        
        return None
    
    async def _handle_mention(self, event: Dict[str, Any]) -> Optional[ConnectorResult]:
        """Handle @mention of the bot."""
        text = event.get("text", "")
        channel = event.get("channel")
        thread_ts = event.get("thread_ts") or event.get("ts")
        
        # Remove the mention and process
        # Simple response for now
        response = "I'm the Mental Models System bot. Use /help to see available commands."
        return await self.connector.reply_to_thread(channel, thread_ts, response)
    
    async def _handle_analyze(self, args: str) -> str:
        """Handle /analyze command."""
        if not args:
            return "Usage: /analyze <text to analyze>"
        return f"Analyzing: {args[:100]}... (analysis would be performed here)"
    
    async def _handle_models(self, args: str) -> str:
        """Handle /models command."""
        return "Mental Models System has 129 models across 8 categories: Psychology, Thinking Tools, Economics, Competitive Advantage, Mathematics, Physics/Systems, Biology, and Organizational."
    
    async def _handle_failures(self, args: str) -> str:
        """Handle /failures command."""
        return "The system tracks 645 failure modes (5 per model) with detection signals and prevention strategies."
    
    async def _handle_help(self, args: str) -> str:
        """Handle /help command."""
        return """Available commands:
- /analyze <text> - Analyze text through mental models lens
- /models - List available mental models
- /failures - Information about failure mode detection
- /help - Show this help message"""
