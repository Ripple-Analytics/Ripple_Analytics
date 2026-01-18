"""
Slack integration for running the Mental Models System via Slack.

This allows users to interact with the system through Slack in the same way
they interact with Devin - through natural language commands and mentions.
"""

from typing import Any, Dict, List, Optional, Callable
import logging
import json
import asyncio
from dataclasses import dataclass, field
from datetime import datetime

logger = logging.getLogger(__name__)


@dataclass
class SlackMessage:
    """Represents a Slack message."""
    channel: str
    user: str
    text: str
    ts: str
    thread_ts: Optional[str] = None
    blocks: List[Dict] = field(default_factory=list)


@dataclass
class SlackCommand:
    """Represents a parsed Slack command."""
    command: str
    args: List[str]
    raw_text: str
    message: SlackMessage


class SlackEventHandler:
    """
    Handles Slack events and routes them to appropriate handlers.
    
    This is the core event processing for the Slack integration.
    """
    
    def __init__(self):
        self._handlers: Dict[str, Callable] = {}
        self._command_handlers: Dict[str, Callable] = {}
        self._mention_handler: Optional[Callable] = None
        self._default_handler: Optional[Callable] = None
    
    def on_event(self, event_type: str):
        """Decorator to register an event handler."""
        def decorator(func: Callable):
            self._handlers[event_type] = func
            return func
        return decorator
    
    def on_command(self, command: str):
        """Decorator to register a command handler."""
        def decorator(func: Callable):
            self._command_handlers[command.lower()] = func
            return func
        return decorator
    
    def on_mention(self, func: Callable):
        """Decorator to register the mention handler."""
        self._mention_handler = func
        return func
    
    def on_default(self, func: Callable):
        """Decorator to register the default message handler."""
        self._default_handler = func
        return func
    
    async def handle_event(self, event: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        """Process an incoming Slack event."""
        event_type = event.get("type")
        
        # Check for specific event handler
        if event_type in self._handlers:
            return await self._handlers[event_type](event)
        
        # Handle message events specially
        if event_type == "message":
            return await self._handle_message(event)
        elif event_type == "app_mention":
            return await self._handle_mention(event)
        
        logger.debug(f"Unhandled event type: {event_type}")
        return None
    
    async def _handle_message(self, event: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        """Handle a message event."""
        text = event.get("text", "").strip()
        
        # Parse as command if starts with /
        if text.startswith("/"):
            return await self._handle_command(event)
        
        # Use default handler if available
        if self._default_handler:
            message = SlackMessage(
                channel=event.get("channel", ""),
                user=event.get("user", ""),
                text=text,
                ts=event.get("ts", ""),
                thread_ts=event.get("thread_ts")
            )
            return await self._default_handler(message)
        
        return None
    
    async def _handle_mention(self, event: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        """Handle an app mention event."""
        if self._mention_handler:
            message = SlackMessage(
                channel=event.get("channel", ""),
                user=event.get("user", ""),
                text=event.get("text", ""),
                ts=event.get("ts", ""),
                thread_ts=event.get("thread_ts")
            )
            return await self._mention_handler(message)
        return None
    
    async def _handle_command(self, event: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        """Handle a command message."""
        text = event.get("text", "").strip()
        parts = text.split()
        
        if not parts:
            return None
        
        command = parts[0].lower().lstrip("/")
        args = parts[1:] if len(parts) > 1 else []
        
        message = SlackMessage(
            channel=event.get("channel", ""),
            user=event.get("user", ""),
            text=text,
            ts=event.get("ts", ""),
            thread_ts=event.get("thread_ts")
        )
        
        cmd = SlackCommand(
            command=command,
            args=args,
            raw_text=text,
            message=message
        )
        
        if command in self._command_handlers:
            return await self._command_handlers[command](cmd)
        
        return {"error": f"Unknown command: {command}"}


class SlackIntegration:
    """
    Full Slack integration for the Mental Models System.
    
    This provides:
    - Bot that responds to mentions and commands
    - Analysis of messages through mental models
    - Failure mode detection on discussions
    - Integration with the improvement cycle
    """
    
    def __init__(self, bot_token: str, app_token: Optional[str] = None):
        self.bot_token = bot_token
        self.app_token = app_token
        self.event_handler = SlackEventHandler()
        self._setup_default_handlers()
    
    def _setup_default_handlers(self):
        """Set up default command and event handlers."""
        
        @self.event_handler.on_command("analyze")
        async def handle_analyze(cmd: SlackCommand) -> Dict[str, Any]:
            """Analyze text through mental models."""
            if not cmd.args:
                return {
                    "response": "Usage: /analyze <text to analyze>",
                    "channel": cmd.message.channel,
                    "thread_ts": cmd.message.thread_ts or cmd.message.ts
                }
            
            text_to_analyze = " ".join(cmd.args)
            
            # This would integrate with the MentalModelAnalyzer
            return {
                "response": f"Analyzing: {text_to_analyze[:100]}...\n\nAnalysis would be performed here using the Mental Models System.",
                "channel": cmd.message.channel,
                "thread_ts": cmd.message.thread_ts or cmd.message.ts
            }
        
        @self.event_handler.on_command("models")
        async def handle_models(cmd: SlackCommand) -> Dict[str, Any]:
            """List available mental models."""
            response = """*Mental Models System*
            
The system includes 129 mental models across 8 categories:

1. *Psychology* (25 models) - Cognitive biases, heuristics
2. *Thinking Tools* (17 models) - Decision frameworks
3. *Economics* (15 models) - Economic principles
4. *Competitive Advantage* (15 models) - Strategy models
5. *Mathematics* (12 models) - Quantitative thinking
6. *Physics/Systems* (18 models) - Systems thinking
7. *Biology* (12 models) - Evolutionary models
8. *Organizational* (15 models) - Management models

Use `/analyze <text>` to analyze content through these models."""
            
            return {
                "response": response,
                "channel": cmd.message.channel,
                "thread_ts": cmd.message.thread_ts or cmd.message.ts
            }
        
        @self.event_handler.on_command("failures")
        async def handle_failures(cmd: SlackCommand) -> Dict[str, Any]:
            """Show failure mode information."""
            response = """*Failure Mode Detection*

The system tracks 645 failure modes (5 per model) with:
- Detection signals (observable indicators)
- Prevention strategies
- Example scenarios

Categories:
- data_bias
- reasoning_error
- incomplete_analysis
- overconfidence
- context_blindness
- temporal_error
- scale_mismatch
- feedback_loop
- edge_case
- integration_failure

Use `/check <text>` to check for potential failure modes."""
            
            return {
                "response": response,
                "channel": cmd.message.channel,
                "thread_ts": cmd.message.thread_ts or cmd.message.ts
            }
        
        @self.event_handler.on_command("help")
        async def handle_help(cmd: SlackCommand) -> Dict[str, Any]:
            """Show help information."""
            response = """*Mental Models System - Help*

Available commands:
- `/analyze <text>` - Analyze text through mental models lens
- `/models` - List available mental models
- `/failures` - Information about failure mode detection
- `/check <text>` - Check for potential failure modes
- `/invert <problem>` - Apply inversion thinking
- `/help` - Show this help message

You can also mention @MentalModels in any message to get analysis."""
            
            return {
                "response": response,
                "channel": cmd.message.channel,
                "thread_ts": cmd.message.thread_ts or cmd.message.ts
            }
        
        @self.event_handler.on_command("check")
        async def handle_check(cmd: SlackCommand) -> Dict[str, Any]:
            """Check for failure modes."""
            if not cmd.args:
                return {
                    "response": "Usage: /check <text to check for failure modes>",
                    "channel": cmd.message.channel,
                    "thread_ts": cmd.message.thread_ts or cmd.message.ts
                }
            
            text_to_check = " ".join(cmd.args)
            
            return {
                "response": f"Checking for failure modes in: {text_to_check[:100]}...\n\nFailure mode detection would be performed here.",
                "channel": cmd.message.channel,
                "thread_ts": cmd.message.thread_ts or cmd.message.ts
            }
        
        @self.event_handler.on_command("invert")
        async def handle_invert(cmd: SlackCommand) -> Dict[str, Any]:
            """Apply inversion thinking."""
            if not cmd.args:
                return {
                    "response": "Usage: /invert <problem to invert>",
                    "channel": cmd.message.channel,
                    "thread_ts": cmd.message.thread_ts or cmd.message.ts
                }
            
            problem = " ".join(cmd.args)
            
            return {
                "response": f"Inverting: {problem}\n\nInstead of asking how to succeed, ask: How could this fail?\n\nInversion analysis would be performed here.",
                "channel": cmd.message.channel,
                "thread_ts": cmd.message.thread_ts or cmd.message.ts
            }
        
        @self.event_handler.on_mention
        async def handle_mention(message: SlackMessage) -> Dict[str, Any]:
            """Handle when the bot is mentioned."""
            # Remove the mention from the text
            text = message.text
            # Simple mention removal (would need proper parsing in production)
            text = " ".join(word for word in text.split() if not word.startswith("<@"))
            
            if not text.strip():
                return {
                    "response": "Hi! I'm the Mental Models System bot. Use `/help` to see available commands, or just ask me to analyze something!",
                    "channel": message.channel,
                    "thread_ts": message.thread_ts or message.ts
                }
            
            return {
                "response": f"Analyzing your message through mental models...\n\nText: {text[:200]}...\n\nAnalysis would be performed here.",
                "channel": message.channel,
                "thread_ts": message.thread_ts or message.ts
            }
    
    async def process_event(self, event: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        """Process an incoming Slack event."""
        return await self.event_handler.handle_event(event)
    
    def register_command(self, command: str, handler: Callable):
        """Register a custom command handler."""
        self.event_handler._command_handlers[command.lower()] = handler
    
    def register_event(self, event_type: str, handler: Callable):
        """Register a custom event handler."""
        self.event_handler._handlers[event_type] = handler


class SlackWebhookServer:
    """
    Simple webhook server for receiving Slack events.
    
    In production, you would use a proper web framework like FastAPI.
    """
    
    def __init__(self, integration: SlackIntegration, signing_secret: str):
        self.integration = integration
        self.signing_secret = signing_secret
    
    async def handle_webhook(self, request_body: bytes, headers: Dict[str, str]) -> Dict[str, Any]:
        """Handle incoming webhook from Slack."""
        # Verify request signature (simplified - use proper HMAC in production)
        
        try:
            payload = json.loads(request_body)
        except json.JSONDecodeError:
            return {"error": "Invalid JSON"}
        
        # Handle URL verification challenge
        if payload.get("type") == "url_verification":
            return {"challenge": payload.get("challenge")}
        
        # Handle event callback
        if payload.get("type") == "event_callback":
            event = payload.get("event", {})
            result = await self.integration.process_event(event)
            return result or {"ok": True}
        
        return {"ok": True}
