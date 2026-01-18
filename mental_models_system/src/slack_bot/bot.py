"""
Slack Bot Interface for Mental Models System

Allows interaction with the system via Slack - same workflow as Manus/Devin.
Supports commands, thread-based conversations, and agent orchestration.

Usage:
    python -m src.slack_bot.bot

Commands:
    /analyze <text>     - Analyze text with mental models
    /predict <title>    - Create a new prediction
    /search <query>     - Semantic search across knowledge base
    /delegate <task>    - Delegate task to an agent
    /status             - Get system status
    /models             - List mental models
    @bot <question>     - Ask a question (thread-based)
"""

import os
import json
import asyncio
import logging
import re
from datetime import datetime
from typing import Dict, List, Optional, Any, Callable
from dataclasses import dataclass, field
from enum import Enum
import hashlib

# Slack SDK
try:
    from slack_bolt import App
    from slack_bolt.adapter.socket_mode import SocketModeHandler
    from slack_sdk import WebClient
    from slack_sdk.errors import SlackApiError
    SLACK_SDK_AVAILABLE = True
except ImportError:
    SLACK_SDK_AVAILABLE = False
    print("Warning: slack_bolt not installed. Run: pip install slack-bolt")

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# =============================================================================
# DATA STRUCTURES
# =============================================================================

class AgentType(Enum):
    """Types of agents that can handle tasks."""
    LOCAL_LLM = "local_llm"
    OPENAI = "openai"
    DEVIN = "devin"
    HUMAN = "human"


@dataclass
class Conversation:
    """A conversation thread."""
    thread_ts: str
    channel_id: str
    user_id: str
    started_at: datetime
    messages: List[Dict] = field(default_factory=list)
    context: Dict = field(default_factory=dict)
    assigned_agent: AgentType = None


@dataclass
class Task:
    """A task to be executed by an agent."""
    id: str
    description: str
    created_at: datetime
    created_by: str
    channel_id: str
    thread_ts: str
    assigned_agent: AgentType = None
    status: str = "pending"
    result: str = None
    completed_at: datetime = None


# =============================================================================
# AGENT REGISTRY
# =============================================================================

class AgentRegistry:
    """Registry of available agents."""
    
    def __init__(self):
        self.agents: Dict[AgentType, Dict] = {}
        self._register_default_agents()
    
    def _register_default_agents(self):
        """Register default agents."""
        # Local LLM agent
        self.register(
            AgentType.LOCAL_LLM,
            name="Local LLM",
            description="Local language model (Ollama, llama.cpp)",
            capabilities=["analyze", "search", "answer"],
            handler=self._local_llm_handler
        )
        
        # OpenAI agent
        self.register(
            AgentType.OPENAI,
            name="OpenAI",
            description="OpenAI GPT models",
            capabilities=["analyze", "search", "answer", "code"],
            handler=self._openai_handler
        )
        
        # Devin agent (placeholder)
        self.register(
            AgentType.DEVIN,
            name="Devin",
            description="Devin AI coding agent",
            capabilities=["code", "test", "deploy"],
            handler=self._devin_handler
        )
    
    def register(self, agent_type: AgentType, name: str, description: str,
                 capabilities: List[str], handler: Callable):
        """Register an agent."""
        self.agents[agent_type] = {
            "name": name,
            "description": description,
            "capabilities": capabilities,
            "handler": handler,
            "available": True
        }
    
    def get_agent_for_task(self, task_type: str) -> Optional[AgentType]:
        """Get the best agent for a task type."""
        for agent_type, info in self.agents.items():
            if task_type in info["capabilities"] and info["available"]:
                return agent_type
        return None
    
    async def execute(self, agent_type: AgentType, task: str, context: Dict) -> str:
        """Execute a task with an agent."""
        if agent_type not in self.agents:
            return f"Agent {agent_type.value} not found"
        
        handler = self.agents[agent_type]["handler"]
        return await handler(task, context)
    
    async def _local_llm_handler(self, task: str, context: Dict) -> str:
        """Handle task with local LLM."""
        try:
            from ..llm import create_llm_client
            
            client = create_llm_client(
                backend=os.getenv("LLM_BACKEND", "ollama"),
                model=os.getenv("LLM_MODEL", "llama3:8b")
            )
            
            response = await client.generate(task)
            return response
        except Exception as e:
            logger.error(f"Local LLM error: {e}")
            return f"Error: {e}"
    
    async def _openai_handler(self, task: str, context: Dict) -> str:
        """Handle task with OpenAI."""
        try:
            from openai import OpenAI
            client = OpenAI()
            
            response = client.chat.completions.create(
                model="gpt-4.1-mini",
                messages=[
                    {"role": "system", "content": "You are a helpful assistant with expertise in mental models and decision-making."},
                    {"role": "user", "content": task}
                ],
                max_tokens=2000
            )
            
            return response.choices[0].message.content
        except Exception as e:
            logger.error(f"OpenAI error: {e}")
            return f"Error: {e}"
    
    async def _devin_handler(self, task: str, context: Dict) -> str:
        """Handle task with Devin (placeholder)."""
        # This would integrate with Devin's API when available
        return f"Task delegated to Devin: {task}\n\nDevin will handle this and report back."


# =============================================================================
# MENTAL MODELS INTERFACE
# =============================================================================

class MentalModelsInterface:
    """Interface to the Mental Models System."""
    
    def __init__(self):
        self.models_data = self._load_models()
    
    def _load_models(self) -> Dict:
        """Load mental models."""
        from pathlib import Path
        models_path = Path(__file__).parent.parent.parent / "data" / "raw" / "mental_models_complete.json"
        
        if models_path.exists():
            with open(models_path, 'r') as f:
                return json.load(f)
        return {"mental_models": [], "categories": {}}
    
    def list_models(self, category: str = None) -> List[Dict]:
        """List mental models."""
        models = self.models_data.get("mental_models", [])
        if category:
            models = [m for m in models if m.get("category", "").lower() == category.lower()]
        return models
    
    def search_models(self, query: str) -> List[Dict]:
        """Search mental models."""
        query_lower = query.lower()
        models = self.models_data.get("mental_models", [])
        
        results = []
        for model in models:
            score = 0
            if query_lower in model.get("name", "").lower():
                score += 10
            if query_lower in model.get("description", "").lower():
                score += 5
            if any(query_lower in tag.lower() for tag in model.get("tags", [])):
                score += 3
            
            if score > 0:
                results.append({"model": model, "score": score})
        
        results.sort(key=lambda x: x["score"], reverse=True)
        return [r["model"] for r in results[:10]]
    
    async def analyze_text(self, text: str, agent_registry: AgentRegistry) -> Dict:
        """Analyze text with mental models."""
        # Get agent
        agent = agent_registry.get_agent_for_task("analyze")
        if not agent:
            return {"error": "No agent available for analysis"}
        
        # Build prompt with models context
        models_summary = "\n".join([
            f"- {m['name']}: {m.get('description', '')[:100]}"
            for m in self.models_data.get("mental_models", [])[:20]
        ])
        
        prompt = f"""Analyze this text using mental models. Identify which models apply and why.

Available Mental Models (sample):
{models_summary}

Text to analyze:
{text}

Provide:
1. Which mental models apply (with confidence 0-1)
2. Key insights from each model's perspective
3. Potential blind spots or failure modes
4. Recommended actions"""

        result = await agent_registry.execute(agent, prompt, {})
        
        return {
            "analysis": result,
            "agent_used": agent.value,
            "timestamp": datetime.now().isoformat()
        }


# =============================================================================
# SLACK BOT
# =============================================================================

class MentalModelsBot:
    """Slack bot for Mental Models System."""
    
    def __init__(self):
        if not SLACK_SDK_AVAILABLE:
            raise ImportError("slack-bolt is required. Run: pip install slack-bolt")
        
        # Initialize Slack app
        self.app = App(
            token=os.environ.get("SLACK_BOT_TOKEN"),
            signing_secret=os.environ.get("SLACK_SIGNING_SECRET")
        )
        
        # Initialize components
        self.agent_registry = AgentRegistry()
        self.models_interface = MentalModelsInterface()
        self.conversations: Dict[str, Conversation] = {}
        self.tasks: Dict[str, Task] = {}
        
        # Register handlers
        self._register_handlers()
    
    def _register_handlers(self):
        """Register Slack event handlers."""
        
        # Slash commands
        @self.app.command("/analyze")
        async def handle_analyze(ack, say, command):
            await ack()
            text = command.get("text", "")
            if not text:
                await say("Please provide text to analyze. Usage: `/analyze <text>`")
                return
            
            await say(f"🔍 Analyzing with mental models...\n\n_{text[:200]}{'...' if len(text) > 200 else ''}_")
            
            result = await self.models_interface.analyze_text(text, self.agent_registry)
            
            await say(f"**Analysis Complete**\n\n{result['analysis']}\n\n_Agent: {result['agent_used']}_")
        
        @self.app.command("/predict")
        async def handle_predict(ack, say, command):
            await ack()
            title = command.get("text", "")
            if not title:
                await say("Please provide a prediction title. Usage: `/predict <title>`")
                return
            
            # Open modal for prediction details
            await say(f"📊 Creating prediction: *{title}*\n\nReply in this thread with:\n- Description\n- Models to use (comma-separated)\n- Confidence (0-1)\n- Time horizon")
        
        @self.app.command("/search")
        async def handle_search(ack, say, command):
            await ack()
            query = command.get("text", "")
            if not query:
                await say("Please provide a search query. Usage: `/search <query>`")
                return
            
            models = self.models_interface.search_models(query)
            
            if not models:
                await say(f"No models found for: *{query}*")
                return
            
            results = "\n".join([
                f"• *{m['name']}* ({m.get('category', 'Unknown')}): {m.get('description', '')[:100]}..."
                for m in models[:5]
            ])
            
            await say(f"🔎 Search results for *{query}*:\n\n{results}")
        
        @self.app.command("/delegate")
        async def handle_delegate(ack, say, command):
            await ack()
            task_desc = command.get("text", "")
            if not task_desc:
                await say("Please provide a task description. Usage: `/delegate <task>`")
                return
            
            # Create task
            task_id = hashlib.md5(f"{task_desc}{datetime.now()}".encode()).hexdigest()[:8]
            task = Task(
                id=task_id,
                description=task_desc,
                created_at=datetime.now(),
                created_by=command.get("user_id", "unknown"),
                channel_id=command.get("channel_id", ""),
                thread_ts=command.get("thread_ts", "")
            )
            
            # Find best agent
            agent = self.agent_registry.get_agent_for_task("code")
            if agent:
                task.assigned_agent = agent
                self.tasks[task_id] = task
                
                await say(f"📋 Task created: `{task_id}`\n\n*Description:* {task_desc}\n*Assigned to:* {agent.value}\n*Status:* Pending\n\nI'll notify you when it's complete.")
                
                # Execute task asynchronously
                asyncio.create_task(self._execute_task(task, say))
            else:
                await say(f"❌ No agent available for this task type.")
        
        @self.app.command("/status")
        async def handle_status(ack, say, command):
            await ack()
            
            models_count = len(self.models_interface.models_data.get("mental_models", []))
            active_tasks = len([t for t in self.tasks.values() if t.status == "pending"])
            
            agents_status = "\n".join([
                f"• *{info['name']}*: {'🟢 Available' if info['available'] else '🔴 Unavailable'}"
                for agent_type, info in self.agent_registry.agents.items()
            ])
            
            await say(f"""📊 *System Status*

*Mental Models:* {models_count}
*Active Tasks:* {active_tasks}
*Conversations:* {len(self.conversations)}

*Agents:*
{agents_status}""")
        
        @self.app.command("/models")
        async def handle_models(ack, say, command):
            await ack()
            category = command.get("text", "").strip()
            
            models = self.models_interface.list_models(category if category else None)
            
            if not models:
                await say(f"No models found{' in category: ' + category if category else ''}.")
                return
            
            # Group by category
            by_category = {}
            for m in models[:30]:
                cat = m.get("category", "Other")
                if cat not in by_category:
                    by_category[cat] = []
                by_category[cat].append(m["name"])
            
            result = "\n".join([
                f"*{cat}:* {', '.join(names[:5])}{'...' if len(names) > 5 else ''}"
                for cat, names in by_category.items()
            ])
            
            await say(f"🧠 *Mental Models*\n\n{result}\n\n_Use `/search <query>` to find specific models._")
        
        # Message handler for @mentions and threads
        @self.app.event("app_mention")
        async def handle_mention(event, say):
            text = event.get("text", "")
            user = event.get("user", "")
            channel = event.get("channel", "")
            thread_ts = event.get("thread_ts") or event.get("ts")
            
            # Remove bot mention from text
            text = re.sub(r'<@[A-Z0-9]+>', '', text).strip()
            
            if not text:
                await say(
                    text="How can I help? Try:\n• Ask a question about mental models\n• `/analyze <text>` - Analyze with models\n• `/search <query>` - Search models\n• `/delegate <task>` - Delegate to an agent",
                    thread_ts=thread_ts
                )
                return
            
            # Get or create conversation
            conv_key = f"{channel}:{thread_ts}"
            if conv_key not in self.conversations:
                self.conversations[conv_key] = Conversation(
                    thread_ts=thread_ts,
                    channel_id=channel,
                    user_id=user,
                    started_at=datetime.now()
                )
            
            conv = self.conversations[conv_key]
            conv.messages.append({"role": "user", "content": text, "ts": event.get("ts")})
            
            # Process with agent
            await say(text="🤔 Thinking...", thread_ts=thread_ts)
            
            # Build context from conversation
            context = {
                "history": conv.messages[-10:],  # Last 10 messages
                "user": user
            }
            
            agent = self.agent_registry.get_agent_for_task("answer")
            if agent:
                response = await self.agent_registry.execute(agent, text, context)
                conv.messages.append({"role": "assistant", "content": response})
                await say(text=response, thread_ts=thread_ts)
            else:
                await say(text="Sorry, no agent is available right now.", thread_ts=thread_ts)
        
        # Handle messages in threads
        @self.app.event("message")
        async def handle_message(event, say):
            # Only handle thread replies
            if "thread_ts" not in event or event.get("subtype"):
                return
            
            channel = event.get("channel", "")
            thread_ts = event.get("thread_ts")
            conv_key = f"{channel}:{thread_ts}"
            
            # Only respond if we're part of this conversation
            if conv_key not in self.conversations:
                return
            
            text = event.get("text", "")
            user = event.get("user", "")
            
            conv = self.conversations[conv_key]
            conv.messages.append({"role": "user", "content": text, "ts": event.get("ts")})
            
            # Process with agent
            context = {"history": conv.messages[-10:], "user": user}
            
            agent = self.agent_registry.get_agent_for_task("answer")
            if agent:
                response = await self.agent_registry.execute(agent, text, context)
                conv.messages.append({"role": "assistant", "content": response})
                await say(text=response, thread_ts=thread_ts)
    
    async def _execute_task(self, task: Task, say):
        """Execute a task asynchronously."""
        try:
            result = await self.agent_registry.execute(
                task.assigned_agent,
                task.description,
                {}
            )
            
            task.status = "completed"
            task.result = result
            task.completed_at = datetime.now()
            
            await say(
                text=f"✅ Task `{task.id}` completed!\n\n*Result:*\n{result[:2000]}{'...' if len(result) > 2000 else ''}",
                channel=task.channel_id,
                thread_ts=task.thread_ts if task.thread_ts else None
            )
        except Exception as e:
            task.status = "failed"
            task.result = str(e)
            
            await say(
                text=f"❌ Task `{task.id}` failed: {e}",
                channel=task.channel_id,
                thread_ts=task.thread_ts if task.thread_ts else None
            )
    
    def run(self):
        """Run the bot."""
        handler = SocketModeHandler(
            self.app,
            os.environ.get("SLACK_APP_TOKEN")
        )
        logger.info("Starting Mental Models Bot...")
        handler.start()


# =============================================================================
# STANDALONE MODE (No Slack)
# =============================================================================

class StandaloneBot:
    """Standalone bot for testing without Slack."""
    
    def __init__(self):
        self.agent_registry = AgentRegistry()
        self.models_interface = MentalModelsInterface()
    
    async def process_command(self, command: str) -> str:
        """Process a command."""
        parts = command.strip().split(maxsplit=1)
        cmd = parts[0].lower()
        args = parts[1] if len(parts) > 1 else ""
        
        if cmd == "/analyze":
            result = await self.models_interface.analyze_text(args, self.agent_registry)
            return result["analysis"]
        
        elif cmd == "/search":
            models = self.models_interface.search_models(args)
            return "\n".join([f"- {m['name']}: {m.get('description', '')[:100]}" for m in models])
        
        elif cmd == "/models":
            models = self.models_interface.list_models(args if args else None)
            return "\n".join([f"- {m['name']} ({m.get('category', '')})" for m in models[:20]])
        
        elif cmd == "/status":
            return f"Models: {len(self.models_interface.models_data.get('mental_models', []))}\nAgents: {len(self.agent_registry.agents)}"
        
        else:
            # Treat as question
            agent = self.agent_registry.get_agent_for_task("answer")
            if agent:
                return await self.agent_registry.execute(agent, command, {})
            return "No agent available"
    
    async def run_interactive(self):
        """Run interactive mode."""
        print("Mental Models Bot (Standalone Mode)")
        print("Commands: /analyze, /search, /models, /status, /delegate")
        print("Or just type a question. Type 'quit' to exit.\n")
        
        while True:
            try:
                user_input = input("You: ").strip()
                if user_input.lower() in ["quit", "exit"]:
                    break
                
                response = await self.process_command(user_input)
                print(f"\nBot: {response}\n")
            except KeyboardInterrupt:
                break
            except Exception as e:
                print(f"\nError: {e}\n")


# =============================================================================
# MAIN
# =============================================================================

def main():
    """Main entry point."""
    import argparse
    
    parser = argparse.ArgumentParser(description="Mental Models Slack Bot")
    parser.add_argument("--standalone", action="store_true", help="Run in standalone mode (no Slack)")
    parser.add_argument("--test", action="store_true", help="Run tests")
    
    args = parser.parse_args()
    
    if args.standalone:
        bot = StandaloneBot()
        asyncio.run(bot.run_interactive())
    elif args.test:
        # Quick test
        bot = StandaloneBot()
        result = asyncio.run(bot.process_command("/models"))
        print(result)
    else:
        # Check for required environment variables
        required_vars = ["SLACK_BOT_TOKEN", "SLACK_SIGNING_SECRET", "SLACK_APP_TOKEN"]
        missing = [v for v in required_vars if not os.environ.get(v)]
        
        if missing:
            print(f"Missing environment variables: {', '.join(missing)}")
            print("\nTo run in standalone mode: python -m src.slack_bot.bot --standalone")
            return
        
        bot = MentalModelsBot()
        bot.run()


if __name__ == "__main__":
    main()
