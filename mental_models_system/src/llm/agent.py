#!/usr/bin/env python3
"""
Mental Models Agent
Agentic search capability for natural language queries over mental models.
"""

import os
import sys
from typing import List, Dict, Any, Optional
from dataclasses import dataclass
from enum import Enum
import json
import re

sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
from config.settings import settings
from .providers import LLMProvider, ChatMessage, LLMResponse, get_provider
from .embeddings import EmbeddingService, SearchResult


class AgentAction(Enum):
    """Actions the agent can take."""
    SEARCH_MODELS = "search_models"
    SEARCH_PRINCIPLES = "search_principles"
    SEARCH_CASES = "search_cases"
    ANALYZE = "analyze"
    SYNTHESIZE = "synthesize"
    ANSWER = "answer"


@dataclass
class AgentStep:
    """A single step in the agent's reasoning."""
    thought: str
    action: AgentAction
    action_input: str
    observation: str


@dataclass
class AgentResponse:
    """Final response from the agent."""
    answer: str
    sources: List[SearchResult]
    reasoning_steps: List[AgentStep]
    models_used: List[str]
    principles_applied: List[str]


SYSTEM_PROMPT = """You are a Mental Models Expert Agent with deep knowledge of Charlie Munger's latticework of mental models, along with wisdom from Soros, Dalio, Simons, Franklin, Seneca, and other great thinkers.

Your role is to help users understand and apply mental models to their questions and problems. You have access to a database of:
- 113+ Mental Models across mathematics, physics, biology, psychology, economics, and more
- 264+ Principles from legendary thinkers
- 100,000+ Case Studies showing models in action

When answering questions:
1. First, identify which mental models are most relevant
2. Search for related principles from the thinkers
3. Find case studies that demonstrate the models
4. Synthesize an answer that applies the models to the user's question

Always cite specific models and principles. Think like Munger - use multiple models in combination (the latticework approach).

Available tools:
- search_models(query): Search for relevant mental models
- search_principles(query): Search for thinker principles
- search_cases(query): Search for case studies
- analyze(data): Analyze search results
- synthesize(findings): Synthesize final answer

Respond in this format:
THOUGHT: [Your reasoning about what to do next]
ACTION: [One of: search_models, search_principles, search_cases, analyze, synthesize, answer]
ACTION_INPUT: [Input for the action]

When you have enough information, use ACTION: answer with your final response."""


class MentalModelsAgent:
    """
    Agentic search over mental models using LLM reasoning.
    
    The agent can:
    1. Understand natural language queries
    2. Search semantically across models, principles, and cases
    3. Chain multiple searches together
    4. Synthesize answers using mental model frameworks
    """
    
    def __init__(
        self, 
        llm_provider: Optional[LLMProvider] = None,
        embedding_service: Optional[EmbeddingService] = None,
        max_steps: int = 10
    ):
        self.llm = llm_provider or get_provider()
        self.embeddings = embedding_service or EmbeddingService()
        self.max_steps = max_steps
        self.conversation_history: List[ChatMessage] = []
    
    def _parse_llm_response(self, response: str) -> tuple:
        """Parse the LLM response to extract thought, action, and action_input."""
        thought_match = re.search(r'THOUGHT:\s*(.+?)(?=ACTION:|$)', response, re.DOTALL)
        action_match = re.search(r'ACTION:\s*(\w+)', response)
        input_match = re.search(r'ACTION_INPUT:\s*(.+?)(?=THOUGHT:|ACTION:|$)', response, re.DOTALL)
        
        thought = thought_match.group(1).strip() if thought_match else ""
        action = action_match.group(1).strip().lower() if action_match else "answer"
        action_input = input_match.group(1).strip() if input_match else response
        
        return thought, action, action_input
    
    def _execute_action(self, action: str, action_input: str) -> str:
        """Execute an agent action and return the observation."""
        if action == "search_models":
            results = self.embeddings.semantic_search(
                action_input, 
                source_types=["model"],
                limit=5
            )
            return self._format_search_results(results)
        
        elif action == "search_principles":
            results = self.embeddings.semantic_search(
                action_input,
                source_types=["principle"],
                limit=5
            )
            return self._format_search_results(results)
        
        elif action == "search_cases":
            results = self.embeddings.semantic_search(
                action_input,
                source_types=["case"],
                limit=5
            )
            return self._format_search_results(results)
        
        elif action == "analyze":
            return f"Analysis of: {action_input}"
        
        elif action == "synthesize":
            return f"Synthesis: {action_input}"
        
        elif action == "answer":
            return action_input
        
        else:
            return f"Unknown action: {action}"
    
    def _format_search_results(self, results: List[SearchResult]) -> str:
        """Format search results for the LLM."""
        if not results:
            return "No results found."
        
        formatted = []
        for i, r in enumerate(results, 1):
            formatted.append(f"{i}. [{r.source_type.upper()}] {r.name}")
            formatted.append(f"   {r.content[:200]}...")
            if r.metadata:
                formatted.append(f"   Metadata: {r.metadata}")
            formatted.append(f"   Similarity: {r.similarity:.3f}")
            formatted.append("")
        
        return "\n".join(formatted)
    
    def query(self, user_query: str) -> AgentResponse:
        """
        Process a user query using agentic reasoning.
        
        Args:
            user_query: Natural language question or problem
        
        Returns:
            AgentResponse with answer, sources, and reasoning steps
        """
        messages = [
            ChatMessage(role="system", content=SYSTEM_PROMPT),
            ChatMessage(role="user", content=user_query)
        ]
        
        steps: List[AgentStep] = []
        all_sources: List[SearchResult] = []
        models_used: List[str] = []
        principles_applied: List[str] = []
        
        for step_num in range(self.max_steps):
            try:
                response = self.llm.chat(messages)
                response_text = response.content
            except Exception as e:
                response_text = f"THOUGHT: LLM unavailable, using fallback search.\nACTION: search_models\nACTION_INPUT: {user_query}"
            
            thought, action, action_input = self._parse_llm_response(response_text)
            
            if action == "answer":
                return AgentResponse(
                    answer=action_input,
                    sources=all_sources,
                    reasoning_steps=steps,
                    models_used=list(set(models_used)),
                    principles_applied=list(set(principles_applied))
                )
            
            observation = self._execute_action(action, action_input)
            
            if action in ["search_models", "search_principles", "search_cases"]:
                results = self.embeddings.semantic_search(
                    action_input,
                    source_types=[action.replace("search_", "")],
                    limit=5
                )
                all_sources.extend(results)
                
                for r in results:
                    if r.source_type == "model":
                        models_used.append(r.name)
                    elif r.source_type == "principle":
                        principles_applied.append(r.name)
            
            step = AgentStep(
                thought=thought,
                action=AgentAction(action) if action in [a.value for a in AgentAction] else AgentAction.ANSWER,
                action_input=action_input,
                observation=observation
            )
            steps.append(step)
            
            messages.append(ChatMessage(role="assistant", content=response_text))
            messages.append(ChatMessage(role="user", content=f"OBSERVATION: {observation}"))
        
        return AgentResponse(
            answer="I was unable to complete the analysis within the allowed steps. Here's what I found so far.",
            sources=all_sources,
            reasoning_steps=steps,
            models_used=list(set(models_used)),
            principles_applied=list(set(principles_applied))
        )
    
    def quick_search(self, query: str, limit: int = 10) -> List[SearchResult]:
        """
        Quick semantic search without full agent reasoning.
        
        Args:
            query: Search query
            limit: Maximum results
        
        Returns:
            List of SearchResult objects
        """
        return self.embeddings.semantic_search(
            query,
            source_types=["model", "principle"],
            limit=limit
        )
    
    def get_relevant_models(self, situation: str) -> List[Dict[str, Any]]:
        """
        Get mental models relevant to a situation.
        
        Args:
            situation: Description of a situation or problem
        
        Returns:
            List of relevant models with explanations
        """
        results = self.embeddings.semantic_search(
            situation,
            source_types=["model"],
            limit=10
        )
        
        models = []
        for r in results:
            models.append({
                "name": r.name,
                "description": r.content,
                "relevance_score": r.similarity,
                "application_hint": f"Consider how {r.name} applies to: {situation[:100]}..."
            })
        
        return models
    
    def munger_analysis(self, problem: str) -> Dict[str, Any]:
        """
        Perform a Munger-style multi-model analysis of a problem.
        
        Uses the latticework approach: apply multiple mental models
        from different disciplines to gain comprehensive understanding.
        
        Args:
            problem: Problem or situation to analyze
        
        Returns:
            Analysis with models from multiple disciplines
        """
        disciplines = [
            "mathematics probability",
            "psychology bias",
            "economics incentives",
            "biology evolution",
            "physics systems"
        ]
        
        analysis = {
            "problem": problem,
            "disciplines": {},
            "synthesis": "",
            "key_models": [],
            "warnings": []
        }
        
        all_models = []
        for discipline in disciplines:
            query = f"{discipline} {problem}"
            results = self.embeddings.semantic_search(
                query,
                source_types=["model"],
                limit=3
            )
            
            analysis["disciplines"][discipline.split()[0]] = [
                {"name": r.name, "relevance": r.similarity}
                for r in results
            ]
            all_models.extend(results)
        
        all_models.sort(key=lambda x: x.similarity, reverse=True)
        analysis["key_models"] = [m.name for m in all_models[:5]]
        
        if len(analysis["key_models"]) >= 3:
            analysis["warnings"].append(
                "Lollapalooza potential: Multiple models converging may indicate "
                "extreme outcome. Consider second-order effects."
            )
        
        return analysis
    
    def close(self):
        """Clean up resources."""
        if self.llm:
            self.llm.close()
        if self.embeddings:
            self.embeddings.close()


def create_agent(provider_type: str = None) -> MentalModelsAgent:
    """Factory function to create a configured agent."""
    provider = get_provider(provider_type) if provider_type else None
    return MentalModelsAgent(llm_provider=provider)


if __name__ == "__main__":
    agent = MentalModelsAgent()
    
    print("Mental Models Agent - Interactive Mode")
    print("=" * 50)
    print("Ask questions about mental models, or describe a situation for analysis.")
    print("Type 'quit' to exit.\n")
    
    while True:
        try:
            query = input("You: ").strip()
            if query.lower() in ['quit', 'exit', 'q']:
                break
            
            if not query:
                continue
            
            print("\nSearching mental models...")
            results = agent.quick_search(query, limit=5)
            
            print(f"\nFound {len(results)} relevant items:\n")
            for r in results:
                print(f"  [{r.source_type}] {r.name}")
                print(f"    {r.content[:100]}...")
                print()
            
        except KeyboardInterrupt:
            break
        except Exception as e:
            print(f"Error: {e}")
    
    agent.close()
    print("\nGoodbye!")
