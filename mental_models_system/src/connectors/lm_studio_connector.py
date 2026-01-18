import os
import json
import logging
import asyncio
from dataclasses import dataclass, field
from typing import List, Optional, Dict, Any, AsyncIterator
from datetime import datetime
from enum import Enum

import aiohttp

from .base import BaseConnector

logger = logging.getLogger(__name__)


class LLAMAModelType(Enum):
    LLAMA_7B = "llama-7b"
    LLAMA_13B = "llama-13b"
    LLAMA_70B = "llama-70b"
    LLAMA_2_7B = "llama-2-7b"
    LLAMA_2_13B = "llama-2-13b"
    LLAMA_2_70B = "llama-2-70b"
    LLAMA_3_8B = "llama-3-8b"
    LLAMA_3_70B = "llama-3-70b"
    MISTRAL_7B = "mistral-7b"
    MIXTRAL_8X7B = "mixtral-8x7b"
    CODELLAMA_7B = "codellama-7b"
    CODELLAMA_34B = "codellama-34b"
    CUSTOM = "custom"


@dataclass
class LMStudioModel:
    id: str
    name: str
    type: str
    context_length: int
    loaded: bool = False
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class ChatMessage:
    role: str
    content: str


@dataclass
class CompletionResponse:
    content: str
    model: str
    tokens_used: int
    finish_reason: str
    metadata: Dict[str, Any] = field(default_factory=dict)


MENTAL_MODEL_PROMPTS = {
    "analyze_document": {
        "system": """You are an expert analyst trained in Charlie Munger's latticework of mental models.
Your task is to analyze text through the lens of 129 mental models from 17 legendary thinkers.

MENTAL MODEL CATEGORIES:
1. Psychology: Tendencies & Biases (Munger's 25 causes of misjudgment)
2. Thinking Tools & Meta-Frameworks (First principles, Inversion, Second-order thinking)
3. Economics & Business (Supply/demand, Opportunity cost, Comparative advantage)
4. Competitive Advantage & Moats (Network effects, Switching costs, Brand)
5. Mathematics & Probability (Bayes theorem, Expected value, Power laws)
6. Physics, Engineering & Systems (Feedback loops, Critical mass, Leverage)
7. Biology & Evolution (Natural selection, Adaptation, Red Queen effect)
8. Organizational & Institutional (Incentives, Principal-agent, Bureaucracy)

When analyzing, you MUST:
1. Identify which mental models are most relevant (with relevance scores 0-1)
2. Detect cognitive biases present in the reasoning
3. Look for patterns and connections across models
4. Identify potential lollapalooza effects (multiple models combining)
5. Apply inversion - consider what could go wrong

Respond in JSON format.""",
        "user": """Analyze the following text using mental models:

---
{content}
---

Return JSON with this structure:
{{
    "applicable_models": [
        {{"model_name": "...", "relevance_score": 0.0-1.0, "explanation": "...", "evidence": ["..."], "category": "..."}}
    ],
    "detected_biases": [
        {{"bias_name": "...", "confidence": 0.0-1.0, "evidence": "...", "mitigation": "..."}}
    ],
    "patterns": ["pattern1", "pattern2"],
    "lollapalooza": {{
        "score": 0.0-1.0,
        "models": ["model1", "model2"],
        "explanation": "..."
    }},
    "key_insights": ["insight1", "insight2"],
    "inverted_perspective": "What could go wrong or what's being missed..."
}}"""
    },
    
    "classify_models": {
        "system": """You are a mental models classifier. Identify which of the 129 mental models apply to the given text.

Focus on these key models:
- Circle of Competence, Margin of Safety, Mr. Market (Buffett/Munger)
- Reflexivity, Fallibility (Soros)
- Radical Transparency, Believability-Weighted Decision Making (Dalio)
- First Principles Thinking, 5-Step Algorithm (Musk)
- Inversion, Lollapalooza Effect, Two-Track Analysis (Munger)
- Antifragility, Black Swan, Lindy Effect (Taleb)
- Network Effects, Power Laws, Zero to One (Thiel)

Return a JSON array of matches with relevance scores.""",
        "user": """Classify this text by applicable mental models:

{content}

Return JSON array: [{{"model_name": "...", "relevance_score": 0.0-1.0, "explanation": "...", "category": "..."}}]"""
    },
    
    "detect_biases": {
        "system": """You are a cognitive bias detector trained in Munger's 25 causes of human misjudgment.

MUNGER'S 25 PSYCHOLOGICAL TENDENCIES:
1. Reward and Punishment Super-Response
2. Liking/Loving Tendency
3. Disliking/Hating Tendency
4. Doubt-Avoidance Tendency
5. Inconsistency-Avoidance Tendency
6. Curiosity Tendency
7. Kantian Fairness Tendency
8. Envy/Jealousy Tendency
9. Reciprocation Tendency
10. Influence-from-Mere-Association Tendency
11. Simple Pain-Avoiding Psychological Denial
12. Excessive Self-Regard Tendency
13. Over-Optimism Tendency
14. Deprival Super-Reaction Tendency
15. Social-Proof Tendency
16. Contrast-Misreaction Tendency
17. Stress-Influence Tendency
18. Availability-Misweighing Tendency
19. Use-It-or-Lose-It Tendency
20. Drug-Misinfluence Tendency
21. Senescence-Misinfluence Tendency
22. Authority-Misinfluence Tendency
23. Twaddle Tendency
24. Reason-Respecting Tendency
25. Lollapalooza Tendency

Detect biases with evidence and suggest mitigations.""",
        "user": """Detect cognitive biases in this text:

{content}

Return JSON array: [{{"bias_name": "...", "munger_number": 1-25, "confidence": 0.0-1.0, "evidence": "...", "mitigation": "..."}}]"""
    },
    
    "find_lollapalooza": {
        "system": """You are an expert in identifying Lollapalooza Effects - when multiple mental models combine to create extreme outcomes.

A Lollapalooza occurs when:
- Multiple psychological tendencies act in the same direction
- Several mental models reinforce each other
- Feedback loops amplify the effect
- The combination creates outcomes far beyond what any single factor would produce

Famous examples:
- Coca-Cola's success (social proof + association + availability + reward)
- Tulip mania (social proof + envy + deprival + contrast)
- Open-source software (network effects + reciprocation + social proof)

Identify potential lollapalooza effects with the interacting models.""",
        "user": """Find lollapalooza effects in this text:

{content}

Return JSON:
{{
    "score": 0.0-1.0,
    "models": ["model1", "model2", ...],
    "interactions": [{{"model1": "...", "model2": "...", "interaction": "..."}}],
    "explanation": "...",
    "historical_parallels": ["..."]
}}"""
    },
    
    "invert_analysis": {
        "system": """You are trained in Munger's inversion technique: "Invert, always invert."

Carl Jacobi's approach: Instead of asking how to succeed, ask how to fail and avoid those things.

When inverting, consider:
1. What could go wrong?
2. What's being ignored or missed?
3. What would the opposite perspective reveal?
4. What are the second and third-order effects?
5. What would make this fail?
6. Who benefits if this is wrong?
7. What assumptions are being made?

Provide a thorough inverted analysis.""",
        "user": """Apply inversion to this content:

{content}

{question}

Provide a detailed inverted analysis covering:
1. Potential failure modes
2. Hidden assumptions
3. Ignored perspectives
4. Second/third-order effects
5. What would need to be true for this to be wrong"""
    },
    
    "summarize_for_models": {
        "system": """You are a summarizer that extracts key information relevant to mental model analysis.

Focus on extracting:
- Key decisions and their rationale
- Assumptions being made
- Stakeholders and their incentives
- Risks and uncertainties mentioned
- Historical context or precedents
- Quantitative data and metrics
- Emotional or psychological factors

Create a concise summary optimized for mental model analysis.""",
        "user": """Summarize this text for mental model analysis:

{content}

Provide a structured summary with:
1. Core thesis/argument (2-3 sentences)
2. Key decisions or actions
3. Stakeholders and incentives
4. Assumptions (explicit and implicit)
5. Risks and uncertainties
6. Relevant data points"""
    },
    
    "extract_principles": {
        "system": """You are an expert at extracting timeless principles from text, in the style of Ray Dalio's Principles.

A good principle:
- Is actionable and specific
- Can be applied across contexts
- Has clear conditions for when it applies
- Includes the reasoning behind it

Extract principles that would be valuable for decision-making.""",
        "user": """Extract principles from this text:

{content}

Return JSON array of principles:
[{{
    "principle": "Clear, actionable statement",
    "reasoning": "Why this principle works",
    "conditions": "When to apply this",
    "source_evidence": "Quote or reference from text",
    "related_mental_models": ["model1", "model2"]
}}]"""
    },
    
    "compare_thinkers": {
        "system": """You are an expert on the thinking styles of legendary investors and thinkers.

Compare how different thinkers would approach a situation:
- Munger: Latticework of mental models, inversion, avoiding stupidity
- Buffett: Circle of competence, margin of safety, long-term thinking
- Soros: Reflexivity, fallibility, boom-bust cycles
- Dalio: Radical transparency, principles, believability weighting
- Musk: First principles, 5-step algorithm, ambitious goals
- Thiel: Zero to one, contrarian thinking, monopoly seeking
- Taleb: Antifragility, black swans, skin in the game

Provide perspectives from multiple thinkers.""",
        "user": """How would different legendary thinkers analyze this:

{content}

Provide analysis from at least 4 different thinkers, including:
1. Their likely perspective
2. Key questions they would ask
3. What they would focus on
4. Their probable conclusion"""
    }
}


class LMStudioConnector(BaseConnector):
    def __init__(
        self,
        base_url: str = None,
        model_name: str = "local-model",
        temperature: float = 0.3,
        max_tokens: int = 4000,
        timeout: float = 120.0,
    ):
        self.base_url = base_url or os.getenv("LM_STUDIO_URL", "http://localhost:1234")
        self.model_name = model_name
        self.temperature = temperature
        self.max_tokens = max_tokens
        self.timeout = timeout
        self._session: Optional[aiohttp.ClientSession] = None
        self._connected = False

    async def connect(self) -> bool:
        try:
            self._session = aiohttp.ClientSession(
                timeout=aiohttp.ClientTimeout(total=self.timeout)
            )
            models = await self.list_models()
            self._connected = len(models) > 0
            if self._connected:
                logger.info(f"Connected to LM Studio at {self.base_url}")
            return self._connected
        except Exception as e:
            logger.error(f"Failed to connect to LM Studio: {e}")
            self._connected = False
            return False

    async def disconnect(self) -> bool:
        if self._session and not self._session.closed:
            await self._session.close()
        self._connected = False
        return True

    async def health_check(self) -> bool:
        try:
            models = await self.list_models()
            return len(models) > 0
        except Exception:
            return False

    def get_info(self) -> Dict[str, Any]:
        return {
            "name": "LM Studio",
            "type": "llm",
            "base_url": self.base_url,
            "model_name": self.model_name,
            "temperature": self.temperature,
            "max_tokens": self.max_tokens,
            "connected": self._connected,
            "available_prompts": list(MENTAL_MODEL_PROMPTS.keys()),
        }

    async def fetch(self, resource: str, params: Dict[str, Any] = None) -> Any:
        if resource == "models":
            return await self.list_models()
        elif resource == "health":
            return await self.health_check()
        return None

    async def push(self, resource: str, data: Any) -> bool:
        return False

    async def _get_session(self) -> aiohttp.ClientSession:
        if self._session is None or self._session.closed:
            self._session = aiohttp.ClientSession(
                timeout=aiohttp.ClientTimeout(total=self.timeout)
            )
        return self._session

    async def list_models(self) -> List[LMStudioModel]:
        session = await self._get_session()
        
        try:
            async with session.get(f"{self.base_url}/v1/models") as response:
                if response.status != 200:
                    return []
                
                data = await response.json()
                models = []
                
                for model_data in data.get("data", []):
                    models.append(LMStudioModel(
                        id=model_data.get("id", ""),
                        name=model_data.get("id", ""),
                        type=model_data.get("object", "model"),
                        context_length=model_data.get("context_length", 4096),
                        loaded=True,
                    ))
                
                return models
                
        except Exception as e:
            logger.debug(f"Failed to list models: {e}")
            return []

    async def chat_completion(
        self,
        messages: List[ChatMessage],
        model: str = None,
        temperature: float = None,
        max_tokens: int = None,
        stream: bool = False,
    ) -> CompletionResponse:
        session = await self._get_session()
        
        payload = {
            "model": model or self.model_name,
            "messages": [{"role": m.role, "content": m.content} for m in messages],
            "temperature": temperature or self.temperature,
            "max_tokens": max_tokens or self.max_tokens,
            "stream": stream,
        }
        
        try:
            async with session.post(
                f"{self.base_url}/v1/chat/completions",
                json=payload,
            ) as response:
                if response.status != 200:
                    error_text = await response.text()
                    logger.error(f"Chat completion failed: {response.status} - {error_text}")
                    return CompletionResponse(
                        content="",
                        model=model or self.model_name,
                        tokens_used=0,
                        finish_reason="error",
                        metadata={"error": error_text},
                    )
                
                data = await response.json()
                choice = data.get("choices", [{}])[0]
                usage = data.get("usage", {})
                
                return CompletionResponse(
                    content=choice.get("message", {}).get("content", ""),
                    model=data.get("model", model or self.model_name),
                    tokens_used=usage.get("total_tokens", 0),
                    finish_reason=choice.get("finish_reason", "stop"),
                    metadata={"usage": usage},
                )
                
        except Exception as e:
            logger.error(f"Chat completion failed: {e}")
            return CompletionResponse(
                content="",
                model=model or self.model_name,
                tokens_used=0,
                finish_reason="error",
                metadata={"error": str(e)},
            )

    async def chat_completion_stream(
        self,
        messages: List[ChatMessage],
        model: str = None,
        temperature: float = None,
        max_tokens: int = None,
    ) -> AsyncIterator[str]:
        session = await self._get_session()
        
        payload = {
            "model": model or self.model_name,
            "messages": [{"role": m.role, "content": m.content} for m in messages],
            "temperature": temperature or self.temperature,
            "max_tokens": max_tokens or self.max_tokens,
            "stream": True,
        }
        
        try:
            async with session.post(
                f"{self.base_url}/v1/chat/completions",
                json=payload,
            ) as response:
                if response.status != 200:
                    return
                
                async for line in response.content:
                    line = line.decode("utf-8").strip()
                    if line.startswith("data: "):
                        data_str = line[6:]
                        if data_str == "[DONE]":
                            break
                        try:
                            data = json.loads(data_str)
                            delta = data.get("choices", [{}])[0].get("delta", {})
                            content = delta.get("content", "")
                            if content:
                                yield content
                        except json.JSONDecodeError:
                            continue
                            
        except Exception as e:
            logger.error(f"Streaming completion failed: {e}")

    async def generate_embeddings(
        self,
        texts: List[str],
        model: str = None,
    ) -> List[List[float]]:
        session = await self._get_session()
        embeddings = []
        
        for text in texts:
            try:
                async with session.post(
                    f"{self.base_url}/v1/embeddings",
                    json={
                        "model": model or "text-embedding-ada-002",
                        "input": text[:8000],
                    },
                ) as response:
                    if response.status == 200:
                        data = await response.json()
                        embedding = data.get("data", [{}])[0].get("embedding", [])
                        embeddings.append(embedding)
                    else:
                        embeddings.append([])
                        
            except Exception as e:
                logger.debug(f"Embedding generation failed: {e}")
                embeddings.append([])
        
        return embeddings

    async def analyze_with_prompt(
        self,
        prompt_name: str,
        content: str,
        question: str = "",
        **kwargs,
    ) -> Dict[str, Any]:
        if prompt_name not in MENTAL_MODEL_PROMPTS:
            return {"error": f"Unknown prompt: {prompt_name}"}
        
        prompt_config = MENTAL_MODEL_PROMPTS[prompt_name]
        system_prompt = prompt_config["system"]
        user_template = prompt_config["user"]
        
        user_prompt = user_template.format(content=content[:4000], question=question, **kwargs)
        
        messages = [
            ChatMessage(role="system", content=system_prompt),
            ChatMessage(role="user", content=user_prompt),
        ]
        
        response = await self.chat_completion(messages)
        
        if response.finish_reason == "error":
            return {"error": response.metadata.get("error", "Unknown error")}
        
        try:
            json_start = response.content.find("{")
            json_end = response.content.rfind("}") + 1
            
            if json_start == -1:
                json_start = response.content.find("[")
                json_end = response.content.rfind("]") + 1
            
            if json_start >= 0 and json_end > json_start:
                return json.loads(response.content[json_start:json_end])
            
            return {"raw_response": response.content}
            
        except json.JSONDecodeError:
            return {"raw_response": response.content}

    async def analyze_document(self, content: str) -> Dict[str, Any]:
        return await self.analyze_with_prompt("analyze_document", content)

    async def classify_models(self, content: str) -> List[Dict[str, Any]]:
        result = await self.analyze_with_prompt("classify_models", content)
        if isinstance(result, list):
            return result
        return result.get("models", [])

    async def detect_biases(self, content: str) -> List[Dict[str, Any]]:
        result = await self.analyze_with_prompt("detect_biases", content)
        if isinstance(result, list):
            return result
        return result.get("biases", [])

    async def find_lollapalooza(self, content: str) -> Dict[str, Any]:
        return await self.analyze_with_prompt("find_lollapalooza", content)

    async def invert_analysis(self, content: str, question: str = "") -> str:
        result = await self.analyze_with_prompt("invert_analysis", content, question=question)
        if isinstance(result, dict) and "raw_response" in result:
            return result["raw_response"]
        return json.dumps(result, indent=2)

    async def summarize_for_models(self, content: str) -> Dict[str, Any]:
        return await self.analyze_with_prompt("summarize_for_models", content)

    async def extract_principles(self, content: str) -> List[Dict[str, Any]]:
        result = await self.analyze_with_prompt("extract_principles", content)
        if isinstance(result, list):
            return result
        return result.get("principles", [])

    async def compare_thinkers(self, content: str) -> Dict[str, Any]:
        return await self.analyze_with_prompt("compare_thinkers", content)


def create_lm_studio_connector(
    base_url: str = None,
    model_name: str = "local-model",
    temperature: float = 0.3,
) -> LMStudioConnector:
    return LMStudioConnector(
        base_url=base_url,
        model_name=model_name,
        temperature=temperature,
    )
