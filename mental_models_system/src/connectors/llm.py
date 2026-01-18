"""
LLM Provider Connectors

Multiple LLM backends:
- Ollama (local, open source)
- llama.cpp (local, open source)
- vLLM (local, open source)
- OpenAI (cloud)
- Anthropic (cloud)
"""

import os
import json
import asyncio
import logging
import aiohttp
from dataclasses import dataclass
from datetime import datetime
from typing import Dict, List, Optional, Any, AsyncGenerator
from abc import abstractmethod

from .base import BaseConnector, ConnectorConfig, ConnectorType, ConnectorStatus

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


@dataclass
class LLMResponse:
    """LLM response."""
    text: str
    model: str
    tokens_used: int
    finish_reason: str
    latency_ms: float
    
    def to_dict(self) -> Dict:
        return {
            "text": self.text,
            "model": self.model,
            "tokens_used": self.tokens_used,
            "finish_reason": self.finish_reason,
            "latency_ms": self.latency_ms
        }


class BaseLLMConnector(BaseConnector):
    """Base class for LLM connectors."""
    
    TYPE = ConnectorType.LLM
    
    @abstractmethod
    async def generate(self, prompt: str, **kwargs) -> LLMResponse:
        """Generate text from prompt."""
        pass
    
    @abstractmethod
    async def generate_stream(self, prompt: str, **kwargs) -> AsyncGenerator[str, None]:
        """Stream generated text."""
        pass
    
    @abstractmethod
    async def embed(self, text: str) -> List[float]:
        """Generate embeddings."""
        pass


# =============================================================================
# OLLAMA CONNECTOR (Local, Open Source)
# =============================================================================

class OllamaConnector(BaseLLMConnector):
    """
    Ollama connector for local LLM inference.
    
    Fully open source, runs locally:
    - No API costs
    - Data stays local
    - Supports many models (Llama, Mistral, etc.)
    """
    
    NAME = "ollama"
    DESCRIPTION = "Local LLM inference with Ollama"
    REQUIRED_CREDENTIALS = []
    OPTIONAL_CREDENTIALS = ["host", "model"]
    OPEN_SOURCE = True
    
    def __init__(self, config: ConnectorConfig = None):
        super().__init__(config)
        self._host = os.environ.get("OLLAMA_HOST", "http://localhost:11434")
        self._model = os.environ.get("OLLAMA_MODEL", "llama3:8b")
    
    async def connect(self) -> bool:
        host = self.config.settings.get("host") or self._host
        self._host = host
        
        try:
            async with aiohttp.ClientSession() as session:
                async with session.get(f"{self._host}/api/tags") as resp:
                    if resp.status == 200:
                        self.status = ConnectorStatus.CONNECTED
                        return True
        except Exception as e:
            logger.warning(f"Ollama not available: {e}")
        
        self.status = ConnectorStatus.ERROR
        return False
    
    async def disconnect(self) -> bool:
        self.status = ConnectorStatus.DISCONNECTED
        return True
    
    async def health_check(self) -> bool:
        try:
            async with aiohttp.ClientSession() as session:
                async with session.get(f"{self._host}/api/tags") as resp:
                    return resp.status == 200
        except:
            return False
    
    async def list_models(self) -> List[str]:
        """List available models."""
        try:
            async with aiohttp.ClientSession() as session:
                async with session.get(f"{self._host}/api/tags") as resp:
                    if resp.status == 200:
                        data = await resp.json()
                        return [m["name"] for m in data.get("models", [])]
        except:
            pass
        return []
    
    async def generate(self, prompt: str, model: str = None,
                      temperature: float = 0.7, max_tokens: int = 2048,
                      system: str = None, **kwargs) -> LLMResponse:
        """Generate text."""
        model = model or self._model
        start_time = datetime.now()
        
        payload = {
            "model": model,
            "prompt": prompt,
            "stream": False,
            "options": {
                "temperature": temperature,
                "num_predict": max_tokens
            }
        }
        
        if system:
            payload["system"] = system
        
        try:
            async with aiohttp.ClientSession() as session:
                async with session.post(
                    f"{self._host}/api/generate",
                    json=payload,
                    timeout=aiohttp.ClientTimeout(total=300)
                ) as resp:
                    if resp.status != 200:
                        error = await resp.text()
                        logger.error(f"Ollama error: {error}")
                        return LLMResponse(
                            text="",
                            model=model,
                            tokens_used=0,
                            finish_reason="error",
                            latency_ms=0
                        )
                    
                    data = await resp.json()
                    
                    latency = (datetime.now() - start_time).total_seconds() * 1000
                    
                    return LLMResponse(
                        text=data.get("response", ""),
                        model=model,
                        tokens_used=data.get("eval_count", 0),
                        finish_reason="stop",
                        latency_ms=latency
                    )
        except Exception as e:
            logger.error(f"Ollama generate failed: {e}")
            return LLMResponse(
                text="",
                model=model,
                tokens_used=0,
                finish_reason="error",
                latency_ms=0
            )
    
    async def generate_stream(self, prompt: str, model: str = None,
                             **kwargs) -> AsyncGenerator[str, None]:
        """Stream generated text."""
        model = model or self._model
        
        payload = {
            "model": model,
            "prompt": prompt,
            "stream": True
        }
        
        try:
            async with aiohttp.ClientSession() as session:
                async with session.post(
                    f"{self._host}/api/generate",
                    json=payload
                ) as resp:
                    async for line in resp.content:
                        if line:
                            data = json.loads(line)
                            if "response" in data:
                                yield data["response"]
        except Exception as e:
            logger.error(f"Ollama stream failed: {e}")
    
    async def embed(self, text: str, model: str = None) -> List[float]:
        """Generate embeddings."""
        model = model or "nomic-embed-text"
        
        payload = {
            "model": model,
            "prompt": text
        }
        
        try:
            async with aiohttp.ClientSession() as session:
                async with session.post(
                    f"{self._host}/api/embeddings",
                    json=payload
                ) as resp:
                    if resp.status == 200:
                        data = await resp.json()
                        return data.get("embedding", [])
        except Exception as e:
            logger.error(f"Ollama embed failed: {e}")
        
        return []
    
    async def chat(self, messages: List[Dict], model: str = None,
                  **kwargs) -> LLMResponse:
        """Chat completion."""
        model = model or self._model
        start_time = datetime.now()
        
        payload = {
            "model": model,
            "messages": messages,
            "stream": False
        }
        
        try:
            async with aiohttp.ClientSession() as session:
                async with session.post(
                    f"{self._host}/api/chat",
                    json=payload,
                    timeout=aiohttp.ClientTimeout(total=300)
                ) as resp:
                    if resp.status == 200:
                        data = await resp.json()
                        latency = (datetime.now() - start_time).total_seconds() * 1000
                        
                        return LLMResponse(
                            text=data.get("message", {}).get("content", ""),
                            model=model,
                            tokens_used=data.get("eval_count", 0),
                            finish_reason="stop",
                            latency_ms=latency
                        )
        except Exception as e:
            logger.error(f"Ollama chat failed: {e}")
        
        return LLMResponse(text="", model=model, tokens_used=0, finish_reason="error", latency_ms=0)


# =============================================================================
# LLAMA.CPP CONNECTOR (Local, Open Source)
# =============================================================================

class LlamaCppConnector(BaseLLMConnector):
    """
    llama.cpp connector for efficient local inference.
    
    Most efficient local inference:
    - Quantized models
    - CPU and GPU support
    - Low memory usage
    """
    
    NAME = "llamacpp"
    DESCRIPTION = "Efficient local inference with llama.cpp"
    REQUIRED_CREDENTIALS = []
    OPTIONAL_CREDENTIALS = ["host", "model_path"]
    OPEN_SOURCE = True
    
    def __init__(self, config: ConnectorConfig = None):
        super().__init__(config)
        self._host = os.environ.get("LLAMACPP_HOST", "http://localhost:8080")
    
    async def connect(self) -> bool:
        host = self.config.settings.get("host") or self._host
        self._host = host
        
        try:
            async with aiohttp.ClientSession() as session:
                async with session.get(f"{self._host}/health") as resp:
                    if resp.status == 200:
                        self.status = ConnectorStatus.CONNECTED
                        return True
        except:
            pass
        
        self.status = ConnectorStatus.ERROR
        return False
    
    async def disconnect(self) -> bool:
        self.status = ConnectorStatus.DISCONNECTED
        return True
    
    async def health_check(self) -> bool:
        try:
            async with aiohttp.ClientSession() as session:
                async with session.get(f"{self._host}/health") as resp:
                    return resp.status == 200
        except:
            return False
    
    async def generate(self, prompt: str, temperature: float = 0.7,
                      max_tokens: int = 2048, **kwargs) -> LLMResponse:
        """Generate text."""
        start_time = datetime.now()
        
        payload = {
            "prompt": prompt,
            "temperature": temperature,
            "n_predict": max_tokens,
            "stream": False
        }
        
        try:
            async with aiohttp.ClientSession() as session:
                async with session.post(
                    f"{self._host}/completion",
                    json=payload,
                    timeout=aiohttp.ClientTimeout(total=300)
                ) as resp:
                    if resp.status == 200:
                        data = await resp.json()
                        latency = (datetime.now() - start_time).total_seconds() * 1000
                        
                        return LLMResponse(
                            text=data.get("content", ""),
                            model="llama.cpp",
                            tokens_used=data.get("tokens_predicted", 0),
                            finish_reason=data.get("stop_type", "stop"),
                            latency_ms=latency
                        )
        except Exception as e:
            logger.error(f"llama.cpp generate failed: {e}")
        
        return LLMResponse(text="", model="llama.cpp", tokens_used=0, finish_reason="error", latency_ms=0)
    
    async def generate_stream(self, prompt: str, **kwargs) -> AsyncGenerator[str, None]:
        """Stream generated text."""
        payload = {
            "prompt": prompt,
            "stream": True
        }
        
        try:
            async with aiohttp.ClientSession() as session:
                async with session.post(
                    f"{self._host}/completion",
                    json=payload
                ) as resp:
                    async for line in resp.content:
                        if line:
                            try:
                                data = json.loads(line.decode().strip("data: "))
                                if "content" in data:
                                    yield data["content"]
                            except:
                                pass
        except Exception as e:
            logger.error(f"llama.cpp stream failed: {e}")
    
    async def embed(self, text: str) -> List[float]:
        """Generate embeddings."""
        payload = {"content": text}
        
        try:
            async with aiohttp.ClientSession() as session:
                async with session.post(
                    f"{self._host}/embedding",
                    json=payload
                ) as resp:
                    if resp.status == 200:
                        data = await resp.json()
                        return data.get("embedding", [])
        except Exception as e:
            logger.error(f"llama.cpp embed failed: {e}")
        
        return []


# =============================================================================
# OPENAI CONNECTOR (Cloud)
# =============================================================================

class OpenAIConnector(BaseLLMConnector):
    """
    OpenAI API connector.
    
    Cloud-based, high quality:
    - GPT-4, GPT-3.5
    - Embeddings
    - Function calling
    """
    
    NAME = "openai"
    DESCRIPTION = "OpenAI API (GPT-4, GPT-3.5)"
    REQUIRED_CREDENTIALS = ["api_key"]
    OPTIONAL_CREDENTIALS = ["organization", "base_url"]
    OPEN_SOURCE = False
    
    def __init__(self, config: ConnectorConfig = None):
        super().__init__(config)
        self._client = None
        self._model = os.environ.get("OPENAI_MODEL", "gpt-4.1-mini")
    
    async def connect(self) -> bool:
        try:
            from openai import AsyncOpenAI
            
            api_key = self.config.credentials.get("api_key") or os.environ.get("OPENAI_API_KEY")
            base_url = self.config.credentials.get("base_url") or os.environ.get("OPENAI_BASE_URL")
            
            if not api_key:
                logger.error("OpenAI API key not provided")
                self.status = ConnectorStatus.ERROR
                return False
            
            self._client = AsyncOpenAI(
                api_key=api_key,
                base_url=base_url
            )
            
            self.status = ConnectorStatus.CONNECTED
            return True
            
        except ImportError:
            logger.error("openai not installed. Run: pip install openai")
            self.status = ConnectorStatus.ERROR
            return False
    
    async def disconnect(self) -> bool:
        self._client = None
        self.status = ConnectorStatus.DISCONNECTED
        return True
    
    async def health_check(self) -> bool:
        return self._client is not None
    
    async def generate(self, prompt: str, model: str = None,
                      temperature: float = 0.7, max_tokens: int = 2048,
                      system: str = None, **kwargs) -> LLMResponse:
        """Generate text using chat completion."""
        if not self._client:
            return LLMResponse(text="", model="", tokens_used=0, finish_reason="error", latency_ms=0)
        
        model = model or self._model
        start_time = datetime.now()
        
        messages = []
        if system:
            messages.append({"role": "system", "content": system})
        messages.append({"role": "user", "content": prompt})
        
        try:
            response = await self._client.chat.completions.create(
                model=model,
                messages=messages,
                temperature=temperature,
                max_tokens=max_tokens
            )
            
            latency = (datetime.now() - start_time).total_seconds() * 1000
            
            return LLMResponse(
                text=response.choices[0].message.content or "",
                model=model,
                tokens_used=response.usage.total_tokens if response.usage else 0,
                finish_reason=response.choices[0].finish_reason or "stop",
                latency_ms=latency
            )
        except Exception as e:
            logger.error(f"OpenAI generate failed: {e}")
            return LLMResponse(text="", model=model, tokens_used=0, finish_reason="error", latency_ms=0)
    
    async def generate_stream(self, prompt: str, model: str = None,
                             **kwargs) -> AsyncGenerator[str, None]:
        """Stream generated text."""
        if not self._client:
            return
        
        model = model or self._model
        messages = [{"role": "user", "content": prompt}]
        
        try:
            stream = await self._client.chat.completions.create(
                model=model,
                messages=messages,
                stream=True
            )
            
            async for chunk in stream:
                if chunk.choices[0].delta.content:
                    yield chunk.choices[0].delta.content
        except Exception as e:
            logger.error(f"OpenAI stream failed: {e}")
    
    async def embed(self, text: str, model: str = "text-embedding-3-small") -> List[float]:
        """Generate embeddings."""
        if not self._client:
            return []
        
        try:
            response = await self._client.embeddings.create(
                model=model,
                input=text
            )
            return response.data[0].embedding
        except Exception as e:
            logger.error(f"OpenAI embed failed: {e}")
            return []
    
    async def chat(self, messages: List[Dict], model: str = None,
                  **kwargs) -> LLMResponse:
        """Chat completion."""
        if not self._client:
            return LLMResponse(text="", model="", tokens_used=0, finish_reason="error", latency_ms=0)
        
        model = model or self._model
        start_time = datetime.now()
        
        try:
            response = await self._client.chat.completions.create(
                model=model,
                messages=messages,
                **kwargs
            )
            
            latency = (datetime.now() - start_time).total_seconds() * 1000
            
            return LLMResponse(
                text=response.choices[0].message.content or "",
                model=model,
                tokens_used=response.usage.total_tokens if response.usage else 0,
                finish_reason=response.choices[0].finish_reason or "stop",
                latency_ms=latency
            )
        except Exception as e:
            logger.error(f"OpenAI chat failed: {e}")
            return LLMResponse(text="", model=model, tokens_used=0, finish_reason="error", latency_ms=0)


# Register connectors
from .base import registry
registry.register_class(OllamaConnector)
registry.register_class(LlamaCppConnector)
registry.register_class(OpenAIConnector)
