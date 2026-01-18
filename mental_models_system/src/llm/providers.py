#!/usr/bin/env python3
"""
LLM Provider Abstraction Layer
Supports LM Studio, OpenAI, Ollama, and any OpenAI-compatible endpoint.
"""

import os
import sys
from abc import ABC, abstractmethod
from typing import List, Dict, Any, Optional, Generator
from dataclasses import dataclass
import json
import httpx
from enum import Enum

sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
from config.settings import settings


class ProviderType(Enum):
    """Supported LLM provider types."""
    LM_STUDIO = "lm_studio"
    OPENAI = "openai"
    OLLAMA = "ollama"
    CUSTOM = "custom"


@dataclass
class LLMConfig:
    """Configuration for LLM provider."""
    provider_type: ProviderType
    base_url: str
    api_key: Optional[str] = None
    model: str = "default"
    temperature: float = 0.7
    max_tokens: int = 2048
    timeout: float = 120.0


@dataclass
class ChatMessage:
    """Chat message structure."""
    role: str  # "system", "user", "assistant"
    content: str


@dataclass
class LLMResponse:
    """LLM response structure."""
    content: str
    model: str
    usage: Dict[str, int]
    finish_reason: str


class LLMProvider(ABC):
    """Abstract base class for LLM providers."""
    
    def __init__(self, config: LLMConfig):
        self.config = config
        self.client = httpx.Client(timeout=config.timeout)
    
    @abstractmethod
    def chat(self, messages: List[ChatMessage], **kwargs) -> LLMResponse:
        """Send chat completion request."""
        pass
    
    @abstractmethod
    def chat_stream(self, messages: List[ChatMessage], **kwargs) -> Generator[str, None, None]:
        """Stream chat completion response."""
        pass
    
    @abstractmethod
    def embed(self, text: str) -> List[float]:
        """Generate embedding for text."""
        pass
    
    def close(self):
        """Close the HTTP client."""
        self.client.close()


class OpenAICompatibleProvider(LLMProvider):
    """Base class for OpenAI-compatible APIs (LM Studio, OpenAI, etc.)."""
    
    def _get_headers(self) -> Dict[str, str]:
        """Get request headers."""
        headers = {"Content-Type": "application/json"}
        if self.config.api_key:
            headers["Authorization"] = f"Bearer {self.config.api_key}"
        return headers
    
    def chat(self, messages: List[ChatMessage], **kwargs) -> LLMResponse:
        """Send chat completion request."""
        url = f"{self.config.base_url}/v1/chat/completions"
        
        payload = {
            "model": kwargs.get("model", self.config.model),
            "messages": [{"role": m.role, "content": m.content} for m in messages],
            "temperature": kwargs.get("temperature", self.config.temperature),
            "max_tokens": kwargs.get("max_tokens", self.config.max_tokens),
            "stream": False
        }
        
        response = self.client.post(url, json=payload, headers=self._get_headers())
        response.raise_for_status()
        data = response.json()
        
        return LLMResponse(
            content=data["choices"][0]["message"]["content"],
            model=data.get("model", self.config.model),
            usage=data.get("usage", {}),
            finish_reason=data["choices"][0].get("finish_reason", "stop")
        )
    
    def chat_stream(self, messages: List[ChatMessage], **kwargs) -> Generator[str, None, None]:
        """Stream chat completion response."""
        url = f"{self.config.base_url}/v1/chat/completions"
        
        payload = {
            "model": kwargs.get("model", self.config.model),
            "messages": [{"role": m.role, "content": m.content} for m in messages],
            "temperature": kwargs.get("temperature", self.config.temperature),
            "max_tokens": kwargs.get("max_tokens", self.config.max_tokens),
            "stream": True
        }
        
        with self.client.stream("POST", url, json=payload, headers=self._get_headers()) as response:
            response.raise_for_status()
            for line in response.iter_lines():
                if line.startswith("data: "):
                    data = line[6:]
                    if data == "[DONE]":
                        break
                    try:
                        chunk = json.loads(data)
                        if chunk["choices"][0].get("delta", {}).get("content"):
                            yield chunk["choices"][0]["delta"]["content"]
                    except json.JSONDecodeError:
                        continue
    
    def embed(self, text: str) -> List[float]:
        """Generate embedding for text."""
        url = f"{self.config.base_url}/v1/embeddings"
        
        payload = {
            "model": self.config.model,
            "input": text
        }
        
        response = self.client.post(url, json=payload, headers=self._get_headers())
        response.raise_for_status()
        data = response.json()
        
        return data["data"][0]["embedding"]


class LMStudioProvider(OpenAICompatibleProvider):
    """LM Studio provider - runs locally with OpenAI-compatible API."""
    
    def __init__(self, 
                 base_url: str = "http://localhost:1234",
                 model: str = "local-model",
                 **kwargs):
        config = LLMConfig(
            provider_type=ProviderType.LM_STUDIO,
            base_url=base_url,
            model=model,
            **kwargs
        )
        super().__init__(config)
    
    @classmethod
    def from_env(cls) -> "LMStudioProvider":
        """Create provider from environment variables."""
        return cls(
            base_url=os.getenv("LM_STUDIO_URL", "http://localhost:1234"),
            model=os.getenv("LM_STUDIO_MODEL", "local-model"),
            temperature=float(os.getenv("LM_STUDIO_TEMPERATURE", "0.7")),
            max_tokens=int(os.getenv("LM_STUDIO_MAX_TOKENS", "2048"))
        )


class OpenAIProvider(OpenAICompatibleProvider):
    """OpenAI API provider."""
    
    def __init__(self,
                 api_key: Optional[str] = None,
                 model: str = "gpt-4-turbo-preview",
                 **kwargs):
        config = LLMConfig(
            provider_type=ProviderType.OPENAI,
            base_url="https://api.openai.com",
            api_key=api_key or os.getenv("OPENAI_API_KEY"),
            model=model,
            **kwargs
        )
        super().__init__(config)
    
    @classmethod
    def from_env(cls) -> "OpenAIProvider":
        """Create provider from environment variables."""
        return cls(
            api_key=os.getenv("OPENAI_API_KEY"),
            model=os.getenv("OPENAI_MODEL", "gpt-4-turbo-preview"),
            temperature=float(os.getenv("OPENAI_TEMPERATURE", "0.7")),
            max_tokens=int(os.getenv("OPENAI_MAX_TOKENS", "2048"))
        )


class OllamaProvider(LLMProvider):
    """Ollama provider for local LLM inference."""
    
    def __init__(self,
                 base_url: str = "http://localhost:11434",
                 model: str = "llama2",
                 **kwargs):
        config = LLMConfig(
            provider_type=ProviderType.OLLAMA,
            base_url=base_url,
            model=model,
            **kwargs
        )
        super().__init__(config)
    
    @classmethod
    def from_env(cls) -> "OllamaProvider":
        """Create provider from environment variables."""
        return cls(
            base_url=os.getenv("OLLAMA_URL", "http://localhost:11434"),
            model=os.getenv("OLLAMA_MODEL", "llama2"),
            temperature=float(os.getenv("OLLAMA_TEMPERATURE", "0.7"))
        )
    
    def chat(self, messages: List[ChatMessage], **kwargs) -> LLMResponse:
        """Send chat completion request to Ollama."""
        url = f"{self.config.base_url}/api/chat"
        
        payload = {
            "model": kwargs.get("model", self.config.model),
            "messages": [{"role": m.role, "content": m.content} for m in messages],
            "stream": False,
            "options": {
                "temperature": kwargs.get("temperature", self.config.temperature)
            }
        }
        
        response = self.client.post(url, json=payload)
        response.raise_for_status()
        data = response.json()
        
        return LLMResponse(
            content=data["message"]["content"],
            model=data.get("model", self.config.model),
            usage={
                "prompt_tokens": data.get("prompt_eval_count", 0),
                "completion_tokens": data.get("eval_count", 0),
                "total_tokens": data.get("prompt_eval_count", 0) + data.get("eval_count", 0)
            },
            finish_reason="stop"
        )
    
    def chat_stream(self, messages: List[ChatMessage], **kwargs) -> Generator[str, None, None]:
        """Stream chat completion response from Ollama."""
        url = f"{self.config.base_url}/api/chat"
        
        payload = {
            "model": kwargs.get("model", self.config.model),
            "messages": [{"role": m.role, "content": m.content} for m in messages],
            "stream": True,
            "options": {
                "temperature": kwargs.get("temperature", self.config.temperature)
            }
        }
        
        with self.client.stream("POST", url, json=payload) as response:
            response.raise_for_status()
            for line in response.iter_lines():
                if line:
                    try:
                        chunk = json.loads(line)
                        if chunk.get("message", {}).get("content"):
                            yield chunk["message"]["content"]
                    except json.JSONDecodeError:
                        continue
    
    def embed(self, text: str) -> List[float]:
        """Generate embedding using Ollama."""
        url = f"{self.config.base_url}/api/embeddings"
        
        payload = {
            "model": self.config.model,
            "prompt": text
        }
        
        response = self.client.post(url, json=payload)
        response.raise_for_status()
        data = response.json()
        
        return data["embedding"]


def get_provider(provider_type: str = None) -> LLMProvider:
    """
    Factory function to get the appropriate LLM provider.
    
    Args:
        provider_type: One of "lm_studio", "openai", "ollama", or None for auto-detect
    
    Returns:
        Configured LLM provider instance
    """
    provider_type = provider_type or os.getenv("LLM_PROVIDER", "lm_studio")
    
    providers = {
        "lm_studio": LMStudioProvider.from_env,
        "openai": OpenAIProvider.from_env,
        "ollama": OllamaProvider.from_env,
    }
    
    if provider_type not in providers:
        raise ValueError(f"Unknown provider type: {provider_type}. Supported: {list(providers.keys())}")
    
    return providers[provider_type]()


class LLMProviderPool:
    """Pool of LLM providers for load balancing and failover."""
    
    def __init__(self, providers: List[LLMProvider]):
        self.providers = providers
        self.current_index = 0
    
    def get_provider(self) -> LLMProvider:
        """Get next provider in round-robin fashion."""
        provider = self.providers[self.current_index]
        self.current_index = (self.current_index + 1) % len(self.providers)
        return provider
    
    def chat_with_fallback(self, messages: List[ChatMessage], **kwargs) -> LLMResponse:
        """Try each provider until one succeeds."""
        last_error = None
        for provider in self.providers:
            try:
                return provider.chat(messages, **kwargs)
            except Exception as e:
                last_error = e
                continue
        raise last_error
    
    def close_all(self):
        """Close all providers."""
        for provider in self.providers:
            provider.close()
