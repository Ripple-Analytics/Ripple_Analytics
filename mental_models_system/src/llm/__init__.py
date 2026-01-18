"""
LLM Integration Module
Provides abstraction layer for LLM providers including LM Studio, OpenAI, and Ollama.
"""

from .providers import LLMProvider, LMStudioProvider, OpenAIProvider, OllamaProvider, get_provider
from .agent import MentalModelsAgent
from .embeddings import EmbeddingService

__all__ = [
    "LLMProvider",
    "LMStudioProvider", 
    "OpenAIProvider",
    "OllamaProvider",
    "get_provider",
    "MentalModelsAgent",
    "EmbeddingService",
]
