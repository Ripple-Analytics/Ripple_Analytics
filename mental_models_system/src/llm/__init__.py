"""
LLM Integration Module
Provides abstraction layer for LLM providers including LM Studio, OpenAI, and Ollama.
"""

from .providers import LLMProvider, LMStudioProvider, OpenAIProvider, OllamaProvider, get_provider
from .agent import MentalModelsAgent
from .embeddings import EmbeddingService

# Local LLM Integration for terabyte-scale processing
from .local_llm import (
    LLMBackend,
    LLMConfig,
    DEFAULT_CONFIGS,
    UnifiedLLMClient,
    BatchDocumentProcessor,
    ProcessingResult,
    EXTRACTION_PROMPTS,
    ManusIntegration,
    create_llm_client,
    quick_extract,
)

__all__ = [
    "LLMProvider",
    "LMStudioProvider", 
    "OpenAIProvider",
    "OllamaProvider",
    "get_provider",
    "MentalModelsAgent",
    "EmbeddingService",
    # Local LLM
    "LLMBackend",
    "LLMConfig",
    "DEFAULT_CONFIGS",
    "UnifiedLLMClient",
    "BatchDocumentProcessor",
    "ProcessingResult",
    "EXTRACTION_PROMPTS",
    "ManusIntegration",
    "create_llm_client",
    "quick_extract",
]
