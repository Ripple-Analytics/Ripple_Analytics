"""
Huggingface Connector

Integrates with Huggingface Hub for accessing models, embeddings, and inference.
Supports both the Inference API and local model downloads.
"""

import aiohttp
import logging
from typing import Dict, List, Optional, Any
from datetime import datetime

from .base import BaseConnector, ConnectorConfig, ConnectorType, ConnectorStatus

logger = logging.getLogger(__name__)


class HuggingfaceConnector(BaseConnector):
    """Connector for Huggingface Hub and Inference API."""
    
    NAME = "huggingface"
    TYPE = ConnectorType.LLM
    DESCRIPTION = "Access Huggingface models for embeddings, classification, and inference"
    REQUIRED_CREDENTIALS = []
    OPTIONAL_CREDENTIALS = ["api_token"]
    OPEN_SOURCE = True
    
    INFERENCE_API_URL = "https://api-inference.huggingface.co/models"
    HUB_API_URL = "https://huggingface.co/api"
    
    DEFAULT_EMBEDDING_MODEL = "sentence-transformers/all-MiniLM-L6-v2"
    DEFAULT_CLASSIFICATION_MODEL = "facebook/bart-large-mnli"
    DEFAULT_SUMMARIZATION_MODEL = "facebook/bart-large-cnn"
    
    def __init__(self, config: ConnectorConfig = None):
        super().__init__(config)
        self._session: Optional[aiohttp.ClientSession] = None
        self._api_token: Optional[str] = None
        self._models_cache: Dict[str, Any] = {}
    
    async def connect(self) -> bool:
        """Initialize the Huggingface connector."""
        try:
            headers = {}
            if self.config and "api_token" in self.config.credentials:
                self._api_token = self.config.credentials["api_token"]
                headers["Authorization"] = f"Bearer {self._api_token}"
            
            self._session = aiohttp.ClientSession(headers=headers)
            self.status = ConnectorStatus.CONNECTED
            logger.info("Huggingface connector initialized")
            return True
        except Exception as e:
            logger.error(f"Failed to initialize Huggingface connector: {e}")
            self.status = ConnectorStatus.ERROR
            return False
    
    async def disconnect(self) -> bool:
        """Close the Huggingface connector."""
        try:
            if self._session:
                await self._session.close()
                self._session = None
            self.status = ConnectorStatus.DISCONNECTED
            return True
        except Exception as e:
            logger.error(f"Failed to disconnect Huggingface connector: {e}")
            return False
    
    async def health_check(self) -> bool:
        """Check if the connector is healthy."""
        if self.status != ConnectorStatus.CONNECTED or not self._session:
            return False
        
        try:
            async with self._session.get(f"{self.HUB_API_URL}/models?limit=1") as response:
                return response.status == 200
        except Exception:
            return False
    
    async def get_embeddings(
        self,
        texts: List[str],
        model: str = None
    ) -> Dict[str, Any]:
        """
        Generate embeddings for texts using a sentence-transformers model.
        
        Args:
            texts: List of texts to embed
            model: Model to use (default: all-MiniLM-L6-v2)
            
        Returns:
            Dictionary with embeddings and metadata
        """
        if not self._session:
            await self.connect()
        
        model = model or self.DEFAULT_EMBEDDING_MODEL
        url = f"{self.INFERENCE_API_URL}/{model}"
        
        try:
            await self._rate_limit_check()
            
            async with self._session.post(url, json={"inputs": texts}) as response:
                if response.status == 200:
                    embeddings = await response.json()
                    logger.info(f"Generated embeddings for {len(texts)} texts using {model}")
                    return {
                        "success": True,
                        "embeddings": embeddings,
                        "model": model,
                        "count": len(texts)
                    }
                elif response.status == 503:
                    result = await response.json()
                    logger.warning(f"Model loading: {result.get('error', 'Model is loading')}")
                    return {
                        "success": False,
                        "error": "Model is loading, please retry",
                        "estimated_time": result.get("estimated_time", 20)
                    }
                else:
                    text = await response.text()
                    logger.error(f"Embedding failed: {response.status} - {text}")
                    return {"success": False, "error": text, "status": response.status}
        except Exception as e:
            logger.error(f"Failed to generate embeddings: {e}")
            return {"success": False, "error": str(e)}
    
    async def classify_text(
        self,
        text: str,
        labels: List[str],
        model: str = None,
        multi_label: bool = False
    ) -> Dict[str, Any]:
        """
        Classify text into given labels using zero-shot classification.
        
        Args:
            text: Text to classify
            labels: List of candidate labels
            model: Model to use (default: bart-large-mnli)
            multi_label: Whether to allow multiple labels
            
        Returns:
            Dictionary with classification results
        """
        if not self._session:
            await self.connect()
        
        model = model or self.DEFAULT_CLASSIFICATION_MODEL
        url = f"{self.INFERENCE_API_URL}/{model}"
        
        payload = {
            "inputs": text,
            "parameters": {
                "candidate_labels": labels,
                "multi_label": multi_label
            }
        }
        
        try:
            await self._rate_limit_check()
            
            async with self._session.post(url, json=payload) as response:
                if response.status == 200:
                    result = await response.json()
                    logger.info(f"Classified text using {model}")
                    return {
                        "success": True,
                        "labels": result.get("labels", []),
                        "scores": result.get("scores", []),
                        "model": model
                    }
                elif response.status == 503:
                    result = await response.json()
                    return {
                        "success": False,
                        "error": "Model is loading, please retry",
                        "estimated_time": result.get("estimated_time", 20)
                    }
                else:
                    text = await response.text()
                    logger.error(f"Classification failed: {response.status} - {text}")
                    return {"success": False, "error": text, "status": response.status}
        except Exception as e:
            logger.error(f"Failed to classify text: {e}")
            return {"success": False, "error": str(e)}
    
    async def summarize_text(
        self,
        text: str,
        model: str = None,
        max_length: int = 150,
        min_length: int = 30
    ) -> Dict[str, Any]:
        """
        Summarize text using a summarization model.
        
        Args:
            text: Text to summarize
            model: Model to use (default: bart-large-cnn)
            max_length: Maximum summary length
            min_length: Minimum summary length
            
        Returns:
            Dictionary with summary
        """
        if not self._session:
            await self.connect()
        
        model = model or self.DEFAULT_SUMMARIZATION_MODEL
        url = f"{self.INFERENCE_API_URL}/{model}"
        
        payload = {
            "inputs": text,
            "parameters": {
                "max_length": max_length,
                "min_length": min_length
            }
        }
        
        try:
            await self._rate_limit_check()
            
            async with self._session.post(url, json=payload) as response:
                if response.status == 200:
                    result = await response.json()
                    summary = result[0].get("summary_text", "") if isinstance(result, list) else result.get("summary_text", "")
                    logger.info(f"Summarized text using {model}")
                    return {
                        "success": True,
                        "summary": summary,
                        "model": model
                    }
                elif response.status == 503:
                    result = await response.json()
                    return {
                        "success": False,
                        "error": "Model is loading, please retry",
                        "estimated_time": result.get("estimated_time", 20)
                    }
                else:
                    text = await response.text()
                    logger.error(f"Summarization failed: {response.status} - {text}")
                    return {"success": False, "error": text, "status": response.status}
        except Exception as e:
            logger.error(f"Failed to summarize text: {e}")
            return {"success": False, "error": str(e)}
    
    async def text_generation(
        self,
        prompt: str,
        model: str,
        max_new_tokens: int = 250,
        temperature: float = 0.7,
        top_p: float = 0.9
    ) -> Dict[str, Any]:
        """
        Generate text using a language model.
        
        Args:
            prompt: Input prompt
            model: Model to use (e.g., 'mistralai/Mistral-7B-Instruct-v0.1')
            max_new_tokens: Maximum tokens to generate
            temperature: Sampling temperature
            top_p: Top-p sampling parameter
            
        Returns:
            Dictionary with generated text
        """
        if not self._session:
            await self.connect()
        
        url = f"{self.INFERENCE_API_URL}/{model}"
        
        payload = {
            "inputs": prompt,
            "parameters": {
                "max_new_tokens": max_new_tokens,
                "temperature": temperature,
                "top_p": top_p,
                "return_full_text": False
            }
        }
        
        try:
            await self._rate_limit_check()
            
            async with self._session.post(url, json=payload) as response:
                if response.status == 200:
                    result = await response.json()
                    generated = result[0].get("generated_text", "") if isinstance(result, list) else result.get("generated_text", "")
                    logger.info(f"Generated text using {model}")
                    return {
                        "success": True,
                        "generated_text": generated,
                        "model": model
                    }
                elif response.status == 503:
                    result = await response.json()
                    return {
                        "success": False,
                        "error": "Model is loading, please retry",
                        "estimated_time": result.get("estimated_time", 20)
                    }
                else:
                    text = await response.text()
                    logger.error(f"Text generation failed: {response.status} - {text}")
                    return {"success": False, "error": text, "status": response.status}
        except Exception as e:
            logger.error(f"Failed to generate text: {e}")
            return {"success": False, "error": str(e)}
    
    async def search_models(
        self,
        query: str = None,
        task: str = None,
        library: str = None,
        limit: int = 10
    ) -> Dict[str, Any]:
        """
        Search for models on Huggingface Hub.
        
        Args:
            query: Search query
            task: Filter by task (e.g., 'text-classification', 'sentence-similarity')
            library: Filter by library (e.g., 'transformers', 'sentence-transformers')
            limit: Maximum number of results
            
        Returns:
            Dictionary with model search results
        """
        if not self._session:
            await self.connect()
        
        params = {"limit": limit}
        if query:
            params["search"] = query
        if task:
            params["pipeline_tag"] = task
        if library:
            params["library"] = library
        
        try:
            await self._rate_limit_check()
            
            async with self._session.get(f"{self.HUB_API_URL}/models", params=params) as response:
                if response.status == 200:
                    models = await response.json()
                    logger.info(f"Found {len(models)} models")
                    return {
                        "success": True,
                        "models": [
                            {
                                "id": m.get("id", ""),
                                "downloads": m.get("downloads", 0),
                                "likes": m.get("likes", 0),
                                "task": m.get("pipeline_tag", ""),
                                "library": m.get("library_name", "")
                            }
                            for m in models
                        ],
                        "count": len(models)
                    }
                else:
                    text = await response.text()
                    logger.error(f"Model search failed: {response.status} - {text}")
                    return {"success": False, "error": text, "status": response.status}
        except Exception as e:
            logger.error(f"Failed to search models: {e}")
            return {"success": False, "error": str(e)}
    
    async def get_model_info(self, model_id: str) -> Dict[str, Any]:
        """
        Get detailed information about a model.
        
        Args:
            model_id: Model identifier (e.g., 'sentence-transformers/all-MiniLM-L6-v2')
            
        Returns:
            Dictionary with model information
        """
        if not self._session:
            await self.connect()
        
        try:
            await self._rate_limit_check()
            
            async with self._session.get(f"{self.HUB_API_URL}/models/{model_id}") as response:
                if response.status == 200:
                    info = await response.json()
                    logger.info(f"Retrieved info for model: {model_id}")
                    return {
                        "success": True,
                        "model_id": model_id,
                        "downloads": info.get("downloads", 0),
                        "likes": info.get("likes", 0),
                        "task": info.get("pipeline_tag", ""),
                        "library": info.get("library_name", ""),
                        "tags": info.get("tags", []),
                        "created_at": info.get("createdAt", ""),
                        "last_modified": info.get("lastModified", "")
                    }
                else:
                    text = await response.text()
                    logger.error(f"Model info failed: {response.status} - {text}")
                    return {"success": False, "error": text, "status": response.status}
        except Exception as e:
            logger.error(f"Failed to get model info: {e}")
            return {"success": False, "error": str(e)}
    
    async def classify_by_mental_models(
        self,
        text: str,
        model_categories: List[str] = None
    ) -> Dict[str, Any]:
        """
        Classify text by mental model categories using zero-shot classification.
        
        Args:
            text: Text to classify
            model_categories: List of mental model categories (default: standard 8 categories)
            
        Returns:
            Dictionary with classification results
        """
        if model_categories is None:
            model_categories = [
                "Psychology & Human Behavior",
                "Thinking Tools & Decision Making",
                "Economics & Markets",
                "Competitive Advantage & Strategy",
                "Mathematics & Probability",
                "Physics & Systems",
                "Biology & Evolution",
                "Organizational Behavior"
            ]
        
        return await self.classify_text(text, model_categories, multi_label=True)
    
    async def detect_cognitive_biases(self, text: str) -> Dict[str, Any]:
        """
        Detect potential cognitive biases in text using zero-shot classification.
        
        Args:
            text: Text to analyze
            
        Returns:
            Dictionary with detected biases
        """
        biases = [
            "Confirmation Bias",
            "Anchoring Bias",
            "Availability Heuristic",
            "Sunk Cost Fallacy",
            "Overconfidence",
            "Loss Aversion",
            "Hindsight Bias",
            "Survivorship Bias",
            "Authority Bias",
            "Bandwagon Effect"
        ]
        
        return await self.classify_text(text, biases, multi_label=True)


def create_huggingface_connector(api_token: str = None) -> HuggingfaceConnector:
    """
    Factory function to create a configured Huggingface connector.
    
    Args:
        api_token: Optional Huggingface API token for higher rate limits
        
    Returns:
        Configured HuggingfaceConnector instance
    """
    config = ConnectorConfig(
        name="huggingface",
        connector_type=ConnectorType.LLM,
        credentials={"api_token": api_token} if api_token else {},
        settings={}
    )
    
    return HuggingfaceConnector(config)
