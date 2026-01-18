"""
Local LLM Integration Layer

Supports multiple local LLM backends for processing terabytes of data:
- Ollama (easiest setup, good for most use cases)
- llama.cpp (most efficient, best for large-scale processing)
- vLLM (best throughput for batch processing)
- Text Generation Inference (HuggingFace TGI)
- Custom OpenAI-compatible endpoints

This enables:
1. Processing terabytes of PDFs, documents, research papers locally
2. No API costs for large-scale knowledge extraction
3. Privacy - all data stays on your infrastructure
4. Integration with Manus for automated improvement cycles

Architecture:
┌─────────────────────────────────────────────────────────────────┐
│                    Local LLM Integration Layer                   │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐           │
│  │ Ollama  │  │llama.cpp│  │  vLLM   │  │   TGI   │           │
│  └────┬────┘  └────┬────┘  └────┬────┘  └────┬────┘           │
│       └────────────┴────────────┴────────────┘                 │
│                          │                                      │
│              ┌───────────┴───────────┐                         │
│              │   Unified LLM Client  │                         │
│              └───────────┬───────────┘                         │
│                          │                                      │
│  ┌───────────────────────┴───────────────────────┐             │
│  │           Batch Processing Engine              │             │
│  │  - Parallel document processing                │             │
│  │  - Chunking and context management            │             │
│  │  - Result aggregation and deduplication       │             │
│  └───────────────────────┬───────────────────────┘             │
│                          │                                      │
│  ┌───────────────────────┴───────────────────────┐             │
│  │         Knowledge Extraction Pipeline          │             │
│  │  - Mental model identification                │             │
│  │  - Failure mode extraction                    │             │
│  │  - Case study mining                          │             │
│  │  - Insight generation                         │             │
│  └───────────────────────────────────────────────┘             │
└─────────────────────────────────────────────────────────────────┘
"""

import os
import json
import asyncio
import aiohttp
import hashlib
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Any, AsyncGenerator, Callable
from enum import Enum
from datetime import datetime
import logging
from concurrent.futures import ThreadPoolExecutor, ProcessPoolExecutor
import queue
import threading

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# =============================================================================
# CONFIGURATION
# =============================================================================

class LLMBackend(Enum):
    OLLAMA = "ollama"
    LLAMA_CPP = "llama_cpp"
    VLLM = "vllm"
    TGI = "tgi"  # Text Generation Inference
    OPENAI_COMPATIBLE = "openai_compatible"


@dataclass
class LLMConfig:
    """Configuration for local LLM."""
    backend: LLMBackend
    base_url: str
    model: str
    api_key: Optional[str] = None
    
    # Generation parameters
    max_tokens: int = 4096
    temperature: float = 0.1
    top_p: float = 0.9
    
    # Performance parameters
    batch_size: int = 10
    max_concurrent: int = 5
    timeout: int = 300
    retry_attempts: int = 3
    
    # Context management
    context_window: int = 8192
    chunk_overlap: int = 200


# Default configurations for common setups
DEFAULT_CONFIGS = {
    "ollama_llama3": LLMConfig(
        backend=LLMBackend.OLLAMA,
        base_url="http://localhost:11434",
        model="llama3:70b",
        context_window=8192
    ),
    "ollama_mixtral": LLMConfig(
        backend=LLMBackend.OLLAMA,
        base_url="http://localhost:11434",
        model="mixtral:8x7b",
        context_window=32768
    ),
    "ollama_qwen": LLMConfig(
        backend=LLMBackend.OLLAMA,
        base_url="http://localhost:11434",
        model="qwen2.5:72b",
        context_window=32768
    ),
    "llama_cpp_local": LLMConfig(
        backend=LLMBackend.LLAMA_CPP,
        base_url="http://localhost:8080",
        model="local",
        context_window=8192
    ),
    "vllm_local": LLMConfig(
        backend=LLMBackend.VLLM,
        base_url="http://localhost:8000",
        model="meta-llama/Llama-3-70b-chat-hf",
        context_window=8192,
        max_concurrent=20  # vLLM handles high concurrency well
    ),
    "tgi_local": LLMConfig(
        backend=LLMBackend.TGI,
        base_url="http://localhost:8080",
        model="local",
        context_window=8192
    )
}


# =============================================================================
# LLM CLIENT INTERFACE
# =============================================================================

class BaseLLMClient(ABC):
    """Abstract base class for LLM clients."""
    
    def __init__(self, config: LLMConfig):
        self.config = config
    
    @abstractmethod
    async def generate(self, prompt: str, **kwargs) -> str:
        """Generate a response from the LLM."""
        pass
    
    @abstractmethod
    async def generate_stream(self, prompt: str, **kwargs) -> AsyncGenerator[str, None]:
        """Generate a streaming response from the LLM."""
        pass
    
    @abstractmethod
    async def health_check(self) -> bool:
        """Check if the LLM service is healthy."""
        pass


class OllamaClient(BaseLLMClient):
    """Client for Ollama local LLM server."""
    
    async def generate(self, prompt: str, **kwargs) -> str:
        """Generate response using Ollama API."""
        url = f"{self.config.base_url}/api/generate"
        
        payload = {
            "model": self.config.model,
            "prompt": prompt,
            "stream": False,
            "options": {
                "num_predict": kwargs.get("max_tokens", self.config.max_tokens),
                "temperature": kwargs.get("temperature", self.config.temperature),
                "top_p": kwargs.get("top_p", self.config.top_p),
            }
        }
        
        async with aiohttp.ClientSession() as session:
            async with session.post(url, json=payload, timeout=self.config.timeout) as resp:
                if resp.status == 200:
                    result = await resp.json()
                    return result.get("response", "")
                else:
                    raise Exception(f"Ollama error: {resp.status}")
    
    async def generate_stream(self, prompt: str, **kwargs) -> AsyncGenerator[str, None]:
        """Generate streaming response using Ollama API."""
        url = f"{self.config.base_url}/api/generate"
        
        payload = {
            "model": self.config.model,
            "prompt": prompt,
            "stream": True,
            "options": {
                "num_predict": kwargs.get("max_tokens", self.config.max_tokens),
                "temperature": kwargs.get("temperature", self.config.temperature),
            }
        }
        
        async with aiohttp.ClientSession() as session:
            async with session.post(url, json=payload, timeout=self.config.timeout) as resp:
                async for line in resp.content:
                    if line:
                        data = json.loads(line)
                        if "response" in data:
                            yield data["response"]
    
    async def health_check(self) -> bool:
        """Check Ollama server health."""
        try:
            async with aiohttp.ClientSession() as session:
                async with session.get(f"{self.config.base_url}/api/tags", timeout=5) as resp:
                    return resp.status == 200
        except:
            return False


class LlamaCppClient(BaseLLMClient):
    """Client for llama.cpp server."""
    
    async def generate(self, prompt: str, **kwargs) -> str:
        """Generate response using llama.cpp server API."""
        url = f"{self.config.base_url}/completion"
        
        payload = {
            "prompt": prompt,
            "n_predict": kwargs.get("max_tokens", self.config.max_tokens),
            "temperature": kwargs.get("temperature", self.config.temperature),
            "top_p": kwargs.get("top_p", self.config.top_p),
            "stream": False
        }
        
        async with aiohttp.ClientSession() as session:
            async with session.post(url, json=payload, timeout=self.config.timeout) as resp:
                if resp.status == 200:
                    result = await resp.json()
                    return result.get("content", "")
                else:
                    raise Exception(f"llama.cpp error: {resp.status}")
    
    async def generate_stream(self, prompt: str, **kwargs) -> AsyncGenerator[str, None]:
        """Generate streaming response using llama.cpp server."""
        url = f"{self.config.base_url}/completion"
        
        payload = {
            "prompt": prompt,
            "n_predict": kwargs.get("max_tokens", self.config.max_tokens),
            "temperature": kwargs.get("temperature", self.config.temperature),
            "stream": True
        }
        
        async with aiohttp.ClientSession() as session:
            async with session.post(url, json=payload, timeout=self.config.timeout) as resp:
                async for line in resp.content:
                    if line.startswith(b"data: "):
                        data = json.loads(line[6:])
                        if "content" in data:
                            yield data["content"]
    
    async def health_check(self) -> bool:
        """Check llama.cpp server health."""
        try:
            async with aiohttp.ClientSession() as session:
                async with session.get(f"{self.config.base_url}/health", timeout=5) as resp:
                    return resp.status == 200
        except:
            return False


class VLLMClient(BaseLLMClient):
    """Client for vLLM server (OpenAI-compatible API)."""
    
    async def generate(self, prompt: str, **kwargs) -> str:
        """Generate response using vLLM OpenAI-compatible API."""
        url = f"{self.config.base_url}/v1/completions"
        
        headers = {}
        if self.config.api_key:
            headers["Authorization"] = f"Bearer {self.config.api_key}"
        
        payload = {
            "model": self.config.model,
            "prompt": prompt,
            "max_tokens": kwargs.get("max_tokens", self.config.max_tokens),
            "temperature": kwargs.get("temperature", self.config.temperature),
            "top_p": kwargs.get("top_p", self.config.top_p),
        }
        
        async with aiohttp.ClientSession() as session:
            async with session.post(url, json=payload, headers=headers, timeout=self.config.timeout) as resp:
                if resp.status == 200:
                    result = await resp.json()
                    return result["choices"][0]["text"]
                else:
                    raise Exception(f"vLLM error: {resp.status}")
    
    async def generate_stream(self, prompt: str, **kwargs) -> AsyncGenerator[str, None]:
        """Generate streaming response using vLLM."""
        url = f"{self.config.base_url}/v1/completions"
        
        headers = {}
        if self.config.api_key:
            headers["Authorization"] = f"Bearer {self.config.api_key}"
        
        payload = {
            "model": self.config.model,
            "prompt": prompt,
            "max_tokens": kwargs.get("max_tokens", self.config.max_tokens),
            "temperature": kwargs.get("temperature", self.config.temperature),
            "stream": True
        }
        
        async with aiohttp.ClientSession() as session:
            async with session.post(url, json=payload, headers=headers, timeout=self.config.timeout) as resp:
                async for line in resp.content:
                    if line.startswith(b"data: "):
                        data_str = line[6:].decode().strip()
                        if data_str and data_str != "[DONE]":
                            data = json.loads(data_str)
                            if "choices" in data and data["choices"]:
                                yield data["choices"][0].get("text", "")
    
    async def health_check(self) -> bool:
        """Check vLLM server health."""
        try:
            async with aiohttp.ClientSession() as session:
                async with session.get(f"{self.config.base_url}/health", timeout=5) as resp:
                    return resp.status == 200
        except:
            return False


class OpenAICompatibleClient(BaseLLMClient):
    """Client for any OpenAI-compatible API (including TGI)."""
    
    async def generate(self, prompt: str, **kwargs) -> str:
        """Generate response using OpenAI-compatible API."""
        url = f"{self.config.base_url}/v1/chat/completions"
        
        headers = {"Content-Type": "application/json"}
        if self.config.api_key:
            headers["Authorization"] = f"Bearer {self.config.api_key}"
        
        payload = {
            "model": self.config.model,
            "messages": [{"role": "user", "content": prompt}],
            "max_tokens": kwargs.get("max_tokens", self.config.max_tokens),
            "temperature": kwargs.get("temperature", self.config.temperature),
            "top_p": kwargs.get("top_p", self.config.top_p),
        }
        
        async with aiohttp.ClientSession() as session:
            async with session.post(url, json=payload, headers=headers, timeout=self.config.timeout) as resp:
                if resp.status == 200:
                    result = await resp.json()
                    return result["choices"][0]["message"]["content"]
                else:
                    error = await resp.text()
                    raise Exception(f"API error {resp.status}: {error}")
    
    async def generate_stream(self, prompt: str, **kwargs) -> AsyncGenerator[str, None]:
        """Generate streaming response."""
        url = f"{self.config.base_url}/v1/chat/completions"
        
        headers = {"Content-Type": "application/json"}
        if self.config.api_key:
            headers["Authorization"] = f"Bearer {self.config.api_key}"
        
        payload = {
            "model": self.config.model,
            "messages": [{"role": "user", "content": prompt}],
            "max_tokens": kwargs.get("max_tokens", self.config.max_tokens),
            "temperature": kwargs.get("temperature", self.config.temperature),
            "stream": True
        }
        
        async with aiohttp.ClientSession() as session:
            async with session.post(url, json=payload, headers=headers, timeout=self.config.timeout) as resp:
                async for line in resp.content:
                    line = line.decode().strip()
                    if line.startswith("data: ") and line != "data: [DONE]":
                        data = json.loads(line[6:])
                        if "choices" in data and data["choices"]:
                            delta = data["choices"][0].get("delta", {})
                            if "content" in delta:
                                yield delta["content"]
    
    async def health_check(self) -> bool:
        """Check API health."""
        try:
            async with aiohttp.ClientSession() as session:
                async with session.get(f"{self.config.base_url}/v1/models", timeout=5) as resp:
                    return resp.status == 200
        except:
            return False


# =============================================================================
# UNIFIED LLM CLIENT
# =============================================================================

class UnifiedLLMClient:
    """
    Unified client that works with any supported LLM backend.
    Provides automatic failover, load balancing, and batch processing.
    """
    
    def __init__(self, configs: List[LLMConfig] = None):
        """Initialize with one or more LLM configurations."""
        self.configs = configs or [DEFAULT_CONFIGS["ollama_llama3"]]
        self.clients: List[BaseLLMClient] = []
        self._initialize_clients()
        
        # Batch processing state
        self.semaphore = None
        self.stats = {
            "total_requests": 0,
            "successful_requests": 0,
            "failed_requests": 0,
            "total_tokens_generated": 0
        }
    
    def _initialize_clients(self):
        """Initialize clients for each configuration."""
        client_classes = {
            LLMBackend.OLLAMA: OllamaClient,
            LLMBackend.LLAMA_CPP: LlamaCppClient,
            LLMBackend.VLLM: VLLMClient,
            LLMBackend.TGI: OpenAICompatibleClient,
            LLMBackend.OPENAI_COMPATIBLE: OpenAICompatibleClient,
        }
        
        for config in self.configs:
            client_class = client_classes.get(config.backend)
            if client_class:
                self.clients.append(client_class(config))
    
    async def generate(self, prompt: str, **kwargs) -> str:
        """Generate response, with automatic failover."""
        last_error = None
        
        for client in self.clients:
            try:
                self.stats["total_requests"] += 1
                result = await client.generate(prompt, **kwargs)
                self.stats["successful_requests"] += 1
                return result
            except Exception as e:
                last_error = e
                logger.warning(f"Client {client.config.backend} failed: {e}")
                continue
        
        self.stats["failed_requests"] += 1
        raise Exception(f"All LLM backends failed. Last error: {last_error}")
    
    async def generate_batch(self, 
                            prompts: List[str], 
                            callback: Callable[[int, str], None] = None,
                            **kwargs) -> List[str]:
        """
        Process multiple prompts in parallel with rate limiting.
        
        Args:
            prompts: List of prompts to process
            callback: Optional callback(index, result) for progress updates
            **kwargs: Additional generation parameters
        
        Returns:
            List of responses in same order as prompts
        """
        max_concurrent = self.configs[0].max_concurrent if self.configs else 5
        self.semaphore = asyncio.Semaphore(max_concurrent)
        
        async def process_one(idx: int, prompt: str) -> tuple:
            async with self.semaphore:
                try:
                    result = await self.generate(prompt, **kwargs)
                    if callback:
                        callback(idx, result)
                    return (idx, result)
                except Exception as e:
                    logger.error(f"Failed to process prompt {idx}: {e}")
                    return (idx, f"ERROR: {e}")
        
        tasks = [process_one(i, p) for i, p in enumerate(prompts)]
        results = await asyncio.gather(*tasks)
        
        # Sort by index and return just the results
        results.sort(key=lambda x: x[0])
        return [r[1] for r in results]
    
    async def health_check_all(self) -> Dict[str, bool]:
        """Check health of all configured backends."""
        results = {}
        for client in self.clients:
            name = f"{client.config.backend.value}@{client.config.base_url}"
            results[name] = await client.health_check()
        return results
    
    def get_stats(self) -> Dict:
        """Get processing statistics."""
        return self.stats.copy()


# =============================================================================
# KNOWLEDGE EXTRACTION PROMPTS
# =============================================================================

EXTRACTION_PROMPTS = {
    "mental_model": """Analyze the following text and extract any mental models, thinking frameworks, or decision-making principles.

For each mental model found, provide:
1. Name of the model
2. Category (psychology, economics, physics, biology, math, systems, etc.)
3. Description (2-3 sentences)
4. How to apply it
5. Common failure modes when using this model
6. Related models

Text to analyze:
{text}

Respond in JSON format with an array of mental models.""",

    "failure_mode": """Analyze the following text and extract any failure modes, mistakes, biases, or ways things can go wrong.

For each failure mode found, provide:
1. Name of the failure mode
2. Which mental model it relates to
3. Description of how the failure occurs
4. Real-world example from the text (if present)
5. Warning signs
6. Prevention strategies

Text to analyze:
{text}

Respond in JSON format with an array of failure modes.""",

    "case_study": """Analyze the following text and extract any case studies, examples, or real-world applications of mental models or decision-making.

For each case study found, provide:
1. Title/Name
2. Year (if mentioned)
3. Entity involved (company, person, etc.)
4. Summary of what happened
5. Mental models demonstrated
6. Lessons learned
7. Quantitative outcomes (if mentioned)

Text to analyze:
{text}

Respond in JSON format with an array of case studies.""",

    "insight": """Analyze the following text and extract key insights, wisdom, or actionable advice.

For each insight found, provide:
1. The insight (one sentence)
2. Source/Attribution (if mentioned)
3. Category (investing, decision-making, psychology, business, etc.)
4. How to apply this insight
5. Potential pitfalls

Text to analyze:
{text}

Respond in JSON format with an array of insights.""",

    "connection": """Analyze the following text and identify connections between different concepts, mental models, or ideas.

For each connection found, provide:
1. First concept
2. Second concept
3. Nature of the connection (reinforcing, opposing, prerequisite, etc.)
4. Explanation of the connection
5. Implications of this connection

Text to analyze:
{text}

Respond in JSON format with an array of connections."""
}


# =============================================================================
# BATCH DOCUMENT PROCESSOR
# =============================================================================

@dataclass
class ProcessingResult:
    """Result from processing a document chunk."""
    chunk_id: str
    document_path: str
    extraction_type: str
    extracted_items: List[Dict]
    processing_time: float
    token_count: int
    error: Optional[str] = None


class BatchDocumentProcessor:
    """
    Process large volumes of documents using local LLMs.
    Designed for terabyte-scale processing.
    """
    
    def __init__(self, llm_client: UnifiedLLMClient):
        self.llm = llm_client
        self.results: List[ProcessingResult] = []
        self.progress_callback: Optional[Callable] = None
    
    async def process_chunk(self, 
                           chunk_id: str,
                           chunk_text: str,
                           document_path: str,
                           extraction_type: str) -> ProcessingResult:
        """Process a single chunk with the specified extraction type."""
        start_time = datetime.now()
        
        prompt_template = EXTRACTION_PROMPTS.get(extraction_type)
        if not prompt_template:
            return ProcessingResult(
                chunk_id=chunk_id,
                document_path=document_path,
                extraction_type=extraction_type,
                extracted_items=[],
                processing_time=0,
                token_count=0,
                error=f"Unknown extraction type: {extraction_type}"
            )
        
        prompt = prompt_template.format(text=chunk_text[:8000])  # Limit text length
        
        try:
            response = await self.llm.generate(prompt, temperature=0.1)
            
            # Parse JSON response
            try:
                # Find JSON in response
                json_start = response.find('[')
                json_end = response.rfind(']') + 1
                if json_start >= 0 and json_end > json_start:
                    items = json.loads(response[json_start:json_end])
                else:
                    items = []
            except json.JSONDecodeError:
                items = []
            
            processing_time = (datetime.now() - start_time).total_seconds()
            
            return ProcessingResult(
                chunk_id=chunk_id,
                document_path=document_path,
                extraction_type=extraction_type,
                extracted_items=items,
                processing_time=processing_time,
                token_count=len(response.split())
            )
        
        except Exception as e:
            return ProcessingResult(
                chunk_id=chunk_id,
                document_path=document_path,
                extraction_type=extraction_type,
                extracted_items=[],
                processing_time=(datetime.now() - start_time).total_seconds(),
                token_count=0,
                error=str(e)
            )
    
    async def process_document(self,
                              document_path: str,
                              chunks: List[str],
                              extraction_types: List[str] = None) -> List[ProcessingResult]:
        """Process all chunks of a document."""
        if extraction_types is None:
            extraction_types = ["mental_model", "failure_mode", "case_study", "insight"]
        
        all_results = []
        total_tasks = len(chunks) * len(extraction_types)
        completed = 0
        
        for extraction_type in extraction_types:
            # Create tasks for all chunks
            tasks = []
            for i, chunk in enumerate(chunks):
                chunk_id = f"{hashlib.md5(document_path.encode()).hexdigest()[:8]}_{i}"
                tasks.append(self.process_chunk(chunk_id, chunk, document_path, extraction_type))
            
            # Process in parallel
            results = await asyncio.gather(*tasks)
            all_results.extend(results)
            
            completed += len(chunks)
            if self.progress_callback:
                self.progress_callback(completed, total_tasks)
        
        self.results.extend(all_results)
        return all_results
    
    def aggregate_results(self) -> Dict:
        """Aggregate all processing results."""
        aggregated = {
            "mental_models": [],
            "failure_modes": [],
            "case_studies": [],
            "insights": [],
            "connections": [],
            "statistics": {
                "total_chunks_processed": len(self.results),
                "successful_extractions": 0,
                "failed_extractions": 0,
                "total_processing_time": 0,
                "items_by_type": {}
            }
        }
        
        for result in self.results:
            if result.error:
                aggregated["statistics"]["failed_extractions"] += 1
            else:
                aggregated["statistics"]["successful_extractions"] += 1
            
            aggregated["statistics"]["total_processing_time"] += result.processing_time
            
            # Add items to appropriate category
            category_map = {
                "mental_model": "mental_models",
                "failure_mode": "failure_modes",
                "case_study": "case_studies",
                "insight": "insights",
                "connection": "connections"
            }
            
            category = category_map.get(result.extraction_type)
            if category:
                for item in result.extracted_items:
                    item["_source_chunk"] = result.chunk_id
                    item["_source_document"] = result.document_path
                    aggregated[category].append(item)
                
                aggregated["statistics"]["items_by_type"][result.extraction_type] = \
                    aggregated["statistics"]["items_by_type"].get(result.extraction_type, 0) + len(result.extracted_items)
        
        return aggregated
    
    def export_results(self, output_path: str):
        """Export aggregated results to JSON."""
        aggregated = self.aggregate_results()
        with open(output_path, 'w') as f:
            json.dump(aggregated, f, indent=2, default=str)
        return output_path


# =============================================================================
# MANUS INTEGRATION
# =============================================================================

class ManusIntegration:
    """
    Integration layer for connecting local LLM processing with Manus.
    
    This enables the automated improvement cycle:
    1. Local LLMs process terabytes of documents
    2. Extract mental models, failure modes, case studies
    3. Generate improvement suggestions
    4. Send to Manus for review and implementation
    5. Manus updates the system
    6. Repeat
    """
    
    def __init__(self, 
                 llm_client: UnifiedLLMClient,
                 output_dir: str = "./manus_integration"):
        self.llm = llm_client
        self.output_dir = output_dir
        os.makedirs(output_dir, exist_ok=True)
        
        self.pending_improvements: List[Dict] = []
        self.implemented_improvements: List[Dict] = []
    
    async def generate_improvement_suggestions(self, 
                                              extracted_data: Dict,
                                              current_system_state: Dict = None) -> List[Dict]:
        """Generate improvement suggestions based on extracted knowledge."""
        
        prompt = f"""Based on the following extracted knowledge, generate specific improvement suggestions for a Mental Models System.

Current system has:
- {current_system_state.get('num_models', 129) if current_system_state else 129} mental models
- {current_system_state.get('num_failure_modes', 645) if current_system_state else 645} failure modes
- {current_system_state.get('num_case_studies', 50) if current_system_state else 50} case studies

New knowledge extracted:
- {len(extracted_data.get('mental_models', []))} potential new mental models
- {len(extracted_data.get('failure_modes', []))} potential new failure modes
- {len(extracted_data.get('case_studies', []))} potential new case studies
- {len(extracted_data.get('insights', []))} insights

Sample of new mental models:
{json.dumps(extracted_data.get('mental_models', [])[:3], indent=2)}

Sample of new insights:
{json.dumps(extracted_data.get('insights', [])[:3], indent=2)}

Generate improvement suggestions in JSON format:
[
  {{
    "type": "add_mental_model" | "add_failure_mode" | "add_case_study" | "update_existing" | "new_connection",
    "priority": 1-5 (1 highest),
    "title": "Brief title",
    "description": "What to add/change",
    "implementation": "Specific code/data changes needed",
    "expected_impact": "How this improves the system"
  }}
]

Focus on high-impact, actionable improvements."""

        try:
            response = await self.llm.generate(prompt, max_tokens=4096, temperature=0.2)
            
            # Parse suggestions
            json_start = response.find('[')
            json_end = response.rfind(']') + 1
            if json_start >= 0 and json_end > json_start:
                suggestions = json.loads(response[json_start:json_end])
                
                # Add metadata
                for s in suggestions:
                    s["generated_at"] = datetime.now().isoformat()
                    s["status"] = "pending"
                
                self.pending_improvements.extend(suggestions)
                return suggestions
        except Exception as e:
            logger.error(f"Error generating suggestions: {e}")
        
        return []
    
    def export_for_manus(self, filename: str = "improvements_for_manus.json") -> str:
        """Export pending improvements in a format Manus can process."""
        output = {
            "generated_at": datetime.now().isoformat(),
            "total_pending": len(self.pending_improvements),
            "improvements": sorted(self.pending_improvements, key=lambda x: x.get("priority", 5)),
            "instructions": """
These improvements were generated by local LLM analysis of your document collection.

To implement:
1. Review each improvement suggestion
2. For approved items, implement the changes described
3. Mark items as 'implemented' or 'rejected'
4. Push changes to the repository

Priority guide:
1 = Critical - implement immediately
2 = High - implement this session
3 = Medium - implement when convenient
4 = Low - nice to have
5 = Future - consider for later
"""
        }
        
        output_path = os.path.join(self.output_dir, filename)
        with open(output_path, 'w') as f:
            json.dump(output, f, indent=2)
        
        return output_path
    
    def mark_implemented(self, improvement_ids: List[str]):
        """Mark improvements as implemented."""
        for imp in self.pending_improvements:
            if imp.get("id") in improvement_ids:
                imp["status"] = "implemented"
                imp["implemented_at"] = datetime.now().isoformat()
                self.implemented_improvements.append(imp)
        
        self.pending_improvements = [i for i in self.pending_improvements if i.get("status") != "implemented"]


# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

def create_llm_client(backend: str = "ollama", 
                     base_url: str = None,
                     model: str = None) -> UnifiedLLMClient:
    """Create an LLM client with sensible defaults."""
    
    backend_enum = LLMBackend(backend)
    
    defaults = {
        LLMBackend.OLLAMA: ("http://localhost:11434", "llama3:70b"),
        LLMBackend.LLAMA_CPP: ("http://localhost:8080", "local"),
        LLMBackend.VLLM: ("http://localhost:8000", "meta-llama/Llama-3-70b-chat-hf"),
        LLMBackend.TGI: ("http://localhost:8080", "local"),
    }
    
    default_url, default_model = defaults.get(backend_enum, ("http://localhost:8080", "local"))
    
    config = LLMConfig(
        backend=backend_enum,
        base_url=base_url or default_url,
        model=model or default_model
    )
    
    return UnifiedLLMClient([config])


async def quick_extract(text: str, 
                       extraction_type: str = "insight",
                       backend: str = "ollama") -> List[Dict]:
    """Quick extraction from a single text."""
    client = create_llm_client(backend)
    processor = BatchDocumentProcessor(client)
    
    result = await processor.process_chunk(
        chunk_id="quick",
        chunk_text=text,
        document_path="<direct_input>",
        extraction_type=extraction_type
    )
    
    return result.extracted_items


# =============================================================================
# CLI INTERFACE
# =============================================================================

if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(description="Local LLM Integration for Knowledge Mining")
    parser.add_argument("--backend", default="ollama", choices=["ollama", "llama_cpp", "vllm", "tgi"])
    parser.add_argument("--url", help="LLM server URL")
    parser.add_argument("--model", help="Model name")
    parser.add_argument("--health-check", action="store_true", help="Check LLM server health")
    
    args = parser.parse_args()
    
    async def main():
        client = create_llm_client(args.backend, args.url, args.model)
        
        if args.health_check:
            health = await client.health_check_all()
            print("Health Check Results:")
            for name, status in health.items():
                print(f"  {name}: {'✓ OK' if status else '✗ FAILED'}")
        else:
            print("Local LLM Integration Layer")
            print("=" * 50)
            print(f"Backend: {args.backend}")
            print(f"URL: {args.url or 'default'}")
            print(f"Model: {args.model or 'default'}")
            print("\nUse --health-check to verify connection")
    
    asyncio.run(main())
