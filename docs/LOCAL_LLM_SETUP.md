# Local LLM Setup Guide

Run the Mental Models System entirely on your own hardware with no API costs.

## Supported Backends

| Backend | Best For | Setup Difficulty | Performance |
|---------|----------|------------------|-------------|
| **Ollama** | Easy setup, good defaults | Easy | Good |
| **llama.cpp** | Maximum efficiency | Medium | Best |
| **vLLM** | High throughput batch processing | Medium | Excellent |
| **TGI** | HuggingFace models | Medium | Good |

## Hardware Requirements

### Minimum (7B models)
- CPU: 4 cores
- RAM: 8GB
- Storage: 10GB

### Recommended (70B models)
- CPU: 8+ cores
- RAM: 48GB+ (or GPU with 48GB+ VRAM)
- Storage: 100GB
- GPU: NVIDIA RTX 3090/4090 or A100

### Model Size Guide

| Model | RAM Required | VRAM Required | Quality |
|-------|--------------|---------------|---------|
| Llama 3 8B | 8GB | 8GB | Good |
| Mistral 7B | 8GB | 8GB | Good |
| Llama 3 70B | 48GB | 48GB | Excellent |
| Mixtral 8x7B | 32GB | 32GB | Very Good |

## Option 1: Ollama (Recommended for Beginners)

### Installation

```bash
# Linux/macOS
curl -fsSL https://ollama.com/install.sh | sh

# Windows
# Download from https://ollama.com/download
```

### Download Models

```bash
# Recommended models for mental model analysis
ollama pull llama3:8b          # Fast, good quality
ollama pull llama3:70b         # Best quality, requires 48GB+ RAM
ollama pull mistral:7b         # Fast, good for general use
ollama pull mixtral:8x7b       # Good balance of speed and quality
```

### Start Server

```bash
ollama serve
```

### Use with Mental Models System

```python
from src.llm import create_llm_client

client = create_llm_client(
    backend="ollama",
    model="llama3:8b",
    base_url="http://localhost:11434"
)
```

Or via CLI:

```bash
python cli.py analyze "Your text" --backend ollama --model llama3:8b
```

## Option 2: llama.cpp (Best Performance)

### Installation

```bash
# Clone and build
git clone https://github.com/ggerganov/llama.cpp
cd llama.cpp
make -j

# For GPU support (CUDA)
make LLAMA_CUDA=1 -j

# For Apple Silicon
make LLAMA_METAL=1 -j
```

### Download Models (GGUF format)

```bash
# Download from HuggingFace
# Example: TheBloke's quantized models
wget https://huggingface.co/TheBloke/Llama-2-70B-GGUF/resolve/main/llama-2-70b.Q4_K_M.gguf
```

### Start Server

```bash
./server -m llama-2-70b.Q4_K_M.gguf -c 4096 --host 0.0.0.0 --port 8080
```

### Use with Mental Models System

```python
from src.llm import create_llm_client

client = create_llm_client(
    backend="llamacpp",
    model="llama-2-70b",
    base_url="http://localhost:8080"
)
```

## Option 3: vLLM (Best for Batch Processing)

### Installation

```bash
pip install vllm
```

### Start Server

```bash
python -m vllm.entrypoints.openai.api_server \
    --model meta-llama/Llama-3-8b-hf \
    --host 0.0.0.0 \
    --port 8000
```

### Use with Mental Models System

```python
from src.llm import create_llm_client

client = create_llm_client(
    backend="vllm",
    model="meta-llama/Llama-3-8b-hf",
    base_url="http://localhost:8000"
)
```

## Option 4: Text Generation Inference (TGI)

### Installation (Docker)

```bash
docker run --gpus all -p 8080:80 \
    -v /path/to/models:/data \
    ghcr.io/huggingface/text-generation-inference:latest \
    --model-id meta-llama/Llama-3-8b-hf
```

### Use with Mental Models System

```python
from src.llm import create_llm_client

client = create_llm_client(
    backend="tgi",
    model="meta-llama/Llama-3-8b-hf",
    base_url="http://localhost:8080"
)
```

## Docker Setup with GPU

### docker-compose.yml with Ollama

```yaml
services:
  ollama:
    image: ollama/ollama:latest
    ports:
      - "11434:11434"
    volumes:
      - ollama_data:/root/.ollama
    deploy:
      resources:
        reservations:
          devices:
            - driver: nvidia
              count: all
              capabilities: [gpu]

volumes:
  ollama_data:
```

### Start with GPU

```bash
docker-compose up -d ollama

# Pull model inside container
docker exec -it ollama ollama pull llama3:70b
```

## Performance Optimization

### 1. Quantization

Use quantized models for better performance:

| Quantization | Quality Loss | Speed Gain | VRAM Reduction |
|--------------|--------------|------------|----------------|
| Q8_0 | Minimal | 1.5x | 50% |
| Q6_K | Small | 2x | 60% |
| Q4_K_M | Moderate | 3x | 75% |
| Q4_0 | Noticeable | 4x | 80% |

### 2. Context Length

Reduce context length for faster inference:

```bash
# Ollama
OLLAMA_NUM_CTX=2048 ollama serve

# llama.cpp
./server -m model.gguf -c 2048
```

### 3. Batch Processing

Use batch processing for multiple documents:

```python
from src.llm import BatchDocumentProcessor

processor = BatchDocumentProcessor(client, batch_size=10)
results = await processor.process_batch(documents)
```

### 4. Caching

Enable Redis caching to avoid redundant LLM calls:

```python
from src.cache import RedisCache

cache = RedisCache("redis://localhost:6379")
# Responses are automatically cached
```

## Troubleshooting

### Out of Memory

```bash
# Use smaller model
ollama pull llama3:8b

# Use more aggressive quantization
# Download Q4_0 instead of Q8_0

# Reduce context length
OLLAMA_NUM_CTX=1024 ollama serve
```

### Slow Inference

```bash
# Enable GPU
make LLAMA_CUDA=1 -j

# Use quantized model
# Use smaller context length
```

### Model Not Loading

```bash
# Check available disk space
df -h

# Check model integrity
ollama show llama3:8b

# Re-download model
ollama rm llama3:8b
ollama pull llama3:8b
```

## Recommended Setup for Production

```yaml
# docker-compose.prod.yml
services:
  ollama:
    image: ollama/ollama:latest
    restart: always
    ports:
      - "11434:11434"
    volumes:
      - ollama_data:/root/.ollama
    deploy:
      resources:
        reservations:
          devices:
            - driver: nvidia
              count: 1
              capabilities: [gpu]
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:11434/api/tags"]
      interval: 30s
      timeout: 10s
      retries: 3
```

## Cost Comparison

| Setup | Monthly Cost | Performance |
|-------|--------------|-------------|
| OpenAI GPT-4 | $100-1000+ | Excellent |
| Local Ollama (8B) | $0 (electricity) | Good |
| Local Ollama (70B) | $0 (electricity) | Excellent |
| Cloud GPU (A100) | $2-4/hour | Excellent |

**Local LLM = Zero API costs, full privacy, unlimited usage**
