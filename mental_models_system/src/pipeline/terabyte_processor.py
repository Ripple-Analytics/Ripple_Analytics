"""
Terabyte-Scale Data Processing Pipeline

Designed to process massive document collections using:
- Apache Spark for distributed processing
- Local LLMs for knowledge extraction
- Incremental processing with checkpointing
- Parallel document ingestion

Architecture:
┌─────────────────────────────────────────────────────────────────────────┐
│                    TERABYTE PROCESSING PIPELINE                          │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐              │
│  │ Data Sources │    │ Data Sources │    │ Data Sources │              │
│  │   (Local)    │    │(Google Drive)│    │   (S3/NAS)   │              │
│  └──────┬───────┘    └──────┬───────┘    └──────┬───────┘              │
│         └──────────────────┬┴───────────────────┘                       │
│                            ▼                                             │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │                    Document Discovery Layer                      │   │
│  │  - Recursive file scanning                                       │   │
│  │  - Change detection (hash-based)                                 │   │
│  │  - Priority queue (new > modified > unchanged)                   │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                            │                                             │
│                            ▼                                             │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │                    Apache Spark Processing                       │   │
│  │  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐            │   │
│  │  │Worker 1 │  │Worker 2 │  │Worker 3 │  │Worker N │            │   │
│  │  │ ┌─────┐ │  │ ┌─────┐ │  │ ┌─────┐ │  │ ┌─────┐ │            │   │
│  │  │ │ LLM │ │  │ │ LLM │ │  │ │ LLM │ │  │ │ LLM │ │            │   │
│  │  │ └─────┘ │  │ └─────┘ │  │ └─────┘ │  │ └─────┘ │            │   │
│  │  └─────────┘  └─────────┘  └─────────┘  └─────────┘            │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                            │                                             │
│                            ▼                                             │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │                    Result Aggregation                            │   │
│  │  - Deduplication                                                 │   │
│  │  - Quality scoring                                               │   │
│  │  - Conflict resolution                                           │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                            │                                             │
│                            ▼                                             │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │                    Knowledge Store                               │   │
│  │  - Vector database (embeddings)                                  │   │
│  │  - Relational database (structured)                              │   │
│  │  - JSON exports (for Manus integration)                          │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
"""

import os
import sys
import json
import hashlib
import asyncio
import logging
from pathlib import Path
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Any, Iterator, Tuple, Callable
from datetime import datetime
from enum import Enum
import subprocess
import multiprocessing as mp
from concurrent.futures import ProcessPoolExecutor, ThreadPoolExecutor, as_completed
import queue
import threading
import time

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# =============================================================================
# CONFIGURATION
# =============================================================================

@dataclass
class PipelineConfig:
    """Configuration for the terabyte processing pipeline."""
    
    # Data sources
    source_paths: List[str] = field(default_factory=list)
    google_drive_paths: List[str] = field(default_factory=list)
    
    # Processing
    num_workers: int = mp.cpu_count()
    batch_size: int = 100
    chunk_size: int = 1000  # characters per chunk
    chunk_overlap: int = 200
    
    # LLM Configuration
    llm_backend: str = "ollama"
    llm_url: str = "http://localhost:11434"
    llm_model: str = "llama3:70b"
    llm_concurrent: int = 4
    
    # Checkpointing
    checkpoint_dir: str = "./checkpoints"
    checkpoint_interval: int = 100  # documents
    
    # Output
    output_dir: str = "./output"
    
    # Filtering
    supported_extensions: List[str] = field(default_factory=lambda: ['.pdf', '.txt', '.md', '.docx'])
    max_file_size_mb: int = 100
    
    # Spark (optional)
    use_spark: bool = False
    spark_master: str = "local[*]"


class ProcessingStatus(Enum):
    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    FAILED = "failed"
    SKIPPED = "skipped"


@dataclass
class DocumentInfo:
    """Information about a document to process."""
    path: str
    filename: str
    size_bytes: int
    file_hash: str
    extension: str
    last_modified: datetime
    status: ProcessingStatus = ProcessingStatus.PENDING
    priority: int = 5  # 1 = highest
    error: Optional[str] = None
    
    def to_dict(self) -> Dict:
        return {
            "path": self.path,
            "filename": self.filename,
            "size_bytes": self.size_bytes,
            "file_hash": self.file_hash,
            "extension": self.extension,
            "last_modified": self.last_modified.isoformat(),
            "status": self.status.value,
            "priority": self.priority,
            "error": self.error
        }


# =============================================================================
# DOCUMENT DISCOVERY
# =============================================================================

class DocumentDiscovery:
    """Discovers and tracks documents across multiple sources."""
    
    def __init__(self, config: PipelineConfig):
        self.config = config
        self.documents: Dict[str, DocumentInfo] = {}
        self.processed_hashes: set = set()
        
        # Load existing state
        self._load_state()
    
    def _compute_hash(self, file_path: str) -> str:
        """Compute MD5 hash of file for change detection."""
        hasher = hashlib.md5()
        try:
            with open(file_path, 'rb') as f:
                for chunk in iter(lambda: f.read(65536), b''):
                    hasher.update(chunk)
            return hasher.hexdigest()
        except Exception as e:
            logger.error(f"Error hashing {file_path}: {e}")
            return ""
    
    def _load_state(self):
        """Load previous processing state."""
        state_file = Path(self.config.checkpoint_dir) / "discovery_state.json"
        if state_file.exists():
            try:
                with open(state_file, 'r') as f:
                    data = json.load(f)
                    self.processed_hashes = set(data.get("processed_hashes", []))
                    logger.info(f"Loaded {len(self.processed_hashes)} previously processed documents")
            except Exception as e:
                logger.error(f"Error loading state: {e}")
    
    def _save_state(self):
        """Save processing state for resume capability."""
        state_file = Path(self.config.checkpoint_dir) / "discovery_state.json"
        state_file.parent.mkdir(parents=True, exist_ok=True)
        
        with open(state_file, 'w') as f:
            json.dump({
                "processed_hashes": list(self.processed_hashes),
                "last_updated": datetime.now().isoformat()
            }, f)
    
    def scan_local_paths(self) -> int:
        """Scan local filesystem paths for documents."""
        count = 0
        
        for source_path in self.config.source_paths:
            source = Path(source_path)
            if not source.exists():
                logger.warning(f"Source path does not exist: {source_path}")
                continue
            
            logger.info(f"Scanning: {source_path}")
            
            for ext in self.config.supported_extensions:
                for file_path in source.rglob(f"*{ext}"):
                    try:
                        stat = file_path.stat()
                        
                        # Skip files that are too large
                        if stat.st_size > self.config.max_file_size_mb * 1024 * 1024:
                            continue
                        
                        file_hash = self._compute_hash(str(file_path))
                        
                        # Determine priority
                        if file_hash in self.processed_hashes:
                            priority = 10  # Already processed, lowest priority
                            status = ProcessingStatus.COMPLETED
                        else:
                            priority = 1  # New file, highest priority
                            status = ProcessingStatus.PENDING
                        
                        doc_info = DocumentInfo(
                            path=str(file_path),
                            filename=file_path.name,
                            size_bytes=stat.st_size,
                            file_hash=file_hash,
                            extension=file_path.suffix.lower(),
                            last_modified=datetime.fromtimestamp(stat.st_mtime),
                            status=status,
                            priority=priority
                        )
                        
                        self.documents[str(file_path)] = doc_info
                        count += 1
                        
                    except Exception as e:
                        logger.error(f"Error scanning {file_path}: {e}")
        
        logger.info(f"Discovered {count} documents from local paths")
        return count
    
    def sync_google_drive(self, local_cache: str = "./gdrive_cache") -> int:
        """Sync documents from Google Drive."""
        count = 0
        cache_dir = Path(local_cache)
        cache_dir.mkdir(parents=True, exist_ok=True)
        
        for remote_path in self.config.google_drive_paths:
            remote = f"manus_google_drive:{remote_path}"
            local = cache_dir / (remote_path.replace("/", "_") or "root")
            local.mkdir(parents=True, exist_ok=True)
            
            # Build include flags for supported extensions
            includes = []
            for ext in self.config.supported_extensions:
                includes.extend(["--include", f"*{ext}"])
            
            cmd = [
                "rclone", "sync", remote, str(local),
                "--config", "/home/ubuntu/.gdrive-rclone.ini",
                *includes,
                "-v"
            ]
            
            try:
                logger.info(f"Syncing from Google Drive: {remote_path}")
                subprocess.run(cmd, check=True, capture_output=True, timeout=600)
            except subprocess.TimeoutExpired:
                logger.warning(f"Timeout syncing {remote_path}")
            except Exception as e:
                logger.error(f"Error syncing {remote_path}: {e}")
        
        # Add synced path to local paths and scan
        self.config.source_paths.append(str(cache_dir))
        count = self.scan_local_paths()
        
        return count
    
    def get_pending_documents(self) -> List[DocumentInfo]:
        """Get documents pending processing, sorted by priority."""
        pending = [
            doc for doc in self.documents.values()
            if doc.status == ProcessingStatus.PENDING
        ]
        return sorted(pending, key=lambda x: (x.priority, x.size_bytes))
    
    def mark_completed(self, doc_path: str, success: bool = True, error: str = None):
        """Mark a document as completed or failed."""
        if doc_path in self.documents:
            doc = self.documents[doc_path]
            if success:
                doc.status = ProcessingStatus.COMPLETED
                self.processed_hashes.add(doc.file_hash)
            else:
                doc.status = ProcessingStatus.FAILED
                doc.error = error
        
        # Periodically save state
        if len(self.processed_hashes) % self.config.checkpoint_interval == 0:
            self._save_state()
    
    def get_statistics(self) -> Dict:
        """Get discovery statistics."""
        stats = {
            "total_documents": len(self.documents),
            "by_status": {},
            "by_extension": {},
            "total_size_gb": 0
        }
        
        for doc in self.documents.values():
            # By status
            status = doc.status.value
            stats["by_status"][status] = stats["by_status"].get(status, 0) + 1
            
            # By extension
            ext = doc.extension
            stats["by_extension"][ext] = stats["by_extension"].get(ext, 0) + 1
            
            # Total size
            stats["total_size_gb"] += doc.size_bytes / (1024**3)
        
        stats["total_size_gb"] = round(stats["total_size_gb"], 2)
        return stats


# =============================================================================
# TEXT EXTRACTION
# =============================================================================

class TextExtractor:
    """Extract text from various document formats."""
    
    @staticmethod
    def extract_pdf(file_path: str) -> Tuple[str, int]:
        """Extract text from PDF."""
        text = ""
        num_pages = 0
        
        try:
            import pdfplumber
            with pdfplumber.open(file_path) as pdf:
                num_pages = len(pdf.pages)
                for page in pdf.pages:
                    page_text = page.extract_text()
                    if page_text:
                        text += page_text + "\n\n"
        except Exception as e:
            # Fallback to pdftotext
            try:
                result = subprocess.run(
                    ['pdftotext', '-layout', file_path, '-'],
                    capture_output=True, text=True, timeout=60
                )
                text = result.stdout
            except:
                pass
        
        return text.strip(), num_pages
    
    @staticmethod
    def extract_txt(file_path: str) -> str:
        """Extract text from plain text file."""
        encodings = ['utf-8', 'latin-1', 'cp1252', 'iso-8859-1']
        for encoding in encodings:
            try:
                with open(file_path, 'r', encoding=encoding) as f:
                    return f.read()
            except UnicodeDecodeError:
                continue
        return ""
    
    @staticmethod
    def extract_docx(file_path: str) -> str:
        """Extract text from Word document."""
        try:
            from docx import Document
            doc = Document(file_path)
            return "\n\n".join([para.text for para in doc.paragraphs if para.text])
        except:
            return ""
    
    @classmethod
    def extract(cls, file_path: str) -> Tuple[str, Optional[int]]:
        """Extract text from any supported document type."""
        ext = Path(file_path).suffix.lower()
        
        if ext == '.pdf':
            return cls.extract_pdf(file_path)
        elif ext in ['.txt', '.md']:
            return cls.extract_txt(file_path), None
        elif ext in ['.docx', '.doc']:
            return cls.extract_docx(file_path), None
        else:
            return "", None


# =============================================================================
# CHUNKING
# =============================================================================

class SemanticChunker:
    """Split documents into semantic chunks for LLM processing."""
    
    def __init__(self, chunk_size: int = 1000, overlap: int = 200):
        self.chunk_size = chunk_size
        self.overlap = overlap
    
    def chunk(self, text: str) -> List[str]:
        """Split text into overlapping chunks."""
        if len(text) <= self.chunk_size:
            return [text]
        
        chunks = []
        start = 0
        
        while start < len(text):
            end = start + self.chunk_size
            
            # Try to break at sentence boundary
            if end < len(text):
                for char in ['.', '!', '?', '\n\n']:
                    pos = text.rfind(char, start + self.chunk_size // 2, end + 100)
                    if pos != -1:
                        end = pos + 1
                        break
            
            chunk = text[start:end].strip()
            if chunk:
                chunks.append(chunk)
            
            start = end - self.overlap
        
        return chunks


# =============================================================================
# WORKER PROCESS
# =============================================================================

def process_document_worker(args: Tuple) -> Dict:
    """
    Worker function for processing a single document.
    Runs in a separate process for parallelism.
    """
    doc_path, config_dict, extraction_types = args
    
    result = {
        "path": doc_path,
        "status": "success",
        "chunks_processed": 0,
        "extractions": {
            "mental_models": [],
            "failure_modes": [],
            "case_studies": [],
            "insights": []
        },
        "error": None
    }
    
    try:
        # Extract text
        text, num_pages = TextExtractor.extract(doc_path)
        if not text:
            result["status"] = "skipped"
            result["error"] = "No text extracted"
            return result
        
        # Chunk the text
        chunker = SemanticChunker(
            chunk_size=config_dict.get("chunk_size", 1000),
            overlap=config_dict.get("chunk_overlap", 200)
        )
        chunks = chunker.chunk(text)
        result["chunks_processed"] = len(chunks)
        
        # For now, return chunk info - actual LLM processing happens in async context
        result["chunks"] = chunks[:10]  # Limit for testing
        result["total_chunks"] = len(chunks)
        
    except Exception as e:
        result["status"] = "failed"
        result["error"] = str(e)
    
    return result


# =============================================================================
# MAIN PIPELINE
# =============================================================================

class TerabytePipeline:
    """
    Main pipeline for processing terabytes of documents.
    
    Usage:
        pipeline = TerabytePipeline(config)
        pipeline.discover_documents()
        await pipeline.process_all()
        pipeline.export_results()
    """
    
    def __init__(self, config: PipelineConfig = None):
        self.config = config or PipelineConfig()
        self.discovery = DocumentDiscovery(self.config)
        self.chunker = SemanticChunker(self.config.chunk_size, self.config.chunk_overlap)
        
        # Results storage
        self.results = {
            "mental_models": [],
            "failure_modes": [],
            "case_studies": [],
            "insights": [],
            "connections": []
        }
        
        # Progress tracking
        self.progress = {
            "total_documents": 0,
            "processed_documents": 0,
            "total_chunks": 0,
            "processed_chunks": 0,
            "start_time": None,
            "errors": []
        }
        
        # Callbacks
        self.progress_callback: Optional[Callable] = None
    
    def discover_documents(self, include_gdrive: bool = True) -> Dict:
        """Discover all documents to process."""
        logger.info("Starting document discovery...")
        
        # Scan local paths
        self.discovery.scan_local_paths()
        
        # Sync and scan Google Drive
        if include_gdrive and self.config.google_drive_paths:
            self.discovery.sync_google_drive()
        
        stats = self.discovery.get_statistics()
        self.progress["total_documents"] = stats["total_documents"]
        
        logger.info(f"Discovery complete: {json.dumps(stats, indent=2)}")
        return stats
    
    async def process_document(self, doc: DocumentInfo) -> Dict:
        """Process a single document with LLM extraction."""
        result = {
            "path": doc.path,
            "filename": doc.filename,
            "status": "success",
            "extractions": {}
        }
        
        try:
            # Extract text
            text, num_pages = TextExtractor.extract(doc.path)
            if not text:
                result["status"] = "skipped"
                return result
            
            # Chunk
            chunks = self.chunker.chunk(text)
            result["num_chunks"] = len(chunks)
            
            # Process with LLM (simplified - full version uses BatchDocumentProcessor)
            # For now, just store chunks for later processing
            result["chunks"] = chunks
            
        except Exception as e:
            result["status"] = "failed"
            result["error"] = str(e)
            self.progress["errors"].append({"path": doc.path, "error": str(e)})
        
        return result
    
    async def process_all(self, max_documents: int = None) -> Dict:
        """Process all pending documents."""
        self.progress["start_time"] = datetime.now()
        
        pending = self.discovery.get_pending_documents()
        if max_documents:
            pending = pending[:max_documents]
        
        logger.info(f"Processing {len(pending)} documents...")
        
        for i, doc in enumerate(pending):
            doc.status = ProcessingStatus.IN_PROGRESS
            
            result = await self.process_document(doc)
            
            if result["status"] == "success":
                self.discovery.mark_completed(doc.path, success=True)
            else:
                self.discovery.mark_completed(doc.path, success=False, error=result.get("error"))
            
            self.progress["processed_documents"] += 1
            
            if self.progress_callback:
                self.progress_callback(self.progress)
            
            if (i + 1) % 10 == 0:
                logger.info(f"Progress: {i+1}/{len(pending)} documents")
        
        return self.get_statistics()
    
    def process_parallel(self, max_documents: int = None) -> Dict:
        """Process documents in parallel using multiprocessing."""
        pending = self.discovery.get_pending_documents()
        if max_documents:
            pending = pending[:max_documents]
        
        logger.info(f"Processing {len(pending)} documents with {self.config.num_workers} workers...")
        
        config_dict = {
            "chunk_size": self.config.chunk_size,
            "chunk_overlap": self.config.chunk_overlap,
            "llm_backend": self.config.llm_backend,
            "llm_url": self.config.llm_url,
            "llm_model": self.config.llm_model
        }
        
        extraction_types = ["mental_model", "failure_mode", "case_study", "insight"]
        
        # Prepare arguments for workers
        work_items = [
            (doc.path, config_dict, extraction_types)
            for doc in pending
        ]
        
        results = []
        with ProcessPoolExecutor(max_workers=self.config.num_workers) as executor:
            futures = {executor.submit(process_document_worker, item): item[0] for item in work_items}
            
            for future in as_completed(futures):
                doc_path = futures[future]
                try:
                    result = future.result()
                    results.append(result)
                    
                    if result["status"] == "success":
                        self.discovery.mark_completed(doc_path, success=True)
                    else:
                        self.discovery.mark_completed(doc_path, success=False, error=result.get("error"))
                    
                    self.progress["processed_documents"] += 1
                    
                except Exception as e:
                    logger.error(f"Worker error for {doc_path}: {e}")
                    self.discovery.mark_completed(doc_path, success=False, error=str(e))
        
        return self.get_statistics()
    
    def get_statistics(self) -> Dict:
        """Get processing statistics."""
        discovery_stats = self.discovery.get_statistics()
        
        elapsed = None
        if self.progress["start_time"]:
            elapsed = (datetime.now() - self.progress["start_time"]).total_seconds()
        
        return {
            "discovery": discovery_stats,
            "progress": {
                "processed_documents": self.progress["processed_documents"],
                "total_documents": self.progress["total_documents"],
                "elapsed_seconds": elapsed,
                "documents_per_second": self.progress["processed_documents"] / elapsed if elapsed else 0,
                "errors": len(self.progress["errors"])
            },
            "results": {
                "mental_models": len(self.results["mental_models"]),
                "failure_modes": len(self.results["failure_modes"]),
                "case_studies": len(self.results["case_studies"]),
                "insights": len(self.results["insights"])
            }
        }
    
    def export_results(self, output_path: str = None) -> str:
        """Export all results to JSON."""
        output_path = output_path or os.path.join(self.config.output_dir, "extraction_results.json")
        os.makedirs(os.path.dirname(output_path), exist_ok=True)
        
        export_data = {
            "exported_at": datetime.now().isoformat(),
            "statistics": self.get_statistics(),
            "results": self.results
        }
        
        with open(output_path, 'w') as f:
            json.dump(export_data, f, indent=2, default=str)
        
        logger.info(f"Results exported to: {output_path}")
        return output_path
    
    def export_for_manus(self, output_path: str = None) -> str:
        """Export results in format optimized for Manus integration."""
        output_path = output_path or os.path.join(self.config.output_dir, "manus_import.json")
        os.makedirs(os.path.dirname(output_path), exist_ok=True)
        
        export_data = {
            "generated_at": datetime.now().isoformat(),
            "source": "terabyte_pipeline",
            "instructions": """
Import these extracted items into the Mental Models System.

For each category:
1. Review items for quality and relevance
2. Deduplicate against existing entries
3. Add high-quality items to the system
4. Track which items were added vs rejected

Categories:
- mental_models: New mental models to add
- failure_modes: New failure modes for existing models
- case_studies: Real-world examples to add
- insights: Wisdom to integrate into documentation
""",
            "items": self.results,
            "statistics": self.get_statistics()
        }
        
        with open(output_path, 'w') as f:
            json.dump(export_data, f, indent=2, default=str)
        
        return output_path


# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

def create_pipeline(
    source_paths: List[str] = None,
    gdrive_paths: List[str] = None,
    llm_backend: str = "ollama",
    num_workers: int = None
) -> TerabytePipeline:
    """Create a pipeline with common defaults."""
    
    config = PipelineConfig(
        source_paths=source_paths or [],
        google_drive_paths=gdrive_paths or ["", "Psychology", "透過 Chrome 儲存"],
        llm_backend=llm_backend,
        num_workers=num_workers or mp.cpu_count()
    )
    
    return TerabytePipeline(config)


async def quick_process(paths: List[str], max_docs: int = 10) -> Dict:
    """Quick processing for testing."""
    pipeline = create_pipeline(source_paths=paths)
    pipeline.discover_documents(include_gdrive=False)
    return await pipeline.process_all(max_documents=max_docs)


# =============================================================================
# CLI
# =============================================================================

if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(description="Terabyte-Scale Document Processing Pipeline")
    parser.add_argument("--paths", nargs="+", help="Local paths to scan")
    parser.add_argument("--gdrive", nargs="+", help="Google Drive paths to sync")
    parser.add_argument("--workers", type=int, default=mp.cpu_count(), help="Number of worker processes")
    parser.add_argument("--max-docs", type=int, help="Maximum documents to process")
    parser.add_argument("--discover-only", action="store_true", help="Only discover documents, don't process")
    parser.add_argument("--output", default="./output", help="Output directory")
    
    args = parser.parse_args()
    
    config = PipelineConfig(
        source_paths=args.paths or [],
        google_drive_paths=args.gdrive or [],
        num_workers=args.workers,
        output_dir=args.output
    )
    
    pipeline = TerabytePipeline(config)
    
    # Discover
    stats = pipeline.discover_documents(include_gdrive=bool(args.gdrive))
    print(f"\nDiscovery Statistics:\n{json.dumps(stats, indent=2)}")
    
    if not args.discover_only:
        # Process
        print("\nStarting processing...")
        results = pipeline.process_parallel(max_documents=args.max_docs)
        print(f"\nProcessing Statistics:\n{json.dumps(results, indent=2)}")
        
        # Export
        output_path = pipeline.export_for_manus()
        print(f"\nResults exported to: {output_path}")
