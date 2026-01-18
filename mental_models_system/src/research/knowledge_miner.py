"""
Knowledge Mining & Automated Idea Generation System

This system creates a self-improving knowledge snowball by:
1. Indexing all PDFs, TXTs, and research papers from Google Drive and local storage
2. Extracting and embedding knowledge using vector search
3. Automatically surfacing relevant insights for new ideas
4. Feeding continuous improvement suggestions to the system

"The best thing a human being can do is to help another human being know more."
- Charlie Munger

Architecture:
- Document Ingestion: PDF, TXT, DOCX, research papers
- Text Extraction: PyPDF2, pdfplumber, python-docx
- Chunking: Semantic chunking with overlap
- Embedding: OpenAI embeddings or local models
- Vector Store: ChromaDB (local) or Pinecone (cloud)
- Idea Generation: LLM-powered insight extraction
- Integration: Feeds into mental models and failure modes systems
"""

import os
import json
import hashlib
import subprocess
from pathlib import Path
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple, Any, Generator
from datetime import datetime
from enum import Enum
import re


# =============================================================================
# DATA STRUCTURES
# =============================================================================

class DocumentType(Enum):
    PDF = "pdf"
    TXT = "txt"
    DOCX = "docx"
    MD = "md"
    RESEARCH_PAPER = "research_paper"
    BOOK = "book"
    ARTICLE = "article"
    NOTES = "notes"


class IdeaCategory(Enum):
    MENTAL_MODEL = "mental_model"
    FAILURE_MODE = "failure_mode"
    CASE_STUDY = "case_study"
    SAFEGUARD = "safeguard"
    QUANTITATIVE_THRESHOLD = "quantitative_threshold"
    THINKER_INSIGHT = "thinker_insight"
    SYSTEM_IMPROVEMENT = "system_improvement"
    NEW_CONNECTION = "new_connection"


@dataclass
class DocumentChunk:
    """A chunk of text from a document."""
    chunk_id: str
    document_id: str
    document_path: str
    document_type: DocumentType
    content: str
    start_page: Optional[int]
    end_page: Optional[int]
    metadata: Dict[str, Any]
    embedding: Optional[List[float]] = None


@dataclass
class ExtractedIdea:
    """An idea extracted from the knowledge base."""
    idea_id: str
    category: IdeaCategory
    title: str
    description: str
    source_chunks: List[str]  # chunk_ids
    source_documents: List[str]  # document paths
    confidence: float  # 0-1
    relevance_to_models: List[int]  # mental model IDs
    actionable_suggestion: str
    priority: int  # 1-5, 1 being highest
    created_at: datetime
    status: str  # "new", "reviewed", "implemented", "rejected"


@dataclass
class DocumentIndex:
    """Index entry for a document."""
    document_id: str
    path: str
    filename: str
    document_type: DocumentType
    file_hash: str
    size_bytes: int
    num_pages: Optional[int]
    num_chunks: int
    indexed_at: datetime
    last_modified: datetime
    metadata: Dict[str, Any]


# =============================================================================
# DOCUMENT PROCESSOR
# =============================================================================

class DocumentProcessor:
    """Processes documents and extracts text content."""
    
    def __init__(self):
        self.supported_extensions = {'.pdf', '.txt', '.docx', '.md', '.doc'}
    
    def extract_text_from_pdf(self, file_path: str) -> Tuple[str, int]:
        """Extract text from PDF using multiple methods for robustness."""
        text = ""
        num_pages = 0
        
        try:
            # Try pdfplumber first (better for complex layouts)
            import pdfplumber
            with pdfplumber.open(file_path) as pdf:
                num_pages = len(pdf.pages)
                for page in pdf.pages:
                    page_text = page.extract_text()
                    if page_text:
                        text += page_text + "\n\n"
        except Exception as e:
            # Fallback to PyPDF2
            try:
                from PyPDF2 import PdfReader
                reader = PdfReader(file_path)
                num_pages = len(reader.pages)
                for page in reader.pages:
                    page_text = page.extract_text()
                    if page_text:
                        text += page_text + "\n\n"
            except Exception as e2:
                # Last resort: use pdftotext command line
                try:
                    result = subprocess.run(
                        ['pdftotext', '-layout', file_path, '-'],
                        capture_output=True, text=True
                    )
                    text = result.stdout
                except:
                    pass
        
        return text.strip(), num_pages
    
    def extract_text_from_txt(self, file_path: str) -> str:
        """Extract text from plain text file."""
        encodings = ['utf-8', 'latin-1', 'cp1252', 'iso-8859-1']
        for encoding in encodings:
            try:
                with open(file_path, 'r', encoding=encoding) as f:
                    return f.read()
            except UnicodeDecodeError:
                continue
        return ""
    
    def extract_text_from_docx(self, file_path: str) -> str:
        """Extract text from Word document."""
        try:
            from docx import Document
            doc = Document(file_path)
            return "\n\n".join([para.text for para in doc.paragraphs if para.text])
        except Exception as e:
            return ""
    
    def extract_text(self, file_path: str) -> Tuple[str, Optional[int]]:
        """Extract text from any supported document type."""
        ext = Path(file_path).suffix.lower()
        
        if ext == '.pdf':
            return self.extract_text_from_pdf(file_path)
        elif ext in ['.txt', '.md']:
            return self.extract_text_from_txt(file_path), None
        elif ext in ['.docx', '.doc']:
            return self.extract_text_from_docx(file_path), None
        else:
            return "", None
    
    def detect_document_type(self, file_path: str, content: str) -> DocumentType:
        """Detect the type of document based on content analysis."""
        filename = Path(file_path).name.lower()
        content_lower = content.lower()[:5000]  # First 5000 chars
        
        # Research paper indicators
        research_indicators = [
            'abstract', 'introduction', 'methodology', 'results', 
            'conclusion', 'references', 'doi:', 'arxiv', 'journal',
            'et al.', 'fig.', 'table', 'hypothesis'
        ]
        research_score = sum(1 for ind in research_indicators if ind in content_lower)
        
        # Book indicators
        book_indicators = [
            'chapter', 'preface', 'acknowledgments', 'table of contents',
            'isbn', 'copyright', 'published by'
        ]
        book_score = sum(1 for ind in book_indicators if ind in content_lower)
        
        if research_score >= 4:
            return DocumentType.RESEARCH_PAPER
        elif book_score >= 3:
            return DocumentType.BOOK
        elif 'munger' in filename or 'buffett' in filename:
            return DocumentType.BOOK
        elif Path(file_path).suffix.lower() == '.pdf':
            return DocumentType.PDF
        elif Path(file_path).suffix.lower() == '.txt':
            return DocumentType.TXT
        elif Path(file_path).suffix.lower() == '.md':
            return DocumentType.MD
        else:
            return DocumentType.ARTICLE


# =============================================================================
# SEMANTIC CHUNKER
# =============================================================================

class SemanticChunker:
    """Chunks documents semantically for better retrieval."""
    
    def __init__(self, 
                 chunk_size: int = 1000,
                 chunk_overlap: int = 200,
                 min_chunk_size: int = 100):
        self.chunk_size = chunk_size
        self.chunk_overlap = chunk_overlap
        self.min_chunk_size = min_chunk_size
    
    def chunk_text(self, text: str, document_id: str) -> List[DocumentChunk]:
        """Split text into semantic chunks."""
        chunks = []
        
        # First, split by major section breaks
        sections = self._split_by_sections(text)
        
        chunk_idx = 0
        for section in sections:
            # Then chunk each section
            section_chunks = self._chunk_section(section)
            
            for chunk_text in section_chunks:
                if len(chunk_text.strip()) >= self.min_chunk_size:
                    chunk_id = f"{document_id}_chunk_{chunk_idx}"
                    chunks.append(DocumentChunk(
                        chunk_id=chunk_id,
                        document_id=document_id,
                        document_path="",  # Set later
                        document_type=DocumentType.TXT,  # Set later
                        content=chunk_text.strip(),
                        start_page=None,
                        end_page=None,
                        metadata={}
                    ))
                    chunk_idx += 1
        
        return chunks
    
    def _split_by_sections(self, text: str) -> List[str]:
        """Split text by major section breaks."""
        # Common section patterns
        section_patterns = [
            r'\n#{1,3}\s+',  # Markdown headers
            r'\n[A-Z][A-Z\s]{10,}\n',  # ALL CAPS HEADERS
            r'\n\d+\.\s+[A-Z]',  # Numbered sections
            r'\n\n\n+',  # Multiple blank lines
        ]
        
        sections = [text]
        for pattern in section_patterns:
            new_sections = []
            for section in sections:
                parts = re.split(pattern, section)
                new_sections.extend(parts)
            sections = new_sections
        
        return [s for s in sections if s.strip()]
    
    def _chunk_section(self, text: str) -> List[str]:
        """Chunk a section with overlap."""
        if len(text) <= self.chunk_size:
            return [text]
        
        chunks = []
        start = 0
        
        while start < len(text):
            end = start + self.chunk_size
            
            # Try to break at sentence boundary
            if end < len(text):
                # Look for sentence end near chunk boundary
                sentence_ends = ['.', '!', '?', '\n\n']
                best_break = end
                for char in sentence_ends:
                    pos = text.rfind(char, start + self.chunk_size // 2, end + 100)
                    if pos != -1:
                        best_break = pos + 1
                        break
                end = best_break
            
            chunks.append(text[start:end])
            start = end - self.chunk_overlap
        
        return chunks


# =============================================================================
# VECTOR STORE
# =============================================================================

class VectorStore:
    """Vector store for semantic search using ChromaDB."""
    
    def __init__(self, persist_directory: str = "./vector_store"):
        self.persist_directory = persist_directory
        self.collection_name = "knowledge_base"
        self._client = None
        self._collection = None
    
    def _get_client(self):
        """Lazy initialization of ChromaDB client."""
        if self._client is None:
            try:
                import chromadb
                from chromadb.config import Settings
                
                self._client = chromadb.Client(Settings(
                    chroma_db_impl="duckdb+parquet",
                    persist_directory=self.persist_directory,
                    anonymized_telemetry=False
                ))
                self._collection = self._client.get_or_create_collection(
                    name=self.collection_name,
                    metadata={"hnsw:space": "cosine"}
                )
            except ImportError:
                # Fallback to simple in-memory store
                self._client = "fallback"
                self._collection = {"documents": [], "embeddings": [], "ids": [], "metadatas": []}
        
        return self._client, self._collection
    
    def add_chunks(self, chunks: List[DocumentChunk], embeddings: List[List[float]]):
        """Add chunks with embeddings to the vector store."""
        client, collection = self._get_client()
        
        if client == "fallback":
            # Simple fallback storage
            for chunk, embedding in zip(chunks, embeddings):
                collection["documents"].append(chunk.content)
                collection["embeddings"].append(embedding)
                collection["ids"].append(chunk.chunk_id)
                collection["metadatas"].append({
                    "document_id": chunk.document_id,
                    "document_path": chunk.document_path,
                    "document_type": chunk.document_type.value
                })
        else:
            collection.add(
                documents=[c.content for c in chunks],
                embeddings=embeddings,
                ids=[c.chunk_id for c in chunks],
                metadatas=[{
                    "document_id": c.document_id,
                    "document_path": c.document_path,
                    "document_type": c.document_type.value
                } for c in chunks]
            )
    
    def search(self, query_embedding: List[float], n_results: int = 10) -> List[Dict]:
        """Search for similar chunks."""
        client, collection = self._get_client()
        
        if client == "fallback":
            # Simple cosine similarity search
            import numpy as np
            query = np.array(query_embedding)
            similarities = []
            for i, emb in enumerate(collection["embeddings"]):
                emb_array = np.array(emb)
                similarity = np.dot(query, emb_array) / (np.linalg.norm(query) * np.linalg.norm(emb_array))
                similarities.append((i, similarity))
            
            similarities.sort(key=lambda x: x[1], reverse=True)
            results = []
            for i, sim in similarities[:n_results]:
                results.append({
                    "id": collection["ids"][i],
                    "document": collection["documents"][i],
                    "metadata": collection["metadatas"][i],
                    "similarity": sim
                })
            return results
        else:
            results = collection.query(
                query_embeddings=[query_embedding],
                n_results=n_results
            )
            return [{
                "id": results["ids"][0][i],
                "document": results["documents"][0][i],
                "metadata": results["metadatas"][0][i],
                "similarity": 1 - results["distances"][0][i] if "distances" in results else None
            } for i in range(len(results["ids"][0]))]


# =============================================================================
# EMBEDDING SERVICE
# =============================================================================

class EmbeddingService:
    """Service for generating text embeddings."""
    
    def __init__(self, model: str = "text-embedding-3-small"):
        self.model = model
        self._client = None
    
    def _get_client(self):
        """Lazy initialization of OpenAI client."""
        if self._client is None:
            try:
                from openai import OpenAI
                self._client = OpenAI()
            except:
                self._client = "fallback"
        return self._client
    
    def embed_text(self, text: str) -> List[float]:
        """Generate embedding for a single text."""
        client = self._get_client()
        
        if client == "fallback":
            # Simple hash-based fallback embedding
            import hashlib
            hash_bytes = hashlib.sha256(text.encode()).digest()
            return [float(b) / 255.0 for b in hash_bytes[:128]]
        
        response = client.embeddings.create(
            model=self.model,
            input=text[:8000]  # Truncate to model limit
        )
        return response.data[0].embedding
    
    def embed_batch(self, texts: List[str], batch_size: int = 100) -> List[List[float]]:
        """Generate embeddings for multiple texts."""
        embeddings = []
        for i in range(0, len(texts), batch_size):
            batch = texts[i:i+batch_size]
            client = self._get_client()
            
            if client == "fallback":
                embeddings.extend([self.embed_text(t) for t in batch])
            else:
                response = client.embeddings.create(
                    model=self.model,
                    input=[t[:8000] for t in batch]
                )
                embeddings.extend([d.embedding for d in response.data])
        
        return embeddings


# =============================================================================
# IDEA EXTRACTOR
# =============================================================================

class IdeaExtractor:
    """Extracts actionable ideas from knowledge chunks using LLM."""
    
    def __init__(self):
        self._client = None
    
    def _get_client(self):
        """Lazy initialization of OpenAI client."""
        if self._client is None:
            try:
                from openai import OpenAI
                self._client = OpenAI()
            except:
                self._client = "fallback"
        return self._client
    
    def extract_ideas(self, chunks: List[DocumentChunk], 
                     context: str = "mental models and decision-making") -> List[ExtractedIdea]:
        """Extract ideas from a set of related chunks."""
        client = self._get_client()
        
        if client == "fallback":
            return []
        
        # Combine chunks into context
        combined_text = "\n\n---\n\n".join([c.content for c in chunks[:5]])
        
        prompt = f"""Analyze the following text excerpts and extract actionable ideas for improving a {context} system.

For each idea, provide:
1. Category (one of: mental_model, failure_mode, case_study, safeguard, quantitative_threshold, thinker_insight, system_improvement, new_connection)
2. Title (concise)
3. Description (detailed)
4. Actionable suggestion (specific implementation)
5. Priority (1-5, 1 being highest)
6. Relevant mental model IDs if applicable (from Munger's 129 models)

Text excerpts:
{combined_text}

Respond in JSON format with an array of ideas."""

        try:
            response = client.chat.completions.create(
                model="gpt-4.1-mini",
                messages=[{"role": "user", "content": prompt}],
                response_format={"type": "json_object"}
            )
            
            result = json.loads(response.choices[0].message.content)
            ideas = []
            
            for item in result.get("ideas", []):
                idea = ExtractedIdea(
                    idea_id=hashlib.md5(item.get("title", "").encode()).hexdigest()[:12],
                    category=IdeaCategory(item.get("category", "system_improvement")),
                    title=item.get("title", ""),
                    description=item.get("description", ""),
                    source_chunks=[c.chunk_id for c in chunks],
                    source_documents=list(set([c.document_path for c in chunks])),
                    confidence=item.get("confidence", 0.7),
                    relevance_to_models=item.get("relevant_models", []),
                    actionable_suggestion=item.get("actionable_suggestion", ""),
                    priority=item.get("priority", 3),
                    created_at=datetime.now(),
                    status="new"
                )
                ideas.append(idea)
            
            return ideas
        except Exception as e:
            print(f"Error extracting ideas: {e}")
            return []


# =============================================================================
# KNOWLEDGE MINER - MAIN CLASS
# =============================================================================

class KnowledgeMiner:
    """
    Main class for mining knowledge from documents and generating improvement ideas.
    
    This creates the automated improvement snowball by:
    1. Continuously indexing new documents
    2. Extracting and embedding knowledge
    3. Finding connections between ideas
    4. Generating actionable improvement suggestions
    """
    
    def __init__(self, 
                 index_path: str = "./knowledge_index",
                 vector_store_path: str = "./vector_store"):
        self.index_path = Path(index_path)
        self.index_path.mkdir(parents=True, exist_ok=True)
        
        self.processor = DocumentProcessor()
        self.chunker = SemanticChunker()
        self.vector_store = VectorStore(vector_store_path)
        self.embedding_service = EmbeddingService()
        self.idea_extractor = IdeaExtractor()
        
        self.document_index: Dict[str, DocumentIndex] = {}
        self.ideas: List[ExtractedIdea] = []
        
        self._load_index()
    
    def _load_index(self):
        """Load existing index from disk."""
        index_file = self.index_path / "document_index.json"
        if index_file.exists():
            with open(index_file, 'r') as f:
                data = json.load(f)
                for doc_id, doc_data in data.items():
                    self.document_index[doc_id] = DocumentIndex(
                        document_id=doc_data["document_id"],
                        path=doc_data["path"],
                        filename=doc_data["filename"],
                        document_type=DocumentType(doc_data["document_type"]),
                        file_hash=doc_data["file_hash"],
                        size_bytes=doc_data["size_bytes"],
                        num_pages=doc_data.get("num_pages"),
                        num_chunks=doc_data["num_chunks"],
                        indexed_at=datetime.fromisoformat(doc_data["indexed_at"]),
                        last_modified=datetime.fromisoformat(doc_data["last_modified"]),
                        metadata=doc_data.get("metadata", {})
                    )
        
        ideas_file = self.index_path / "ideas.json"
        if ideas_file.exists():
            with open(ideas_file, 'r') as f:
                data = json.load(f)
                for idea_data in data:
                    self.ideas.append(ExtractedIdea(
                        idea_id=idea_data["idea_id"],
                        category=IdeaCategory(idea_data["category"]),
                        title=idea_data["title"],
                        description=idea_data["description"],
                        source_chunks=idea_data["source_chunks"],
                        source_documents=idea_data["source_documents"],
                        confidence=idea_data["confidence"],
                        relevance_to_models=idea_data["relevance_to_models"],
                        actionable_suggestion=idea_data["actionable_suggestion"],
                        priority=idea_data["priority"],
                        created_at=datetime.fromisoformat(idea_data["created_at"]),
                        status=idea_data["status"]
                    ))
    
    def _save_index(self):
        """Save index to disk."""
        index_file = self.index_path / "document_index.json"
        with open(index_file, 'w') as f:
            data = {}
            for doc_id, doc in self.document_index.items():
                data[doc_id] = {
                    "document_id": doc.document_id,
                    "path": doc.path,
                    "filename": doc.filename,
                    "document_type": doc.document_type.value,
                    "file_hash": doc.file_hash,
                    "size_bytes": doc.size_bytes,
                    "num_pages": doc.num_pages,
                    "num_chunks": doc.num_chunks,
                    "indexed_at": doc.indexed_at.isoformat(),
                    "last_modified": doc.last_modified.isoformat(),
                    "metadata": doc.metadata
                }
            json.dump(data, f, indent=2)
        
        ideas_file = self.index_path / "ideas.json"
        with open(ideas_file, 'w') as f:
            data = []
            for idea in self.ideas:
                data.append({
                    "idea_id": idea.idea_id,
                    "category": idea.category.value,
                    "title": idea.title,
                    "description": idea.description,
                    "source_chunks": idea.source_chunks,
                    "source_documents": idea.source_documents,
                    "confidence": idea.confidence,
                    "relevance_to_models": idea.relevance_to_models,
                    "actionable_suggestion": idea.actionable_suggestion,
                    "priority": idea.priority,
                    "created_at": idea.created_at.isoformat(),
                    "status": idea.status
                })
            json.dump(data, f, indent=2)
    
    def _compute_file_hash(self, file_path: str) -> str:
        """Compute hash of file for change detection."""
        hasher = hashlib.md5()
        with open(file_path, 'rb') as f:
            for chunk in iter(lambda: f.read(4096), b''):
                hasher.update(chunk)
        return hasher.hexdigest()
    
    def index_document(self, file_path: str, force: bool = False) -> Optional[DocumentIndex]:
        """Index a single document."""
        file_path = str(Path(file_path).resolve())
        
        if not os.path.exists(file_path):
            print(f"File not found: {file_path}")
            return None
        
        # Check if already indexed and unchanged
        file_hash = self._compute_file_hash(file_path)
        doc_id = hashlib.md5(file_path.encode()).hexdigest()[:12]
        
        if doc_id in self.document_index and not force:
            existing = self.document_index[doc_id]
            if existing.file_hash == file_hash:
                print(f"Already indexed (unchanged): {file_path}")
                return existing
        
        print(f"Indexing: {file_path}")
        
        # Extract text
        text, num_pages = self.processor.extract_text(file_path)
        if not text:
            print(f"Could not extract text from: {file_path}")
            return None
        
        # Detect document type
        doc_type = self.processor.detect_document_type(file_path, text)
        
        # Chunk the document
        chunks = self.chunker.chunk_text(text, doc_id)
        for chunk in chunks:
            chunk.document_path = file_path
            chunk.document_type = doc_type
        
        # Generate embeddings
        embeddings = self.embedding_service.embed_batch([c.content for c in chunks])
        
        # Store in vector store
        self.vector_store.add_chunks(chunks, embeddings)
        
        # Create index entry
        stat = os.stat(file_path)
        index_entry = DocumentIndex(
            document_id=doc_id,
            path=file_path,
            filename=Path(file_path).name,
            document_type=doc_type,
            file_hash=file_hash,
            size_bytes=stat.st_size,
            num_pages=num_pages,
            num_chunks=len(chunks),
            indexed_at=datetime.now(),
            last_modified=datetime.fromtimestamp(stat.st_mtime),
            metadata={"text_length": len(text)}
        )
        
        self.document_index[doc_id] = index_entry
        self._save_index()
        
        print(f"Indexed {len(chunks)} chunks from {file_path}")
        return index_entry
    
    def index_directory(self, directory: str, recursive: bool = True) -> List[DocumentIndex]:
        """Index all documents in a directory."""
        directory = Path(directory)
        indexed = []
        
        pattern = "**/*" if recursive else "*"
        for file_path in directory.glob(pattern):
            if file_path.is_file() and file_path.suffix.lower() in self.processor.supported_extensions:
                result = self.index_document(str(file_path))
                if result:
                    indexed.append(result)
        
        return indexed
    
    def index_google_drive(self, remote_path: str = "", local_cache: str = "./gdrive_cache") -> List[DocumentIndex]:
        """Index documents from Google Drive using rclone."""
        cache_dir = Path(local_cache)
        cache_dir.mkdir(parents=True, exist_ok=True)
        
        # Sync from Google Drive
        remote = f"manus_google_drive:{remote_path}"
        cmd = [
            "rclone", "sync", remote, str(cache_dir),
            "--config", "/home/ubuntu/.gdrive-rclone.ini",
            "--include", "*.pdf",
            "--include", "*.txt",
            "--include", "*.docx",
            "--include", "*.md",
            "-v"
        ]
        
        try:
            subprocess.run(cmd, check=True, capture_output=True)
            print(f"Synced Google Drive to {cache_dir}")
        except subprocess.CalledProcessError as e:
            print(f"Error syncing Google Drive: {e}")
        
        # Index the cached files
        return self.index_directory(str(cache_dir))
    
    def search(self, query: str, n_results: int = 10) -> List[Dict]:
        """Search the knowledge base."""
        query_embedding = self.embedding_service.embed_text(query)
        return self.vector_store.search(query_embedding, n_results)
    
    def generate_ideas(self, query: str = None, n_chunks: int = 10) -> List[ExtractedIdea]:
        """Generate improvement ideas from the knowledge base."""
        if query:
            # Search for relevant chunks
            results = self.search(query, n_chunks)
            chunks = [DocumentChunk(
                chunk_id=r["id"],
                document_id=r["metadata"]["document_id"],
                document_path=r["metadata"]["document_path"],
                document_type=DocumentType(r["metadata"]["document_type"]),
                content=r["document"],
                start_page=None,
                end_page=None,
                metadata={}
            ) for r in results]
        else:
            # Use random sampling for serendipitous discovery
            # (In production, would sample from vector store)
            chunks = []
        
        if not chunks:
            return []
        
        # Extract ideas
        new_ideas = self.idea_extractor.extract_ideas(chunks)
        
        # Add to idea store
        self.ideas.extend(new_ideas)
        self._save_index()
        
        return new_ideas
    
    def get_improvement_suggestions(self, 
                                   category: IdeaCategory = None,
                                   min_priority: int = 3,
                                   status: str = "new") -> List[ExtractedIdea]:
        """Get filtered improvement suggestions."""
        suggestions = self.ideas
        
        if category:
            suggestions = [i for i in suggestions if i.category == category]
        if min_priority:
            suggestions = [i for i in suggestions if i.priority <= min_priority]
        if status:
            suggestions = [i for i in suggestions if i.status == status]
        
        return sorted(suggestions, key=lambda x: x.priority)
    
    def mark_idea_status(self, idea_id: str, status: str):
        """Update the status of an idea."""
        for idea in self.ideas:
            if idea.idea_id == idea_id:
                idea.status = status
                break
        self._save_index()
    
    def get_statistics(self) -> Dict:
        """Get statistics about the knowledge base."""
        return {
            "total_documents": len(self.document_index),
            "total_chunks": sum(d.num_chunks for d in self.document_index.values()),
            "total_ideas": len(self.ideas),
            "ideas_by_category": {
                cat.value: len([i for i in self.ideas if i.category == cat])
                for cat in IdeaCategory
            },
            "ideas_by_status": {
                status: len([i for i in self.ideas if i.status == status])
                for status in ["new", "reviewed", "implemented", "rejected"]
            },
            "documents_by_type": {
                dtype.value: len([d for d in self.document_index.values() if d.document_type == dtype])
                for dtype in DocumentType
            }
        }


# =============================================================================
# AUTOMATED IMPROVEMENT RUNNER
# =============================================================================

class AutomatedImprovementRunner:
    """
    Runs automated improvement cycles using the knowledge miner.
    
    This creates the snowball effect by:
    1. Periodically scanning for new documents
    2. Generating ideas from the knowledge base
    3. Prioritizing and surfacing actionable improvements
    4. Tracking implementation and learning from outcomes
    """
    
    def __init__(self, miner: KnowledgeMiner):
        self.miner = miner
        self.run_history: List[Dict] = []
    
    def run_improvement_cycle(self, 
                             focus_areas: List[str] = None,
                             max_ideas: int = 10) -> Dict:
        """Run a single improvement cycle."""
        cycle_start = datetime.now()
        
        # Default focus areas for mental models system
        if not focus_areas:
            focus_areas = [
                "mental model failure modes",
                "decision-making biases",
                "investment mistakes case studies",
                "cognitive safeguards",
                "Munger wisdom",
                "quantitative thresholds for bias detection"
            ]
        
        all_ideas = []
        for focus in focus_areas:
            ideas = self.miner.generate_ideas(focus, n_chunks=5)
            all_ideas.extend(ideas)
        
        # Deduplicate and prioritize
        seen_titles = set()
        unique_ideas = []
        for idea in sorted(all_ideas, key=lambda x: x.priority):
            if idea.title not in seen_titles:
                seen_titles.add(idea.title)
                unique_ideas.append(idea)
                if len(unique_ideas) >= max_ideas:
                    break
        
        cycle_result = {
            "cycle_start": cycle_start.isoformat(),
            "cycle_end": datetime.now().isoformat(),
            "focus_areas": focus_areas,
            "ideas_generated": len(all_ideas),
            "unique_ideas": len(unique_ideas),
            "top_ideas": [
                {
                    "title": i.title,
                    "category": i.category.value,
                    "priority": i.priority,
                    "suggestion": i.actionable_suggestion[:200]
                }
                for i in unique_ideas[:5]
            ]
        }
        
        self.run_history.append(cycle_result)
        return cycle_result
    
    def get_daily_digest(self) -> str:
        """Generate a daily digest of improvement suggestions."""
        new_ideas = self.miner.get_improvement_suggestions(status="new", min_priority=2)
        
        digest = f"""
# Knowledge Mining Daily Digest
Generated: {datetime.now().strftime('%Y-%m-%d %H:%M')}

## Statistics
{json.dumps(self.miner.get_statistics(), indent=2)}

## Top Priority Improvements ({len(new_ideas)} new ideas)

"""
        for i, idea in enumerate(new_ideas[:10], 1):
            digest += f"""
### {i}. {idea.title}
- **Category:** {idea.category.value}
- **Priority:** {idea.priority}/5
- **Confidence:** {idea.confidence:.0%}
- **Description:** {idea.description[:300]}...
- **Action:** {idea.actionable_suggestion}
- **Sources:** {', '.join(idea.source_documents[:3])}

"""
        
        return digest


# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

def create_knowledge_miner(index_path: str = None, vector_store_path: str = None) -> KnowledgeMiner:
    """Create a knowledge miner instance with default paths."""
    base_path = Path("/home/ubuntu/Ripple_Analytics/mental_models_system/data")
    
    if index_path is None:
        index_path = str(base_path / "knowledge_index")
    if vector_store_path is None:
        vector_store_path = str(base_path / "vector_store")
    
    return KnowledgeMiner(index_path, vector_store_path)


def quick_search(query: str, n_results: int = 5) -> List[Dict]:
    """Quick search across the knowledge base."""
    miner = create_knowledge_miner()
    return miner.search(query, n_results)


def generate_improvement_ideas(focus: str = "mental models") -> List[ExtractedIdea]:
    """Generate improvement ideas for a focus area."""
    miner = create_knowledge_miner()
    return miner.generate_ideas(focus)


if __name__ == "__main__":
    print("=" * 70)
    print("KNOWLEDGE MINER - AUTOMATED IMPROVEMENT SYSTEM")
    print("=" * 70)
    
    # Create miner
    miner = create_knowledge_miner()
    
    print(f"\nCurrent Statistics:")
    print(json.dumps(miner.get_statistics(), indent=2))
    
    print("\n" + "=" * 70)
    print("Ready to index documents and generate ideas.")
    print("=" * 70)
    
    # Example usage:
    # miner.index_google_drive()  # Index all PDFs from Google Drive
    # miner.index_directory("/path/to/research/papers")
    # ideas = miner.generate_ideas("mental model failure modes")
