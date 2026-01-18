import os
import json
import hashlib
import logging
from pathlib import Path
from dataclasses import dataclass, field
from typing import List, Optional, Dict, Any, Callable
from datetime import datetime
from concurrent.futures import ThreadPoolExecutor, as_completed
import threading

from .chunker import TextChunker, TextChunk, ChunkingStrategy
from .watcher import FolderWatcher, FileEvent

logger = logging.getLogger(__name__)


@dataclass
class DocumentChunk:
    id: str
    document_id: str
    document_path: str
    content: str
    chunk_index: int
    start_pos: int
    end_pos: int
    embedding: Optional[List[float]] = None
    metadata: dict = field(default_factory=dict)
    created_at: datetime = field(default_factory=datetime.now)


@dataclass
class ProcessedDocument:
    id: str
    path: str
    file_hash: str
    chunk_count: int
    total_chars: int
    processed_at: datetime
    metadata: dict = field(default_factory=dict)


class DataIngestionPipeline:
    def __init__(
        self,
        watch_paths: Optional[List[str]] = None,
        db_connection_string: Optional[str] = None,
        lm_studio_url: str = "http://localhost:1234",
        embedding_model: str = "text-embedding-ada-002",
        chunk_size: int = 1000,
        chunk_overlap: int = 200,
        chunking_strategy: ChunkingStrategy = ChunkingStrategy.SLIDING_WINDOW,
        batch_size: int = 50,
        max_workers: int = 4,
        auto_start: bool = False,
    ):
        self.watch_paths = watch_paths or []
        self.db_connection_string = db_connection_string or os.getenv(
            "DATABASE_URL", "postgresql://postgres:postgres@localhost:5432/mental_models"
        )
        self.lm_studio_url = lm_studio_url
        self.embedding_model = embedding_model
        self.batch_size = batch_size
        self.max_workers = max_workers
        
        self.chunker = TextChunker(
            strategy=chunking_strategy,
            chunk_size=chunk_size,
            chunk_overlap=chunk_overlap,
        )
        
        self.watcher: Optional[FolderWatcher] = None
        self._processed_docs: Dict[str, ProcessedDocument] = {}
        self._lock = threading.Lock()
        self._callbacks: List[Callable[[List[DocumentChunk]], None]] = []
        self._db_conn = None
        self._http_client = None
        
        if auto_start and watch_paths:
            self.start_watching()

    def _get_http_client(self):
        if self._http_client is None:
            import httpx
            self._http_client = httpx.Client(timeout=60.0)
        return self._http_client

    def _get_db_connection(self):
        if self._db_conn is None:
            try:
                import psycopg2
                self._db_conn = psycopg2.connect(self.db_connection_string)
            except Exception as e:
                logger.warning(f"Database connection failed: {e}")
                return None
        return self._db_conn

    def setup_database(self) -> bool:
        conn = self._get_db_connection()
        if not conn:
            logger.warning("No database connection, skipping setup")
            return False
        
        try:
            with conn.cursor() as cur:
                cur.execute("CREATE EXTENSION IF NOT EXISTS vector")
                
                cur.execute("""
                    CREATE TABLE IF NOT EXISTS ingested_documents (
                        id TEXT PRIMARY KEY,
                        path TEXT NOT NULL,
                        file_hash TEXT NOT NULL,
                        chunk_count INTEGER,
                        total_chars INTEGER,
                        processed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                        metadata JSONB DEFAULT '{}'
                    )
                """)
                
                cur.execute("""
                    CREATE TABLE IF NOT EXISTS document_chunks (
                        id TEXT PRIMARY KEY,
                        document_id TEXT NOT NULL REFERENCES ingested_documents(id) ON DELETE CASCADE,
                        document_path TEXT NOT NULL,
                        content TEXT NOT NULL,
                        chunk_index INTEGER NOT NULL,
                        start_pos INTEGER,
                        end_pos INTEGER,
                        embedding vector(1536),
                        metadata JSONB DEFAULT '{}',
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                    )
                """)
                
                cur.execute("""
                    CREATE INDEX IF NOT EXISTS idx_document_chunks_document_id 
                    ON document_chunks(document_id)
                """)
                
                cur.execute("""
                    CREATE INDEX IF NOT EXISTS idx_document_chunks_embedding 
                    ON document_chunks USING ivfflat (embedding vector_cosine_ops)
                    WITH (lists = 100)
                """)
                
                conn.commit()
                logger.info("Database setup complete")
                return True
                
        except Exception as e:
            logger.error(f"Database setup failed: {e}")
            conn.rollback()
            return False

    def _generate_document_id(self, path: str, file_hash: str) -> str:
        return hashlib.sha256(f"{path}:{file_hash}".encode()).hexdigest()[:32]

    def _generate_chunk_id(self, document_id: str, chunk_index: int) -> str:
        return hashlib.sha256(f"{document_id}:{chunk_index}".encode()).hexdigest()[:32]

    def _read_file(self, path: Path) -> Optional[str]:
        encodings = ["utf-8", "utf-8-sig", "latin-1", "cp1252"]
        
        for encoding in encodings:
            try:
                with open(path, "r", encoding=encoding) as f:
                    return f.read()
            except UnicodeDecodeError:
                continue
            except Exception as e:
                logger.error(f"Error reading {path}: {e}")
                return None
        
        logger.warning(f"Could not decode {path} with any encoding")
        return None

    def _generate_embedding(self, text: str) -> Optional[List[float]]:
        try:
            client = self._get_http_client()
            response = client.post(
                f"{self.lm_studio_url}/v1/embeddings",
                json={
                    "model": self.embedding_model,
                    "input": text[:8000],
                },
            )
            
            if response.status_code == 200:
                data = response.json()
                return data["data"][0]["embedding"]
            else:
                logger.warning(f"Embedding request failed: {response.status_code}")
                return None
                
        except Exception as e:
            logger.debug(f"Embedding generation failed (LM Studio may not be running): {e}")
            return None

    def _generate_embeddings_batch(self, texts: List[str]) -> List[Optional[List[float]]]:
        embeddings = []
        for text in texts:
            embedding = self._generate_embedding(text)
            embeddings.append(embedding)
        return embeddings

    def process_file(self, path: Path, file_hash: Optional[str] = None) -> List[DocumentChunk]:
        content = self._read_file(path)
        if not content:
            return []
        
        if not file_hash:
            file_hash = hashlib.md5(content.encode()).hexdigest()
        
        document_id = self._generate_document_id(str(path), file_hash)
        
        with self._lock:
            if document_id in self._processed_docs:
                logger.debug(f"Document already processed: {path}")
                return []
        
        metadata = {
            "filename": path.name,
            "extension": path.suffix,
            "directory": str(path.parent),
        }
        
        text_chunks = self.chunker.chunk(content, metadata)
        
        document_chunks = []
        for text_chunk in text_chunks:
            chunk_id = self._generate_chunk_id(document_id, text_chunk.chunk_index)
            
            doc_chunk = DocumentChunk(
                id=chunk_id,
                document_id=document_id,
                document_path=str(path),
                content=text_chunk.content,
                chunk_index=text_chunk.chunk_index,
                start_pos=text_chunk.start_pos,
                end_pos=text_chunk.end_pos,
                metadata={**metadata, **text_chunk.metadata},
            )
            document_chunks.append(doc_chunk)
        
        chunk_texts = [chunk.content for chunk in document_chunks]
        embeddings = self._generate_embeddings_batch(chunk_texts)
        
        for chunk, embedding in zip(document_chunks, embeddings):
            chunk.embedding = embedding
        
        processed_doc = ProcessedDocument(
            id=document_id,
            path=str(path),
            file_hash=file_hash,
            chunk_count=len(document_chunks),
            total_chars=len(content),
            processed_at=datetime.now(),
            metadata=metadata,
        )
        
        with self._lock:
            self._processed_docs[document_id] = processed_doc
        
        self._store_chunks(processed_doc, document_chunks)
        
        for callback in self._callbacks:
            try:
                callback(document_chunks)
            except Exception as e:
                logger.error(f"Callback error: {e}")
        
        logger.info(f"Processed {path}: {len(document_chunks)} chunks")
        return document_chunks

    def _store_chunks(self, doc: ProcessedDocument, chunks: List[DocumentChunk]) -> bool:
        conn = self._get_db_connection()
        if not conn:
            return False
        
        try:
            with conn.cursor() as cur:
                cur.execute("""
                    INSERT INTO ingested_documents (id, path, file_hash, chunk_count, total_chars, metadata)
                    VALUES (%s, %s, %s, %s, %s, %s)
                    ON CONFLICT (id) DO UPDATE SET
                        chunk_count = EXCLUDED.chunk_count,
                        total_chars = EXCLUDED.total_chars,
                        processed_at = CURRENT_TIMESTAMP
                """, (doc.id, doc.path, doc.file_hash, doc.chunk_count, doc.total_chars, 
                      json.dumps(doc.metadata)))
                
                for chunk in chunks:
                    embedding_str = None
                    if chunk.embedding:
                        embedding_str = "[" + ",".join(str(x) for x in chunk.embedding) + "]"
                    
                    cur.execute("""
                        INSERT INTO document_chunks 
                        (id, document_id, document_path, content, chunk_index, start_pos, end_pos, embedding, metadata)
                        VALUES (%s, %s, %s, %s, %s, %s, %s, %s::vector, %s)
                        ON CONFLICT (id) DO UPDATE SET
                            content = EXCLUDED.content,
                            embedding = EXCLUDED.embedding,
                            metadata = EXCLUDED.metadata
                    """, (chunk.id, chunk.document_id, chunk.document_path, chunk.content,
                          chunk.chunk_index, chunk.start_pos, chunk.end_pos, embedding_str,
                          json.dumps(chunk.metadata)))
                
                conn.commit()
                return True
                
        except Exception as e:
            logger.error(f"Failed to store chunks: {e}")
            conn.rollback()
            return False

    def process_files(self, paths: List[Path]) -> List[DocumentChunk]:
        all_chunks = []
        
        with ThreadPoolExecutor(max_workers=self.max_workers) as executor:
            futures = {executor.submit(self.process_file, path): path for path in paths}
            
            for future in as_completed(futures):
                path = futures[future]
                try:
                    chunks = future.result()
                    all_chunks.extend(chunks)
                except Exception as e:
                    logger.error(f"Error processing {path}: {e}")
        
        return all_chunks

    def _handle_file_events(self, events: List[FileEvent]) -> None:
        paths_to_process = []
        
        for event in events:
            if event.event_type in ("created", "modified", "existing"):
                paths_to_process.append(event.path)
            elif event.event_type == "deleted":
                self._handle_deleted_file(event.path)
        
        if paths_to_process:
            self.process_files(paths_to_process)

    def _handle_deleted_file(self, path: Path) -> None:
        conn = self._get_db_connection()
        if not conn:
            return
        
        try:
            with conn.cursor() as cur:
                cur.execute("""
                    DELETE FROM ingested_documents WHERE path = %s
                """, (str(path),))
                conn.commit()
                logger.info(f"Removed deleted document: {path}")
        except Exception as e:
            logger.error(f"Error removing deleted document: {e}")
            conn.rollback()

    def start_watching(self) -> None:
        if not self.watch_paths:
            logger.warning("No watch paths configured")
            return
        
        self.watcher = FolderWatcher(
            watch_paths=self.watch_paths,
            extensions=[".txt"],
            recursive=True,
            batch_size=self.batch_size,
        )
        
        self.watcher.add_callback(self._handle_file_events)
        
        existing_events = self.watcher.scan_existing()
        if existing_events:
            self._handle_file_events(existing_events)
        
        self.watcher.start()
        logger.info(f"Started watching {len(self.watch_paths)} paths")

    def stop_watching(self) -> None:
        if self.watcher:
            self.watcher.stop()
            self.watcher = None

    def add_callback(self, callback: Callable[[List[DocumentChunk]], None]) -> None:
        self._callbacks.append(callback)

    def semantic_search(
        self,
        query: str,
        limit: int = 10,
        min_similarity: float = 0.5,
    ) -> List[Dict[str, Any]]:
        query_embedding = self._generate_embedding(query)
        if not query_embedding:
            return self._keyword_search(query, limit)
        
        conn = self._get_db_connection()
        if not conn:
            return []
        
        try:
            embedding_str = "[" + ",".join(str(x) for x in query_embedding) + "]"
            
            with conn.cursor() as cur:
                cur.execute("""
                    SELECT 
                        id, document_path, content, chunk_index, metadata,
                        1 - (embedding <=> %s::vector) as similarity
                    FROM document_chunks
                    WHERE embedding IS NOT NULL
                    ORDER BY embedding <=> %s::vector
                    LIMIT %s
                """, (embedding_str, embedding_str, limit))
                
                results = []
                for row in cur.fetchall():
                    similarity = row[5] if row[5] else 0
                    if similarity >= min_similarity:
                        results.append({
                            "id": row[0],
                            "document_path": row[1],
                            "content": row[2],
                            "chunk_index": row[3],
                            "metadata": row[4],
                            "similarity": similarity,
                        })
                
                return results
                
        except Exception as e:
            logger.error(f"Semantic search failed: {e}")
            return self._keyword_search(query, limit)

    def _keyword_search(self, query: str, limit: int = 10) -> List[Dict[str, Any]]:
        conn = self._get_db_connection()
        if not conn:
            return []
        
        try:
            with conn.cursor() as cur:
                search_terms = query.lower().split()
                like_clauses = " OR ".join(["LOWER(content) LIKE %s" for _ in search_terms])
                params = [f"%{term}%" for term in search_terms]
                params.append(limit)
                
                cur.execute(f"""
                    SELECT id, document_path, content, chunk_index, metadata
                    FROM document_chunks
                    WHERE {like_clauses}
                    LIMIT %s
                """, params)
                
                results = []
                for row in cur.fetchall():
                    results.append({
                        "id": row[0],
                        "document_path": row[1],
                        "content": row[2],
                        "chunk_index": row[3],
                        "metadata": row[4],
                        "similarity": 0.5,
                    })
                
                return results
                
        except Exception as e:
            logger.error(f"Keyword search failed: {e}")
            return []

    def get_stats(self) -> Dict[str, Any]:
        conn = self._get_db_connection()
        stats = {
            "documents_in_memory": len(self._processed_docs),
            "watch_paths": self.watch_paths,
            "watching": self.watcher is not None and self.watcher._running,
        }
        
        if conn:
            try:
                with conn.cursor() as cur:
                    cur.execute("SELECT COUNT(*) FROM ingested_documents")
                    stats["documents_in_db"] = cur.fetchone()[0]
                    
                    cur.execute("SELECT COUNT(*) FROM document_chunks")
                    stats["chunks_in_db"] = cur.fetchone()[0]
                    
                    cur.execute("SELECT COUNT(*) FROM document_chunks WHERE embedding IS NOT NULL")
                    stats["chunks_with_embeddings"] = cur.fetchone()[0]
                    
                    cur.execute("SELECT SUM(total_chars) FROM ingested_documents")
                    result = cur.fetchone()[0]
                    stats["total_chars"] = result if result else 0
                    
            except Exception as e:
                logger.warning(f"Could not get DB stats: {e}")
        
        return stats

    def close(self) -> None:
        self.stop_watching()
        if self._http_client:
            self._http_client.close()
        if self._db_conn:
            self._db_conn.close()

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()
        return False
