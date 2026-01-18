#!/usr/bin/env python3
"""
Embedding Service
Generates and manages embeddings for semantic search over mental models.
"""

import os
import sys
from typing import List, Dict, Any, Optional, Tuple
from dataclasses import dataclass
import numpy as np
import psycopg2
from psycopg2.extras import execute_values

sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
from config.settings import settings
from .providers import LLMProvider, get_provider, ChatMessage


@dataclass
class SearchResult:
    """Semantic search result."""
    id: int
    name: str
    content: str
    similarity: float
    source_type: str  # "model", "principle", "case"
    metadata: Dict[str, Any]


class EmbeddingService:
    """Service for generating and searching embeddings."""
    
    EMBEDDING_DIM = 1536  # Default for OpenAI ada-002, adjust for other models
    
    def __init__(self, provider: Optional[LLMProvider] = None):
        self.provider = provider
        self.conn = None
    
    def connect(self):
        """Connect to database."""
        self.conn = psycopg2.connect(
            dbname=settings.database.name,
            user=settings.database.user,
            host=settings.database.host,
            port=settings.database.port,
            password=settings.database.password or None
        )
    
    def close(self):
        """Close connections."""
        if self.conn:
            self.conn.close()
        if self.provider:
            self.provider.close()
    
    def setup_vector_tables(self):
        """Create tables for storing embeddings (requires pgvector extension)."""
        self.connect()
        cur = self.conn.cursor()
        
        try:
            cur.execute("CREATE EXTENSION IF NOT EXISTS vector")
            
            cur.execute(f"""
                CREATE TABLE IF NOT EXISTS model_embeddings (
                    id SERIAL PRIMARY KEY,
                    model_id INTEGER REFERENCES mental_models(id),
                    model_name VARCHAR(200),
                    content TEXT,
                    embedding vector({self.EMBEDDING_DIM}),
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                )
            """)
            
            cur.execute(f"""
                CREATE TABLE IF NOT EXISTS principle_embeddings (
                    id SERIAL PRIMARY KEY,
                    principle_id INTEGER REFERENCES thinker_principles(id),
                    thinker VARCHAR(100),
                    principle_name VARCHAR(200),
                    content TEXT,
                    embedding vector({self.EMBEDDING_DIM}),
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                )
            """)
            
            cur.execute(f"""
                CREATE TABLE IF NOT EXISTS case_embeddings (
                    id SERIAL PRIMARY KEY,
                    case_id INTEGER,
                    case_name VARCHAR(500),
                    content TEXT,
                    embedding vector({self.EMBEDDING_DIM}),
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                )
            """)
            
            cur.execute("""
                CREATE INDEX IF NOT EXISTS idx_model_embeddings_vector 
                ON model_embeddings USING ivfflat (embedding vector_cosine_ops)
            """)
            cur.execute("""
                CREATE INDEX IF NOT EXISTS idx_principle_embeddings_vector 
                ON principle_embeddings USING ivfflat (embedding vector_cosine_ops)
            """)
            cur.execute("""
                CREATE INDEX IF NOT EXISTS idx_case_embeddings_vector 
                ON case_embeddings USING ivfflat (embedding vector_cosine_ops)
            """)
            
            self.conn.commit()
            print("Vector tables created successfully.")
            
        except Exception as e:
            self.conn.rollback()
            print(f"Note: pgvector extension may not be available. Error: {e}")
            print("Falling back to non-vector search.")
        finally:
            cur.close()
            self.close()
    
    def _generate_embedding(self, text: str) -> List[float]:
        """Generate embedding for text using the LLM provider."""
        if self.provider:
            return self.provider.embed(text)
        else:
            return self._simple_embedding(text)
    
    def _simple_embedding(self, text: str, dim: int = 1536) -> List[float]:
        """
        Simple TF-IDF-like embedding fallback when no LLM provider is available.
        This is a basic implementation for testing without an LLM.
        """
        import hashlib
        
        words = text.lower().split()
        embedding = [0.0] * dim
        
        for i, word in enumerate(words):
            hash_val = int(hashlib.md5(word.encode()).hexdigest(), 16)
            indices = [(hash_val >> (j * 8)) % dim for j in range(4)]
            for idx in indices:
                embedding[idx] += 1.0 / (1 + i * 0.1)
        
        norm = np.linalg.norm(embedding)
        if norm > 0:
            embedding = [x / norm for x in embedding]
        
        return embedding
    
    def embed_mental_models(self, batch_size: int = 100):
        """Generate embeddings for all mental models."""
        self.connect()
        cur = self.conn.cursor()
        
        try:
            cur.execute("""
                SELECT id, name, category, description, originator, modern_synthesizer
                FROM mental_models
            """)
            models = cur.fetchall()
            
            for model in models:
                model_id, name, category, description, originator, synthesizer = model
                
                content = f"{name}. Category: {category}. {description}. "
                content += f"Originated by {originator}, synthesized by {synthesizer}."
                
                embedding = self._generate_embedding(content)
                
                cur.execute("""
                    INSERT INTO model_embeddings (model_id, model_name, content, embedding)
                    VALUES (%s, %s, %s, %s)
                    ON CONFLICT DO NOTHING
                """, (model_id, name, content, embedding))
            
            self.conn.commit()
            print(f"Embedded {len(models)} mental models.")
            
        finally:
            cur.close()
            self.close()
    
    def embed_principles(self, batch_size: int = 100):
        """Generate embeddings for all thinker principles."""
        self.connect()
        cur = self.conn.cursor()
        
        try:
            cur.execute("""
                SELECT id, thinker, principle_name, principle_description, category
                FROM thinker_principles
            """)
            principles = cur.fetchall()
            
            for principle in principles:
                p_id, thinker, name, description, category = principle
                
                content = f"{thinker}'s principle: {name}. {description}. Category: {category}."
                
                embedding = self._generate_embedding(content)
                
                cur.execute("""
                    INSERT INTO principle_embeddings 
                    (principle_id, thinker, principle_name, content, embedding)
                    VALUES (%s, %s, %s, %s, %s)
                    ON CONFLICT DO NOTHING
                """, (p_id, thinker, name, content, embedding))
            
            self.conn.commit()
            print(f"Embedded {len(principles)} principles.")
            
        finally:
            cur.close()
            self.close()
    
    def semantic_search(
        self, 
        query: str, 
        source_types: List[str] = None,
        limit: int = 10
    ) -> List[SearchResult]:
        """
        Perform semantic search across mental models, principles, and cases.
        
        Args:
            query: Natural language search query
            source_types: List of types to search ("model", "principle", "case")
            limit: Maximum results to return
        
        Returns:
            List of SearchResult objects sorted by similarity
        """
        source_types = source_types or ["model", "principle"]
        query_embedding = self._generate_embedding(query)
        
        self.connect()
        cur = self.conn.cursor()
        results = []
        
        try:
            if "model" in source_types:
                cur.execute("""
                    SELECT model_id, model_name, content, 
                           1 - (embedding <=> %s::vector) as similarity
                    FROM model_embeddings
                    ORDER BY embedding <=> %s::vector
                    LIMIT %s
                """, (query_embedding, query_embedding, limit))
                
                for row in cur.fetchall():
                    results.append(SearchResult(
                        id=row[0],
                        name=row[1],
                        content=row[2],
                        similarity=row[3],
                        source_type="model",
                        metadata={}
                    ))
            
            if "principle" in source_types:
                cur.execute("""
                    SELECT principle_id, principle_name, content, thinker,
                           1 - (embedding <=> %s::vector) as similarity
                    FROM principle_embeddings
                    ORDER BY embedding <=> %s::vector
                    LIMIT %s
                """, (query_embedding, query_embedding, limit))
                
                for row in cur.fetchall():
                    results.append(SearchResult(
                        id=row[0],
                        name=row[1],
                        content=row[2],
                        similarity=row[4],
                        source_type="principle",
                        metadata={"thinker": row[3]}
                    ))
            
            results.sort(key=lambda x: x.similarity, reverse=True)
            return results[:limit]
            
        except Exception as e:
            print(f"Vector search failed, falling back to keyword search: {e}")
            return self._keyword_search(query, source_types, limit)
        finally:
            cur.close()
            self.close()
    
    def _keyword_search(
        self, 
        query: str, 
        source_types: List[str],
        limit: int
    ) -> List[SearchResult]:
        """Fallback keyword search when vector search is unavailable."""
        self.connect()
        cur = self.conn.cursor()
        results = []
        
        try:
            search_term = f"%{query}%"
            
            if "model" in source_types:
                cur.execute("""
                    SELECT id, name, description, category
                    FROM mental_models
                    WHERE name ILIKE %s OR description ILIKE %s OR category ILIKE %s
                    LIMIT %s
                """, (search_term, search_term, search_term, limit))
                
                for row in cur.fetchall():
                    results.append(SearchResult(
                        id=row[0],
                        name=row[1],
                        content=row[2] or "",
                        similarity=0.5,
                        source_type="model",
                        metadata={"category": row[3]}
                    ))
            
            if "principle" in source_types:
                cur.execute("""
                    SELECT id, principle_name, principle_description, thinker
                    FROM thinker_principles
                    WHERE principle_name ILIKE %s OR principle_description ILIKE %s OR thinker ILIKE %s
                    LIMIT %s
                """, (search_term, search_term, search_term, limit))
                
                for row in cur.fetchall():
                    results.append(SearchResult(
                        id=row[0],
                        name=row[1],
                        content=row[2] or "",
                        similarity=0.5,
                        source_type="principle",
                        metadata={"thinker": row[3]}
                    ))
            
            return results[:limit]
            
        finally:
            cur.close()
            self.close()


def setup_embeddings():
    """Set up embedding tables and generate initial embeddings."""
    service = EmbeddingService()
    service.setup_vector_tables()
    service.embed_mental_models()
    service.embed_principles()
    print("Embedding setup complete.")


if __name__ == "__main__":
    setup_embeddings()
