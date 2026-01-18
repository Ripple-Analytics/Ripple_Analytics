import os
import json
import logging
from dataclasses import dataclass, field
from typing import List, Optional, Dict, Any
from datetime import datetime
from enum import Enum

logger = logging.getLogger(__name__)


class ImprovementType(Enum):
    CODE_FIX = "code_fix"
    DOCUMENTATION = "documentation"
    NEW_FEATURE = "new_feature"
    REFACTOR = "refactor"
    TEST = "test"
    MENTAL_MODEL = "mental_model"
    INSIGHT = "insight"


class ImprovementStatus(Enum):
    PROPOSED = "proposed"
    APPROVED = "approved"
    REJECTED = "rejected"
    IMPLEMENTED = "implemented"
    VERIFIED = "verified"


@dataclass
class Insight:
    id: str
    query: str
    sources: List[Dict[str, Any]]
    synthesis: str
    mental_models_applied: List[str]
    confidence: float
    created_at: datetime = field(default_factory=datetime.now)
    metadata: dict = field(default_factory=dict)


@dataclass
class Improvement:
    id: str
    type: ImprovementType
    title: str
    description: str
    source_insights: List[str]
    proposed_changes: Dict[str, Any]
    status: ImprovementStatus = ImprovementStatus.PROPOSED
    created_at: datetime = field(default_factory=datetime.now)
    implemented_at: Optional[datetime] = None
    metadata: dict = field(default_factory=dict)


class FeedbackLoop:
    def __init__(
        self,
        db_connection_string: Optional[str] = None,
        lm_studio_url: str = "http://localhost:1234",
        auto_approve_threshold: float = 0.9,
    ):
        self.db_connection_string = db_connection_string or os.getenv(
            "DATABASE_URL", "postgresql://postgres:postgres@localhost:5432/mental_models"
        )
        self.lm_studio_url = lm_studio_url
        self.auto_approve_threshold = auto_approve_threshold
        self._db_conn = None
        self._http_client = None
        self._insights: Dict[str, Insight] = {}
        self._improvements: Dict[str, Improvement] = {}

    def _get_http_client(self):
        if self._http_client is None:
            import httpx
            self._http_client = httpx.Client(timeout=120.0)
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
            return False
        
        try:
            with conn.cursor() as cur:
                cur.execute("""
                    CREATE TABLE IF NOT EXISTS insights (
                        id TEXT PRIMARY KEY,
                        query TEXT NOT NULL,
                        sources JSONB DEFAULT '[]',
                        synthesis TEXT,
                        mental_models_applied JSONB DEFAULT '[]',
                        confidence FLOAT,
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                        metadata JSONB DEFAULT '{}'
                    )
                """)
                
                cur.execute("""
                    CREATE TABLE IF NOT EXISTS improvements (
                        id TEXT PRIMARY KEY,
                        type TEXT NOT NULL,
                        title TEXT NOT NULL,
                        description TEXT,
                        source_insights JSONB DEFAULT '[]',
                        proposed_changes JSONB DEFAULT '{}',
                        status TEXT DEFAULT 'proposed',
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                        implemented_at TIMESTAMP,
                        metadata JSONB DEFAULT '{}'
                    )
                """)
                
                cur.execute("""
                    CREATE TABLE IF NOT EXISTS feedback_queries (
                        id SERIAL PRIMARY KEY,
                        query TEXT NOT NULL,
                        response TEXT,
                        sources_used JSONB DEFAULT '[]',
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                    )
                """)
                
                conn.commit()
                logger.info("Feedback database setup complete")
                return True
                
        except Exception as e:
            logger.error(f"Feedback database setup failed: {e}")
            conn.rollback()
            return False

    def query_knowledge(
        self,
        query: str,
        context: Optional[str] = None,
        max_sources: int = 10,
    ) -> Dict[str, Any]:
        from .pipeline import DataIngestionPipeline
        
        pipeline = DataIngestionPipeline(
            db_connection_string=self.db_connection_string,
            lm_studio_url=self.lm_studio_url,
        )
        
        search_results = pipeline.semantic_search(query, limit=max_sources)
        
        if not search_results:
            return {
                "query": query,
                "answer": "No relevant information found in the knowledge base.",
                "sources": [],
                "confidence": 0.0,
            }
        
        context_text = "\n\n".join([
            f"Source {i+1} ({r['document_path']}):\n{r['content']}"
            for i, r in enumerate(search_results)
        ])
        
        system_prompt = """You are an AI assistant with access to a knowledge base. 
Answer the user's question based on the provided sources. 
Be specific and cite which sources support your answer.
If the sources don't contain relevant information, say so."""
        
        user_prompt = f"""Question: {query}

{f'Additional context: {context}' if context else ''}

Sources:
{context_text}

Please provide a comprehensive answer based on these sources."""
        
        try:
            client = self._get_http_client()
            response = client.post(
                f"{self.lm_studio_url}/v1/chat/completions",
                json={
                    "model": "local-model",
                    "messages": [
                        {"role": "system", "content": system_prompt},
                        {"role": "user", "content": user_prompt},
                    ],
                    "temperature": 0.3,
                    "max_tokens": 2000,
                },
            )
            
            if response.status_code == 200:
                data = response.json()
                answer = data["choices"][0]["message"]["content"]
                
                avg_similarity = sum(r.get("similarity", 0) for r in search_results) / len(search_results)
                
                self._log_query(query, answer, search_results)
                
                return {
                    "query": query,
                    "answer": answer,
                    "sources": search_results,
                    "confidence": avg_similarity,
                }
            else:
                logger.warning(f"LLM request failed: {response.status_code}")
                
        except Exception as e:
            logger.error(f"Query failed: {e}")
        
        return {
            "query": query,
            "answer": f"Found {len(search_results)} relevant sources but could not generate synthesis (LM Studio may not be running).",
            "sources": search_results,
            "confidence": 0.5,
        }

    def _log_query(self, query: str, response: str, sources: List[Dict]) -> None:
        conn = self._get_db_connection()
        if not conn:
            return
        
        try:
            with conn.cursor() as cur:
                cur.execute("""
                    INSERT INTO feedback_queries (query, response, sources_used)
                    VALUES (%s, %s, %s)
                """, (query, response, json.dumps(sources)))
                conn.commit()
        except Exception as e:
            logger.warning(f"Failed to log query: {e}")
            conn.rollback()

    def propose_improvement(
        self,
        improvement_type: ImprovementType,
        title: str,
        description: str,
        proposed_changes: Dict[str, Any],
        source_insights: Optional[List[str]] = None,
        metadata: Optional[Dict] = None,
    ) -> Improvement:
        import hashlib
        
        improvement_id = hashlib.sha256(
            f"{title}:{datetime.now().isoformat()}".encode()
        ).hexdigest()[:16]
        
        improvement = Improvement(
            id=improvement_id,
            type=improvement_type,
            title=title,
            description=description,
            source_insights=source_insights or [],
            proposed_changes=proposed_changes,
            metadata=metadata or {},
        )
        
        self._improvements[improvement_id] = improvement
        self._store_improvement(improvement)
        
        logger.info(f"Proposed improvement: {title} ({improvement_id})")
        return improvement

    def _store_improvement(self, improvement: Improvement) -> bool:
        conn = self._get_db_connection()
        if not conn:
            return False
        
        try:
            with conn.cursor() as cur:
                cur.execute("""
                    INSERT INTO improvements 
                    (id, type, title, description, source_insights, proposed_changes, status, metadata)
                    VALUES (%s, %s, %s, %s, %s, %s, %s, %s)
                    ON CONFLICT (id) DO UPDATE SET
                        status = EXCLUDED.status,
                        implemented_at = EXCLUDED.implemented_at
                """, (
                    improvement.id,
                    improvement.type.value,
                    improvement.title,
                    improvement.description,
                    json.dumps(improvement.source_insights),
                    json.dumps(improvement.proposed_changes),
                    improvement.status.value,
                    json.dumps(improvement.metadata),
                ))
                conn.commit()
                return True
        except Exception as e:
            logger.error(f"Failed to store improvement: {e}")
            conn.rollback()
            return False

    def get_pending_improvements(self) -> List[Improvement]:
        conn = self._get_db_connection()
        if not conn:
            return list(self._improvements.values())
        
        try:
            with conn.cursor() as cur:
                cur.execute("""
                    SELECT id, type, title, description, source_insights, 
                           proposed_changes, status, created_at, implemented_at, metadata
                    FROM improvements
                    WHERE status = 'proposed'
                    ORDER BY created_at DESC
                """)
                
                improvements = []
                for row in cur.fetchall():
                    improvements.append(Improvement(
                        id=row[0],
                        type=ImprovementType(row[1]),
                        title=row[2],
                        description=row[3],
                        source_insights=row[4] or [],
                        proposed_changes=row[5] or {},
                        status=ImprovementStatus(row[6]),
                        created_at=row[7],
                        implemented_at=row[8],
                        metadata=row[9] or {},
                    ))
                
                return improvements
                
        except Exception as e:
            logger.error(f"Failed to get improvements: {e}")
            return []

    def approve_improvement(self, improvement_id: str) -> bool:
        conn = self._get_db_connection()
        if not conn:
            if improvement_id in self._improvements:
                self._improvements[improvement_id].status = ImprovementStatus.APPROVED
                return True
            return False
        
        try:
            with conn.cursor() as cur:
                cur.execute("""
                    UPDATE improvements SET status = 'approved'
                    WHERE id = %s AND status = 'proposed'
                """, (improvement_id,))
                conn.commit()
                
                if improvement_id in self._improvements:
                    self._improvements[improvement_id].status = ImprovementStatus.APPROVED
                
                return cur.rowcount > 0
                
        except Exception as e:
            logger.error(f"Failed to approve improvement: {e}")
            conn.rollback()
            return False

    def reject_improvement(self, improvement_id: str, reason: Optional[str] = None) -> bool:
        conn = self._get_db_connection()
        if not conn:
            if improvement_id in self._improvements:
                self._improvements[improvement_id].status = ImprovementStatus.REJECTED
                return True
            return False
        
        try:
            with conn.cursor() as cur:
                metadata_update = json.dumps({"rejection_reason": reason}) if reason else "{}"
                cur.execute("""
                    UPDATE improvements 
                    SET status = 'rejected', metadata = metadata || %s::jsonb
                    WHERE id = %s AND status = 'proposed'
                """, (metadata_update, improvement_id))
                conn.commit()
                
                if improvement_id in self._improvements:
                    self._improvements[improvement_id].status = ImprovementStatus.REJECTED
                
                return cur.rowcount > 0
                
        except Exception as e:
            logger.error(f"Failed to reject improvement: {e}")
            conn.rollback()
            return False

    def mark_implemented(self, improvement_id: str) -> bool:
        conn = self._get_db_connection()
        if not conn:
            if improvement_id in self._improvements:
                self._improvements[improvement_id].status = ImprovementStatus.IMPLEMENTED
                self._improvements[improvement_id].implemented_at = datetime.now()
                return True
            return False
        
        try:
            with conn.cursor() as cur:
                cur.execute("""
                    UPDATE improvements 
                    SET status = 'implemented', implemented_at = CURRENT_TIMESTAMP
                    WHERE id = %s AND status = 'approved'
                """, (improvement_id,))
                conn.commit()
                
                if improvement_id in self._improvements:
                    self._improvements[improvement_id].status = ImprovementStatus.IMPLEMENTED
                    self._improvements[improvement_id].implemented_at = datetime.now()
                
                return cur.rowcount > 0
                
        except Exception as e:
            logger.error(f"Failed to mark implemented: {e}")
            conn.rollback()
            return False

    def analyze_for_improvements(self, topic: Optional[str] = None) -> List[Dict[str, Any]]:
        queries = [
            "What patterns or insights emerge from the data that could improve decision-making?",
            "What mental models are most frequently applicable based on the documents?",
            "What gaps or missing information should be addressed?",
            "What contradictions or inconsistencies exist in the knowledge base?",
        ]
        
        if topic:
            queries.append(f"What specific improvements could be made regarding {topic}?")
        
        suggestions = []
        for query in queries:
            result = self.query_knowledge(query)
            if result["confidence"] > 0.3:
                suggestions.append({
                    "query": query,
                    "insight": result["answer"],
                    "confidence": result["confidence"],
                    "source_count": len(result["sources"]),
                })
        
        return suggestions

    def get_stats(self) -> Dict[str, Any]:
        conn = self._get_db_connection()
        stats = {
            "improvements_in_memory": len(self._improvements),
            "insights_in_memory": len(self._insights),
        }
        
        if conn:
            try:
                with conn.cursor() as cur:
                    cur.execute("""
                        SELECT status, COUNT(*) FROM improvements GROUP BY status
                    """)
                    stats["improvements_by_status"] = {
                        row[0]: row[1] for row in cur.fetchall()
                    }
                    
                    cur.execute("SELECT COUNT(*) FROM feedback_queries")
                    stats["total_queries"] = cur.fetchone()[0]
                    
                    cur.execute("SELECT COUNT(*) FROM insights")
                    stats["total_insights"] = cur.fetchone()[0]
                    
            except Exception as e:
                logger.warning(f"Could not get feedback stats: {e}")
        
        return stats

    def close(self) -> None:
        if self._http_client:
            self._http_client.close()
        if self._db_conn:
            self._db_conn.close()

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()
        return False
