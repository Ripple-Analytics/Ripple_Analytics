import os
import json
import logging
from dataclasses import dataclass, field
from typing import List, Dict, Any, Optional
from datetime import datetime
from enum import Enum

logger = logging.getLogger(__name__)


class AnalysisType(Enum):
    CLASSIFY = "classify"
    DETECT_BIAS = "detect_bias"
    FIND_PATTERNS = "find_patterns"
    LOLLAPALOOZA = "lollapalooza"
    INVERT = "invert"
    FULL = "full"


@dataclass
class ModelMatch:
    model_id: int
    model_name: str
    relevance_score: float
    explanation: str
    evidence: List[str]
    category: str


@dataclass
class BiasDetection:
    bias_name: str
    confidence: float
    evidence: str
    mitigation: str


@dataclass
class DocumentAnalysis:
    document_id: str
    document_path: str
    chunk_id: Optional[str]
    content_preview: str
    applicable_models: List[ModelMatch]
    detected_biases: List[BiasDetection]
    patterns: List[str]
    lollapalooza_score: float
    lollapalooza_models: List[str]
    key_insights: List[str]
    inverted_perspective: str
    analyzed_at: datetime = field(default_factory=datetime.now)
    metadata: dict = field(default_factory=dict)


class MentalModelAnalyzer:
    def __init__(
        self,
        lm_studio_url: str = "http://localhost:1234",
        db_connection_string: Optional[str] = None,
        model_name: str = "local-model",
        temperature: float = 0.3,
    ):
        self.lm_studio_url = lm_studio_url
        self.db_connection_string = db_connection_string or os.getenv(
            "DATABASE_URL", "postgresql://postgres:postgres@localhost:5432/mental_models"
        )
        self.model_name = model_name
        self.temperature = temperature
        self._http_client = None
        self._db_conn = None
        self._mental_models_cache: List[Dict] = []

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

    def _load_mental_models(self) -> List[Dict]:
        if self._mental_models_cache:
            return self._mental_models_cache
        
        models_path = os.path.join(
            os.path.dirname(os.path.dirname(os.path.dirname(__file__))),
            "data", "raw", "mental_models_complete.json"
        )
        
        try:
            with open(models_path, "r") as f:
                data = json.load(f)
                self._mental_models_cache = data.get("mental_models", [])
                return self._mental_models_cache
        except Exception as e:
            logger.error(f"Failed to load mental models: {e}")
            return []

    def _get_models_summary(self) -> str:
        models = self._load_mental_models()
        
        categories = {}
        for model in models:
            cat_id = model.get("category_id", 0)
            if cat_id not in categories:
                categories[cat_id] = []
            categories[cat_id].append(f"- {model['name']}: {model['description'][:100]}...")
        
        summary = "MENTAL MODELS BY CATEGORY:\n\n"
        category_names = {
            1: "Psychology: Tendencies & Biases",
            2: "Thinking Tools & Meta-Frameworks",
            3: "Economics & Business",
            4: "Competitive Advantage & Moats",
            5: "Mathematics & Probability",
            6: "Physics, Engineering & Systems",
            7: "Biology & Evolution",
            8: "Organizational & Institutional",
        }
        
        for cat_id, model_list in sorted(categories.items()):
            cat_name = category_names.get(cat_id, f"Category {cat_id}")
            summary += f"\n{cat_name}:\n"
            summary += "\n".join(model_list[:10])
            if len(model_list) > 10:
                summary += f"\n... and {len(model_list) - 10} more"
            summary += "\n"
        
        return summary

    def _call_llm(self, system_prompt: str, user_prompt: str) -> Optional[str]:
        try:
            client = self._get_http_client()
            response = client.post(
                f"{self.lm_studio_url}/v1/chat/completions",
                json={
                    "model": self.model_name,
                    "messages": [
                        {"role": "system", "content": system_prompt},
                        {"role": "user", "content": user_prompt},
                    ],
                    "temperature": self.temperature,
                    "max_tokens": 4000,
                },
            )
            
            if response.status_code == 200:
                data = response.json()
                return data["choices"][0]["message"]["content"]
            else:
                logger.warning(f"LLM request failed: {response.status_code}")
                return None
                
        except Exception as e:
            logger.error(f"LLM call failed: {e}")
            return None

    def analyze_document(
        self,
        content: str,
        document_id: str = "",
        document_path: str = "",
        chunk_id: Optional[str] = None,
        analysis_type: AnalysisType = AnalysisType.FULL,
    ) -> DocumentAnalysis:
        models_summary = self._get_models_summary()
        
        system_prompt = f"""You are an expert analyst trained in Charlie Munger's latticework of mental models.
Your task is to analyze text through the lens of these mental models and identify which ones apply.

{models_summary}

When analyzing, you should:
1. Identify which mental models are most relevant to the content
2. Detect any cognitive biases present in the reasoning
3. Look for patterns and connections
4. Identify potential lollapalooza effects (multiple models combining)
5. Apply inversion - consider what could go wrong

Respond in JSON format with the following structure:
{{
    "applicable_models": [
        {{"model_name": "...", "relevance_score": 0.0-1.0, "explanation": "...", "evidence": ["quote1", "quote2"], "category": "..."}}
    ],
    "detected_biases": [
        {{"bias_name": "...", "confidence": 0.0-1.0, "evidence": "...", "mitigation": "..."}}
    ],
    "patterns": ["pattern1", "pattern2"],
    "lollapalooza": {{
        "score": 0.0-1.0,
        "models": ["model1", "model2"],
        "explanation": "..."
    }},
    "key_insights": ["insight1", "insight2"],
    "inverted_perspective": "What could go wrong or what's being missed..."
}}"""

        user_prompt = f"""Analyze the following text using mental models:

---
{content[:4000]}
---

Provide a comprehensive analysis identifying applicable mental models, biases, patterns, and insights."""

        response = self._call_llm(system_prompt, user_prompt)
        
        if response:
            try:
                json_start = response.find("{")
                json_end = response.rfind("}") + 1
                if json_start >= 0 and json_end > json_start:
                    analysis_data = json.loads(response[json_start:json_end])
                    return self._parse_analysis(
                        analysis_data, content, document_id, document_path, chunk_id
                    )
            except json.JSONDecodeError as e:
                logger.warning(f"Failed to parse LLM response as JSON: {e}")
        
        return self._create_fallback_analysis(content, document_id, document_path, chunk_id)

    def _parse_analysis(
        self,
        data: Dict,
        content: str,
        document_id: str,
        document_path: str,
        chunk_id: Optional[str],
    ) -> DocumentAnalysis:
        applicable_models = []
        for model_data in data.get("applicable_models", []):
            applicable_models.append(ModelMatch(
                model_id=0,
                model_name=model_data.get("model_name", "Unknown"),
                relevance_score=float(model_data.get("relevance_score", 0.5)),
                explanation=model_data.get("explanation", ""),
                evidence=model_data.get("evidence", []),
                category=model_data.get("category", ""),
            ))
        
        detected_biases = []
        for bias_data in data.get("detected_biases", []):
            detected_biases.append(BiasDetection(
                bias_name=bias_data.get("bias_name", "Unknown"),
                confidence=float(bias_data.get("confidence", 0.5)),
                evidence=bias_data.get("evidence", ""),
                mitigation=bias_data.get("mitigation", ""),
            ))
        
        lollapalooza = data.get("lollapalooza", {})
        
        return DocumentAnalysis(
            document_id=document_id,
            document_path=document_path,
            chunk_id=chunk_id,
            content_preview=content[:200] + "..." if len(content) > 200 else content,
            applicable_models=applicable_models,
            detected_biases=detected_biases,
            patterns=data.get("patterns", []),
            lollapalooza_score=float(lollapalooza.get("score", 0)),
            lollapalooza_models=lollapalooza.get("models", []),
            key_insights=data.get("key_insights", []),
            inverted_perspective=data.get("inverted_perspective", ""),
        )

    def _create_fallback_analysis(
        self,
        content: str,
        document_id: str,
        document_path: str,
        chunk_id: Optional[str],
    ) -> DocumentAnalysis:
        return DocumentAnalysis(
            document_id=document_id,
            document_path=document_path,
            chunk_id=chunk_id,
            content_preview=content[:200] + "..." if len(content) > 200 else content,
            applicable_models=[],
            detected_biases=[],
            patterns=[],
            lollapalooza_score=0.0,
            lollapalooza_models=[],
            key_insights=["Analysis unavailable - LM Studio may not be running"],
            inverted_perspective="",
        )

    def classify_by_model(
        self,
        content: str,
        target_models: Optional[List[str]] = None,
    ) -> List[ModelMatch]:
        if target_models:
            models_text = ", ".join(target_models)
            prompt = f"Focus on these models: {models_text}"
        else:
            prompt = "Identify all applicable mental models"
        
        system_prompt = f"""You are a mental models classifier. {prompt}

Return JSON array of matches:
[{{"model_name": "...", "relevance_score": 0.0-1.0, "explanation": "...", "evidence": ["..."], "category": "..."}}]"""

        response = self._call_llm(system_prompt, f"Classify this text:\n\n{content[:3000]}")
        
        if response:
            try:
                json_start = response.find("[")
                json_end = response.rfind("]") + 1
                if json_start >= 0 and json_end > json_start:
                    matches_data = json.loads(response[json_start:json_end])
                    return [
                        ModelMatch(
                            model_id=0,
                            model_name=m.get("model_name", ""),
                            relevance_score=float(m.get("relevance_score", 0.5)),
                            explanation=m.get("explanation", ""),
                            evidence=m.get("evidence", []),
                            category=m.get("category", ""),
                        )
                        for m in matches_data
                    ]
            except json.JSONDecodeError:
                pass
        
        return []

    def detect_biases(self, content: str) -> List[BiasDetection]:
        system_prompt = """You are a cognitive bias detector trained in Munger's 25 causes of human misjudgment.

Analyze text for these biases:
- Confirmation Bias, Anchoring, Availability, Social Proof
- Loss Aversion, Overconfidence, Hindsight Bias
- Authority Bias, Reciprocation, Liking/Loving Tendency
- And all other Munger biases

Return JSON array:
[{"bias_name": "...", "confidence": 0.0-1.0, "evidence": "...", "mitigation": "..."}]"""

        response = self._call_llm(system_prompt, f"Detect biases in:\n\n{content[:3000]}")
        
        if response:
            try:
                json_start = response.find("[")
                json_end = response.rfind("]") + 1
                if json_start >= 0 and json_end > json_start:
                    biases_data = json.loads(response[json_start:json_end])
                    return [
                        BiasDetection(
                            bias_name=b.get("bias_name", ""),
                            confidence=float(b.get("confidence", 0.5)),
                            evidence=b.get("evidence", ""),
                            mitigation=b.get("mitigation", ""),
                        )
                        for b in biases_data
                    ]
            except json.JSONDecodeError:
                pass
        
        return []

    def find_lollapalooza(self, content: str) -> Dict[str, Any]:
        system_prompt = """You are an expert in identifying Lollapalooza Effects - when multiple mental models combine to create extreme outcomes.

Analyze text for:
1. Multiple interacting mental models
2. Reinforcing feedback loops
3. Extreme or unusual outcomes
4. Confluence of psychological tendencies

Return JSON:
{
    "score": 0.0-1.0,
    "models": ["model1", "model2", ...],
    "interactions": [{"model1": "...", "model2": "...", "interaction": "..."}],
    "explanation": "...",
    "historical_parallels": ["..."]
}"""

        response = self._call_llm(system_prompt, f"Find lollapalooza effects in:\n\n{content[:3000]}")
        
        if response:
            try:
                json_start = response.find("{")
                json_end = response.rfind("}") + 1
                if json_start >= 0 and json_end > json_start:
                    return json.loads(response[json_start:json_end])
            except json.JSONDecodeError:
                pass
        
        return {"score": 0, "models": [], "interactions": [], "explanation": "", "historical_parallels": []}

    def invert_analysis(self, content: str, question: str = "") -> str:
        system_prompt = """You are trained in Munger's inversion technique: "Invert, always invert."

When analyzing, consider:
1. What could go wrong?
2. What's being ignored or missed?
3. What would the opposite perspective reveal?
4. What are the second and third-order effects?
5. What would make this fail?

Provide a thorough inverted analysis."""

        prompt = f"Apply inversion to this content"
        if question:
            prompt += f" with focus on: {question}"
        prompt += f"\n\n{content[:3000]}"

        response = self._call_llm(system_prompt, prompt)
        return response or "Inversion analysis unavailable"

    def batch_analyze(
        self,
        documents: List[Dict[str, Any]],
        analysis_type: AnalysisType = AnalysisType.CLASSIFY,
    ) -> List[DocumentAnalysis]:
        results = []
        
        for doc in documents:
            content = doc.get("content", "")
            doc_id = doc.get("id", "")
            doc_path = doc.get("path", "")
            chunk_id = doc.get("chunk_id")
            
            if analysis_type == AnalysisType.FULL:
                analysis = self.analyze_document(content, doc_id, doc_path, chunk_id)
            elif analysis_type == AnalysisType.CLASSIFY:
                models = self.classify_by_model(content)
                analysis = DocumentAnalysis(
                    document_id=doc_id,
                    document_path=doc_path,
                    chunk_id=chunk_id,
                    content_preview=content[:200],
                    applicable_models=models,
                    detected_biases=[],
                    patterns=[],
                    lollapalooza_score=0,
                    lollapalooza_models=[],
                    key_insights=[],
                    inverted_perspective="",
                )
            elif analysis_type == AnalysisType.DETECT_BIAS:
                biases = self.detect_biases(content)
                analysis = DocumentAnalysis(
                    document_id=doc_id,
                    document_path=doc_path,
                    chunk_id=chunk_id,
                    content_preview=content[:200],
                    applicable_models=[],
                    detected_biases=biases,
                    patterns=[],
                    lollapalooza_score=0,
                    lollapalooza_models=[],
                    key_insights=[],
                    inverted_perspective="",
                )
            else:
                analysis = self.analyze_document(content, doc_id, doc_path, chunk_id)
            
            results.append(analysis)
        
        return results

    def store_analysis(self, analysis: DocumentAnalysis) -> bool:
        conn = self._get_db_connection()
        if not conn:
            return False
        
        try:
            with conn.cursor() as cur:
                cur.execute("""
                    CREATE TABLE IF NOT EXISTS document_analyses (
                        id SERIAL PRIMARY KEY,
                        document_id TEXT,
                        document_path TEXT,
                        chunk_id TEXT,
                        content_preview TEXT,
                        applicable_models JSONB,
                        detected_biases JSONB,
                        patterns JSONB,
                        lollapalooza_score FLOAT,
                        lollapalooza_models JSONB,
                        key_insights JSONB,
                        inverted_perspective TEXT,
                        analyzed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                        metadata JSONB DEFAULT '{}'
                    )
                """)
                
                models_json = json.dumps([
                    {
                        "model_name": m.model_name,
                        "relevance_score": m.relevance_score,
                        "explanation": m.explanation,
                        "evidence": m.evidence,
                        "category": m.category,
                    }
                    for m in analysis.applicable_models
                ])
                
                biases_json = json.dumps([
                    {
                        "bias_name": b.bias_name,
                        "confidence": b.confidence,
                        "evidence": b.evidence,
                        "mitigation": b.mitigation,
                    }
                    for b in analysis.detected_biases
                ])
                
                cur.execute("""
                    INSERT INTO document_analyses 
                    (document_id, document_path, chunk_id, content_preview, 
                     applicable_models, detected_biases, patterns,
                     lollapalooza_score, lollapalooza_models, key_insights, inverted_perspective)
                    VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
                """, (
                    analysis.document_id,
                    analysis.document_path,
                    analysis.chunk_id,
                    analysis.content_preview,
                    models_json,
                    biases_json,
                    json.dumps(analysis.patterns),
                    analysis.lollapalooza_score,
                    json.dumps(analysis.lollapalooza_models),
                    json.dumps(analysis.key_insights),
                    analysis.inverted_perspective,
                ))
                
                conn.commit()
                return True
                
        except Exception as e:
            logger.error(f"Failed to store analysis: {e}")
            conn.rollback()
            return False

    def query_by_model(
        self,
        model_name: str,
        min_relevance: float = 0.5,
        limit: int = 50,
    ) -> List[Dict[str, Any]]:
        conn = self._get_db_connection()
        if not conn:
            return []
        
        try:
            with conn.cursor() as cur:
                cur.execute("""
                    SELECT document_id, document_path, content_preview, applicable_models, key_insights
                    FROM document_analyses
                    WHERE EXISTS (
                        SELECT 1 FROM jsonb_array_elements(applicable_models) AS m
                        WHERE m->>'model_name' ILIKE %s
                        AND (m->>'relevance_score')::float >= %s
                    )
                    ORDER BY analyzed_at DESC
                    LIMIT %s
                """, (f"%{model_name}%", min_relevance, limit))
                
                results = []
                for row in cur.fetchall():
                    results.append({
                        "document_id": row[0],
                        "document_path": row[1],
                        "content_preview": row[2],
                        "applicable_models": row[3],
                        "key_insights": row[4],
                    })
                
                return results
                
        except Exception as e:
            logger.error(f"Query failed: {e}")
            return []

    def get_analysis_stats(self) -> Dict[str, Any]:
        conn = self._get_db_connection()
        if not conn:
            return {}
        
        try:
            with conn.cursor() as cur:
                cur.execute("SELECT COUNT(*) FROM document_analyses")
                total_analyses = cur.fetchone()[0]
                
                cur.execute("""
                    SELECT m->>'model_name' as model_name, COUNT(*) as count
                    FROM document_analyses, jsonb_array_elements(applicable_models) AS m
                    GROUP BY m->>'model_name'
                    ORDER BY count DESC
                    LIMIT 20
                """)
                top_models = [{"model": row[0], "count": row[1]} for row in cur.fetchall()]
                
                cur.execute("""
                    SELECT AVG(lollapalooza_score) FROM document_analyses
                    WHERE lollapalooza_score > 0
                """)
                avg_lollapalooza = cur.fetchone()[0] or 0
                
                return {
                    "total_analyses": total_analyses,
                    "top_models": top_models,
                    "avg_lollapalooza_score": float(avg_lollapalooza),
                }
                
        except Exception as e:
            logger.error(f"Stats query failed: {e}")
            return {}

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
