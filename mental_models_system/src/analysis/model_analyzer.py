"""
Mental Model Document Analyzer

Uses your locally-run LLM to analyze documents through the lens of all 129 mental models.

Capabilities:
1. Identify which mental models apply to each document/chunk
2. Score relevance of each model (0-1)
3. Extract model-specific insights and examples
4. Detect Lollapalooza patterns (multiple models converging)
5. Auto-tag and categorize documents
6. Build a searchable knowledge graph

Architecture:
┌─────────────────────────────────────────────────────────────────────────────┐
│                    MENTAL MODEL DOCUMENT ANALYZER                            │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌──────────────┐                                                           │
│  │   Document   │                                                           │
│  │    Input     │                                                           │
│  └──────┬───────┘                                                           │
│         │                                                                    │
│         ▼                                                                    │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │                    Mental Model Lens                                 │   │
│  │  ┌─────────────────────────────────────────────────────────────┐   │   │
│  │  │  129 Mental Models (loaded from mental_models_complete.json) │   │   │
│  │  │  - Psychology (34)      - Moats (19)                        │   │   │
│  │  │  - Thinking Tools (18)  - Math (12)                         │   │   │
│  │  │  - Economics (20)       - Physics (11)                      │   │   │
│  │  │  - Biology (6)          - Organizational (9)                │   │   │
│  │  └─────────────────────────────────────────────────────────────┘   │   │
│  └─────────────────────────────────────────────────────────────────────┘   │
│         │                                                                    │
│         ▼                                                                    │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │                    Local LLM Analysis                                │   │
│  │  For each chunk:                                                     │   │
│  │  1. Which models apply? (multi-label classification)                │   │
│  │  2. How strongly? (relevance score 0-1)                             │   │
│  │  3. What insights? (model-specific extraction)                      │   │
│  │  4. Lollapalooza? (detect model convergence)                        │   │
│  └─────────────────────────────────────────────────────────────────────┘   │
│         │                                                                    │
│         ▼                                                                    │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │                    Output                                            │   │
│  │  - Document tags (which models apply)                               │   │
│  │  - Model relevance scores                                           │   │
│  │  - Extracted insights per model                                     │   │
│  │  - Lollapalooza alerts                                              │   │
│  │  - Searchable index                                                 │   │
│  └─────────────────────────────────────────────────────────────────────┘   │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
"""

import os
import json
import asyncio
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Any, Tuple
from datetime import datetime
from enum import Enum
from pathlib import Path
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# =============================================================================
# DATA STRUCTURES
# =============================================================================

@dataclass
class MentalModel:
    """A mental model from the latticework."""
    id: str
    name: str
    category: str
    description: str
    how_to_apply: str
    keywords: List[str] = field(default_factory=list)
    related_models: List[str] = field(default_factory=list)
    
    @classmethod
    def from_dict(cls, data: Dict) -> 'MentalModel':
        return cls(
            id=data.get("id", ""),
            name=data.get("name", ""),
            category=data.get("category", ""),
            description=data.get("description", ""),
            how_to_apply=data.get("how_to_apply", ""),
            keywords=data.get("keywords", []),
            related_models=data.get("related_models", [])
        )
    
    def to_dict(self) -> Dict:
        """Convert to dictionary for backward compatibility."""
        return {
            "id": self.id,
            "name": self.name,
            "category": self.category,
            "description": self.description,
            "how_to_apply": self.how_to_apply,
            "keywords": self.keywords,
            "related_models": self.related_models
        }
    
    def __getitem__(self, key: str):
        """Allow dictionary-style access for backward compatibility."""
        return getattr(self, key)


@dataclass
class ModelMatch:
    """A match between a document and a mental model."""
    model_id: str
    model_name: str
    relevance_score: float  # 0-1
    confidence: float  # 0-1
    evidence: str  # Quote or explanation from document
    insights: List[str] = field(default_factory=list)
    
    def to_dict(self) -> Dict:
        return {
            "model_id": self.model_id,
            "model_name": self.model_name,
            "relevance_score": self.relevance_score,
            "confidence": self.confidence,
            "evidence": self.evidence,
            "insights": self.insights
        }


@dataclass
class LollapaloozaAlert:
    """Alert when multiple models converge (Lollapalooza effect)."""
    models: List[str]  # List of model names
    convergence_score: float  # 0-1, higher = stronger convergence
    description: str
    implications: List[str]
    source_text: str
    
    def to_dict(self) -> Dict:
        return {
            "models": self.models,
            "num_models": len(self.models),
            "convergence_score": self.convergence_score,
            "description": self.description,
            "implications": self.implications,
            "source_text": self.source_text[:500]
        }


@dataclass
class DocumentAnalysis:
    """Complete analysis of a document through mental model lens."""
    document_path: str
    document_name: str
    analyzed_at: datetime
    
    # Model matches
    model_matches: List[ModelMatch] = field(default_factory=list)
    
    # Lollapalooza detection
    lollapalooza_alerts: List[LollapaloozaAlert] = field(default_factory=list)
    
    # Summary
    primary_models: List[str] = field(default_factory=list)  # Top 5 most relevant
    categories: List[str] = field(default_factory=list)
    tags: List[str] = field(default_factory=list)
    summary: str = ""
    
    # Metadata
    num_chunks: int = 0
    processing_time: float = 0
    
    def to_dict(self) -> Dict:
        return {
            "document_path": self.document_path,
            "document_name": self.document_name,
            "analyzed_at": self.analyzed_at.isoformat(),
            "model_matches": [m.to_dict() for m in self.model_matches],
            "lollapalooza_alerts": [l.to_dict() for l in self.lollapalooza_alerts],
            "primary_models": self.primary_models,
            "categories": self.categories,
            "tags": self.tags,
            "summary": self.summary,
            "num_chunks": self.num_chunks,
            "processing_time": self.processing_time,
            "statistics": {
                "total_model_matches": len(self.model_matches),
                "avg_relevance": sum(m.relevance_score for m in self.model_matches) / len(self.model_matches) if self.model_matches else 0,
                "lollapalooza_count": len(self.lollapalooza_alerts)
            }
        }


# =============================================================================
# MENTAL MODEL LOADER
# =============================================================================

class MentalModelLoader:
    """Load and manage the 129 mental models."""
    
    def __init__(self, models_path: str = None):
        # If models_path is a directory, look for the file inside it
        if models_path and os.path.isdir(models_path):
            # Check if it already contains 'raw' subdirectory
            raw_path = os.path.join(models_path, "mental_models_complete.json")
            if os.path.exists(raw_path):
                models_path = raw_path
            else:
                models_path = os.path.join(models_path, "raw", "mental_models_complete.json")
        self.models_path = models_path or self._find_models_file()
        self.models: Dict[str, MentalModel] = {}
        self.models_by_category: Dict[str, List[MentalModel]] = {}
        self._load_models()
    
    def _find_models_file(self) -> str:
        """Find the mental models JSON file."""
        possible_paths = [
            "/home/ubuntu/Ripple_Analytics/mental_models_system/data/raw/mental_models_complete.json",
            "./data/raw/mental_models_complete.json",
            "../data/raw/mental_models_complete.json",
        ]
        for path in possible_paths:
            if os.path.exists(path):
                return path
        raise FileNotFoundError("Could not find mental_models_complete.json")
    
    def _load_models(self):
        """Load models from JSON file."""
        with open(self.models_path, 'r') as f:
            data = json.load(f)
        
        # Build category lookup from categories list
        category_lookup = {}
        for cat in data.get("categories", []):
            category_lookup[cat.get("id")] = cat.get("name", "")
        
        for model_data in data.get("mental_models", []):
            # Map category_id to category name
            category_id = model_data.get("category_id")
            category_name = category_lookup.get(category_id, "Uncategorized")
            model_data["category"] = category_name
            
            model = MentalModel.from_dict(model_data)
            self.models[model.id] = model
            
            # Group by category
            if model.category not in self.models_by_category:
                self.models_by_category[model.category] = []
            self.models_by_category[model.category].append(model)
        
        logger.info(f"Loaded {len(self.models)} mental models across {len(self.models_by_category)} categories")
    
    def get_model(self, model_id: str) -> Optional[MentalModel]:
        """Get a model by ID."""
        return self.models.get(model_id)
    
    def get_all_models(self) -> List[MentalModel]:
        """Get all models."""
        return list(self.models.values())
    
    def get_models_by_category(self, category: str) -> List[MentalModel]:
        """Get models in a category."""
        return self.models_by_category.get(category, [])
    
    def get_by_category(self, category: str) -> List[MentalModel]:
        """Alias for get_models_by_category for backward compatibility."""
        return self.get_models_by_category(category)
    
    def search(self, query: str) -> List[MentalModel]:
        """Search models by name, description, or keywords."""
        query_lower = query.lower()
        results = []
        for model in self.models.values():
            if (query_lower in model.name.lower() or 
                query_lower in model.description.lower() or
                any(query_lower in kw.lower() for kw in model.keywords)):
                results.append(model)
        return results
    
    def get_model_summary(self) -> str:
        """Get a summary of all models for LLM prompt."""
        summary_parts = []
        for category, models in self.models_by_category.items():
            model_names = [m.name for m in models]
            summary_parts.append(f"{category}: {', '.join(model_names)}")
        return "\n".join(summary_parts)
    
    def get_model_descriptions(self) -> str:
        """Get detailed descriptions for LLM prompt."""
        descriptions = []
        for model in self.models.values():
            descriptions.append(f"- {model.name} ({model.category}): {model.description[:200]}")
        return "\n".join(descriptions)


# =============================================================================
# ANALYSIS PROMPTS
# =============================================================================

ANALYSIS_PROMPTS = {
    "identify_models": """You are an expert in Charlie Munger's mental models latticework. Analyze the following text and identify which mental models apply.

AVAILABLE MENTAL MODELS BY CATEGORY:
{model_summary}

TEXT TO ANALYZE:
{text}

For each applicable mental model, provide:
1. Model name (must match exactly from the list above)
2. Relevance score (0.0-1.0, where 1.0 = highly relevant)
3. Confidence (0.0-1.0, how confident you are in this match)
4. Evidence (quote or explanation from the text)
5. Insights (what this model reveals about the text)

Respond in JSON format:
[
  {{
    "model_name": "Model Name",
    "relevance_score": 0.85,
    "confidence": 0.9,
    "evidence": "Quote or explanation from text",
    "insights": ["Insight 1", "Insight 2"]
  }}
]

Only include models with relevance_score >= 0.5. Be thorough but precise.""",

    "detect_lollapalooza": """You are an expert in detecting Lollapalooza effects - situations where multiple mental models converge to create extreme outcomes.

The following mental models have been identified in this text:
{identified_models}

TEXT:
{text}

Analyze whether these models are converging to create a Lollapalooza effect. Consider:
1. Are multiple models reinforcing each other?
2. Is there a multiplicative (not just additive) effect?
3. What are the implications of this convergence?

Respond in JSON format:
{{
  "is_lollapalooza": true/false,
  "convergence_score": 0.0-1.0,
  "converging_models": ["Model 1", "Model 2", ...],
  "description": "How these models are converging",
  "implications": ["Implication 1", "Implication 2"]
}}""",

    "extract_model_insights": """You are analyzing a document through the lens of the mental model: {model_name}

MODEL DESCRIPTION:
{model_description}

HOW TO APPLY:
{how_to_apply}

TEXT TO ANALYZE:
{text}

Extract specific insights about how this mental model applies to the text. Provide:
1. Key observations through this model's lens
2. Specific examples or evidence from the text
3. Actionable implications
4. Potential pitfalls or failure modes to watch for

Respond in JSON format:
{{
  "observations": ["Observation 1", "Observation 2"],
  "evidence": ["Evidence 1", "Evidence 2"],
  "implications": ["Implication 1", "Implication 2"],
  "failure_modes": ["Failure mode 1", "Failure mode 2"]
}}""",

    "categorize_document": """Based on the following mental model analysis, categorize this document.

DOCUMENT: {document_name}

MODEL MATCHES:
{model_matches}

Provide:
1. Primary category (the dominant theme)
2. Secondary categories
3. Tags (specific topics, entities, concepts)
4. One-paragraph summary focusing on the mental model insights

Respond in JSON format:
{{
  "primary_category": "Category name",
  "secondary_categories": ["Category 1", "Category 2"],
  "tags": ["tag1", "tag2", "tag3"],
  "summary": "Summary paragraph"
}}"""
}


# =============================================================================
# MENTAL MODEL ANALYZER
# =============================================================================

class MentalModelAnalyzer:
    """
    Analyze documents through the lens of 129 mental models using local LLM.
    
    Usage:
        analyzer = MentalModelAnalyzer(llm_client)
        analysis = await analyzer.analyze_document(doc_path, text)
        
        # Or analyze text directly
        analysis = await analyzer.analyze_text(text)
    """
    
    def __init__(self, llm_client=None, models_path: str = None):
        """
        Initialize analyzer.
        
        Args:
            llm_client: UnifiedLLMClient instance (from local_llm.py)
            models_path: Path to mental_models_complete.json
        """
        self.llm = llm_client
        self.model_loader = MentalModelLoader(models_path)
        
        # Cache for efficiency
        self._model_summary = self.model_loader.get_model_summary()
        self._model_descriptions = self.model_loader.get_model_descriptions()
        
        # Results storage
        self.analyses: List[DocumentAnalysis] = []
    
    async def _call_llm(self, prompt: str) -> str:
        """Call the LLM with a prompt."""
        if self.llm is None:
            raise ValueError("LLM client not initialized. Pass llm_client to constructor.")
        return await self.llm.generate(prompt, temperature=0.1)
    
    def _parse_json_response(self, response: str) -> Any:
        """Parse JSON from LLM response."""
        # Handle mock objects in tests
        if not isinstance(response, str):
            return None
        
        try:
            # Find JSON in response
            start = response.find('[') if '[' in response else response.find('{')
            end_bracket = response.rfind(']')
            end_brace = response.rfind('}')
            
            # Handle case where neither bracket nor brace is found
            if end_bracket == -1 and end_brace == -1:
                return None
            
            # Get the maximum valid end position
            end = max(end_bracket, end_brace) + 1
            
            if start >= 0 and end > start:
                return json.loads(response[start:end])
        except (json.JSONDecodeError, TypeError, AttributeError):
            pass
        return None
    
    async def identify_models(self, text: str) -> List[ModelMatch]:
        """Identify which mental models apply to the text."""
        prompt = ANALYSIS_PROMPTS["identify_models"].format(
            model_summary=self._model_summary,
            text=text[:6000]  # Limit text length
        )
        
        response = await self._call_llm(prompt)
        matches_data = self._parse_json_response(response)
        
        if not matches_data or not isinstance(matches_data, list):
            return []
        
        matches = []
        for match_data in matches_data:
            # Find model ID from name
            model_id = None
            for mid, model in self.model_loader.models.items():
                if model.name.lower() == match_data.get("model_name", "").lower():
                    model_id = mid
                    break
            
            if model_id:
                matches.append(ModelMatch(
                    model_id=model_id,
                    model_name=match_data.get("model_name", ""),
                    relevance_score=float(match_data.get("relevance_score", 0.5)),
                    confidence=float(match_data.get("confidence", 0.5)),
                    evidence=match_data.get("evidence", ""),
                    insights=match_data.get("insights", [])
                ))
        
        return sorted(matches, key=lambda x: x.relevance_score, reverse=True)
    
    async def detect_lollapalooza(self, text: str, model_matches: List[ModelMatch]) -> Optional[LollapaloozaAlert]:
        """Detect if multiple models are converging (Lollapalooza effect)."""
        if len(model_matches) < 3:
            return None  # Need at least 3 models for Lollapalooza
        
        # Format identified models for prompt
        identified = "\n".join([
            f"- {m.model_name} (relevance: {m.relevance_score:.2f}): {m.evidence[:100]}"
            for m in model_matches[:10]
        ])
        
        prompt = ANALYSIS_PROMPTS["detect_lollapalooza"].format(
            identified_models=identified,
            text=text[:4000]
        )
        
        response = await self._call_llm(prompt)
        result = self._parse_json_response(response)
        
        if not result or not result.get("is_lollapalooza"):
            return None
        
        return LollapaloozaAlert(
            models=result.get("converging_models", []),
            convergence_score=float(result.get("convergence_score", 0.5)),
            description=result.get("description", ""),
            implications=result.get("implications", []),
            source_text=text[:500]
        )
    
    async def extract_model_insights(self, text: str, model: MentalModel) -> Dict:
        """Extract detailed insights for a specific model."""
        prompt = ANALYSIS_PROMPTS["extract_model_insights"].format(
            model_name=model.name,
            model_description=model.description,
            how_to_apply=model.how_to_apply,
            text=text[:5000]
        )
        
        response = await self._call_llm(prompt)
        return self._parse_json_response(response) or {}
    
    async def categorize_document(self, document_name: str, model_matches: List[ModelMatch]) -> Dict:
        """Categorize document based on model analysis."""
        matches_str = "\n".join([
            f"- {m.model_name} (score: {m.relevance_score:.2f}): {m.evidence[:100]}"
            for m in model_matches[:15]
        ])
        
        prompt = ANALYSIS_PROMPTS["categorize_document"].format(
            document_name=document_name,
            model_matches=matches_str
        )
        
        response = await self._call_llm(prompt)
        return self._parse_json_response(response) or {}
    
    async def analyze_text(self, text: str, document_name: str = "Unknown") -> DocumentAnalysis:
        """
        Perform complete mental model analysis on text.
        
        Args:
            text: The text to analyze
            document_name: Name for identification
        
        Returns:
            DocumentAnalysis with all findings
        """
        start_time = datetime.now()
        
        # Step 1: Identify applicable models
        logger.info(f"Identifying mental models for: {document_name}")
        model_matches = await self.identify_models(text)
        
        # Step 2: Detect Lollapalooza
        logger.info("Checking for Lollapalooza effects...")
        lollapalooza = await self.detect_lollapalooza(text, model_matches)
        lollapalooza_alerts = [lollapalooza] if lollapalooza else []
        
        # Step 3: Categorize document
        logger.info("Categorizing document...")
        categorization = await self.categorize_document(document_name, model_matches)
        
        # Build analysis result
        processing_time = (datetime.now() - start_time).total_seconds()
        
        analysis = DocumentAnalysis(
            document_path="",
            document_name=document_name,
            analyzed_at=datetime.now(),
            model_matches=model_matches,
            lollapalooza_alerts=lollapalooza_alerts,
            primary_models=[m.model_name for m in model_matches[:5]],
            categories=[categorization.get("primary_category", "")] + categorization.get("secondary_categories", []),
            tags=categorization.get("tags", []),
            summary=categorization.get("summary", ""),
            num_chunks=1,
            processing_time=processing_time
        )
        
        self.analyses.append(analysis)
        return analysis
    
    async def analyze_document(self, document_path: str, text: str) -> DocumentAnalysis:
        """Analyze a document file."""
        document_name = Path(document_path).name
        analysis = await self.analyze_text(text, document_name)
        analysis.document_path = document_path
        return analysis
    
    async def analyze_chunks(self, chunks: List[str], document_name: str = "Unknown") -> DocumentAnalysis:
        """
        Analyze multiple chunks and aggregate results.
        
        Args:
            chunks: List of text chunks from a document
            document_name: Name for identification
        
        Returns:
            Aggregated DocumentAnalysis
        """
        start_time = datetime.now()
        all_matches: Dict[str, ModelMatch] = {}
        all_lollapalooza: List[LollapaloozaAlert] = []
        
        for i, chunk in enumerate(chunks):
            logger.info(f"Analyzing chunk {i+1}/{len(chunks)} of {document_name}")
            
            # Identify models in this chunk
            matches = await self.identify_models(chunk)
            
            # Aggregate matches (keep highest score for each model)
            for match in matches:
                if match.model_id not in all_matches or match.relevance_score > all_matches[match.model_id].relevance_score:
                    all_matches[match.model_id] = match
            
            # Check for Lollapalooza in this chunk
            lollapalooza = await self.detect_lollapalooza(chunk, matches)
            if lollapalooza:
                all_lollapalooza.append(lollapalooza)
        
        # Sort matches by relevance
        sorted_matches = sorted(all_matches.values(), key=lambda x: x.relevance_score, reverse=True)
        
        # Categorize based on aggregated matches
        categorization = await self.categorize_document(document_name, sorted_matches)
        
        processing_time = (datetime.now() - start_time).total_seconds()
        
        analysis = DocumentAnalysis(
            document_path="",
            document_name=document_name,
            analyzed_at=datetime.now(),
            model_matches=sorted_matches,
            lollapalooza_alerts=all_lollapalooza,
            primary_models=[m.model_name for m in sorted_matches[:5]],
            categories=[categorization.get("primary_category", "")] + categorization.get("secondary_categories", []),
            tags=categorization.get("tags", []),
            summary=categorization.get("summary", ""),
            num_chunks=len(chunks),
            processing_time=processing_time
        )
        
        self.analyses.append(analysis)
        return analysis
    
    def get_model_index(self) -> Dict[str, List[str]]:
        """
        Get an index of which documents contain which models.
        
        Returns:
            Dict mapping model names to list of document names
        """
        index = {}
        for analysis in self.analyses:
            for match in analysis.model_matches:
                if match.model_name not in index:
                    index[match.model_name] = []
                index[match.model_name].append({
                    "document": analysis.document_name,
                    "relevance": match.relevance_score,
                    "evidence": match.evidence[:200]
                })
        return index
    
    def get_lollapalooza_summary(self) -> List[Dict]:
        """Get summary of all Lollapalooza alerts across documents."""
        alerts = []
        for analysis in self.analyses:
            for alert in analysis.lollapalooza_alerts:
                alerts.append({
                    "document": analysis.document_name,
                    **alert.to_dict()
                })
        return sorted(alerts, key=lambda x: x["convergence_score"], reverse=True)
    
    def export_analyses(self, output_path: str) -> str:
        """Export all analyses to JSON."""
        export_data = {
            "exported_at": datetime.now().isoformat(),
            "total_documents": len(self.analyses),
            "analyses": [a.to_dict() for a in self.analyses],
            "model_index": self.get_model_index(),
            "lollapalooza_summary": self.get_lollapalooza_summary(),
            "statistics": {
                "total_model_matches": sum(len(a.model_matches) for a in self.analyses),
                "total_lollapalooza_alerts": sum(len(a.lollapalooza_alerts) for a in self.analyses),
                "avg_models_per_document": sum(len(a.model_matches) for a in self.analyses) / len(self.analyses) if self.analyses else 0
            }
        }
        
        with open(output_path, 'w') as f:
            json.dump(export_data, f, indent=2, default=str)
        
        return output_path


# =============================================================================
# BATCH ANALYZER
# =============================================================================

class BatchModelAnalyzer:
    """
    Analyze multiple documents in batch using mental models.
    
    Designed for processing your terabytes of documents.
    """
    
    def __init__(self, llm_client=None, models_path: str = None):
        self.analyzer = MentalModelAnalyzer(llm_client, models_path)
        self.results: List[DocumentAnalysis] = []
        self.progress_callback = None
    
    async def analyze_directory(self, 
                               directory: str,
                               extensions: List[str] = None,
                               max_files: int = None) -> List[DocumentAnalysis]:
        """
        Analyze all documents in a directory.
        
        Args:
            directory: Path to directory
            extensions: File extensions to process (default: .txt, .md)
            max_files: Maximum files to process
        
        Returns:
            List of DocumentAnalysis results
        """
        from ..pipeline.terabyte_processor import TextExtractor, SemanticChunker
        
        extensions = extensions or ['.txt', '.md', '.pdf']
        directory = Path(directory)
        
        # Find files
        files = []
        for ext in extensions:
            files.extend(directory.rglob(f"*{ext}"))
        
        if max_files:
            files = files[:max_files]
        
        logger.info(f"Found {len(files)} files to analyze")
        
        chunker = SemanticChunker(chunk_size=2000, overlap=200)
        
        for i, file_path in enumerate(files):
            try:
                logger.info(f"Processing {i+1}/{len(files)}: {file_path.name}")
                
                # Extract text
                text, _ = TextExtractor.extract(str(file_path))
                if not text:
                    continue
                
                # Chunk if large
                if len(text) > 3000:
                    chunks = chunker.chunk(text)
                    analysis = await self.analyzer.analyze_chunks(chunks, file_path.name)
                else:
                    analysis = await self.analyzer.analyze_document(str(file_path), text)
                
                self.results.append(analysis)
                
                if self.progress_callback:
                    self.progress_callback(i + 1, len(files), analysis)
                
            except Exception as e:
                logger.error(f"Error processing {file_path}: {e}")
        
        return self.results
    
    def export_results(self, output_path: str) -> str:
        """Export all results."""
        return self.analyzer.export_analyses(output_path)
    
    def get_statistics(self) -> Dict:
        """Get processing statistics."""
        if not self.results:
            return {"total_documents": 0}
        
        all_models = set()
        for r in self.results:
            for m in r.model_matches:
                all_models.add(m.model_name)
        
        return {
            "total_documents": len(self.results),
            "unique_models_found": len(all_models),
            "total_model_matches": sum(len(r.model_matches) for r in self.results),
            "total_lollapalooza_alerts": sum(len(r.lollapalooza_alerts) for r in self.results),
            "avg_models_per_document": sum(len(r.model_matches) for r in self.results) / len(self.results),
            "avg_processing_time": sum(r.processing_time for r in self.results) / len(self.results),
            "top_models": self._get_top_models(10)
        }
    
    def _get_top_models(self, n: int = 10) -> List[Dict]:
        """Get the most frequently appearing models."""
        model_counts = {}
        for r in self.results:
            for m in r.model_matches:
                if m.model_name not in model_counts:
                    model_counts[m.model_name] = {"count": 0, "total_relevance": 0}
                model_counts[m.model_name]["count"] += 1
                model_counts[m.model_name]["total_relevance"] += m.relevance_score
        
        top = sorted(model_counts.items(), key=lambda x: x[1]["count"], reverse=True)[:n]
        return [
            {
                "model": name,
                "count": data["count"],
                "avg_relevance": data["total_relevance"] / data["count"]
            }
            for name, data in top
        ]


# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

def create_analyzer(llm_client=None, models_path: str = None) -> MentalModelAnalyzer:
    """Create a MentalModelAnalyzer with default settings."""
    return MentalModelAnalyzer(llm_client, models_path)


async def quick_analyze(text: str, llm_client=None) -> DocumentAnalysis:
    """Quick analysis of a single text."""
    analyzer = MentalModelAnalyzer(llm_client)
    return await analyzer.analyze_text(text)


def load_models() -> MentalModelLoader:
    """Load mental models for inspection."""
    return MentalModelLoader()


# =============================================================================
# CLI
# =============================================================================

if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(description="Mental Model Document Analyzer")
    parser.add_argument("--list-models", action="store_true", help="List all mental models")
    parser.add_argument("--category", help="Filter models by category")
    
    args = parser.parse_args()
    
    loader = MentalModelLoader()
    
    if args.list_models:
        if args.category:
            models = loader.get_models_by_category(args.category)
            print(f"\nModels in category '{args.category}':")
        else:
            models = loader.get_all_models()
            print(f"\nAll {len(models)} Mental Models:")
        
        for model in models:
            print(f"  - {model.name} ({model.category})")
    else:
        print("Mental Model Document Analyzer")
        print("=" * 50)
        print(f"Loaded {len(loader.models)} models across {len(loader.models_by_category)} categories")
        print("\nCategories:")
        for cat, models in loader.models_by_category.items():
            print(f"  - {cat}: {len(models)} models")
        print("\nUse --list-models to see all models")
        print("Use --category <name> to filter by category")
