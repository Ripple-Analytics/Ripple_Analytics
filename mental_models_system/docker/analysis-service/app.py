"""
Analysis Service

Mental model analysis microservice.
Provides keyword-based and AI-powered analysis.
"""

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from typing import List, Optional
import os
import logging

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

app = FastAPI(
    title="Mental Models Analysis Service",
    description="Mental model detection and analysis",
    version="1.0.0"
)

# CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Mental Models Database
MENTAL_MODELS = [
    {
        "id": 1,
        "name": "Incentive-Caused Bias",
        "category": "Psychology",
        "keywords": ["incentive", "reward", "punishment", "motivation", "bonus", "commission", "salary", "pay"],
        "description": "People tend to act in ways that serve their incentives, even unconsciously.",
        "failure_modes": ["Incentive Blindness", "Perverse Incentive Creation"],
        "safeguards": ["Consider non-monetary incentives", "Map conflicts of interest"]
    },
    {
        "id": 2,
        "name": "Confirmation Bias",
        "category": "Psychology",
        "keywords": ["confirm", "belief", "evidence", "ignore", "dismiss", "agree", "disagree", "opinion"],
        "description": "The tendency to search for, interpret, and recall information that confirms pre-existing beliefs.",
        "failure_modes": ["Echo Chamber Effect", "Selective Evidence Gathering"],
        "safeguards": ["Actively seek disconfirming evidence", "Devil's advocate role"]
    },
    {
        "id": 3,
        "name": "Availability Heuristic",
        "category": "Psychology",
        "keywords": ["recent", "memorable", "vivid", "news", "media", "remember", "recall", "example"],
        "description": "Overweighting information that comes easily to mind.",
        "failure_modes": ["Recency Bias", "Media Influence Distortion"],
        "safeguards": ["Use base rates", "Seek statistical data"]
    },
    {
        "id": 4,
        "name": "Sunk Cost Fallacy",
        "category": "Economics",
        "keywords": ["sunk", "cost", "invested", "spent", "waste", "continue", "quit", "abandon"],
        "description": "Continuing a behavior due to previously invested resources rather than future value.",
        "failure_modes": ["Escalation of Commitment", "Loss Aversion Trap"],
        "safeguards": ["Focus on future value only", "Set kill criteria upfront"]
    },
    {
        "id": 5,
        "name": "Opportunity Cost",
        "category": "Economics",
        "keywords": ["opportunity", "alternative", "trade-off", "choice", "option", "instead", "rather"],
        "description": "The cost of the next best alternative foregone.",
        "failure_modes": ["Hidden Cost Blindness", "Comparison Neglect"],
        "safeguards": ["Always list alternatives", "Quantify trade-offs"]
    },
    {
        "id": 6,
        "name": "Margin of Safety",
        "category": "Engineering",
        "keywords": ["margin", "safety", "buffer", "redundancy", "backup", "cushion", "reserve"],
        "description": "Building in a buffer to account for errors, unknowns, and variability.",
        "failure_modes": ["Overconfidence in Estimates", "Insufficient Buffer"],
        "safeguards": ["Add 50% buffer minimum", "Plan for worst case"]
    },
    {
        "id": 7,
        "name": "Feedback Loops",
        "category": "Systems",
        "keywords": ["feedback", "loop", "cycle", "reinforce", "amplify", "dampen", "stabilize"],
        "description": "Systems where outputs become inputs, creating reinforcing or balancing dynamics.",
        "failure_modes": ["Runaway Amplification", "Delayed Feedback Blindness"],
        "safeguards": ["Map feedback mechanisms", "Monitor leading indicators"]
    },
    {
        "id": 8,
        "name": "Second-Order Thinking",
        "category": "Thinking Tools",
        "keywords": ["consequence", "effect", "result", "then what", "downstream", "ripple", "chain"],
        "description": "Considering the consequences of consequences.",
        "failure_modes": ["First-Order Fixation", "Unintended Consequence Blindness"],
        "safeguards": ["Ask 'and then what?' 3 times", "Map causal chains"]
    },
    {
        "id": 9,
        "name": "Inversion",
        "category": "Thinking Tools",
        "keywords": ["invert", "reverse", "opposite", "avoid", "prevent", "failure", "mistake"],
        "description": "Approaching problems backwards - thinking about what to avoid rather than what to do.",
        "failure_modes": ["Positive-Only Thinking", "Failure Mode Neglect"],
        "safeguards": ["Pre-mortem exercise", "List ways to fail"]
    },
    {
        "id": 10,
        "name": "Circle of Competence",
        "category": "Thinking Tools",
        "keywords": ["competence", "expertise", "knowledge", "understand", "limit", "boundary", "edge"],
        "description": "Understanding the boundaries of your knowledge and staying within them.",
        "failure_modes": ["Overconfidence Outside Circle", "Circle Creep"],
        "safeguards": ["Define circle explicitly", "Seek expert input outside circle"]
    },
    {
        "id": 11,
        "name": "First Principles",
        "category": "Thinking Tools",
        "keywords": ["first principle", "fundamental", "basic", "assumption", "ground up", "foundation"],
        "description": "Breaking down problems to their most basic elements and building up from there.",
        "failure_modes": ["Assumption Inheritance", "Complexity Avoidance"],
        "safeguards": ["Question every assumption", "Start from physics/math"]
    },
    {
        "id": 12,
        "name": "Occam's Razor",
        "category": "Thinking Tools",
        "keywords": ["simple", "simplest", "complexity", "complicated", "explanation", "assumption"],
        "description": "The simplest explanation is usually the correct one.",
        "failure_modes": ["Oversimplification", "Complexity Bias"],
        "safeguards": ["Start simple, add complexity only when needed"]
    },
    {
        "id": 13,
        "name": "Hanlon's Razor",
        "category": "Psychology",
        "keywords": ["malice", "stupidity", "incompetence", "mistake", "intention", "deliberate"],
        "description": "Never attribute to malice that which can be explained by incompetence.",
        "failure_modes": ["Paranoid Attribution", "Malice Assumption"],
        "safeguards": ["Assume good intent first", "Consider alternative explanations"]
    },
    {
        "id": 14,
        "name": "Network Effects",
        "category": "Economics",
        "keywords": ["network", "user", "platform", "value", "growth", "viral", "adoption"],
        "description": "The value of a product increases as more people use it.",
        "failure_modes": ["Network Dependency Risk", "Winner-Take-All Blindness"],
        "safeguards": ["Diversify platforms", "Build owned audience"]
    },
    {
        "id": 15,
        "name": "Compounding",
        "category": "Mathematics",
        "keywords": ["compound", "exponential", "growth", "interest", "accumulate", "snowball"],
        "description": "Small consistent gains accumulate into large results over time.",
        "failure_modes": ["Linear Thinking", "Impatience with Growth"],
        "safeguards": ["Think in decades", "Protect the principal"]
    }
]


class AnalysisRequest(BaseModel):
    text: str
    top_n: int = 5


class ModelMatch(BaseModel):
    id: int
    name: str
    category: str
    description: str
    relevance: float
    matched_keywords: List[str]
    failure_modes: List[str]
    safeguards: List[str]


class AnalysisResponse(BaseModel):
    success: bool
    mode: str
    text_preview: str
    models: List[ModelMatch]
    total_models_checked: int
    timestamp: str


@app.get("/")
async def root():
    return {
        "service": "Mental Models Analysis Service",
        "version": "1.0.0",
        "models_count": len(MENTAL_MODELS)
    }


@app.get("/health")
async def health_check():
    return {"status": "healthy", "service": "analysis"}


@app.post("/analyze", response_model=AnalysisResponse)
async def analyze_text(request: AnalysisRequest):
    """Analyze text for mental model patterns"""
    from datetime import datetime
    
    if not request.text or len(request.text.strip()) == 0:
        raise HTTPException(status_code=400, detail="No text provided for analysis")
    
    lower_text = request.text.lower()
    results = []
    
    for model in MENTAL_MODELS:
        match_count = 0
        matched_keywords = []
        
        for keyword in model["keywords"]:
            if keyword.lower() in lower_text:
                match_count += 1
                matched_keywords.append(keyword)
        
        if match_count > 0:
            relevance = min(match_count / len(model["keywords"]) * 2, 1.0)
            results.append(ModelMatch(
                id=model["id"],
                name=model["name"],
                category=model["category"],
                description=model["description"],
                relevance=round(relevance, 2),
                matched_keywords=matched_keywords,
                failure_modes=model.get("failure_modes", []),
                safeguards=model.get("safeguards", [])
            ))
    
    # Sort by relevance
    results.sort(key=lambda x: x.relevance, reverse=True)
    
    return AnalysisResponse(
        success=True,
        mode="keyword",
        text_preview=request.text[:100] + ("..." if len(request.text) > 100 else ""),
        models=results[:request.top_n],
        total_models_checked=len(MENTAL_MODELS),
        timestamp=datetime.utcnow().isoformat()
    )


@app.get("/models")
async def get_models():
    """Get all mental models"""
    return {
        "models": [
            {
                "id": m["id"],
                "name": m["name"],
                "category": m["category"],
                "description": m["description"]
            }
            for m in MENTAL_MODELS
        ],
        "count": len(MENTAL_MODELS)
    }


@app.get("/models/{model_id}")
async def get_model(model_id: int):
    """Get a specific mental model"""
    model = next((m for m in MENTAL_MODELS if m["id"] == model_id), None)
    if not model:
        raise HTTPException(status_code=404, detail="Model not found")
    return model


@app.get("/categories")
async def get_categories():
    """Get all model categories"""
    categories = list(set(m["category"] for m in MENTAL_MODELS))
    return {
        "categories": sorted(categories),
        "count": len(categories)
    }


@app.get("/categories/{category}")
async def get_models_by_category(category: str):
    """Get models by category"""
    models = [m for m in MENTAL_MODELS if m["category"].lower() == category.lower()]
    if not models:
        raise HTTPException(status_code=404, detail="Category not found")
    return {
        "category": category,
        "models": models,
        "count": len(models)
    }


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8001)
