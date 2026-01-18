"""
Decision Recommendation Engine.

Uses historical decision data and mental model effectiveness
to provide intelligent recommendations for new decisions.

Features:
- Pattern matching from historical decisions
- Model effectiveness weighting
- Context-aware recommendations
- Risk-adjusted suggestions
- Synergy detection between models
"""

import json
import os
from dataclasses import dataclass, field
from typing import List, Dict, Optional, Tuple, Any
from enum import Enum
from datetime import datetime
from collections import defaultdict
import math


class RecommendationType(Enum):
    """Types of recommendations."""
    MODEL_SUGGESTION = "model_suggestion"
    RISK_WARNING = "risk_warning"
    SYNERGY_OPPORTUNITY = "synergy_opportunity"
    HISTORICAL_PATTERN = "historical_pattern"
    FAILURE_MODE_ALERT = "failure_mode_alert"


@dataclass
class Recommendation:
    """A single recommendation."""
    type: RecommendationType
    title: str
    description: str
    confidence: float  # 0-1
    models: List[str]
    evidence: List[str]
    action_items: List[str]
    risk_level: str  # low, medium, high, critical
    priority: int  # 1-10
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    def to_dict(self) -> Dict:
        return {
            "type": self.type.value,
            "title": self.title,
            "description": self.description,
            "confidence": self.confidence,
            "models": self.models,
            "evidence": self.evidence,
            "action_items": self.action_items,
            "risk_level": self.risk_level,
            "priority": self.priority,
            "metadata": self.metadata
        }


@dataclass
class DecisionContext:
    """Context for a decision being analyzed."""
    description: str
    domain: str  # investment, business, personal, career
    stakes: str  # low, medium, high, critical
    time_horizon: str  # short, medium, long
    reversibility: str  # reversible, partially_reversible, irreversible
    constraints: List[str] = field(default_factory=list)
    goals: List[str] = field(default_factory=list)
    
    def to_dict(self) -> Dict:
        return {
            "description": self.description,
            "domain": self.domain,
            "stakes": self.stakes,
            "time_horizon": self.time_horizon,
            "reversibility": self.reversibility,
            "constraints": self.constraints,
            "goals": self.goals
        }


class DecisionRecommender:
    """
    Intelligent decision recommendation engine.
    
    Uses historical data and mental model effectiveness
    to provide context-aware recommendations.
    """
    
    def __init__(self, data_dir: str = None):
        self.data_dir = data_dir or os.path.join(
            os.path.dirname(__file__), 
            "../../data"
        )
        
        # Load historical decision patterns
        self.decision_patterns = self._load_decision_patterns()
        
        # Load model effectiveness data
        self.model_effectiveness = self._load_model_effectiveness()
        
        # Load model synergies
        self.model_synergies = self._load_model_synergies()
        
        # Domain-specific model mappings
        self.domain_models = {
            "investment": [
                "Margin of Safety", "Circle of Competence", "Economic Moats",
                "Mr. Market", "Compounding", "Opportunity Cost", "Sunk Cost",
                "Network Effects", "Winner-Take-All Markets", "Reflexivity"
            ],
            "business": [
                "Incentive-Caused Bias", "Principal-Agent Problem", "Network Effects",
                "Economies of Scale", "Switching Costs", "First-Mover Advantage",
                "Competitive Advantage", "Supply and Demand", "Opportunity Cost"
            ],
            "personal": [
                "Inversion", "Second-Order Thinking", "Probabilistic Thinking",
                "Confirmation Bias", "Availability Bias", "Hindsight Bias",
                "Regret Minimization", "Compounding", "Opportunity Cost"
            ],
            "career": [
                "Circle of Competence", "Compounding", "Opportunity Cost",
                "Leverage", "Optionality", "Network Effects", "First-Mover Advantage",
                "Incentive-Caused Bias", "Principal-Agent Problem"
            ]
        }
        
        # Stakes-based model priorities
        self.stakes_models = {
            "critical": [
                "Margin of Safety", "Inversion", "Pre-Mortem Analysis",
                "Second-Order Thinking", "Probabilistic Thinking"
            ],
            "high": [
                "Margin of Safety", "Second-Order Thinking", "Circle of Competence",
                "Opportunity Cost", "Inversion"
            ],
            "medium": [
                "Second-Order Thinking", "Opportunity Cost", "Probabilistic Thinking"
            ],
            "low": [
                "Opportunity Cost", "Sunk Cost"
            ]
        }
        
        # Time horizon model mappings
        self.time_horizon_models = {
            "short": ["Opportunity Cost", "Sunk Cost", "Availability Bias"],
            "medium": ["Second-Order Thinking", "Compounding", "Network Effects"],
            "long": ["Compounding", "Economic Moats", "Winner-Take-All Markets", "Reflexivity"]
        }
    
    def _load_decision_patterns(self) -> Dict:
        """Load historical decision patterns."""
        patterns_file = os.path.join(self.data_dir, "decision_patterns.json")
        if os.path.exists(patterns_file):
            with open(patterns_file) as f:
                return json.load(f)
        
        # Default patterns based on common decision types
        return {
            "investment_acquisition": {
                "keywords": ["acquire", "buy", "purchase", "invest", "acquisition"],
                "recommended_models": [
                    "Margin of Safety", "Circle of Competence", "Economic Moats",
                    "Inversion", "Pre-Mortem Analysis"
                ],
                "common_failures": [
                    "Overconfidence", "Confirmation Bias", "Winner's Curse"
                ],
                "success_rate": 0.45
            },
            "market_entry": {
                "keywords": ["enter", "launch", "expand", "new market", "entry"],
                "recommended_models": [
                    "First-Mover Advantage", "Network Effects", "Competitive Advantage",
                    "Economies of Scale", "Circle of Competence"
                ],
                "common_failures": [
                    "Overoptimism", "Planning Fallacy", "Survivorship Bias"
                ],
                "success_rate": 0.35
            },
            "hiring": {
                "keywords": ["hire", "recruit", "team", "talent", "employee"],
                "recommended_models": [
                    "Incentive-Caused Bias", "Principal-Agent Problem",
                    "Circle of Competence", "Inversion"
                ],
                "common_failures": [
                    "Halo Effect", "Liking/Loving Tendency", "Social Proof"
                ],
                "success_rate": 0.50
            },
            "partnership": {
                "keywords": ["partner", "alliance", "joint venture", "collaborate"],
                "recommended_models": [
                    "Principal-Agent Problem", "Incentive-Caused Bias",
                    "Game Theory", "Trust", "Reciprocity"
                ],
                "common_failures": [
                    "Commitment and Consistency Bias", "Liking/Loving Tendency"
                ],
                "success_rate": 0.40
            },
            "pricing": {
                "keywords": ["price", "pricing", "cost", "margin", "revenue"],
                "recommended_models": [
                    "Supply and Demand", "Anchoring", "Contrast Effect",
                    "Opportunity Cost", "Elasticity"
                ],
                "common_failures": [
                    "Anchoring", "Loss Aversion", "Status Quo Bias"
                ],
                "success_rate": 0.55
            },
            "exit": {
                "keywords": ["sell", "exit", "divest", "close", "shutdown"],
                "recommended_models": [
                    "Sunk Cost", "Opportunity Cost", "Regret Minimization",
                    "Inversion", "Second-Order Thinking"
                ],
                "common_failures": [
                    "Sunk Cost Fallacy", "Loss Aversion", "Endowment Effect"
                ],
                "success_rate": 0.50
            }
        }
    
    def _load_model_effectiveness(self) -> Dict[str, float]:
        """Load model effectiveness scores."""
        effectiveness_file = os.path.join(self.data_dir, "model_effectiveness.json")
        if os.path.exists(effectiveness_file):
            with open(effectiveness_file) as f:
                return json.load(f)
        
        # Default effectiveness scores (0-1)
        return {
            "Margin of Safety": 0.85,
            "Circle of Competence": 0.80,
            "Inversion": 0.82,
            "Second-Order Thinking": 0.78,
            "Economic Moats": 0.75,
            "Compounding": 0.90,
            "Network Effects": 0.72,
            "Incentive-Caused Bias": 0.85,
            "Sunk Cost": 0.70,
            "Opportunity Cost": 0.75,
            "Confirmation Bias": 0.65,
            "Availability Bias": 0.60,
            "Probabilistic Thinking": 0.80,
            "Pre-Mortem Analysis": 0.78,
            "First-Mover Advantage": 0.55,
            "Winner-Take-All Markets": 0.65
        }
    
    def _load_model_synergies(self) -> Dict[str, List[str]]:
        """Load model synergy pairs."""
        return {
            "Margin of Safety": ["Circle of Competence", "Inversion", "Pre-Mortem Analysis"],
            "Circle of Competence": ["Margin of Safety", "Opportunity Cost"],
            "Inversion": ["Pre-Mortem Analysis", "Second-Order Thinking", "Margin of Safety"],
            "Second-Order Thinking": ["Inversion", "Probabilistic Thinking", "Compounding"],
            "Compounding": ["Second-Order Thinking", "Network Effects", "Economic Moats"],
            "Network Effects": ["Winner-Take-All Markets", "First-Mover Advantage", "Compounding"],
            "Economic Moats": ["Competitive Advantage", "Compounding", "Margin of Safety"],
            "Incentive-Caused Bias": ["Principal-Agent Problem", "Game Theory"],
            "Sunk Cost": ["Opportunity Cost", "Regret Minimization"],
            "Opportunity Cost": ["Sunk Cost", "Circle of Competence", "Margin of Safety"]
        }
    
    def recommend(self, context: DecisionContext) -> List[Recommendation]:
        """
        Generate recommendations for a decision context.
        
        Args:
            context: The decision context to analyze
            
        Returns:
            List of prioritized recommendations
        """
        recommendations = []
        
        # 1. Pattern-based recommendations
        pattern_recs = self._recommend_from_patterns(context)
        recommendations.extend(pattern_recs)
        
        # 2. Domain-specific model recommendations
        domain_recs = self._recommend_domain_models(context)
        recommendations.extend(domain_recs)
        
        # 3. Stakes-based recommendations
        stakes_recs = self._recommend_stakes_models(context)
        recommendations.extend(stakes_recs)
        
        # 4. Time horizon recommendations
        time_recs = self._recommend_time_horizon_models(context)
        recommendations.extend(time_recs)
        
        # 5. Synergy recommendations
        synergy_recs = self._recommend_synergies(recommendations)
        recommendations.extend(synergy_recs)
        
        # 6. Risk warnings based on context
        risk_recs = self._generate_risk_warnings(context)
        recommendations.extend(risk_recs)
        
        # 7. Failure mode alerts
        failure_recs = self._generate_failure_alerts(context, recommendations)
        recommendations.extend(failure_recs)
        
        # Deduplicate and prioritize
        recommendations = self._deduplicate_recommendations(recommendations)
        recommendations = self._prioritize_recommendations(recommendations, context)
        
        return recommendations[:15]  # Top 15 recommendations
    
    def _recommend_from_patterns(self, context: DecisionContext) -> List[Recommendation]:
        """Recommend based on historical decision patterns."""
        recommendations = []
        description_lower = context.description.lower()
        
        for pattern_name, pattern_data in self.decision_patterns.items():
            # Check if any keywords match
            keyword_matches = [
                kw for kw in pattern_data["keywords"]
                if kw in description_lower
            ]
            
            if keyword_matches:
                confidence = min(0.9, 0.5 + 0.1 * len(keyword_matches))
                
                recommendations.append(Recommendation(
                    type=RecommendationType.HISTORICAL_PATTERN,
                    title=f"Historical Pattern: {pattern_name.replace('_', ' ').title()}",
                    description=f"This decision matches the '{pattern_name}' pattern. "
                               f"Historical success rate: {pattern_data['success_rate']:.0%}",
                    confidence=confidence,
                    models=pattern_data["recommended_models"],
                    evidence=[f"Keyword match: {kw}" for kw in keyword_matches],
                    action_items=[
                        f"Apply {model}" for model in pattern_data["recommended_models"][:3]
                    ],
                    risk_level="medium" if pattern_data["success_rate"] < 0.5 else "low",
                    priority=8,
                    metadata={
                        "pattern": pattern_name,
                        "success_rate": pattern_data["success_rate"],
                        "common_failures": pattern_data["common_failures"]
                    }
                ))
        
        return recommendations
    
    def _recommend_domain_models(self, context: DecisionContext) -> List[Recommendation]:
        """Recommend models based on decision domain."""
        domain_models = self.domain_models.get(context.domain, [])
        
        if not domain_models:
            return []
        
        # Weight by effectiveness
        weighted_models = sorted(
            domain_models,
            key=lambda m: self.model_effectiveness.get(m, 0.5),
            reverse=True
        )
        
        return [Recommendation(
            type=RecommendationType.MODEL_SUGGESTION,
            title=f"Domain-Specific Models for {context.domain.title()}",
            description=f"These mental models are most effective for {context.domain} decisions.",
            confidence=0.8,
            models=weighted_models[:5],
            evidence=[
                f"{m}: {self.model_effectiveness.get(m, 0.5):.0%} effectiveness"
                for m in weighted_models[:5]
            ],
            action_items=[
                f"Analyze decision through {weighted_models[0]} lens",
                f"Cross-check with {weighted_models[1]}",
                f"Validate using {weighted_models[2]}"
            ],
            risk_level="low",
            priority=7
        )]
    
    def _recommend_stakes_models(self, context: DecisionContext) -> List[Recommendation]:
        """Recommend models based on decision stakes."""
        stakes_models = self.stakes_models.get(context.stakes, [])
        
        if not stakes_models:
            return []
        
        risk_level = {
            "critical": "critical",
            "high": "high",
            "medium": "medium",
            "low": "low"
        }.get(context.stakes, "medium")
        
        return [Recommendation(
            type=RecommendationType.MODEL_SUGGESTION,
            title=f"High-Stakes Decision Framework ({context.stakes.title()} Stakes)",
            description=f"Given the {context.stakes} stakes, these models are essential.",
            confidence=0.85,
            models=stakes_models,
            evidence=[
                f"Stakes level: {context.stakes}",
                f"Reversibility: {context.reversibility}"
            ],
            action_items=[
                "Apply Margin of Safety with extra buffer" if context.stakes in ["critical", "high"] else "Standard analysis",
                "Conduct Pre-Mortem Analysis" if context.stakes == "critical" else "Review potential failures",
                "Get external validation" if context.stakes in ["critical", "high"] else "Self-review"
            ],
            risk_level=risk_level,
            priority=9 if context.stakes in ["critical", "high"] else 6
        )]
    
    def _recommend_time_horizon_models(self, context: DecisionContext) -> List[Recommendation]:
        """Recommend models based on time horizon."""
        time_models = self.time_horizon_models.get(context.time_horizon, [])
        
        if not time_models:
            return []
        
        return [Recommendation(
            type=RecommendationType.MODEL_SUGGESTION,
            title=f"{context.time_horizon.title()}-Term Decision Models",
            description=f"For {context.time_horizon}-term decisions, these models are most relevant.",
            confidence=0.75,
            models=time_models,
            evidence=[f"Time horizon: {context.time_horizon}"],
            action_items=[
                f"Consider {context.time_horizon}-term effects using {time_models[0]}"
            ],
            risk_level="low",
            priority=5
        )]
    
    def _recommend_synergies(self, existing_recs: List[Recommendation]) -> List[Recommendation]:
        """Recommend model synergies based on already recommended models."""
        all_recommended_models = set()
        for rec in existing_recs:
            all_recommended_models.update(rec.models)
        
        synergy_recommendations = []
        
        for model in all_recommended_models:
            synergies = self.model_synergies.get(model, [])
            new_synergies = [s for s in synergies if s not in all_recommended_models]
            
            if new_synergies:
                synergy_recommendations.append(Recommendation(
                    type=RecommendationType.SYNERGY_OPPORTUNITY,
                    title=f"Synergy: {model} + {new_synergies[0]}",
                    description=f"Combining {model} with {new_synergies[0]} creates powerful synergy.",
                    confidence=0.7,
                    models=[model, new_synergies[0]],
                    evidence=[f"Historical synergy between {model} and {new_synergies[0]}"],
                    action_items=[
                        f"Apply {model} first",
                        f"Validate findings with {new_synergies[0]}",
                        "Look for reinforcing patterns"
                    ],
                    risk_level="low",
                    priority=6
                ))
        
        return synergy_recommendations[:3]  # Top 3 synergies
    
    def _generate_risk_warnings(self, context: DecisionContext) -> List[Recommendation]:
        """Generate risk warnings based on context."""
        warnings = []
        
        # Irreversible + high stakes = major warning
        if context.reversibility == "irreversible" and context.stakes in ["critical", "high"]:
            warnings.append(Recommendation(
                type=RecommendationType.RISK_WARNING,
                title="⚠️ CRITICAL: Irreversible High-Stakes Decision",
                description="This decision is irreversible with high stakes. "
                           "Apply maximum rigor and seek external validation.",
                confidence=0.95,
                models=["Margin of Safety", "Inversion", "Pre-Mortem Analysis"],
                evidence=[
                    f"Reversibility: {context.reversibility}",
                    f"Stakes: {context.stakes}"
                ],
                action_items=[
                    "Sleep on it - minimum 24 hour waiting period",
                    "Get at least 2 independent opinions",
                    "Document your reasoning in writing",
                    "Identify the 3 ways this could fail catastrophically",
                    "Ensure you can survive the worst case"
                ],
                risk_level="critical",
                priority=10
            ))
        
        # Short time horizon + high stakes = warning
        if context.time_horizon == "short" and context.stakes in ["critical", "high"]:
            warnings.append(Recommendation(
                type=RecommendationType.RISK_WARNING,
                title="⚠️ Time Pressure Warning",
                description="High-stakes decision with short time horizon. "
                           "Beware of rushed judgment.",
                confidence=0.85,
                models=["Availability Bias", "Stress-Influence Tendency"],
                evidence=[
                    f"Time horizon: {context.time_horizon}",
                    f"Stakes: {context.stakes}"
                ],
                action_items=[
                    "Question if the time pressure is real",
                    "Identify what you're NOT seeing due to time pressure",
                    "Consider: What would you advise a friend in this situation?"
                ],
                risk_level="high",
                priority=9
            ))
        
        return warnings
    
    def _generate_failure_alerts(
        self, 
        context: DecisionContext, 
        recommendations: List[Recommendation]
    ) -> List[Recommendation]:
        """Generate failure mode alerts."""
        alerts = []
        
        # Get common failures from matched patterns
        common_failures = set()
        for rec in recommendations:
            if rec.metadata.get("common_failures"):
                common_failures.update(rec.metadata["common_failures"])
        
        if common_failures:
            alerts.append(Recommendation(
                type=RecommendationType.FAILURE_MODE_ALERT,
                title="🚨 Common Failure Modes to Watch",
                description=f"Decisions like this commonly fail due to: {', '.join(list(common_failures)[:3])}",
                confidence=0.8,
                models=list(common_failures)[:5],
                evidence=[f"Historical failure pattern: {f}" for f in list(common_failures)[:3]],
                action_items=[
                    f"Actively check for {list(common_failures)[0]}" if common_failures else "Review biases",
                    "Ask: What evidence would change my mind?",
                    "Seek disconfirming evidence"
                ],
                risk_level="high",
                priority=8
            ))
        
        return alerts
    
    def _deduplicate_recommendations(self, recommendations: List[Recommendation]) -> List[Recommendation]:
        """Remove duplicate recommendations."""
        seen_titles = set()
        unique = []
        
        for rec in recommendations:
            if rec.title not in seen_titles:
                seen_titles.add(rec.title)
                unique.append(rec)
        
        return unique
    
    def _prioritize_recommendations(
        self, 
        recommendations: List[Recommendation],
        context: DecisionContext
    ) -> List[Recommendation]:
        """Prioritize recommendations based on context."""
        # Score each recommendation
        def score(rec: Recommendation) -> float:
            base_score = rec.priority * 10
            
            # Boost risk warnings for high-stakes decisions
            if rec.type == RecommendationType.RISK_WARNING:
                if context.stakes in ["critical", "high"]:
                    base_score *= 1.5
            
            # Boost pattern matches
            if rec.type == RecommendationType.HISTORICAL_PATTERN:
                base_score *= 1.2
            
            # Factor in confidence
            base_score *= rec.confidence
            
            return base_score
        
        return sorted(recommendations, key=score, reverse=True)
    
    def quick_recommend(self, description: str, domain: str = "business") -> List[Recommendation]:
        """Quick recommendation with minimal context."""
        context = DecisionContext(
            description=description,
            domain=domain,
            stakes="medium",
            time_horizon="medium",
            reversibility="partially_reversible"
        )
        return self.recommend(context)
    
    def export_recommendations(
        self, 
        recommendations: List[Recommendation],
        output_path: str = None
    ) -> str:
        """Export recommendations to JSON."""
        output_path = output_path or os.path.join(
            self.data_dir, 
            "recommendations",
            f"recommendations_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        )
        
        os.makedirs(os.path.dirname(output_path), exist_ok=True)
        
        data = {
            "generated_at": datetime.now().isoformat(),
            "count": len(recommendations),
            "recommendations": [r.to_dict() for r in recommendations]
        }
        
        with open(output_path, 'w') as f:
            json.dump(data, f, indent=2)
        
        return output_path


# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

def get_recommendations(
    description: str,
    domain: str = "business",
    stakes: str = "medium",
    time_horizon: str = "medium",
    reversibility: str = "partially_reversible"
) -> List[Recommendation]:
    """
    Get recommendations for a decision.
    
    Args:
        description: Description of the decision
        domain: Domain (investment, business, personal, career)
        stakes: Stakes level (low, medium, high, critical)
        time_horizon: Time horizon (short, medium, long)
        reversibility: Reversibility (reversible, partially_reversible, irreversible)
        
    Returns:
        List of recommendations
    """
    context = DecisionContext(
        description=description,
        domain=domain,
        stakes=stakes,
        time_horizon=time_horizon,
        reversibility=reversibility
    )
    
    recommender = DecisionRecommender()
    return recommender.recommend(context)


def print_recommendations(recommendations: List[Recommendation]):
    """Pretty print recommendations."""
    print("\n" + "=" * 70)
    print("DECISION RECOMMENDATIONS")
    print("=" * 70)
    
    for i, rec in enumerate(recommendations, 1):
        risk_emoji = {
            "low": "🟢",
            "medium": "🟡",
            "high": "🟠",
            "critical": "🔴"
        }.get(rec.risk_level, "⚪")
        
        print(f"\n{i}. {risk_emoji} [{rec.type.value.upper()}] {rec.title}")
        print(f"   Confidence: {rec.confidence:.0%} | Priority: {rec.priority}/10")
        print(f"   {rec.description}")
        
        if rec.models:
            print(f"   Models: {', '.join(rec.models[:3])}")
        
        if rec.action_items:
            print("   Actions:")
            for action in rec.action_items[:3]:
                print(f"     • {action}")
    
    print("\n" + "=" * 70)


if __name__ == "__main__":
    # Test the recommender
    recommendations = get_recommendations(
        description="Considering acquiring a competitor to expand market share",
        domain="business",
        stakes="high",
        time_horizon="long",
        reversibility="irreversible"
    )
    
    print_recommendations(recommendations)
