"""
Failure Mode Search and Recommendation Engine

Provides intelligent search and recommendations for failure modes
based on context, mental models, and historical patterns.
"""

import json
from pathlib import Path
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass, field
from collections import defaultdict
import re


@dataclass
class FailureModeMatch:
    """A matched failure mode with relevance score"""
    model_name: str
    mode_name: str
    description: str
    real_world_case: str
    warning_signs: List[str]
    safeguards: List[str]
    relevance_score: float
    match_reason: str


@dataclass
class RiskAssessment:
    """Risk assessment for a decision or situation"""
    overall_risk_score: float  # 0-1
    top_failure_modes: List[FailureModeMatch]
    risk_by_category: Dict[str, float]
    recommended_safeguards: List[str]
    warning_signs_to_watch: List[str]


class FailureModeSearchEngine:
    """
    Intelligent search engine for failure modes.
    
    Features:
    - Full-text search across all failure modes
    - Context-aware recommendations
    - Risk scoring based on multiple models
    - Safeguard recommendations
    """
    
    def __init__(self, data_dir: Optional[Path] = None):
        if data_dir is None:
            data_dir = Path(__file__).parent.parent.parent / 'data' / 'raw'
        self.data_dir = Path(data_dir)
        
        # Load all failure modes
        self.failure_modes: Dict[str, List[Dict]] = {}
        self.model_to_category: Dict[str, str] = {}
        self._load_all_failure_modes()
        
        # Build search index
        self._build_search_index()
    
    def _load_all_failure_modes(self):
        """Load all failure mode JSON files"""
        json_files = list(self.data_dir.glob('failure_modes*.json'))
        
        for filepath in json_files:
            try:
                with open(filepath, 'r') as f:
                    data = json.load(f)
                
                category = data.get('category', 'Unknown')
                
                for model in data.get('models', []):
                    model_name = model.get('name', '')
                    if model_name:
                        self.failure_modes[model_name] = model.get('failure_modes', [])
                        self.model_to_category[model_name] = category
            except Exception as e:
                print(f"Error loading {filepath}: {e}")
    
    def _build_search_index(self):
        """Build inverted index for fast search"""
        self.word_index: Dict[str, List[Tuple[str, str, float]]] = defaultdict(list)
        
        for model_name, modes in self.failure_modes.items():
            for mode in modes:
                # Index mode name (high weight)
                for word in self._tokenize(mode.get('mode', '')):
                    self.word_index[word].append((model_name, mode.get('mode', ''), 1.0))
                
                # Index description (medium weight)
                for word in self._tokenize(mode.get('description', '')):
                    self.word_index[word].append((model_name, mode.get('mode', ''), 0.7))
                
                # Index real world case (medium weight)
                for word in self._tokenize(mode.get('real_world_case', '')):
                    self.word_index[word].append((model_name, mode.get('mode', ''), 0.6))
                
                # Index warning signs (high weight)
                for sign in mode.get('warning_signs', []):
                    for word in self._tokenize(sign):
                        self.word_index[word].append((model_name, mode.get('mode', ''), 0.9))
                
                # Index safeguards (medium weight)
                for safeguard in mode.get('safeguards', []):
                    for word in self._tokenize(safeguard):
                        self.word_index[word].append((model_name, mode.get('mode', ''), 0.5))
    
    def _tokenize(self, text: str) -> List[str]:
        """Tokenize text into searchable words"""
        text = text.lower()
        words = re.findall(r'\b\w+\b', text)
        # Filter out common stop words
        stop_words = {'the', 'a', 'an', 'is', 'are', 'was', 'were', 'be', 'been', 
                      'being', 'have', 'has', 'had', 'do', 'does', 'did', 'will',
                      'would', 'could', 'should', 'may', 'might', 'must', 'shall',
                      'can', 'to', 'of', 'in', 'for', 'on', 'with', 'at', 'by',
                      'from', 'as', 'into', 'through', 'during', 'before', 'after',
                      'above', 'below', 'between', 'under', 'again', 'further',
                      'then', 'once', 'here', 'there', 'when', 'where', 'why',
                      'how', 'all', 'each', 'few', 'more', 'most', 'other', 'some',
                      'such', 'no', 'nor', 'not', 'only', 'own', 'same', 'so',
                      'than', 'too', 'very', 'just', 'and', 'but', 'if', 'or',
                      'because', 'until', 'while', 'this', 'that', 'these', 'those'}
        return [w for w in words if w not in stop_words and len(w) > 2]
    
    def search(self, query: str, limit: int = 10) -> List[FailureModeMatch]:
        """
        Search for failure modes matching the query.
        
        Args:
            query: Search query text
            limit: Maximum number of results
            
        Returns:
            List of matching failure modes with relevance scores
        """
        query_words = self._tokenize(query)
        
        # Score each (model, mode) pair
        scores: Dict[Tuple[str, str], float] = defaultdict(float)
        match_reasons: Dict[Tuple[str, str], List[str]] = defaultdict(list)
        
        for word in query_words:
            if word in self.word_index:
                for model_name, mode_name, weight in self.word_index[word]:
                    key = (model_name, mode_name)
                    scores[key] += weight
                    if word not in match_reasons[key]:
                        match_reasons[key].append(word)
        
        # Sort by score and get top results
        sorted_results = sorted(scores.items(), key=lambda x: x[1], reverse=True)[:limit]
        
        # Build result objects
        results = []
        for (model_name, mode_name), score in sorted_results:
            mode_data = self._get_mode_data(model_name, mode_name)
            if mode_data:
                results.append(FailureModeMatch(
                    model_name=model_name,
                    mode_name=mode_name,
                    description=mode_data.get('description', ''),
                    real_world_case=mode_data.get('real_world_case', ''),
                    warning_signs=mode_data.get('warning_signs', []),
                    safeguards=mode_data.get('safeguards', []),
                    relevance_score=min(score / len(query_words), 1.0) if query_words else 0,
                    match_reason=f"Matched: {', '.join(match_reasons[(model_name, mode_name)])}"
                ))
        
        return results
    
    def _get_mode_data(self, model_name: str, mode_name: str) -> Optional[Dict]:
        """Get failure mode data by model and mode name"""
        modes = self.failure_modes.get(model_name, [])
        for mode in modes:
            if mode.get('mode') == mode_name:
                return mode
        return None
    
    def get_failure_modes_for_model(self, model_name: str) -> List[Dict]:
        """Get all failure modes for a specific mental model"""
        return self.failure_modes.get(model_name, [])
    
    def get_failure_modes_for_models(self, model_names: List[str]) -> Dict[str, List[Dict]]:
        """Get failure modes for multiple mental models"""
        return {name: self.get_failure_modes_for_model(name) for name in model_names}
    
    def assess_risk(self, 
                    context: str, 
                    models_in_use: Optional[List[str]] = None) -> RiskAssessment:
        """
        Assess risk for a given context and set of mental models.
        
        Args:
            context: Description of the decision/situation
            models_in_use: Mental models being applied (optional)
            
        Returns:
            Risk assessment with scores and recommendations
        """
        # Search for relevant failure modes
        relevant_modes = self.search(context, limit=20)
        
        # If specific models are in use, add their failure modes
        if models_in_use:
            for model_name in models_in_use:
                modes = self.get_failure_modes_for_model(model_name)
                for mode in modes:
                    # Check if already in results
                    existing = [m for m in relevant_modes 
                               if m.model_name == model_name and m.mode_name == mode.get('mode')]
                    if not existing:
                        relevant_modes.append(FailureModeMatch(
                            model_name=model_name,
                            mode_name=mode.get('mode', ''),
                            description=mode.get('description', ''),
                            real_world_case=mode.get('real_world_case', ''),
                            warning_signs=mode.get('warning_signs', []),
                            safeguards=mode.get('safeguards', []),
                            relevance_score=0.8,  # High relevance for explicitly used models
                            match_reason="Model explicitly in use"
                        ))
        
        # Calculate risk by category
        risk_by_category: Dict[str, List[float]] = defaultdict(list)
        for mode in relevant_modes:
            category = self.model_to_category.get(mode.model_name, 'Unknown')
            risk_by_category[category].append(mode.relevance_score)
        
        category_risks = {
            cat: sum(scores) / len(scores) if scores else 0 
            for cat, scores in risk_by_category.items()
        }
        
        # Calculate overall risk score
        if relevant_modes:
            # Weight by relevance
            overall_risk = sum(m.relevance_score for m in relevant_modes[:10]) / 10
        else:
            overall_risk = 0.1  # Base risk
        
        # Collect unique safeguards and warning signs
        all_safeguards = []
        all_warnings = []
        for mode in relevant_modes[:10]:
            all_safeguards.extend(mode.safeguards)
            all_warnings.extend(mode.warning_signs)
        
        # Deduplicate and rank by frequency
        safeguard_counts = defaultdict(int)
        for s in all_safeguards:
            safeguard_counts[s] += 1
        top_safeguards = sorted(safeguard_counts.keys(), 
                                key=lambda x: safeguard_counts[x], 
                                reverse=True)[:10]
        
        warning_counts = defaultdict(int)
        for w in all_warnings:
            warning_counts[w] += 1
        top_warnings = sorted(warning_counts.keys(), 
                             key=lambda x: warning_counts[x], 
                             reverse=True)[:10]
        
        return RiskAssessment(
            overall_risk_score=min(overall_risk, 1.0),
            top_failure_modes=relevant_modes[:10],
            risk_by_category=category_risks,
            recommended_safeguards=top_safeguards,
            warning_signs_to_watch=top_warnings
        )
    
    def get_safeguards_for_decision(self, 
                                     decision_description: str,
                                     models_applied: List[str]) -> Dict[str, List[str]]:
        """
        Get recommended safeguards for a specific decision.
        
        Args:
            decision_description: Description of the decision
            models_applied: Mental models being applied
            
        Returns:
            Dictionary mapping model names to recommended safeguards
        """
        safeguards = {}
        
        for model_name in models_applied:
            model_safeguards = []
            modes = self.get_failure_modes_for_model(model_name)
            
            for mode in modes:
                model_safeguards.extend(mode.get('safeguards', []))
            
            # Deduplicate
            safeguards[model_name] = list(set(model_safeguards))
        
        return safeguards
    
    def find_similar_cases(self, situation: str, limit: int = 5) -> List[Dict]:
        """
        Find historical cases similar to the current situation.
        
        Args:
            situation: Description of current situation
            limit: Maximum number of cases to return
            
        Returns:
            List of similar historical cases with failure modes
        """
        cases = []
        
        # Search for relevant failure modes
        matches = self.search(situation, limit=limit * 2)
        
        for match in matches:
            if match.real_world_case:
                cases.append({
                    'model': match.model_name,
                    'failure_mode': match.mode_name,
                    'case': match.real_world_case,
                    'warning_signs': match.warning_signs,
                    'safeguards': match.safeguards,
                    'relevance': match.relevance_score
                })
        
        # Deduplicate by case description
        seen_cases = set()
        unique_cases = []
        for case in cases:
            if case['case'] not in seen_cases:
                seen_cases.add(case['case'])
                unique_cases.append(case)
        
        return unique_cases[:limit]
    
    def get_statistics(self) -> Dict:
        """Get statistics about the failure modes database"""
        total_modes = sum(len(modes) for modes in self.failure_modes.values())
        
        category_counts = defaultdict(int)
        for model, category in self.model_to_category.items():
            category_counts[category] += len(self.failure_modes.get(model, []))
        
        return {
            'total_models': len(self.failure_modes),
            'total_failure_modes': total_modes,
            'average_modes_per_model': total_modes / len(self.failure_modes) if self.failure_modes else 0,
            'modes_by_category': dict(category_counts),
            'unique_words_indexed': len(self.word_index)
        }


# Convenience functions
def search_failure_modes(query: str, limit: int = 10) -> List[FailureModeMatch]:
    """Quick search for failure modes"""
    engine = FailureModeSearchEngine()
    return engine.search(query, limit)


def assess_decision_risk(context: str, models: Optional[List[str]] = None) -> RiskAssessment:
    """Quick risk assessment for a decision"""
    engine = FailureModeSearchEngine()
    return engine.assess_risk(context, models)


def get_safeguards(models: List[str]) -> Dict[str, List[str]]:
    """Get safeguards for a list of mental models"""
    engine = FailureModeSearchEngine()
    return engine.get_safeguards_for_decision("", models)


if __name__ == "__main__":
    # Test the search engine
    engine = FailureModeSearchEngine()
    
    print("=" * 60)
    print("FAILURE MODE SEARCH ENGINE")
    print("=" * 60)
    
    # Print statistics
    stats = engine.get_statistics()
    print(f"\nDatabase Statistics:")
    print(f"  Total models: {stats['total_models']}")
    print(f"  Total failure modes: {stats['total_failure_modes']}")
    print(f"  Average modes per model: {stats['average_modes_per_model']:.1f}")
    print(f"  Unique words indexed: {stats['unique_words_indexed']}")
    
    # Test search
    print(f"\n\nSearch: 'overconfidence bias'")
    results = engine.search("overconfidence bias", limit=5)
    for r in results:
        print(f"  [{r.relevance_score:.2f}] {r.model_name}: {r.mode_name}")
        print(f"         {r.description[:60]}...")
    
    # Test risk assessment
    print(f"\n\nRisk Assessment: 'Investing in a hot tech startup'")
    assessment = engine.assess_risk("Investing in a hot tech startup with network effects")
    print(f"  Overall risk score: {assessment.overall_risk_score:.2f}")
    print(f"  Top warning signs:")
    for sign in assessment.warning_signs_to_watch[:5]:
        print(f"    - {sign}")
    print(f"  Top safeguards:")
    for safeguard in assessment.recommended_safeguards[:5]:
        print(f"    - {safeguard}")
