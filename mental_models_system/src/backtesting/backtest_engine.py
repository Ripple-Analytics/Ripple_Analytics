"""
Backtesting Framework for Mental Model Combinations.

Tests mental model combinations against historical data to find
which combinations have the highest predictive power.

Inspired by Jim Simons' approach at Renaissance Technologies:
- Let the data reveal what works
- Systematic testing over intuition
- Statistical significance over anecdotes
"""

import json
import hashlib
from pathlib import Path
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Tuple, Callable
from dataclasses import dataclass, field
from enum import Enum
from itertools import combinations
import statistics
import random


class OutcomeType(Enum):
    SUCCESS = "success"
    PARTIAL_SUCCESS = "partial_success"
    NEUTRAL = "neutral"
    PARTIAL_FAILURE = "partial_failure"
    FAILURE = "failure"


class SignalType(Enum):
    BUY = "buy"
    SELL = "sell"
    HOLD = "hold"
    STRONG_BUY = "strong_buy"
    STRONG_SELL = "strong_sell"


@dataclass
class HistoricalDecision:
    """A historical decision with known outcome."""
    id: str
    timestamp: datetime
    context: Dict[str, Any]
    models_applied: List[int]
    signal: SignalType
    confidence: float  # 0-1
    outcome: OutcomeType
    return_pct: Optional[float] = None  # For investment decisions
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "id": self.id,
            "timestamp": self.timestamp.isoformat(),
            "context": self.context,
            "models_applied": self.models_applied,
            "signal": self.signal.value,
            "confidence": self.confidence,
            "outcome": self.outcome.value,
            "return_pct": self.return_pct,
            "metadata": self.metadata
        }
    
    @classmethod
    def from_dict(cls, data: dict) -> "HistoricalDecision":
        return cls(
            id=data["id"],
            timestamp=datetime.fromisoformat(data["timestamp"]),
            context=data.get("context", {}),
            models_applied=data.get("models_applied", []),
            signal=SignalType(data.get("signal", "hold")),
            confidence=data.get("confidence", 0.5),
            outcome=OutcomeType(data.get("outcome", "neutral")),
            return_pct=data.get("return_pct"),
            metadata=data.get("metadata", {})
        )


@dataclass
class BacktestResult:
    """Results from backtesting a model combination."""
    model_combination: Tuple[int, ...]
    model_names: List[str]
    total_decisions: int
    success_rate: float
    avg_return: Optional[float]
    sharpe_ratio: Optional[float]
    max_drawdown: Optional[float]
    win_rate: float
    profit_factor: Optional[float]
    avg_confidence: float
    calibration_score: float  # How well confidence predicts outcome
    statistical_significance: float  # p-value
    sample_decisions: List[str]  # IDs of sample decisions
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "model_combination": list(self.model_combination),
            "model_names": self.model_names,
            "total_decisions": self.total_decisions,
            "success_rate": self.success_rate,
            "avg_return": self.avg_return,
            "sharpe_ratio": self.sharpe_ratio,
            "max_drawdown": self.max_drawdown,
            "win_rate": self.win_rate,
            "profit_factor": self.profit_factor,
            "avg_confidence": self.avg_confidence,
            "calibration_score": self.calibration_score,
            "statistical_significance": self.statistical_significance,
            "sample_decisions": self.sample_decisions
        }


@dataclass
class SynergyResult:
    """Results showing synergy between model pairs."""
    model_a: int
    model_b: int
    model_a_name: str
    model_b_name: str
    combined_success_rate: float
    model_a_alone_rate: float
    model_b_alone_rate: float
    synergy_score: float  # How much better together vs alone
    sample_size: int
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "model_a": self.model_a,
            "model_b": self.model_b,
            "model_a_name": self.model_a_name,
            "model_b_name": self.model_b_name,
            "combined_success_rate": self.combined_success_rate,
            "model_a_alone_rate": self.model_a_alone_rate,
            "model_b_alone_rate": self.model_b_alone_rate,
            "synergy_score": self.synergy_score,
            "sample_size": self.sample_size
        }


class BacktestEngine:
    """
    Engine for backtesting mental model combinations.
    
    Usage:
        engine = BacktestEngine()
        
        # Load historical decisions
        engine.load_decisions("decisions.json")
        
        # Or add decisions programmatically
        engine.add_decision(decision)
        
        # Run backtest on specific combination
        result = engine.backtest_combination([1, 5, 23])
        
        # Find best combinations
        best = engine.find_best_combinations(min_models=2, max_models=5, top_n=10)
        
        # Find synergistic pairs
        synergies = engine.find_synergies()
    """
    
    def __init__(self, data_path: Optional[Path] = None):
        self.decisions: List[HistoricalDecision] = []
        self.model_names: Dict[int, str] = {}
        self.data_path = data_path
        
        # Load model names
        self._load_model_names()
    
    def _load_model_names(self):
        """Load model names from the mental models data."""
        models_path = Path(__file__).parent.parent.parent / "data" / "raw" / "mental_models_complete.json"
        
        if models_path.exists():
            with open(models_path) as f:
                data = json.load(f)
            
            for model in data.get("mental_models", []):
                self.model_names[model["id"]] = model["name"]
    
    def add_decision(self, decision: HistoricalDecision):
        """Add a historical decision to the dataset."""
        self.decisions.append(decision)
    
    def load_decisions(self, path: Path):
        """Load historical decisions from a JSON file."""
        with open(path) as f:
            data = json.load(f)
        
        for item in data.get("decisions", []):
            self.decisions.append(HistoricalDecision.from_dict(item))
    
    def save_decisions(self, path: Path):
        """Save historical decisions to a JSON file."""
        data = {
            "decisions": [d.to_dict() for d in self.decisions],
            "metadata": {
                "total": len(self.decisions),
                "saved_at": datetime.now().isoformat()
            }
        }
        
        with open(path, "w") as f:
            json.dump(data, f, indent=2)
    
    def backtest_combination(
        self, 
        model_ids: List[int],
        min_confidence: float = 0.0
    ) -> BacktestResult:
        """
        Backtest a specific combination of mental models.
        
        Args:
            model_ids: List of model IDs to test together
            min_confidence: Minimum confidence threshold
            
        Returns:
            BacktestResult with performance metrics
        """
        model_set = set(model_ids)
        
        # Find decisions that used ALL models in the combination
        matching_decisions = [
            d for d in self.decisions
            if model_set.issubset(set(d.models_applied))
            and d.confidence >= min_confidence
        ]
        
        if not matching_decisions:
            return BacktestResult(
                model_combination=tuple(model_ids),
                model_names=[self.model_names.get(m, f"Model {m}") for m in model_ids],
                total_decisions=0,
                success_rate=0.0,
                avg_return=None,
                sharpe_ratio=None,
                max_drawdown=None,
                win_rate=0.0,
                profit_factor=None,
                avg_confidence=0.0,
                calibration_score=0.0,
                statistical_significance=1.0,
                sample_decisions=[]
            )
        
        # Calculate metrics
        successes = sum(
            1 for d in matching_decisions 
            if d.outcome in [OutcomeType.SUCCESS, OutcomeType.PARTIAL_SUCCESS]
        )
        
        success_rate = successes / len(matching_decisions)
        
        # Calculate returns if available
        returns = [d.return_pct for d in matching_decisions if d.return_pct is not None]
        avg_return = statistics.mean(returns) if returns else None
        
        # Calculate Sharpe ratio (assuming risk-free rate of 0 for simplicity)
        sharpe_ratio = None
        if returns and len(returns) > 1:
            std_dev = statistics.stdev(returns)
            if std_dev > 0:
                sharpe_ratio = (statistics.mean(returns)) / std_dev
        
        # Calculate max drawdown
        max_drawdown = None
        if returns:
            cumulative = []
            total = 0
            for r in returns:
                total += r
                cumulative.append(total)
            
            peak = cumulative[0]
            max_dd = 0
            for value in cumulative:
                if value > peak:
                    peak = value
                dd = (peak - value) / peak if peak > 0 else 0
                max_dd = max(max_dd, dd)
            max_drawdown = max_dd
        
        # Win rate (positive returns)
        wins = sum(1 for r in returns if r and r > 0) if returns else 0
        win_rate = wins / len(returns) if returns else success_rate
        
        # Profit factor
        profit_factor = None
        if returns:
            gross_profit = sum(r for r in returns if r > 0)
            gross_loss = abs(sum(r for r in returns if r < 0))
            if gross_loss > 0:
                profit_factor = gross_profit / gross_loss
        
        # Average confidence
        avg_confidence = statistics.mean(d.confidence for d in matching_decisions)
        
        # Calibration score (how well confidence predicts success)
        calibration_score = self._calculate_calibration(matching_decisions)
        
        # Statistical significance (simplified chi-square test)
        statistical_significance = self._calculate_significance(
            successes, len(matching_decisions)
        )
        
        return BacktestResult(
            model_combination=tuple(model_ids),
            model_names=[self.model_names.get(m, f"Model {m}") for m in model_ids],
            total_decisions=len(matching_decisions),
            success_rate=success_rate,
            avg_return=avg_return,
            sharpe_ratio=sharpe_ratio,
            max_drawdown=max_drawdown,
            win_rate=win_rate,
            profit_factor=profit_factor,
            avg_confidence=avg_confidence,
            calibration_score=calibration_score,
            statistical_significance=statistical_significance,
            sample_decisions=[d.id for d in matching_decisions[:5]]
        )
    
    def _calculate_calibration(self, decisions: List[HistoricalDecision]) -> float:
        """Calculate how well confidence predicts outcomes."""
        if not decisions:
            return 0.0
        
        # Bin decisions by confidence
        bins = {i/10: [] for i in range(10)}
        
        for d in decisions:
            bin_key = int(d.confidence * 10) / 10
            bin_key = min(bin_key, 0.9)  # Cap at 0.9
            
            is_success = d.outcome in [OutcomeType.SUCCESS, OutcomeType.PARTIAL_SUCCESS]
            bins[bin_key].append(is_success)
        
        # Calculate calibration error
        total_error = 0
        total_samples = 0
        
        for confidence, outcomes in bins.items():
            if outcomes:
                actual_rate = sum(outcomes) / len(outcomes)
                expected_rate = confidence + 0.05  # Midpoint of bin
                total_error += abs(actual_rate - expected_rate) * len(outcomes)
                total_samples += len(outcomes)
        
        if total_samples == 0:
            return 0.0
        
        # Return 1 - normalized error (higher is better)
        return 1 - (total_error / total_samples)
    
    def _calculate_significance(self, successes: int, total: int) -> float:
        """Calculate statistical significance (simplified)."""
        if total < 10:
            return 1.0  # Not enough data
        
        # Expected under null hypothesis (50% success rate)
        expected = total * 0.5
        
        # Chi-square approximation
        chi_sq = ((successes - expected) ** 2) / expected
        chi_sq += ((total - successes - expected) ** 2) / expected
        
        # Simplified p-value approximation
        # For chi-square with 1 df: p < 0.05 when chi_sq > 3.84
        if chi_sq > 10.83:
            return 0.001
        elif chi_sq > 6.63:
            return 0.01
        elif chi_sq > 3.84:
            return 0.05
        elif chi_sq > 2.71:
            return 0.10
        else:
            return 0.5
    
    def find_best_combinations(
        self,
        min_models: int = 2,
        max_models: int = 5,
        top_n: int = 10,
        min_decisions: int = 10,
        sort_by: str = "success_rate"
    ) -> List[BacktestResult]:
        """
        Find the best performing model combinations.
        
        Args:
            min_models: Minimum number of models in combination
            max_models: Maximum number of models in combination
            top_n: Number of top combinations to return
            min_decisions: Minimum decisions required for consideration
            sort_by: Metric to sort by (success_rate, sharpe_ratio, win_rate)
            
        Returns:
            List of top BacktestResults
        """
        # Get all unique models used in decisions
        all_models = set()
        for d in self.decisions:
            all_models.update(d.models_applied)
        
        all_models = list(all_models)
        results = []
        
        # Test all combinations
        for size in range(min_models, min(max_models + 1, len(all_models) + 1)):
            for combo in combinations(all_models, size):
                result = self.backtest_combination(list(combo))
                
                if result.total_decisions >= min_decisions:
                    results.append(result)
        
        # Sort by specified metric
        if sort_by == "sharpe_ratio":
            results.sort(
                key=lambda r: r.sharpe_ratio if r.sharpe_ratio else -999,
                reverse=True
            )
        elif sort_by == "win_rate":
            results.sort(key=lambda r: r.win_rate, reverse=True)
        else:
            results.sort(key=lambda r: r.success_rate, reverse=True)
        
        return results[:top_n]
    
    def find_synergies(self, min_decisions: int = 5) -> List[SynergyResult]:
        """
        Find model pairs that work better together than alone.
        
        Returns:
            List of SynergyResults sorted by synergy score
        """
        # Get all unique models
        all_models = set()
        for d in self.decisions:
            all_models.update(d.models_applied)
        
        all_models = list(all_models)
        synergies = []
        
        # Test all pairs
        for i, model_a in enumerate(all_models):
            for model_b in all_models[i+1:]:
                # Get success rate when both are used
                combined = [
                    d for d in self.decisions
                    if model_a in d.models_applied and model_b in d.models_applied
                ]
                
                if len(combined) < min_decisions:
                    continue
                
                combined_success = sum(
                    1 for d in combined
                    if d.outcome in [OutcomeType.SUCCESS, OutcomeType.PARTIAL_SUCCESS]
                ) / len(combined)
                
                # Get success rate for model A alone (without B)
                a_alone = [
                    d for d in self.decisions
                    if model_a in d.models_applied and model_b not in d.models_applied
                ]
                
                a_alone_rate = 0.5
                if len(a_alone) >= min_decisions:
                    a_alone_rate = sum(
                        1 for d in a_alone
                        if d.outcome in [OutcomeType.SUCCESS, OutcomeType.PARTIAL_SUCCESS]
                    ) / len(a_alone)
                
                # Get success rate for model B alone (without A)
                b_alone = [
                    d for d in self.decisions
                    if model_b in d.models_applied and model_a not in d.models_applied
                ]
                
                b_alone_rate = 0.5
                if len(b_alone) >= min_decisions:
                    b_alone_rate = sum(
                        1 for d in b_alone
                        if d.outcome in [OutcomeType.SUCCESS, OutcomeType.PARTIAL_SUCCESS]
                    ) / len(b_alone)
                
                # Calculate synergy (how much better together)
                expected_combined = (a_alone_rate + b_alone_rate) / 2
                synergy_score = combined_success - expected_combined
                
                synergies.append(SynergyResult(
                    model_a=model_a,
                    model_b=model_b,
                    model_a_name=self.model_names.get(model_a, f"Model {model_a}"),
                    model_b_name=self.model_names.get(model_b, f"Model {model_b}"),
                    combined_success_rate=combined_success,
                    model_a_alone_rate=a_alone_rate,
                    model_b_alone_rate=b_alone_rate,
                    synergy_score=synergy_score,
                    sample_size=len(combined)
                ))
        
        # Sort by synergy score
        synergies.sort(key=lambda s: s.synergy_score, reverse=True)
        
        return synergies
    
    def generate_report(self) -> str:
        """Generate a comprehensive backtest report."""
        lines = ["# Mental Model Backtest Report", ""]
        lines.append(f"**Generated**: {datetime.now().isoformat()}")
        lines.append(f"**Total Decisions**: {len(self.decisions)}")
        lines.append("")
        
        # Overall statistics
        lines.append("## Overall Statistics")
        lines.append("")
        
        if self.decisions:
            success_count = sum(
                1 for d in self.decisions
                if d.outcome in [OutcomeType.SUCCESS, OutcomeType.PARTIAL_SUCCESS]
            )
            overall_rate = success_count / len(self.decisions)
            lines.append(f"- Overall Success Rate: {overall_rate:.1%}")
            
            returns = [d.return_pct for d in self.decisions if d.return_pct]
            if returns:
                lines.append(f"- Average Return: {statistics.mean(returns):.2%}")
                lines.append(f"- Median Return: {statistics.median(returns):.2%}")
        
        lines.append("")
        
        # Top combinations
        lines.append("## Top Model Combinations")
        lines.append("")
        
        best = self.find_best_combinations(min_models=2, max_models=4, top_n=10)
        
        if best:
            lines.append("| Rank | Models | Success Rate | Decisions | Sharpe |")
            lines.append("|------|--------|--------------|-----------|--------|")
            
            for i, result in enumerate(best, 1):
                models_str = " + ".join(result.model_names[:3])
                if len(result.model_names) > 3:
                    models_str += f" +{len(result.model_names)-3} more"
                
                sharpe = f"{result.sharpe_ratio:.2f}" if result.sharpe_ratio else "N/A"
                
                lines.append(
                    f"| {i} | {models_str} | {result.success_rate:.1%} | "
                    f"{result.total_decisions} | {sharpe} |"
                )
        
        lines.append("")
        
        # Top synergies
        lines.append("## Top Model Synergies")
        lines.append("")
        
        synergies = self.find_synergies()[:10]
        
        if synergies:
            lines.append("| Model A | Model B | Combined | A Alone | B Alone | Synergy |")
            lines.append("|---------|---------|----------|---------|---------|---------|")
            
            for s in synergies:
                lines.append(
                    f"| {s.model_a_name[:20]} | {s.model_b_name[:20]} | "
                    f"{s.combined_success_rate:.1%} | {s.model_a_alone_rate:.1%} | "
                    f"{s.model_b_alone_rate:.1%} | {s.synergy_score:+.1%} |"
                )
        
        return "\n".join(lines)
    
    def monte_carlo_simulation(
        self,
        model_ids: List[int],
        num_simulations: int = 1000,
        sample_size: int = 100
    ) -> Dict[str, Any]:
        """
        Run Monte Carlo simulation to estimate confidence intervals.
        
        Args:
            model_ids: Model combination to test
            num_simulations: Number of simulations
            sample_size: Size of each sample
            
        Returns:
            Dictionary with simulation results
        """
        model_set = set(model_ids)
        
        # Get matching decisions
        matching = [
            d for d in self.decisions
            if model_set.issubset(set(d.models_applied))
        ]
        
        if len(matching) < sample_size:
            sample_size = len(matching)
        
        if sample_size < 10:
            return {"error": "Insufficient data for simulation"}
        
        success_rates = []
        returns = []
        
        for _ in range(num_simulations):
            sample = random.choices(matching, k=sample_size)
            
            successes = sum(
                1 for d in sample
                if d.outcome in [OutcomeType.SUCCESS, OutcomeType.PARTIAL_SUCCESS]
            )
            success_rates.append(successes / sample_size)
            
            sample_returns = [d.return_pct for d in sample if d.return_pct]
            if sample_returns:
                returns.append(statistics.mean(sample_returns))
        
        return {
            "model_combination": model_ids,
            "num_simulations": num_simulations,
            "sample_size": sample_size,
            "success_rate": {
                "mean": statistics.mean(success_rates),
                "std": statistics.stdev(success_rates),
                "ci_95_low": sorted(success_rates)[int(0.025 * num_simulations)],
                "ci_95_high": sorted(success_rates)[int(0.975 * num_simulations)]
            },
            "returns": {
                "mean": statistics.mean(returns) if returns else None,
                "std": statistics.stdev(returns) if len(returns) > 1 else None,
                "ci_95_low": sorted(returns)[int(0.025 * len(returns))] if returns else None,
                "ci_95_high": sorted(returns)[int(0.975 * len(returns))] if returns else None
            } if returns else None
        }


def create_sample_decisions(num_decisions: int = 100) -> List[HistoricalDecision]:
    """Create sample historical decisions for testing."""
    decisions = []
    
    # Sample model combinations that tend to work well
    good_combos = [
        [1, 5, 23],  # Incentives + Social Proof + Network Effects
        [3, 7, 15],  # Confirmation Bias + Availability + Compounding
        [2, 8, 19],  # Social Proof + Reciprocity + Moats
    ]
    
    # Sample model combinations that tend to fail
    bad_combos = [
        [4, 6],  # Reciprocity + Commitment alone
        [10, 11],  # Single-factor combos
    ]
    
    for i in range(num_decisions):
        # Randomly select combo type
        if random.random() < 0.6:
            models = random.choice(good_combos)
            base_success_prob = 0.7
        else:
            models = random.choice(bad_combos)
            base_success_prob = 0.4
        
        # Add some random models
        extra = random.sample(range(1, 30), random.randint(0, 2))
        models = list(set(models + extra))
        
        # Determine outcome
        confidence = random.uniform(0.5, 0.95)
        success_prob = base_success_prob * (0.8 + 0.4 * confidence)
        
        if random.random() < success_prob:
            outcome = random.choice([OutcomeType.SUCCESS, OutcomeType.PARTIAL_SUCCESS])
            return_pct = random.uniform(0.05, 0.50)
        else:
            outcome = random.choice([OutcomeType.FAILURE, OutcomeType.PARTIAL_FAILURE, OutcomeType.NEUTRAL])
            return_pct = random.uniform(-0.30, 0.05)
        
        decision = HistoricalDecision(
            id=f"decision_{i:04d}",
            timestamp=datetime.now() - timedelta(days=random.randint(1, 365)),
            context={"type": random.choice(["investment", "business", "personal"])},
            models_applied=models,
            signal=random.choice(list(SignalType)),
            confidence=confidence,
            outcome=outcome,
            return_pct=return_pct
        )
        
        decisions.append(decision)
    
    return decisions


if __name__ == "__main__":
    # Test the backtesting engine
    engine = BacktestEngine()
    
    # Create sample data
    print("Creating sample decisions...")
    sample_decisions = create_sample_decisions(200)
    
    for d in sample_decisions:
        engine.add_decision(d)
    
    print(f"Loaded {len(engine.decisions)} decisions")
    
    # Find best combinations
    print("\nFinding best combinations...")
    best = engine.find_best_combinations(min_models=2, max_models=4, top_n=5)
    
    print("\nTop 5 Model Combinations:")
    for i, result in enumerate(best, 1):
        print(f"{i}. {result.model_names}")
        print(f"   Success Rate: {result.success_rate:.1%}")
        print(f"   Decisions: {result.total_decisions}")
        print()
    
    # Find synergies
    print("\nTop Synergies:")
    synergies = engine.find_synergies()[:5]
    
    for s in synergies:
        print(f"- {s.model_a_name} + {s.model_b_name}")
        print(f"  Synergy: {s.synergy_score:+.1%}")
    
    # Generate report
    print("\n" + "="*50)
    print(engine.generate_report())
