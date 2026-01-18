"""
Bayesian Updater for Mental Models System

Implements Bayesian reasoning for updating beliefs based on evidence.
Essential for rational decision-making under uncertainty.

"It is remarkable how much long-term advantage people like us have gotten
by trying to be consistently not stupid, instead of trying to be very intelligent."
- Charlie Munger
"""

import logging
from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass, field
from datetime import datetime

from ..exceptions import InvalidInputError, validate_input

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


@dataclass
class BayesianUpdate:
    """Record of a Bayesian update."""
    prior: float
    likelihood_if_true: float
    likelihood_if_false: float
    posterior: float
    evidence_description: str
    timestamp: datetime = field(default_factory=datetime.now)


class BayesianUpdater:
    """
    Implements Bayesian updating for belief revision.
    
    Usage:
        updater = BayesianUpdater()
        
        # Start with prior belief
        prior = 0.3  # 30% chance product will succeed
        
        # Update based on evidence
        posterior = updater.update(
            prior=prior,
            likelihood_if_true=0.8,  # P(evidence | hypothesis true)
            likelihood_if_false=0.2   # P(evidence | hypothesis false)
        )
        
        # Continue updating with new evidence
        posterior2 = updater.update(
            prior=posterior,
            likelihood_if_true=0.9,
            likelihood_if_false=0.1
        )
    """
    
    def __init__(self):
        self.update_history: List[BayesianUpdate] = []
    
    def update(
        self,
        prior: float,
        likelihood_if_true: float,
        likelihood_if_false: float,
        evidence_description: str = ""
    ) -> float:
        """
        Update belief using Bayes' theorem.
        
        Bayes' Theorem:
        P(H|E) = P(E|H) * P(H) / P(E)
        
        Where:
        - P(H|E) = posterior probability (what we want)
        - P(E|H) = likelihood if hypothesis true
        - P(H) = prior probability
        - P(E) = total probability of evidence
        
        Args:
            prior: Prior probability (0-1)
            likelihood_if_true: P(evidence | hypothesis true)
            likelihood_if_false: P(evidence | hypothesis false)
            evidence_description: Description of the evidence
        
        Returns:
            Posterior probability (0-1)
        """
        # Validate inputs
        validate_input(
            0 <= prior <= 1,
            "Prior must be between 0 and 1",
            {"prior": prior}
        )
        validate_input(
            0 <= likelihood_if_true <= 1,
            "Likelihood if true must be between 0 and 1",
            {"likelihood_if_true": likelihood_if_true}
        )
        validate_input(
            0 <= likelihood_if_false <= 1,
            "Likelihood if false must be between 0 and 1",
            {"likelihood_if_false": likelihood_if_false}
        )
        
        # Calculate posterior using Bayes' theorem
        # P(H|E) = P(E|H) * P(H) / [P(E|H) * P(H) + P(E|~H) * P(~H)]
        
        numerator = likelihood_if_true * prior
        denominator = (likelihood_if_true * prior) + (likelihood_if_false * (1 - prior))
        
        # Handle edge case where denominator is 0
        if denominator == 0:
            posterior = prior
        else:
            posterior = numerator / denominator
        
        # Record update
        update_record = BayesianUpdate(
            prior=prior,
            likelihood_if_true=likelihood_if_true,
            likelihood_if_false=likelihood_if_false,
            posterior=posterior,
            evidence_description=evidence_description
        )
        self.update_history.append(update_record)
        
        logger.info(
            f"Bayesian update: {prior:.3f} -> {posterior:.3f} "
            f"(evidence: {evidence_description})"
        )
        
        return posterior
    
    def update_with_odds(
        self,
        prior_odds: float,
        likelihood_ratio: float,
        evidence_description: str = ""
    ) -> Tuple[float, float]:
        """
        Update using odds form of Bayes' theorem.
        
        Odds form: Posterior Odds = Prior Odds * Likelihood Ratio
        
        This is often more intuitive and easier to work with.
        
        Args:
            prior_odds: Prior odds (e.g., 2.0 means 2:1 odds)
            likelihood_ratio: P(E|H) / P(E|~H)
            evidence_description: Description of the evidence
        
        Returns:
            Tuple of (posterior_odds, posterior_probability)
        """
        validate_input(
            prior_odds > 0,
            "Prior odds must be positive",
            {"prior_odds": prior_odds}
        )
        validate_input(
            likelihood_ratio > 0,
            "Likelihood ratio must be positive",
            {"likelihood_ratio": likelihood_ratio}
        )
        
        # Calculate posterior odds
        posterior_odds = prior_odds * likelihood_ratio
        
        # Convert to probability
        posterior_prob = posterior_odds / (1 + posterior_odds)
        
        # Convert prior odds to probability for recording
        prior_prob = prior_odds / (1 + prior_odds)
        
        # Calculate individual likelihoods for recording
        # This is approximate, assuming likelihood_if_true ≈ 0.5 * likelihood_ratio
        likelihood_if_true = min(0.99, 0.5 * likelihood_ratio)
        likelihood_if_false = likelihood_if_true / likelihood_ratio if likelihood_ratio > 0 else 0.01
        
        # Record update
        update_record = BayesianUpdate(
            prior=prior_prob,
            likelihood_if_true=likelihood_if_true,
            likelihood_if_false=likelihood_if_false,
            posterior=posterior_prob,
            evidence_description=f"{evidence_description} (odds form)"
        )
        self.update_history.append(update_record)
        
        logger.info(
            f"Bayesian update (odds): {prior_odds:.2f} -> {posterior_odds:.2f} "
            f"(prob: {posterior_prob:.3f}, evidence: {evidence_description})"
        )
        
        return posterior_odds, posterior_prob
    
    def sequential_update(
        self,
        prior: float,
        evidence_list: List[Tuple[float, float, str]]
    ) -> float:
        """
        Perform sequential Bayesian updates with multiple pieces of evidence.
        
        Args:
            prior: Initial prior probability
            evidence_list: List of (likelihood_if_true, likelihood_if_false, description) tuples
        
        Returns:
            Final posterior probability
        """
        current_belief = prior
        
        for likelihood_true, likelihood_false, description in evidence_list:
            current_belief = self.update(
                prior=current_belief,
                likelihood_if_true=likelihood_true,
                likelihood_if_false=likelihood_false,
                evidence_description=description
            )
        
        return current_belief
    
    def get_update_history(self) -> List[Dict]:
        """Get history of all updates."""
        return [
            {
                "prior": u.prior,
                "likelihood_if_true": u.likelihood_if_true,
                "likelihood_if_false": u.likelihood_if_false,
                "posterior": u.posterior,
                "evidence": u.evidence_description,
                "timestamp": u.timestamp.isoformat()
            }
            for u in self.update_history
        ]
    
    def calculate_bayes_factor(
        self,
        likelihood_if_true: float,
        likelihood_if_false: float
    ) -> float:
        """
        Calculate Bayes factor (strength of evidence).
        
        Bayes Factor = P(E|H) / P(E|~H)
        
        Interpretation:
        - BF > 10: Strong evidence for hypothesis
        - BF 3-10: Moderate evidence for hypothesis
        - BF 1-3: Weak evidence for hypothesis
        - BF = 1: No evidence either way
        - BF < 1: Evidence against hypothesis
        
        Args:
            likelihood_if_true: P(evidence | hypothesis true)
            likelihood_if_false: P(evidence | hypothesis false)
        
        Returns:
            Bayes factor
        """
        if likelihood_if_false == 0:
            return float('inf') if likelihood_if_true > 0 else 1.0
        
        return likelihood_if_true / likelihood_if_false
    
    def interpret_bayes_factor(self, bayes_factor: float) -> str:
        """
        Interpret the strength of evidence from Bayes factor.
        
        Args:
            bayes_factor: Bayes factor value
        
        Returns:
            Interpretation string
        """
        if bayes_factor > 100:
            return "Decisive evidence for hypothesis"
        elif bayes_factor > 30:
            return "Very strong evidence for hypothesis"
        elif bayes_factor > 10:
            return "Strong evidence for hypothesis"
        elif bayes_factor > 3:
            return "Moderate evidence for hypothesis"
        elif bayes_factor > 1:
            return "Weak evidence for hypothesis"
        elif bayes_factor == 1:
            return "No evidence either way"
        elif bayes_factor > 0.33:
            return "Weak evidence against hypothesis"
        elif bayes_factor > 0.1:
            return "Moderate evidence against hypothesis"
        elif bayes_factor > 0.03:
            return "Strong evidence against hypothesis"
        else:
            return "Very strong evidence against hypothesis"
    
    @staticmethod
    def probability_to_odds(probability: float) -> float:
        """Convert probability to odds."""
        validate_input(
            0 < probability < 1,
            "Probability must be between 0 and 1 (exclusive)",
            {"probability": probability}
        )
        return probability / (1 - probability)
    
    @staticmethod
    def odds_to_probability(odds: float) -> float:
        """Convert odds to probability."""
        validate_input(
            odds > 0,
            "Odds must be positive",
            {"odds": odds}
        )
        return odds / (1 + odds)


def demonstrate_bayesian_reasoning():
    """
    Demonstrate Bayesian reasoning with a practical example.
    
    Example: Evaluating a new product launch
    """
    print("=== Bayesian Reasoning Example ===\n")
    
    updater = BayesianUpdater()
    
    # Initial belief: 30% chance product will succeed
    prior = 0.3
    print(f"Prior belief: {prior:.1%} chance of success\n")
    
    # Evidence 1: Positive customer surveys
    print("Evidence 1: 85% positive customer surveys")
    print("  - If product will succeed: 80% chance of positive surveys")
    print("  - If product will fail: 20% chance of positive surveys")
    posterior1 = updater.update(
        prior=prior,
        likelihood_if_true=0.8,
        likelihood_if_false=0.2,
        evidence_description="Positive customer surveys"
    )
    print(f"  → Updated belief: {posterior1:.1%}\n")
    
    # Evidence 2: Strong pre-orders
    print("Evidence 2: Pre-orders exceed expectations by 50%")
    print("  - If product will succeed: 90% chance of strong pre-orders")
    print("  - If product will fail: 10% chance of strong pre-orders")
    posterior2 = updater.update(
        prior=posterior1,
        likelihood_if_true=0.9,
        likelihood_if_false=0.1,
        evidence_description="Strong pre-orders"
    )
    print(f"  → Updated belief: {posterior2:.1%}\n")
    
    # Evidence 3: Competitor launches similar product
    print("Evidence 3: Major competitor launches similar product")
    print("  - If our product will succeed: 40% chance (competition is bad)")
    print("  - If our product will fail: 60% chance (validates market)")
    posterior3 = updater.update(
        prior=posterior2,
        likelihood_if_true=0.4,
        likelihood_if_false=0.6,
        evidence_description="Competitor launch"
    )
    print(f"  → Updated belief: {posterior3:.1%}\n")
    
    print(f"Final assessment: {posterior3:.1%} chance of success")
    print(f"Change from prior: {(posterior3 - prior):.1%} increase")


if __name__ == "__main__":
    demonstrate_bayesian_reasoning()
