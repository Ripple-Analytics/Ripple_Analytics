#!/usr/bin/env python3
"""
Bayesian Analysis Module
Advanced Bayesian inference for mental models effectiveness.

Inspired by Renaissance Technologies' probabilistic approach to decision-making.
"""

import os
import sys
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import dataclass
import numpy as np
import pandas as pd
from scipy import stats
from scipy.stats import beta, gamma, norm
import psycopg2

sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
from config.settings import settings


@dataclass
class BayesianResult:
    """Bayesian inference result."""
    prior_mean: float
    prior_std: float
    posterior_mean: float
    posterior_std: float
    credible_interval_95: Tuple[float, float]
    bayes_factor: Optional[float] = None


@dataclass
class ModelEffectiveness:
    """Mental model effectiveness metrics."""
    model_name: str
    success_rate: float
    confidence_lower: float
    confidence_upper: float
    n_applications: int
    avg_severity_reduction: float
    bayesian_score: float


class BayesianAnalyzer:
    """
    Bayesian analysis for mental models effectiveness.
    
    Principles:
    - Jim Simons: Use probabilistic reasoning, not deterministic predictions
    - Ray Dalio: Update beliefs based on evidence (Bayesian updating)
    - Charlie Munger: Seek disconfirming evidence to refine models
    """
    
    def __init__(self):
        self.conn = None
        self.random_seed = settings.analysis.random_seed
        np.random.seed(self.random_seed)
    
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
        """Close database connection."""
        if self.conn:
            self.conn.close()
    
    def beta_binomial_inference(
        self,
        successes: int,
        trials: int,
        prior_alpha: float = 1.0,
        prior_beta: float = 1.0
    ) -> BayesianResult:
        """
        Perform Beta-Binomial Bayesian inference.
        
        Used for estimating success rates of mental models.
        
        Args:
            successes: Number of successful applications
            trials: Total number of applications
            prior_alpha: Prior alpha parameter (pseudo-successes)
            prior_beta: Prior beta parameter (pseudo-failures)
            
        Returns:
            BayesianResult with posterior distribution
        """
        # Prior distribution
        prior_mean = prior_alpha / (prior_alpha + prior_beta)
        prior_std = np.sqrt(
            (prior_alpha * prior_beta) / 
            ((prior_alpha + prior_beta)**2 * (prior_alpha + prior_beta + 1))
        )
        
        # Posterior distribution (Beta conjugate prior)
        posterior_alpha = prior_alpha + successes
        posterior_beta = prior_beta + (trials - successes)
        
        posterior_mean = posterior_alpha / (posterior_alpha + posterior_beta)
        posterior_std = np.sqrt(
            (posterior_alpha * posterior_beta) / 
            ((posterior_alpha + posterior_beta)**2 * (posterior_alpha + posterior_beta + 1))
        )
        
        # 95% credible interval
        posterior_dist = beta(posterior_alpha, posterior_beta)
        ci_lower = posterior_dist.ppf(0.025)
        ci_upper = posterior_dist.ppf(0.975)
        
        return BayesianResult(
            prior_mean=prior_mean,
            prior_std=prior_std,
            posterior_mean=posterior_mean,
            posterior_std=posterior_std,
            credible_interval_95=(ci_lower, ci_upper)
        )
    
    def calculate_model_effectiveness(
        self,
        model_name: str,
        threshold_severity: float = 0.5
    ) -> ModelEffectiveness:
        """
        Calculate effectiveness of a mental model using Bayesian inference.
        
        Effectiveness is measured by:
        - Success rate (cases where severity was reduced)
        - Confidence intervals
        - Average severity reduction
        
        Args:
            model_name: Name of the mental model
            threshold_severity: Severity threshold for "success"
            
        Returns:
            ModelEffectiveness object
        """
        query = """
            SELECT 
                cs.severity,
                pm.effect_size,
                cs.financial_impact
            FROM planck_matrix pm
            JOIN case_studies cs ON pm.case_id = cs.id
            WHERE pm.model_name = %s
        """
        
        df = pd.read_sql_query(query, self.conn, params=(model_name,))
        
        if len(df) == 0:
            return ModelEffectiveness(
                model_name=model_name,
                success_rate=0.0,
                confidence_lower=0.0,
                confidence_upper=0.0,
                n_applications=0,
                avg_severity_reduction=0.0,
                bayesian_score=0.0
            )
        
        # Define "success" as cases where effect_size > threshold
        successes = (df['effect_size'] > threshold_severity).sum()
        trials = len(df)
        
        # Bayesian inference with weakly informative prior
        result = self.beta_binomial_inference(
            successes=successes,
            trials=trials,
            prior_alpha=2.0,  # Weakly informative prior
            prior_beta=2.0
        )
        
        # Calculate average severity reduction
        avg_severity_reduction = df['effect_size'].mean()
        
        # Bayesian score: posterior mean weighted by confidence (inverse of std)
        bayesian_score = result.posterior_mean / (result.posterior_std + 1e-6)
        
        return ModelEffectiveness(
            model_name=model_name,
            success_rate=result.posterior_mean,
            confidence_lower=result.credible_interval_95[0],
            confidence_upper=result.credible_interval_95[1],
            n_applications=trials,
            avg_severity_reduction=avg_severity_reduction,
            bayesian_score=bayesian_score
        )
    
    def rank_models_by_effectiveness(
        self,
        min_applications: int = 10
    ) -> List[ModelEffectiveness]:
        """
        Rank all mental models by effectiveness using Bayesian inference.
        
        Args:
            min_applications: Minimum number of applications to consider
            
        Returns:
            List of ModelEffectiveness objects, sorted by bayesian_score
        """
        # Get all models with sufficient applications
        query = """
            SELECT model_name, COUNT(*) as count
            FROM planck_matrix
            GROUP BY model_name
            HAVING COUNT(*) >= %s
            ORDER BY count DESC
        """
        
        cur = self.conn.cursor()
        cur.execute(query, (min_applications,))
        models = [row[0] for row in cur.fetchall()]
        cur.close()
        
        # Calculate effectiveness for each model
        effectiveness_list = []
        for model_name in models:
            effectiveness = self.calculate_model_effectiveness(model_name)
            effectiveness_list.append(effectiveness)
        
        # Sort by Bayesian score
        effectiveness_list.sort(key=lambda x: x.bayesian_score, reverse=True)
        
        return effectiveness_list
    
    def calculate_lollapalooza_probability(
        self,
        model_combination: List[str],
        context_features: Dict[str, float]
    ) -> float:
        """
        Calculate probability of Lollapalooza effect for a model combination.
        
        Uses Bayesian inference to estimate the probability that combining
        these models will produce a multiplicative (not additive) effect.
        
        Args:
            model_combination: List of model names
            context_features: Context features (severity, region, etc.)
            
        Returns:
            Probability of Lollapalooza effect (0-1)
        """
        # Query historical cases with these models
        placeholders = ','.join(['%s'] * len(model_combination))
        query = f"""
            SELECT 
                cs.lollapalooza_score,
                cs.severity,
                cs.models_involved
            FROM case_studies cs
            WHERE cs.id IN (
                SELECT DISTINCT case_id
                FROM planck_matrix
                WHERE model_name IN ({placeholders})
                GROUP BY case_id
                HAVING COUNT(DISTINCT model_name) >= %s
            )
        """
        
        params = model_combination + [len(model_combination)]
        df = pd.read_sql_query(query, self.conn, params=params)
        
        if len(df) == 0:
            # No historical data, use prior based on number of models
            # More models = higher prior probability of Lollapalooza
            prior_prob = min(0.8, 0.2 + 0.1 * len(model_combination))
            return prior_prob
        
        # Use Beta-Binomial with historical data
        # Define Lollapalooza as score > 0.7
        lollapalooza_cases = (df['lollapalooza_score'] > 0.7).sum()
        total_cases = len(df)
        
        result = self.beta_binomial_inference(
            successes=lollapalooza_cases,
            trials=total_cases,
            prior_alpha=2.0,
            prior_beta=2.0
        )
        
        return result.posterior_mean
    
    def bayesian_ab_test(
        self,
        model_a: str,
        model_b: str,
        metric: str = 'effect_size'
    ) -> Dict[str, Any]:
        """
        Perform Bayesian A/B test between two mental models.
        
        Args:
            model_a: First model name
            model_b: Second model name
            metric: Metric to compare ('effect_size', 'severity', etc.)
            
        Returns:
            Dictionary with comparison results
        """
        query = """
            SELECT pm.model_name, pm.effect_size, cs.severity
            FROM planck_matrix pm
            JOIN case_studies cs ON pm.case_id = cs.id
            WHERE pm.model_name IN (%s, %s)
        """
        
        df = pd.read_sql_query(query, self.conn, params=(model_a, model_b))
        
        data_a = df[df['model_name'] == model_a][metric].values
        data_b = df[df['model_name'] == model_b][metric].values
        
        if len(data_a) == 0 or len(data_b) == 0:
            return {
                'model_a': model_a,
                'model_b': model_b,
                'error': 'Insufficient data for comparison'
            }
        
        # Calculate posterior distributions (assuming normal likelihood)
        mean_a, std_a = data_a.mean(), data_a.std()
        mean_b, std_b = data_b.mean(), data_b.std()
        
        # Monte Carlo sampling for probability that A > B
        n_samples = 10000
        samples_a = np.random.normal(mean_a, std_a / np.sqrt(len(data_a)), n_samples)
        samples_b = np.random.normal(mean_b, std_b / np.sqrt(len(data_b)), n_samples)
        
        prob_a_better = (samples_a > samples_b).mean()
        
        # Effect size (Cohen's d)
        pooled_std = np.sqrt(((len(data_a) - 1) * std_a**2 + (len(data_b) - 1) * std_b**2) / 
                             (len(data_a) + len(data_b) - 2))
        cohens_d = (mean_a - mean_b) / pooled_std if pooled_std > 0 else 0
        
        return {
            'model_a': model_a,
            'model_b': model_b,
            'metric': metric,
            'mean_a': float(mean_a),
            'mean_b': float(mean_b),
            'std_a': float(std_a),
            'std_b': float(std_b),
            'n_a': len(data_a),
            'n_b': len(data_b),
            'prob_a_better': float(prob_a_better),
            'cohens_d': float(cohens_d),
            'interpretation': self._interpret_cohens_d(cohens_d)
        }
    
    def _interpret_cohens_d(self, d: float) -> str:
        """Interpret Cohen's d effect size."""
        abs_d = abs(d)
        if abs_d < 0.2:
            return "negligible"
        elif abs_d < 0.5:
            return "small"
        elif abs_d < 0.8:
            return "medium"
        else:
            return "large"
    
    def export_effectiveness_report(self, output_path: str):
        """
        Export model effectiveness report to JSON.
        
        Args:
            output_path: Path to output JSON file
        """
        import json
        
        effectiveness_list = self.rank_models_by_effectiveness(min_applications=5)
        
        report = {
            'timestamp': pd.Timestamp.now().isoformat(),
            'n_models': len(effectiveness_list),
            'models': [
                {
                    'model_name': e.model_name,
                    'success_rate': e.success_rate,
                    'confidence_interval': [e.confidence_lower, e.confidence_upper],
                    'n_applications': e.n_applications,
                    'avg_severity_reduction': e.avg_severity_reduction,
                    'bayesian_score': e.bayesian_score
                }
                for e in effectiveness_list
            ]
        }
        
        with open(output_path, 'w') as f:
            json.dump(report, f, indent=2)


def main():
    """Run Bayesian analysis."""
    print("=" * 80)
    print("BAYESIAN ANALYSIS - Mental Models System")
    print("=" * 80)
    print()
    
    analyzer = BayesianAnalyzer()
    analyzer.connect()
    
    try:
        print("Ranking models by effectiveness...")
        effectiveness_list = analyzer.rank_models_by_effectiveness(min_applications=5)
        print(f"✓ Analyzed {len(effectiveness_list)} models")
        print()
        
        print("Top 10 Most Effective Models:")
        print("-" * 80)
        for i, e in enumerate(effectiveness_list[:10], 1):
            print(f"{i}. {e.model_name}")
            print(f"   Success Rate: {e.success_rate:.2%} "
                  f"[{e.confidence_lower:.2%}, {e.confidence_upper:.2%}]")
            print(f"   Applications: {e.n_applications}")
            print(f"   Bayesian Score: {e.bayesian_score:.2f}")
            print()
        
        output_path = "data/processed/model_effectiveness.json"
        print(f"Exporting report to {output_path}...")
        analyzer.export_effectiveness_report(output_path)
        print("✓ Export complete")
        
    finally:
        analyzer.close()
    
    print()
    print("=" * 80)
    print("Analysis complete!")
    print("=" * 80)


if __name__ == "__main__":
    main()
