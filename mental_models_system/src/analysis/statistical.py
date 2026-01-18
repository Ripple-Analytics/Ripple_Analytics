#!/usr/bin/env python3
"""
Statistical Analysis Tools
Advanced statistical analysis for the Mental Models System.
"""

import os
import sys
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import dataclass
import numpy as np
import pandas as pd
from scipy import stats
from scipy.stats import norm, t as t_dist
import psycopg2

sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
from config.settings import settings


@dataclass
class ConfidenceInterval:
    """Confidence interval result."""
    mean: float
    lower: float
    upper: float
    confidence_level: float


@dataclass
class HypothesisTestResult:
    """Hypothesis test result."""
    statistic: float
    p_value: float
    reject_null: bool
    alpha: float


@dataclass
class MonteCarloResult:
    """Monte Carlo simulation result."""
    mean: float
    std: float
    percentile_5: float
    percentile_25: float
    percentile_50: float
    percentile_75: float
    percentile_95: float
    n_simulations: int


class StatisticalAnalyzer:
    """Statistical analysis tools for mental models data."""
    
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
    
    def _query_to_array(self, query: str, column: str) -> np.ndarray:
        """Execute query and return column as numpy array."""
        df = pd.read_sql_query(query, self.conn)
        return df[column].dropna().values
    
    def calculate_confidence_interval(
        self, 
        data: np.ndarray, 
        confidence: float = 0.95
    ) -> ConfidenceInterval:
        """Calculate confidence interval for mean."""
        n = len(data)
        mean = np.mean(data)
        se = stats.sem(data)
        
        h = se * t_dist.ppf((1 + confidence) / 2, n - 1)
        
        return ConfidenceInterval(
            mean=mean,
            lower=mean - h,
            upper=mean + h,
            confidence_level=confidence
        )
    
    def bootstrap_confidence_interval(
        self,
        data: np.ndarray,
        statistic_func: callable = np.mean,
        n_bootstrap: int = 10000,
        confidence: float = 0.95
    ) -> ConfidenceInterval:
        """Calculate bootstrap confidence interval."""
        bootstrap_stats = []
        n = len(data)
        
        for _ in range(n_bootstrap):
            sample = np.random.choice(data, size=n, replace=True)
            bootstrap_stats.append(statistic_func(sample))
        
        bootstrap_stats = np.array(bootstrap_stats)
        alpha = 1 - confidence
        
        return ConfidenceInterval(
            mean=np.mean(bootstrap_stats),
            lower=np.percentile(bootstrap_stats, alpha / 2 * 100),
            upper=np.percentile(bootstrap_stats, (1 - alpha / 2) * 100),
            confidence_level=confidence
        )
    
    def two_sample_t_test(
        self,
        group1: np.ndarray,
        group2: np.ndarray,
        alpha: float = 0.05
    ) -> HypothesisTestResult:
        """Perform two-sample t-test."""
        statistic, p_value = stats.ttest_ind(group1, group2)
        
        return HypothesisTestResult(
            statistic=statistic,
            p_value=p_value,
            reject_null=p_value < alpha,
            alpha=alpha
        )
    
    def correlation_analysis(
        self,
        x: np.ndarray,
        y: np.ndarray
    ) -> Dict[str, float]:
        """Calculate various correlation coefficients."""
        pearson_r, pearson_p = stats.pearsonr(x, y)
        spearman_r, spearman_p = stats.spearmanr(x, y)
        
        return {
            "pearson_r": pearson_r,
            "pearson_p": pearson_p,
            "spearman_r": spearman_r,
            "spearman_p": spearman_p
        }
    
    def monte_carlo_simulation(
        self,
        data: np.ndarray,
        n_simulations: int = None,
        aggregation_func: callable = np.sum
    ) -> MonteCarloResult:
        """Run Monte Carlo simulation on data."""
        n_simulations = n_simulations or settings.analysis.monte_carlo_simulations
        n = len(data)
        
        simulated_results = []
        for _ in range(n_simulations):
            sample = np.random.choice(data, size=n, replace=True)
            simulated_results.append(aggregation_func(sample))
        
        simulated_results = np.array(simulated_results)
        
        return MonteCarloResult(
            mean=np.mean(simulated_results),
            std=np.std(simulated_results),
            percentile_5=np.percentile(simulated_results, 5),
            percentile_25=np.percentile(simulated_results, 25),
            percentile_50=np.percentile(simulated_results, 50),
            percentile_75=np.percentile(simulated_results, 75),
            percentile_95=np.percentile(simulated_results, 95),
            n_simulations=n_simulations
        )
    
    def analyze_severity_distribution(self) -> Dict[str, Any]:
        """Analyze the distribution of case severity."""
        self.connect()
        
        try:
            severity = self._query_to_array(
                "SELECT severity FROM case_studies WHERE severity IS NOT NULL",
                "severity"
            )
            
            shapiro_stat, shapiro_p = stats.shapiro(severity[:5000])
            
            ci = self.calculate_confidence_interval(severity)
            
            mc = self.monte_carlo_simulation(severity, aggregation_func=np.mean)
            
            return {
                "n": len(severity),
                "mean": np.mean(severity),
                "std": np.std(severity),
                "median": np.median(severity),
                "min": np.min(severity),
                "max": np.max(severity),
                "skewness": stats.skew(severity),
                "kurtosis": stats.kurtosis(severity),
                "shapiro_statistic": shapiro_stat,
                "shapiro_p_value": shapiro_p,
                "is_normal": shapiro_p > 0.05,
                "confidence_interval": ci,
                "monte_carlo": mc
            }
        finally:
            self.close()
    
    def analyze_effect_sizes(self) -> Dict[str, Any]:
        """Analyze effect sizes from Planck matrix."""
        self.connect()
        
        try:
            effects = self._query_to_array(
                "SELECT effect_size FROM planck_matrix WHERE effect_size IS NOT NULL",
                "effect_size"
            )
            
            ci = self.calculate_confidence_interval(effects)
            bootstrap_ci = self.bootstrap_confidence_interval(effects)
            
            return {
                "n": len(effects),
                "mean": np.mean(effects),
                "std": np.std(effects),
                "median": np.median(effects),
                "parametric_ci": ci,
                "bootstrap_ci": bootstrap_ci
            }
        finally:
            self.close()
    
    def compare_categories(
        self,
        category1: str,
        category2: str
    ) -> Dict[str, Any]:
        """Compare severity between two case categories."""
        self.connect()
        
        try:
            df = pd.read_sql_query(
                """
                SELECT category, severity 
                FROM case_studies 
                WHERE category IN (%s, %s) AND severity IS NOT NULL
                """,
                self.conn,
                params=(category1, category2)
            )
            
            group1 = df[df['category'] == category1]['severity'].values
            group2 = df[df['category'] == category2]['severity'].values
            
            t_test = self.two_sample_t_test(group1, group2)
            
            mann_whitney_stat, mann_whitney_p = stats.mannwhitneyu(group1, group2)
            
            cohens_d = (np.mean(group1) - np.mean(group2)) / np.sqrt(
                ((len(group1) - 1) * np.var(group1) + (len(group2) - 1) * np.var(group2)) /
                (len(group1) + len(group2) - 2)
            )
            
            return {
                "category1": {
                    "name": category1,
                    "n": len(group1),
                    "mean": np.mean(group1),
                    "std": np.std(group1)
                },
                "category2": {
                    "name": category2,
                    "n": len(group2),
                    "mean": np.mean(group2),
                    "std": np.std(group2)
                },
                "t_test": t_test,
                "mann_whitney_u": mann_whitney_stat,
                "mann_whitney_p": mann_whitney_p,
                "cohens_d": cohens_d,
                "effect_size_interpretation": (
                    "negligible" if abs(cohens_d) < 0.2 else
                    "small" if abs(cohens_d) < 0.5 else
                    "medium" if abs(cohens_d) < 0.8 else
                    "large"
                )
            }
        finally:
            self.close()
    
    def analyze_model_correlations(self) -> pd.DataFrame:
        """Analyze correlations between model occurrences."""
        self.connect()
        
        try:
            df = pd.read_sql_query(
                """
                SELECT case_id, model_name, 1 as present
                FROM planck_matrix
                """,
                self.conn
            )
            
            pivot = df.pivot_table(
                index='case_id',
                columns='model_name',
                values='present',
                fill_value=0
            )
            
            correlation_matrix = pivot.corr()
            
            return correlation_matrix
        finally:
            self.close()
    
    def bayesian_update(
        self,
        prior_mean: float,
        prior_std: float,
        likelihood_mean: float,
        likelihood_std: float
    ) -> Tuple[float, float]:
        """Perform Bayesian update with normal distributions."""
        prior_precision = 1 / (prior_std ** 2)
        likelihood_precision = 1 / (likelihood_std ** 2)
        
        posterior_precision = prior_precision + likelihood_precision
        posterior_std = np.sqrt(1 / posterior_precision)
        
        posterior_mean = (
            prior_precision * prior_mean + likelihood_precision * likelihood_mean
        ) / posterior_precision
        
        return posterior_mean, posterior_std
    
    def calculate_lollapalooza_probability(
        self,
        n_models: int,
        base_probability: float = 0.1
    ) -> float:
        """
        Calculate probability of lollapalooza effect.
        Based on Munger's concept that multiple biases compound.
        """
        independent_prob = 1 - (1 - base_probability) ** n_models
        
        synergy_factor = 1 + 0.1 * (n_models - 1)
        
        lollapalooza_prob = min(independent_prob * synergy_factor, 0.99)
        
        return lollapalooza_prob


def run_full_analysis():
    """Run complete statistical analysis."""
    analyzer = StatisticalAnalyzer()
    
    print("=" * 60)
    print("MENTAL MODELS SYSTEM - STATISTICAL ANALYSIS")
    print("=" * 60)
    
    print("\n--- Severity Distribution Analysis ---")
    severity_analysis = analyzer.analyze_severity_distribution()
    print(f"N: {severity_analysis['n']}")
    print(f"Mean: {severity_analysis['mean']:.4f}")
    print(f"Std: {severity_analysis['std']:.4f}")
    print(f"Median: {severity_analysis['median']:.4f}")
    print(f"Skewness: {severity_analysis['skewness']:.4f}")
    print(f"Is Normal: {severity_analysis['is_normal']}")
    print(f"95% CI: [{severity_analysis['confidence_interval'].lower:.4f}, "
          f"{severity_analysis['confidence_interval'].upper:.4f}]")
    
    print("\n--- Effect Size Analysis ---")
    effect_analysis = analyzer.analyze_effect_sizes()
    print(f"N: {effect_analysis['n']}")
    print(f"Mean Effect: {effect_analysis['mean']:.4f}")
    print(f"Bootstrap 95% CI: [{effect_analysis['bootstrap_ci'].lower:.4f}, "
          f"{effect_analysis['bootstrap_ci'].upper:.4f}]")
    
    print("\n--- Lollapalooza Probability ---")
    for n in [3, 5, 7, 10]:
        prob = analyzer.calculate_lollapalooza_probability(n)
        print(f"  {n} models: {prob:.2%}")
    
    print("\n" + "=" * 60)
    print("Analysis Complete")


if __name__ == "__main__":
    run_full_analysis()
