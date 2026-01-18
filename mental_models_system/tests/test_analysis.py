#!/usr/bin/env python3
"""
Analysis Tests
Unit tests for statistical analysis tools.
"""

import unittest
import numpy as np
import os
import sys

sys.path.append(os.path.dirname(os.path.dirname(__file__)))

from src.analysis.statistical import StatisticalAnalyzer, ConfidenceInterval


class TestStatisticalAnalyzer(unittest.TestCase):
    """Test statistical analysis functions."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.analyzer = StatisticalAnalyzer()
        np.random.seed(42)
    
    def test_confidence_interval_calculation(self):
        """Test confidence interval calculation."""
        data = np.array([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
        ci = self.analyzer.calculate_confidence_interval(data, confidence=0.95)
        
        self.assertIsInstance(ci, ConfidenceInterval)
        self.assertAlmostEqual(ci.mean, 5.5, places=5)
        self.assertLess(ci.lower, ci.mean)
        self.assertGreater(ci.upper, ci.mean)
        self.assertEqual(ci.confidence_level, 0.95)
    
    def test_bootstrap_confidence_interval(self):
        """Test bootstrap confidence interval."""
        data = np.random.normal(100, 15, 1000)
        ci = self.analyzer.bootstrap_confidence_interval(data, n_bootstrap=1000)
        
        self.assertIsInstance(ci, ConfidenceInterval)
        self.assertAlmostEqual(ci.mean, 100, delta=2)
        self.assertLess(ci.lower, ci.mean)
        self.assertGreater(ci.upper, ci.mean)
    
    def test_two_sample_t_test_different_means(self):
        """Test t-test with different means."""
        group1 = np.random.normal(100, 10, 100)
        group2 = np.random.normal(110, 10, 100)
        
        result = self.analyzer.two_sample_t_test(group1, group2)
        
        self.assertLess(result.p_value, 0.05)
        self.assertTrue(result.reject_null)
    
    def test_two_sample_t_test_same_means(self):
        """Test t-test with same means."""
        group1 = np.random.normal(100, 10, 100)
        group2 = np.random.normal(100, 10, 100)
        
        result = self.analyzer.two_sample_t_test(group1, group2)
        
        self.assertGreater(result.p_value, 0.05)
        self.assertFalse(result.reject_null)
    
    def test_correlation_analysis(self):
        """Test correlation analysis."""
        x = np.array([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
        y = np.array([2, 4, 6, 8, 10, 12, 14, 16, 18, 20])
        
        result = self.analyzer.correlation_analysis(x, y)
        
        self.assertAlmostEqual(result['pearson_r'], 1.0, places=5)
        self.assertLess(result['pearson_p'], 0.001)
    
    def test_monte_carlo_simulation(self):
        """Test Monte Carlo simulation."""
        data = np.array([1, 2, 3, 4, 5])
        result = self.analyzer.monte_carlo_simulation(data, n_simulations=1000)
        
        self.assertEqual(result.n_simulations, 1000)
        self.assertIsNotNone(result.mean)
        self.assertIsNotNone(result.std)
        self.assertLess(result.percentile_5, result.percentile_50)
        self.assertLess(result.percentile_50, result.percentile_95)
    
    def test_bayesian_update(self):
        """Test Bayesian update."""
        prior_mean, prior_std = 100, 15
        likelihood_mean, likelihood_std = 110, 10
        
        posterior_mean, posterior_std = self.analyzer.bayesian_update(
            prior_mean, prior_std, likelihood_mean, likelihood_std
        )
        
        self.assertGreater(posterior_mean, prior_mean)
        self.assertLess(posterior_mean, likelihood_mean)
        self.assertLess(posterior_std, prior_std)
        self.assertLess(posterior_std, likelihood_std)
    
    def test_lollapalooza_probability_increases_with_models(self):
        """Test that lollapalooza probability increases with more models."""
        prob_3 = self.analyzer.calculate_lollapalooza_probability(3)
        prob_5 = self.analyzer.calculate_lollapalooza_probability(5)
        prob_10 = self.analyzer.calculate_lollapalooza_probability(10)
        
        self.assertLess(prob_3, prob_5)
        self.assertLess(prob_5, prob_10)
        self.assertLessEqual(prob_10, 0.99)
    
    def test_lollapalooza_probability_bounded(self):
        """Test that lollapalooza probability is bounded."""
        prob = self.analyzer.calculate_lollapalooza_probability(100)
        
        self.assertGreaterEqual(prob, 0)
        self.assertLessEqual(prob, 0.99)


class TestConfidenceIntervalDataclass(unittest.TestCase):
    """Test ConfidenceInterval dataclass."""
    
    def test_confidence_interval_creation(self):
        """Test creating a ConfidenceInterval."""
        ci = ConfidenceInterval(
            mean=100,
            lower=95,
            upper=105,
            confidence_level=0.95
        )
        
        self.assertEqual(ci.mean, 100)
        self.assertEqual(ci.lower, 95)
        self.assertEqual(ci.upper, 105)
        self.assertEqual(ci.confidence_level, 0.95)


if __name__ == "__main__":
    unittest.main()
