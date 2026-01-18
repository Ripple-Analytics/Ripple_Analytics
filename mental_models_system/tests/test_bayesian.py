#!/usr/bin/env python3
"""
Tests for Bayesian Analysis
"""

import os
import sys
import unittest
from unittest.mock import Mock, patch
import numpy as np
import pandas as pd

sys.path.append(os.path.dirname(os.path.dirname(__file__)))
from src.analysis.bayesian import (
    BayesianAnalyzer, 
    BayesianResult, 
    ModelEffectiveness
)


class TestBayesianAnalyzer(unittest.TestCase):
    """Test Bayesian analysis functionality."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.analyzer = BayesianAnalyzer()
    
    def test_initialization(self):
        """Test analyzer initialization."""
        self.assertIsNone(self.analyzer.conn)
        self.assertEqual(self.analyzer.random_seed, 42)
    
    def test_beta_binomial_inference(self):
        """Test Beta-Binomial inference."""
        result = self.analyzer.beta_binomial_inference(
            successes=70,
            trials=100,
            prior_alpha=2.0,
            prior_beta=2.0
        )
        
        # Check result structure
        self.assertIsInstance(result, BayesianResult)
        self.assertGreater(result.posterior_mean, 0)
        self.assertLess(result.posterior_mean, 1)
        
        # Posterior mean should be close to 70/100 = 0.7
        self.assertAlmostEqual(result.posterior_mean, 0.7, delta=0.05)
        
        # Confidence interval should contain posterior mean
        self.assertLess(result.credible_interval_95[0], result.posterior_mean)
        self.assertGreater(result.credible_interval_95[1], result.posterior_mean)
    
    def test_beta_binomial_with_weak_prior(self):
        """Test that weak prior has minimal effect with large sample."""
        result = self.analyzer.beta_binomial_inference(
            successes=700,
            trials=1000,
            prior_alpha=1.0,
            prior_beta=1.0
        )
        
        # With large sample, posterior should be close to MLE
        self.assertAlmostEqual(result.posterior_mean, 0.7, delta=0.01)
    
    def test_beta_binomial_with_strong_prior(self):
        """Test that strong prior affects posterior with small sample."""
        # Strong prior favoring 0.5
        result = self.analyzer.beta_binomial_inference(
            successes=7,
            trials=10,
            prior_alpha=50.0,
            prior_beta=50.0
        )
        
        # Posterior should be pulled toward prior mean of 0.5
        self.assertGreater(result.posterior_mean, 0.5)
        self.assertLess(result.posterior_mean, 0.7)
    
    def test_calculate_model_effectiveness_no_data(self):
        """Test effectiveness calculation with no data."""
        with patch.object(pd, 'read_sql_query', return_value=pd.DataFrame()):
            self.analyzer.conn = Mock()
            
            effectiveness = self.analyzer.calculate_model_effectiveness("NonexistentModel")
            
            self.assertEqual(effectiveness.model_name, "NonexistentModel")
            self.assertEqual(effectiveness.success_rate, 0.0)
            self.assertEqual(effectiveness.n_applications, 0)
    
    def test_calculate_model_effectiveness_with_data(self):
        """Test effectiveness calculation with data."""
        mock_data = pd.DataFrame({
            'severity': [0.8, 0.7, 0.9, 0.6],
            'effect_size': [0.7, 0.6, 0.8, 0.4],
            'financial_impact': [1000000, 500000, 2000000, 300000]
        })
        
        with patch.object(pd, 'read_sql_query', return_value=mock_data):
            self.analyzer.conn = Mock()
            
            effectiveness = self.analyzer.calculate_model_effectiveness(
                "TestModel",
                threshold_severity=0.5
            )
            
            self.assertEqual(effectiveness.model_name, "TestModel")
            self.assertEqual(effectiveness.n_applications, 4)
            self.assertGreater(effectiveness.success_rate, 0)
            self.assertLess(effectiveness.success_rate, 1)
            self.assertGreater(effectiveness.bayesian_score, 0)
    
    def test_bayesian_ab_test_equal_models(self):
        """Test A/B test with equal models."""
        mock_data = pd.DataFrame({
            'model_name': ['A'] * 50 + ['B'] * 50,
            'effect_size': np.concatenate([
                np.random.normal(0.7, 0.1, 50),
                np.random.normal(0.7, 0.1, 50)
            ]),
            'severity': np.random.uniform(0.5, 0.9, 100)
        })
        
        with patch.object(pd, 'read_sql_query', return_value=mock_data):
            self.analyzer.conn = Mock()
            
            result = self.analyzer.bayesian_ab_test('A', 'B', metric='effect_size')
            
            self.assertEqual(result['model_a'], 'A')
            self.assertEqual(result['model_b'], 'B')
            # Probability should be close to 0.5 for equal models
            self.assertGreater(result['prob_a_better'], 0.3)
            self.assertLess(result['prob_a_better'], 0.7)
    
    def test_bayesian_ab_test_different_models(self):
        """Test A/B test with clearly different models."""
        mock_data = pd.DataFrame({
            'model_name': ['A'] * 50 + ['B'] * 50,
            'effect_size': np.concatenate([
                np.random.normal(0.8, 0.05, 50),  # Model A better
                np.random.normal(0.5, 0.05, 50)   # Model B worse
            ]),
            'severity': np.random.uniform(0.5, 0.9, 100)
        })
        
        with patch.object(pd, 'read_sql_query', return_value=mock_data):
            self.analyzer.conn = Mock()
            
            result = self.analyzer.bayesian_ab_test('A', 'B', metric='effect_size')
            
            # Model A should be clearly better
            self.assertGreater(result['prob_a_better'], 0.9)
            self.assertGreater(abs(result['cohens_d']), 0.5)
    
    def test_cohens_d_interpretation(self):
        """Test Cohen's d interpretation."""
        self.assertEqual(self.analyzer._interpret_cohens_d(0.1), "negligible")
        self.assertEqual(self.analyzer._interpret_cohens_d(0.3), "small")
        self.assertEqual(self.analyzer._interpret_cohens_d(0.6), "medium")
        self.assertEqual(self.analyzer._interpret_cohens_d(1.0), "large")
        self.assertEqual(self.analyzer._interpret_cohens_d(-0.6), "medium")
    
    def test_lollapalooza_probability_no_data(self):
        """Test Lollapalooza probability with no historical data."""
        with patch.object(pd, 'read_sql_query', return_value=pd.DataFrame()):
            self.analyzer.conn = Mock()
            
            prob = self.analyzer.calculate_lollapalooza_probability(
                model_combination=['Model A', 'Model B', 'Model C'],
                context_features={'severity': 0.8}
            )
            
            # Should return prior based on number of models
            self.assertGreater(prob, 0)
            self.assertLess(prob, 1)
    
    def test_lollapalooza_probability_with_data(self):
        """Test Lollapalooza probability with historical data."""
        mock_data = pd.DataFrame({
            'lollapalooza_score': [0.8, 0.9, 0.6, 0.75],
            'severity': [0.8, 0.9, 0.7, 0.85],
            'models_involved': [3, 4, 3, 3]
        })
        
        with patch.object(pd, 'read_sql_query', return_value=mock_data):
            self.analyzer.conn = Mock()
            
            prob = self.analyzer.calculate_lollapalooza_probability(
                model_combination=['Model A', 'Model B', 'Model C'],
                context_features={'severity': 0.8}
            )
            
            # Should return probability based on historical data
            self.assertGreater(prob, 0)
            self.assertLess(prob, 1)


class TestBayesianDataClasses(unittest.TestCase):
    """Test Bayesian data classes."""
    
    def test_bayesian_result_creation(self):
        """Test BayesianResult creation."""
        result = BayesianResult(
            prior_mean=0.5,
            prior_std=0.1,
            posterior_mean=0.7,
            posterior_std=0.05,
            credible_interval_95=(0.6, 0.8),
            bayes_factor=3.5
        )
        
        self.assertEqual(result.prior_mean, 0.5)
        self.assertEqual(result.posterior_mean, 0.7)
        self.assertEqual(result.bayes_factor, 3.5)
    
    def test_model_effectiveness_creation(self):
        """Test ModelEffectiveness creation."""
        effectiveness = ModelEffectiveness(
            model_name="Test Model",
            success_rate=0.75,
            confidence_lower=0.65,
            confidence_upper=0.85,
            n_applications=100,
            avg_severity_reduction=0.6,
            bayesian_score=12.5
        )
        
        self.assertEqual(effectiveness.model_name, "Test Model")
        self.assertEqual(effectiveness.success_rate, 0.75)
        self.assertEqual(effectiveness.n_applications, 100)


if __name__ == '__main__':
    unittest.main()
