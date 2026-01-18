#!/usr/bin/env python3
"""
Tests for HMM Regime Detection
"""

import os
import sys
import unittest
from unittest.mock import Mock, patch, MagicMock
import numpy as np
import pandas as pd
from datetime import datetime, date

sys.path.append(os.path.dirname(os.path.dirname(__file__)))
from src.analysis.hmm_regime import HMMRegimeDetector, RegimeState, RegimeTransition


class TestHMMRegimeDetector(unittest.TestCase):
    """Test HMM regime detection functionality."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.detector = HMMRegimeDetector(n_regimes=3, random_state=42)
    
    def test_initialization(self):
        """Test detector initialization."""
        self.assertEqual(self.detector.n_regimes, 3)
        self.assertEqual(self.detector.random_state, 42)
        self.assertIsNone(self.detector.model)
    
    def test_prepare_features_shape(self):
        """Test that feature preparation returns correct shape."""
        # Mock database connection and query
        mock_data = pd.DataFrame({
            'id': [1, 2, 3],
            'name': ['Case 1', 'Case 2', 'Case 3'],
            'date': [date(2020, 1, 1), date(2020, 6, 1), date(2021, 1, 1)],
            'category': ['Crisis', 'Innovation', 'Crisis'],
            'region': ['US', 'EU', 'Asia'],
            'severity': [0.8, 0.5, 0.9],
            'financial_impact': [1000000, 500000, 2000000],
            'models_involved': [5, 3, 7],
            'lollapalooza_score': [0.7, 0.4, 0.85],
            'year': [2020, 2020, 2021],
            'month': [1, 6, 1]
        })
        
        with patch.object(pd, 'read_sql_query', return_value=mock_data):
            self.detector.conn = Mock()
            features, metadata = self.detector.prepare_features()
            
            # Check shapes
            self.assertEqual(features.shape[0], 3)  # 3 samples
            self.assertEqual(features.shape[1], 5)  # 5 features
            self.assertEqual(len(metadata), 3)
    
    def test_train_creates_model(self):
        """Test that training creates HMM model."""
        # Create synthetic features
        features = np.random.randn(100, 5)
        
        self.detector.train(features, n_iter=10)
        
        self.assertIsNotNone(self.detector.model)
        self.assertEqual(len(self.detector.regime_labels), self.detector.n_regimes)
    
    def test_predict_regimes(self):
        """Test regime prediction."""
        # Train on synthetic data
        features = np.random.randn(100, 5)
        self.detector.train(features, n_iter=10)
        
        # Predict
        states = self.detector.predict_regimes(features)
        
        self.assertEqual(len(states), 100)
        self.assertTrue(all(0 <= s < self.detector.n_regimes for s in states))
    
    def test_predict_without_training_raises_error(self):
        """Test that prediction without training raises error."""
        features = np.random.randn(10, 5)
        
        with self.assertRaises(ValueError):
            self.detector.predict_regimes(features)
    
    def test_get_regime_probabilities(self):
        """Test regime probability calculation."""
        # Train on synthetic data
        features = np.random.randn(100, 5)
        self.detector.train(features, n_iter=10)
        
        # Get probabilities
        probs = self.detector.get_regime_probabilities(features)
        
        self.assertEqual(probs.shape, (100, self.detector.n_regimes))
        # Check probabilities sum to 1
        np.testing.assert_array_almost_equal(probs.sum(axis=1), np.ones(100))
    
    def test_detect_regime_transitions(self):
        """Test regime transition detection."""
        states = np.array([0, 0, 1, 1, 2, 2, 0])
        dates = pd.Series([
            date(2020, 1, 1),
            date(2020, 2, 1),
            date(2020, 3, 1),
            date(2020, 4, 1),
            date(2020, 5, 1),
            date(2020, 6, 1),
            date(2020, 7, 1)
        ])
        
        transitions = self.detector.detect_regime_transitions(states, dates)
        
        # Should detect 3 transitions: 0->1, 1->2, 2->0
        self.assertEqual(len(transitions), 3)
        self.assertEqual(transitions[0].from_regime, 0)
        self.assertEqual(transitions[0].to_regime, 1)
    
    def test_regime_labels_meaningful(self):
        """Test that regime labels are meaningful."""
        # Create features with distinct patterns
        features = np.vstack([
            np.random.randn(30, 5) + np.array([2, 2, 1, 2, 0]),  # High values
            np.random.randn(30, 5) + np.array([0, 0, 0, 0, 0]),  # Low values
            np.random.randn(30, 5) + np.array([1, 1, 1, 1, 1])   # Medium values
        ])
        
        self.detector.train(features, n_iter=20)
        
        # Check that labels exist
        for regime_id in range(self.detector.n_regimes):
            self.assertIn(regime_id, self.detector.regime_labels)
            self.assertIsInstance(self.detector.regime_labels[regime_id], str)
    
    def test_transition_matrix_properties(self):
        """Test transition matrix has valid properties."""
        features = np.random.randn(100, 5)
        self.detector.train(features, n_iter=10)
        
        trans_matrix = self.detector.get_transition_matrix()
        
        # Check shape
        self.assertEqual(trans_matrix.shape, (self.detector.n_regimes, self.detector.n_regimes))
        
        # Check rows sum to 1 (probability distribution)
        np.testing.assert_array_almost_equal(trans_matrix.sum(axis=1), np.ones(self.detector.n_regimes))
        
        # Check all values are probabilities
        self.assertTrue(np.all(trans_matrix >= 0))
        self.assertTrue(np.all(trans_matrix <= 1))


class TestRegimeDataClasses(unittest.TestCase):
    """Test regime data classes."""
    
    def test_regime_state_creation(self):
        """Test RegimeState creation."""
        state = RegimeState(
            regime_id=1,
            regime_name="Crisis Regime",
            start_date=datetime(2020, 1, 1),
            end_date=datetime(2020, 12, 31),
            probability=0.85,
            characteristics={'severity': 0.9, 'models': 5},
            dominant_models=['Model A', 'Model B']
        )
        
        self.assertEqual(state.regime_id, 1)
        self.assertEqual(state.regime_name, "Crisis Regime")
        self.assertEqual(state.probability, 0.85)
    
    def test_regime_transition_creation(self):
        """Test RegimeTransition creation."""
        transition = RegimeTransition(
            from_regime=0,
            to_regime=1,
            transition_date=datetime(2020, 6, 1),
            probability=0.75,
            trigger_events=['Event A', 'Event B']
        )
        
        self.assertEqual(transition.from_regime, 0)
        self.assertEqual(transition.to_regime, 1)
        self.assertEqual(len(transition.trigger_events), 2)


if __name__ == '__main__':
    unittest.main()
