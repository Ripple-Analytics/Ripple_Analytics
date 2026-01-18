#!/usr/bin/env python3
"""
HMM Regime Detection
Hidden Markov Model for detecting decision-making and market regimes.

Inspired by Jim Simons' Renaissance Technologies approach to regime detection.
"""

import os
import sys
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import dataclass
import numpy as np
import pandas as pd
from hmmlearn import hmm
import psycopg2
from datetime import datetime

sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
from config.settings import settings


@dataclass
class RegimeState:
    """Represents a detected regime state."""
    regime_id: int
    regime_name: str
    start_date: datetime
    end_date: Optional[datetime]
    probability: float
    characteristics: Dict[str, float]
    dominant_models: List[str]


@dataclass
class RegimeTransition:
    """Represents a transition between regimes."""
    from_regime: int
    to_regime: int
    transition_date: datetime
    probability: float
    trigger_events: List[str]


class HMMRegimeDetector:
    """
    Hidden Markov Model for regime detection in decision-making patterns.
    
    This class implements regime detection inspired by Renaissance Technologies'
    approach to identifying market states. Applied to mental models, it detects
    patterns in how different models cluster together in different contexts.
    
    Key Principles:
    - Jim Simons: "We don't predict. We react to what the data tells us."
    - Charlie Munger: "Lollapalooza effects occur when multiple models combine."
    - Ray Dalio: "Understanding the machine means understanding regime changes."
    """
    
    def __init__(self, n_regimes: int = 4, random_state: int = 42):
        """
        Initialize HMM regime detector.
        
        Args:
            n_regimes: Number of hidden states (regimes) to detect
            random_state: Random seed for reproducibility
        """
        self.n_regimes = n_regimes
        self.random_state = random_state
        self.model = None
        self.regime_labels = {}
        self.conn = None
        
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
    
    def prepare_features(self) -> Tuple[np.ndarray, pd.DataFrame]:
        """
        Prepare feature matrix from case studies for HMM training.
        
        Features include:
        - Number of models involved
        - Severity score
        - Financial impact (normalized)
        - Lollapalooza score
        - Temporal features (decade, season)
        
        Returns:
            Tuple of (feature_matrix, metadata_dataframe)
        """
        query = """
            SELECT 
                cs.id,
                cs.name,
                cs.date,
                cs.category,
                cs.region,
                cs.severity,
                cs.financial_impact,
                cs.models_involved,
                cs.lollapalooza_score,
                EXTRACT(YEAR FROM cs.date) as year,
                EXTRACT(MONTH FROM cs.date) as month
            FROM case_studies cs
            WHERE cs.date IS NOT NULL
            ORDER BY cs.date
        """
        
        df = pd.read_sql_query(query, self.conn)
        
        # Normalize financial impact (log scale)
        df['log_financial_impact'] = np.log1p(df['financial_impact'].fillna(0))
        
        # Create temporal features
        df['decade'] = (df['year'] // 10) * 10
        df['season'] = (df['month'] - 1) // 3  # 0=Q1, 1=Q2, 2=Q3, 3=Q4
        
        # Feature matrix
        features = df[[
            'models_involved',
            'severity',
            'log_financial_impact',
            'lollapalooza_score',
            'season'
        ]].fillna(0).values
        
        # Standardize features
        features = (features - features.mean(axis=0)) / (features.std(axis=0) + 1e-8)
        
        return features, df
    
    def fit(self, features: np.ndarray, n_iter: int = 100) -> 'HMMRegimeDetector':
        """
        Fit HMM on feature data (alias for train).
        
        Args:
            features: Feature matrix (n_samples, n_features)
            n_iter: Number of EM iterations
            
        Returns:
            Self for method chaining
        """
        return self.train(features, n_iter)
    
    def train(self, features: np.ndarray, n_iter: int = 100) -> 'HMMRegimeDetector':
        """
        Train HMM on feature data.
        
        Args:
            features: Feature matrix (n_samples, n_features)
            n_iter: Number of EM iterations
            
        Returns:
            Self for method chaining
        """
        # Use Gaussian HMM
        self.model = hmm.GaussianHMM(
            n_components=self.n_regimes,
            covariance_type="full",
            n_iter=n_iter,
            random_state=self.random_state,
            verbose=False
        )
        
        self.model.fit(features)
        
        # Label regimes based on characteristics
        self._label_regimes(features)
        
        return self
    
    def _label_regimes(self, features: np.ndarray):
        """
        Assign meaningful labels to detected regimes based on their characteristics.
        
        Args:
            features: Feature matrix used for training
        """
        states = self.model.predict(features)
        
        for regime_id in range(self.n_regimes):
            regime_mask = states == regime_id
            regime_features = features[regime_mask]
            
            if len(regime_features) == 0:
                self.regime_labels[regime_id] = f"Regime {regime_id}"
                continue
            
            # Calculate mean characteristics
            mean_features = regime_features.mean(axis=0)
            
            # Assign label based on dominant characteristics
            # Features: [models_involved, severity, log_financial_impact, lollapalooza_score, season]
            if mean_features[3] > 0.5:  # High lollapalooza score
                label = "Lollapalooza Regime"
            elif mean_features[1] > 0.5:  # High severity
                label = "Crisis Regime"
            elif mean_features[0] > 0.5:  # High model involvement
                label = "Complex Decision Regime"
            else:
                label = "Stable Regime"
            
            self.regime_labels[regime_id] = label
    
    def predict_regimes(self, features: np.ndarray) -> np.ndarray:
        """
        Predict regime states for given features.
        
        Args:
            features: Feature matrix (n_samples, n_features)
            
        Returns:
            Array of predicted regime states
        """
        if self.model is None:
            raise ValueError("Model not trained. Call train() first.")
        
        return self.model.predict(features)
    
    def get_regime_probabilities(self, features: np.ndarray) -> np.ndarray:
        """
        Get probability distribution over regimes for each sample.
        
        Args:
            features: Feature matrix (n_samples, n_features)
            
        Returns:
            Array of shape (n_samples, n_regimes) with probabilities
        """
        if self.model is None:
            raise ValueError("Model not trained. Call train() first.")
        
        return self.model.predict_proba(features)
    
    def detect_regime_transitions(
        self, 
        states: np.ndarray, 
        dates: pd.Series
    ) -> List[RegimeTransition]:
        """
        Detect transitions between regimes.
        
        Args:
            states: Array of regime states
            dates: Series of dates corresponding to states
            
        Returns:
            List of RegimeTransition objects
        """
        transitions = []
        
        for i in range(1, len(states)):
            if states[i] != states[i-1]:
                transition = RegimeTransition(
                    from_regime=int(states[i-1]),
                    to_regime=int(states[i]),
                    transition_date=dates.iloc[i],
                    probability=1.0,  # Could be enhanced with transition probabilities
                    trigger_events=[]  # Could be enhanced with event detection
                )
                transitions.append(transition)
        
        return transitions
    
    def analyze_regime_characteristics(
        self, 
        features: np.ndarray, 
        states: np.ndarray,
        metadata: pd.DataFrame
    ) -> Dict[int, Dict[str, Any]]:
        """
        Analyze characteristics of each detected regime.
        
        Args:
            features: Feature matrix
            states: Predicted regime states
            metadata: Metadata dataframe with case study information
            
        Returns:
            Dictionary mapping regime_id to characteristics
        """
        regime_chars = {}
        
        feature_names = [
            'models_involved',
            'severity',
            'log_financial_impact',
            'lollapalooza_score',
            'season'
        ]
        
        for regime_id in range(self.n_regimes):
            regime_mask = states == regime_id
            regime_features = features[regime_mask]
            regime_meta = metadata[regime_mask]
            
            if len(regime_features) == 0:
                continue
            
            # Calculate statistics
            mean_features = regime_features.mean(axis=0)
            std_features = regime_features.std(axis=0)
            
            # Get dominant categories and regions
            top_categories = regime_meta['category'].value_counts().head(3).to_dict()
            top_regions = regime_meta['region'].value_counts().head(3).to_dict()
            
            # Get most common models (would need to query planck_matrix)
            case_ids = regime_meta['id'].tolist()
            dominant_models = self._get_dominant_models_for_cases(case_ids)
            
            regime_chars[regime_id] = {
                'label': self.regime_labels.get(regime_id, f"Regime {regime_id}"),
                'n_cases': int(regime_mask.sum()),
                'mean_features': {name: float(val) for name, val in zip(feature_names, mean_features)},
                'std_features': {name: float(val) for name, val in zip(feature_names, std_features)},
                'top_categories': top_categories,
                'top_regions': top_regions,
                'dominant_models': dominant_models,
                'date_range': (
                    regime_meta['date'].min().isoformat() if not regime_meta['date'].empty else None,
                    regime_meta['date'].max().isoformat() if not regime_meta['date'].empty else None
                )
            }
        
        return regime_chars
    
    def _get_dominant_models_for_cases(self, case_ids: List[int], top_n: int = 5) -> List[str]:
        """
        Get dominant mental models for a set of case studies.
        
        Args:
            case_ids: List of case study IDs
            top_n: Number of top models to return
            
        Returns:
            List of model names
        """
        if not case_ids:
            return []
        
        query = """
            SELECT model_name, COUNT(*) as count
            FROM planck_matrix
            WHERE case_id = ANY(%s)
            GROUP BY model_name
            ORDER BY count DESC
            LIMIT %s
        """
        
        cur = self.conn.cursor()
        cur.execute(query, (case_ids, top_n))
        results = cur.fetchall()
        cur.close()
        
        return [row[0] for row in results]
    
    def get_transition_matrix(self) -> np.ndarray:
        """
        Get the regime transition probability matrix.
        
        Returns:
            Matrix of shape (n_regimes, n_regimes) with transition probabilities
        """
        if self.model is None:
            raise ValueError("Model not trained. Call train() first.")
        
        return self.model.transmat_
    
    def export_regime_analysis(self, output_path: str):
        """
        Export regime analysis to JSON file.
        
        Args:
            output_path: Path to output JSON file
        """
        import json
        
        features, metadata = self.prepare_features()
        states = self.predict_regimes(features)
        regime_chars = self.analyze_regime_characteristics(features, states, metadata)
        transitions = self.detect_regime_transitions(states, metadata['date'])
        
        analysis = {
            'n_regimes': self.n_regimes,
            'regime_characteristics': regime_chars,
            'transition_matrix': self.get_transition_matrix().tolist(),
            'transitions': [
                {
                    'from_regime': t.from_regime,
                    'to_regime': t.to_regime,
                    'date': t.transition_date.isoformat(),
                    'from_label': self.regime_labels.get(t.from_regime, f"Regime {t.from_regime}"),
                    'to_label': self.regime_labels.get(t.to_regime, f"Regime {t.to_regime}")
                }
                for t in transitions
            ]
        }
        
        with open(output_path, 'w') as f:
            json.dump(analysis, f, indent=2)


def main():
    """Run HMM regime detection analysis."""
    print("=" * 80)
    print("HMM REGIME DETECTION - Mental Models System")
    print("=" * 80)
    print()
    
    detector = HMMRegimeDetector(n_regimes=4, random_state=42)
    detector.connect()
    
    try:
        print("Preparing features from case studies...")
        features, metadata = detector.prepare_features()
        print(f"✓ Prepared {len(features)} case studies with {features.shape[1]} features")
        print()
        
        print("Training HMM with 4 regimes...")
        detector.train(features, n_iter=100)
        print("✓ Training complete")
        print()
        
        print("Predicting regimes...")
        states = detector.predict_regimes(features)
        print("✓ Regime prediction complete")
        print()
        
        print("Analyzing regime characteristics...")
        regime_chars = detector.analyze_regime_characteristics(features, states, metadata)
        print()
        
        for regime_id, chars in regime_chars.items():
            print(f"Regime {regime_id}: {chars['label']}")
            print(f"  Cases: {chars['n_cases']}")
            print(f"  Top Categories: {list(chars['top_categories'].keys())[:3]}")
            print(f"  Dominant Models: {chars['dominant_models'][:3]}")
            print()
        
        print("Detecting regime transitions...")
        transitions = detector.detect_regime_transitions(states, metadata['date'])
        print(f"✓ Detected {len(transitions)} regime transitions")
        print()
        
        print("Transition Matrix:")
        trans_matrix = detector.get_transition_matrix()
        print(trans_matrix)
        print()
        
        output_path = "data/processed/regime_analysis.json"
        print(f"Exporting analysis to {output_path}...")
        detector.export_regime_analysis(output_path)
        print("✓ Export complete")
        
    finally:
        detector.close()
    
    print()
    print("=" * 80)
    print("Analysis complete!")
    print("=" * 80)


if __name__ == "__main__":
    main()
