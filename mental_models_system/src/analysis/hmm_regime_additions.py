# Additional methods for HMMRegimeDetector to support testing
# This will be merged into hmm_regime.py

from dataclasses import dataclass
from typing import Optional
import numpy as np

@dataclass
class RegimeInfo:
    """Information about a detected regime."""
    regime_id: int
    mean_return: float
    volatility: float
    probability: float
    label: str = ""

def predict_current_regime(self, recent_data: np.ndarray) -> Optional[RegimeInfo]:
    """
    Predict the current regime based on recent data.
    
    Args:
        recent_data: Recent observations (1D or 2D array)
        
    Returns:
        RegimeInfo object with current regime characteristics
    """
    if self.model is None:
        raise ValueError("Model not trained. Call train() first.")
    
    # Ensure data is 2D
    if recent_data.ndim == 1:
        recent_data = recent_data.reshape(-1, 1)
    
    # Predict regime for recent data
    regime_states = self.model.predict(recent_data)
    current_regime_id = regime_states[-1]  # Most recent regime
    
    # Get probability distribution
    probs = self.model.predict_proba(recent_data)
    current_prob = probs[-1, current_regime_id]
    
    # Calculate statistics for the recent data
    mean_return = float(np.mean(recent_data))
    volatility = float(np.std(recent_data))
    
    return RegimeInfo(
        regime_id=int(current_regime_id),
        mean_return=mean_return,
        volatility=volatility,
        probability=float(current_prob),
        label=self.regime_labels.get(current_regime_id, f"Regime {current_regime_id}")
    )

def get_regime_statistics(self) -> dict:
    """
    Get statistics for all detected regimes.
    
    Returns:
        Dictionary mapping regime_id to statistics
    """
    if self.model is None:
        raise ValueError("Model not trained. Call train() first.")
    
    stats = {}
    for regime_id in range(self.n_regimes):
        stats[regime_id] = {
            'regime_id': regime_id,
            'label': self.regime_labels.get(regime_id, f"Regime {regime_id}"),
            'mean': self.model.means_[regime_id].tolist() if hasattr(self.model, 'means_') else [],
            'covariance': self.model.covars_[regime_id].tolist() if hasattr(self.model, 'covars_') else []
        }
    
    return stats
