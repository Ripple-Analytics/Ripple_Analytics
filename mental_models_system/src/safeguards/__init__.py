"""
Safeguards module for Mental Models System.

Provides failure mode analysis and prevention for all 129 mental models.
"""

from .failure_modes import (
    FailureMode,
    FailureCategory,
    FailureSeverity,
    ModelFailureAnalysis,
    FailurePreventionEngine,
    FAILURE_MODES_DATABASE,
    get_failure_modes_for_model,
    get_all_failure_modes,
    create_prevention_engine,
    count_failure_modes
)

__all__ = [
    'FailureMode',
    'FailureCategory', 
    'FailureSeverity',
    'ModelFailureAnalysis',
    'FailurePreventionEngine',
    'FAILURE_MODES_DATABASE',
    'get_failure_modes_for_model',
    'get_all_failure_modes',
    'create_prevention_engine',
    'count_failure_modes'
]
