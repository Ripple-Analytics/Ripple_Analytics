"""
Detection module for Mental Models System.

Provides real-time detection of Lollapalooza effects and pattern recognition.
"""

from .lollapalooza_engine import (
    LollapaloozaDetectionEngine,
    Signal,
    SignalType,
    ModelActivation,
    LollapaloozaEvent,
    AlertLevel,
    create_engine
)

__all__ = [
    'LollapaloozaDetectionEngine',
    'Signal',
    'SignalType',
    'ModelActivation',
    'LollapaloozaEvent',
    'AlertLevel',
    'create_engine'
]
