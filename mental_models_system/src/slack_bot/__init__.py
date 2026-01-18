"""
Slack Bot Interface for Mental Models System

Allows interaction via Slack - same workflow as Manus/Devin.
"""

from .bot import (
    MentalModelsBot,
    StandaloneBot,
    AgentRegistry,
    AgentType,
    MentalModelsInterface
)

__all__ = [
    "MentalModelsBot",
    "StandaloneBot",
    "AgentRegistry",
    "AgentType",
    "MentalModelsInterface"
]
