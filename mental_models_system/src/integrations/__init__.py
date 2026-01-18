"""
Integrations for the Mental Models System.

Available Integrations:
- Slack: Run the system via Slack commands and bot
- GitHub: Automated PR analysis and issue management
- Devin: Integration with Devin AI for automated improvements
"""

from .slack_integration import SlackIntegration, SlackEventHandler
from .github_integration import GitHubIntegration
from .devin_integration import DevinIntegration
from .ai_agents_integration import (
    AutomatedImprovementCycle,
    DevinIntegration as DevinAgentIntegration,
    ManusIntegration,
    ImprovementRequest,
    ImprovementResult,
    ImprovementType,
    ImprovementStatus,
    AgentType,
    AgentConfig,
    create_devin_config,
    create_manus_config,
    create_improvement_cycle,
)

__all__ = [
    "SlackIntegration",
    "SlackEventHandler",
    "GitHubIntegration",
    "DevinIntegration",
    "AutomatedImprovementCycle",
    "DevinAgentIntegration",
    "ManusIntegration",
    "ImprovementRequest",
    "ImprovementResult",
    "ImprovementType",
    "ImprovementStatus",
    "AgentType",
    "AgentConfig",
    "create_devin_config",
    "create_manus_config",
    "create_improvement_cycle",
]
