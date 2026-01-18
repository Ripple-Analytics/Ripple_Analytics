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

__all__ = [
    "SlackIntegration",
    "SlackEventHandler",
    "GitHubIntegration",
    "DevinIntegration",
]
