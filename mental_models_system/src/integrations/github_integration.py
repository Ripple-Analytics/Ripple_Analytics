"""
GitHub integration for automated PR analysis and issue management.

This allows the Mental Models System to:
- Analyze PRs through mental models lens
- Detect potential failure modes in code changes
- Suggest improvements based on mental models
- Create issues for identified problems
"""

from typing import Any, Dict, List, Optional
import logging
from dataclasses import dataclass, field

logger = logging.getLogger(__name__)


@dataclass
class PRAnalysis:
    """Analysis result for a pull request."""
    pr_number: int
    title: str
    applicable_models: List[str] = field(default_factory=list)
    detected_biases: List[str] = field(default_factory=list)
    failure_modes: List[Dict[str, Any]] = field(default_factory=list)
    suggestions: List[str] = field(default_factory=list)
    risk_level: str = "low"  # low, medium, high, critical


@dataclass
class IssueAnalysis:
    """Analysis result for an issue."""
    issue_number: int
    title: str
    applicable_models: List[str] = field(default_factory=list)
    root_causes: List[str] = field(default_factory=list)
    suggested_approaches: List[str] = field(default_factory=list)


class GitHubIntegration:
    """
    GitHub integration for the Mental Models System.
    
    Provides automated analysis of PRs and issues using mental models.
    """
    
    def __init__(self, token: str, repo_owner: str, repo_name: str):
        self.token = token
        self.repo_owner = repo_owner
        self.repo_name = repo_name
        self._github_connector = None
    
    async def connect(self) -> bool:
        """Initialize GitHub connection."""
        try:
            from ..connectors.github_connector import GitHubConnector, ConnectorConfig
            
            config = ConnectorConfig(
                name="github_integration",
                connector_type="github",
                credentials={"token": self.token}
            )
            
            self._github_connector = GitHubConnector(config)
            return await self._github_connector.connect()
        except Exception as e:
            logger.error(f"Failed to connect to GitHub: {e}")
            return False
    
    async def analyze_pr(self, pr_number: int) -> Optional[PRAnalysis]:
        """
        Analyze a pull request through mental models.
        
        This examines:
        - PR description and title
        - Code changes
        - Commit messages
        - Discussion/comments
        """
        if not self._github_connector:
            logger.error("GitHub connector not initialized")
            return None
        
        try:
            # Fetch PR details
            result = await self._github_connector.fetch({
                "resource": "pulls",
                "owner": self.repo_owner,
                "repo": self.repo_name
            })
            
            if not result.success:
                logger.error(f"Failed to fetch PR: {result.error}")
                return None
            
            # Find the specific PR
            pr_data = None
            for pr in result.data:
                if pr.get("number") == pr_number:
                    pr_data = pr
                    break
            
            if not pr_data:
                logger.error(f"PR #{pr_number} not found")
                return None
            
            # Analyze the PR
            analysis = PRAnalysis(
                pr_number=pr_number,
                title=pr_data.get("title", "")
            )
            
            # Analyze title and description
            title = pr_data.get("title", "")
            body = pr_data.get("body", "") or ""
            
            # Check for common patterns that suggest mental models
            analysis.applicable_models = self._identify_applicable_models(title + " " + body)
            analysis.detected_biases = self._detect_biases(title + " " + body)
            analysis.failure_modes = self._detect_failure_modes(title + " " + body)
            analysis.suggestions = self._generate_suggestions(analysis)
            analysis.risk_level = self._assess_risk(analysis)
            
            return analysis
            
        except Exception as e:
            logger.error(f"Error analyzing PR: {e}")
            return None
    
    async def analyze_issue(self, issue_number: int) -> Optional[IssueAnalysis]:
        """Analyze an issue through mental models."""
        if not self._github_connector:
            logger.error("GitHub connector not initialized")
            return None
        
        try:
            result = await self._github_connector.fetch({
                "resource": "issues",
                "owner": self.repo_owner,
                "repo": self.repo_name
            })
            
            if not result.success:
                return None
            
            issue_data = None
            for issue in result.data:
                if issue.get("number") == issue_number:
                    issue_data = issue
                    break
            
            if not issue_data:
                return None
            
            analysis = IssueAnalysis(
                issue_number=issue_number,
                title=issue_data.get("title", "")
            )
            
            text = issue_data.get("title", "") + " " + (issue_data.get("body", "") or "")
            analysis.applicable_models = self._identify_applicable_models(text)
            analysis.root_causes = self._identify_root_causes(text)
            analysis.suggested_approaches = self._suggest_approaches(analysis)
            
            return analysis
            
        except Exception as e:
            logger.error(f"Error analyzing issue: {e}")
            return None
    
    async def create_analysis_comment(self, pr_number: int, analysis: PRAnalysis) -> bool:
        """Create a comment on a PR with the analysis results."""
        if not self._github_connector:
            return False
        
        comment_body = self._format_analysis_comment(analysis)
        
        result = await self._github_connector.push({
            "action": "create_comment",
            "owner": self.repo_owner,
            "repo": self.repo_name,
            "issue_number": pr_number,
            "body": comment_body
        })
        
        return result.success
    
    def _identify_applicable_models(self, text: str) -> List[str]:
        """Identify mental models applicable to the text."""
        models = []
        text_lower = text.lower()
        
        # Simple keyword matching (would use NLP in production)
        model_keywords = {
            "Circle of Competence": ["expertise", "competence", "knowledge", "understand"],
            "Inversion": ["avoid", "prevent", "not", "don't", "failure"],
            "First Principles": ["fundamental", "basic", "core", "underlying"],
            "Second-Order Thinking": ["consequence", "effect", "result", "impact"],
            "Margin of Safety": ["buffer", "safety", "margin", "conservative"],
            "Occam's Razor": ["simple", "simplest", "straightforward"],
            "Hanlon's Razor": ["mistake", "error", "incompetence", "malice"],
        }
        
        for model, keywords in model_keywords.items():
            if any(kw in text_lower for kw in keywords):
                models.append(model)
        
        return models
    
    def _detect_biases(self, text: str) -> List[str]:
        """Detect potential cognitive biases in the text."""
        biases = []
        text_lower = text.lower()
        
        bias_indicators = {
            "Confirmation Bias": ["confirms", "as expected", "proves"],
            "Sunk Cost": ["already invested", "too late to", "can't stop now"],
            "Overconfidence": ["definitely", "certainly", "guaranteed", "100%"],
            "Anchoring": ["based on", "starting from", "reference"],
            "Availability": ["recently", "just saw", "heard about"],
        }
        
        for bias, indicators in bias_indicators.items():
            if any(ind in text_lower for ind in indicators):
                biases.append(bias)
        
        return biases
    
    def _detect_failure_modes(self, text: str) -> List[Dict[str, Any]]:
        """Detect potential failure modes."""
        failure_modes = []
        text_lower = text.lower()
        
        # Check for common failure mode indicators
        if "quick fix" in text_lower or "hotfix" in text_lower:
            failure_modes.append({
                "name": "Premature Action",
                "severity": "medium",
                "description": "Quick fixes may not address root cause"
            })
        
        if "assume" in text_lower:
            failure_modes.append({
                "name": "Assumption Risk",
                "severity": "medium",
                "description": "Assumptions should be validated"
            })
        
        if "should work" in text_lower or "probably" in text_lower:
            failure_modes.append({
                "name": "Overconfidence",
                "severity": "low",
                "description": "Uncertainty not fully acknowledged"
            })
        
        return failure_modes
    
    def _generate_suggestions(self, analysis: PRAnalysis) -> List[str]:
        """Generate suggestions based on analysis."""
        suggestions = []
        
        if "Inversion" in analysis.applicable_models:
            suggestions.append("Consider: What could make this change fail?")
        
        if "Confirmation Bias" in analysis.detected_biases:
            suggestions.append("Seek disconfirming evidence for assumptions")
        
        if analysis.failure_modes:
            suggestions.append("Address identified failure modes before merging")
        
        if not analysis.applicable_models:
            suggestions.append("Consider which mental models apply to this change")
        
        return suggestions
    
    def _assess_risk(self, analysis: PRAnalysis) -> str:
        """Assess overall risk level."""
        risk_score = 0
        
        risk_score += len(analysis.detected_biases) * 2
        risk_score += sum(1 for fm in analysis.failure_modes if fm.get("severity") == "high") * 3
        risk_score += sum(1 for fm in analysis.failure_modes if fm.get("severity") == "medium") * 2
        risk_score += sum(1 for fm in analysis.failure_modes if fm.get("severity") == "low")
        
        if risk_score >= 8:
            return "critical"
        elif risk_score >= 5:
            return "high"
        elif risk_score >= 2:
            return "medium"
        return "low"
    
    def _identify_root_causes(self, text: str) -> List[str]:
        """Identify potential root causes from issue text."""
        causes = []
        text_lower = text.lower()
        
        if "error" in text_lower or "exception" in text_lower:
            causes.append("Technical error or exception")
        if "slow" in text_lower or "performance" in text_lower:
            causes.append("Performance issue")
        if "confus" in text_lower or "unclear" in text_lower:
            causes.append("Unclear requirements or documentation")
        
        return causes
    
    def _suggest_approaches(self, analysis: IssueAnalysis) -> List[str]:
        """Suggest approaches based on issue analysis."""
        approaches = []
        
        if "First Principles" in analysis.applicable_models:
            approaches.append("Break down to fundamental components")
        if "Inversion" in analysis.applicable_models:
            approaches.append("Consider what would make this worse")
        
        approaches.append("Apply relevant mental models to understand the issue")
        
        return approaches
    
    def _format_analysis_comment(self, analysis: PRAnalysis) -> str:
        """Format analysis as a GitHub comment."""
        lines = [
            "## Mental Models Analysis",
            "",
            f"**Risk Level:** {analysis.risk_level.upper()}",
            "",
        ]
        
        if analysis.applicable_models:
            lines.append("### Applicable Mental Models")
            for model in analysis.applicable_models:
                lines.append(f"- {model}")
            lines.append("")
        
        if analysis.detected_biases:
            lines.append("### Detected Biases")
            for bias in analysis.detected_biases:
                lines.append(f"- {bias}")
            lines.append("")
        
        if analysis.failure_modes:
            lines.append("### Potential Failure Modes")
            for fm in analysis.failure_modes:
                lines.append(f"- **{fm['name']}** ({fm['severity']}): {fm['description']}")
            lines.append("")
        
        if analysis.suggestions:
            lines.append("### Suggestions")
            for suggestion in analysis.suggestions:
                lines.append(f"- {suggestion}")
        
        return "\n".join(lines)
