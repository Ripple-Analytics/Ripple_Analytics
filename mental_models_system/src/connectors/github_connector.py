"""
GitHub Connector

Full GitHub integration for the Mental Models System:
- Issue creation and management
- PR management and code review
- Webhook listeners
- Actions integration
- Repository management
"""

import os
import json
import asyncio
import logging
import subprocess
from dataclasses import dataclass
from datetime import datetime
from typing import Dict, List, Optional, Any
from enum import Enum

from .base import BaseConnector, ConnectorConfig, ConnectorType, ConnectorStatus

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class IssueState(Enum):
    OPEN = "open"
    CLOSED = "closed"
    ALL = "all"


class PRState(Enum):
    OPEN = "open"
    CLOSED = "closed"
    MERGED = "merged"
    ALL = "all"


@dataclass
class Issue:
    """GitHub issue."""
    number: int
    title: str
    body: str
    state: str
    labels: List[str]
    assignees: List[str]
    created_at: str
    updated_at: str
    url: str
    
    @classmethod
    def from_dict(cls, data: Dict) -> "Issue":
        return cls(
            number=data.get("number"),
            title=data.get("title"),
            body=data.get("body", ""),
            state=data.get("state"),
            labels=[l.get("name") for l in data.get("labels", [])],
            assignees=[a.get("login") for a in data.get("assignees", [])],
            created_at=data.get("created_at"),
            updated_at=data.get("updated_at"),
            url=data.get("html_url")
        )


@dataclass
class PullRequest:
    """GitHub pull request."""
    number: int
    title: str
    body: str
    state: str
    head_branch: str
    base_branch: str
    mergeable: bool
    created_at: str
    updated_at: str
    url: str
    
    @classmethod
    def from_dict(cls, data: Dict) -> "PullRequest":
        return cls(
            number=data.get("number"),
            title=data.get("title"),
            body=data.get("body", ""),
            state=data.get("state"),
            head_branch=data.get("head", {}).get("ref"),
            base_branch=data.get("base", {}).get("ref"),
            mergeable=data.get("mergeable"),
            created_at=data.get("created_at"),
            updated_at=data.get("updated_at"),
            url=data.get("html_url")
        )


class GitHubConnector(BaseConnector):
    """
    GitHub connector using gh CLI and REST API.
    
    Supports:
    - Issue management (create, update, close, list, search)
    - PR management (create, review, merge, list)
    - Repository operations (clone, fork, create)
    - Webhook management
    - Actions workflow triggers
    - Code search
    """
    
    NAME = "github"
    TYPE = ConnectorType.VERSION_CONTROL
    DESCRIPTION = "GitHub integration for issues, PRs, repos, and actions"
    REQUIRED_CREDENTIALS = []  # Uses gh CLI auth
    OPTIONAL_CREDENTIALS = ["token", "api_url"]
    OPEN_SOURCE = True
    
    def __init__(self, config: ConnectorConfig = None):
        super().__init__(config)
        self.default_repo = os.environ.get("GITHUB_REPO", "")
        self._gh_available = self._check_gh_cli()
    
    def _check_gh_cli(self) -> bool:
        """Check if gh CLI is available and authenticated."""
        try:
            result = subprocess.run(
                ["gh", "auth", "status"],
                capture_output=True,
                text=True,
                timeout=10
            )
            return result.returncode == 0
        except Exception:
            return False
    
    def _run_gh(self, args: List[str], json_output: bool = True) -> Optional[Any]:
        """Run gh CLI command."""
        cmd = ["gh"] + args
        if json_output:
            cmd.append("--json")
            # Add common fields
            if "issue" in args or "pr" in args:
                cmd.append("number,title,body,state,labels,assignees,createdAt,updatedAt,url")
        
        try:
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=60
            )
            
            if result.returncode != 0:
                logger.error(f"gh command failed: {result.stderr}")
                return None
            
            if json_output and result.stdout.strip():
                return json.loads(result.stdout)
            return result.stdout
            
        except subprocess.TimeoutExpired:
            logger.error("gh command timed out")
            return None
        except json.JSONDecodeError as e:
            logger.error(f"Failed to parse gh output: {e}")
            return None
        except Exception as e:
            logger.error(f"gh command error: {e}")
            return None
    
    async def connect(self) -> bool:
        """Verify GitHub connection."""
        if self._gh_available:
            self.status = ConnectorStatus.CONNECTED
            return True
        
        self.status = ConnectorStatus.ERROR
        return False
    
    async def disconnect(self) -> bool:
        """Disconnect (no-op for GitHub)."""
        self.status = ConnectorStatus.DISCONNECTED
        return True
    
    async def health_check(self) -> bool:
        """Check GitHub connection health."""
        return self._check_gh_cli()
    
    # =========================================================================
    # ISSUE MANAGEMENT
    # =========================================================================
    
    async def create_issue(self, title: str, body: str, repo: str = None,
                          labels: List[str] = None, assignees: List[str] = None) -> Optional[Issue]:
        """Create a new issue."""
        repo = repo or self.default_repo
        if not repo:
            logger.error("No repository specified")
            return None
        
        args = ["issue", "create", "-R", repo, "-t", title, "-b", body]
        
        if labels:
            for label in labels:
                args.extend(["-l", label])
        
        if assignees:
            for assignee in assignees:
                args.extend(["-a", assignee])
        
        result = self._run_gh(args, json_output=False)
        if result:
            # Parse issue URL to get number
            logger.info(f"Created issue: {result.strip()}")
            # Get the issue details
            issue_url = result.strip()
            issue_number = issue_url.split("/")[-1]
            return await self.get_issue(int(issue_number), repo)
        
        return None
    
    async def get_issue(self, number: int, repo: str = None) -> Optional[Issue]:
        """Get an issue by number."""
        repo = repo or self.default_repo
        args = ["issue", "view", str(number), "-R", repo]
        
        result = self._run_gh(args)
        if result:
            return Issue.from_dict(result)
        return None
    
    async def list_issues(self, repo: str = None, state: IssueState = IssueState.OPEN,
                         labels: List[str] = None, limit: int = 30) -> List[Issue]:
        """List issues."""
        repo = repo or self.default_repo
        args = ["issue", "list", "-R", repo, "-s", state.value, "-L", str(limit)]
        
        if labels:
            for label in labels:
                args.extend(["-l", label])
        
        result = self._run_gh(args)
        if result:
            return [Issue.from_dict(i) for i in result]
        return []
    
    async def update_issue(self, number: int, repo: str = None,
                          title: str = None, body: str = None,
                          add_labels: List[str] = None,
                          remove_labels: List[str] = None) -> bool:
        """Update an issue."""
        repo = repo or self.default_repo
        args = ["issue", "edit", str(number), "-R", repo]
        
        if title:
            args.extend(["-t", title])
        if body:
            args.extend(["-b", body])
        if add_labels:
            args.extend(["--add-label", ",".join(add_labels)])
        if remove_labels:
            args.extend(["--remove-label", ",".join(remove_labels)])
        
        result = self._run_gh(args, json_output=False)
        return result is not None
    
    async def close_issue(self, number: int, repo: str = None,
                         reason: str = "completed") -> bool:
        """Close an issue."""
        repo = repo or self.default_repo
        args = ["issue", "close", str(number), "-R", repo, "-r", reason]
        
        result = self._run_gh(args, json_output=False)
        return result is not None
    
    async def comment_on_issue(self, number: int, body: str, repo: str = None) -> bool:
        """Add a comment to an issue."""
        repo = repo or self.default_repo
        args = ["issue", "comment", str(number), "-R", repo, "-b", body]
        
        result = self._run_gh(args, json_output=False)
        return result is not None
    
    async def search_issues(self, query: str, repo: str = None, limit: int = 30) -> List[Issue]:
        """Search issues."""
        repo = repo or self.default_repo
        search_query = f"{query} repo:{repo}" if repo else query
        args = ["search", "issues", search_query, "-L", str(limit)]
        
        result = self._run_gh(args)
        if result:
            return [Issue.from_dict(i) for i in result.get("items", [])]
        return []
    
    # =========================================================================
    # PULL REQUEST MANAGEMENT
    # =========================================================================
    
    async def create_pr(self, title: str, body: str, head: str, base: str = "main",
                       repo: str = None, draft: bool = False) -> Optional[PullRequest]:
        """Create a pull request."""
        repo = repo or self.default_repo
        args = ["pr", "create", "-R", repo, "-t", title, "-b", body,
                "-H", head, "-B", base]
        
        if draft:
            args.append("-d")
        
        result = self._run_gh(args, json_output=False)
        if result:
            logger.info(f"Created PR: {result.strip()}")
            # Get PR details
            pr_url = result.strip()
            pr_number = pr_url.split("/")[-1]
            return await self.get_pr(int(pr_number), repo)
        
        return None
    
    async def get_pr(self, number: int, repo: str = None) -> Optional[PullRequest]:
        """Get a pull request by number."""
        repo = repo or self.default_repo
        args = ["pr", "view", str(number), "-R", repo]
        
        result = self._run_gh(args)
        if result:
            return PullRequest.from_dict(result)
        return None
    
    async def list_prs(self, repo: str = None, state: PRState = PRState.OPEN,
                      limit: int = 30) -> List[PullRequest]:
        """List pull requests."""
        repo = repo or self.default_repo
        args = ["pr", "list", "-R", repo, "-s", state.value, "-L", str(limit)]
        
        result = self._run_gh(args)
        if result:
            return [PullRequest.from_dict(pr) for pr in result]
        return []
    
    async def merge_pr(self, number: int, repo: str = None,
                      method: str = "merge", delete_branch: bool = True) -> bool:
        """Merge a pull request."""
        repo = repo or self.default_repo
        args = ["pr", "merge", str(number), "-R", repo, f"--{method}"]
        
        if delete_branch:
            args.append("-d")
        
        result = self._run_gh(args, json_output=False)
        return result is not None
    
    async def review_pr(self, number: int, repo: str = None,
                       approve: bool = True, body: str = None) -> bool:
        """Review a pull request."""
        repo = repo or self.default_repo
        args = ["pr", "review", str(number), "-R", repo]
        
        if approve:
            args.append("-a")
        else:
            args.append("-r")
        
        if body:
            args.extend(["-b", body])
        
        result = self._run_gh(args, json_output=False)
        return result is not None
    
    async def comment_on_pr(self, number: int, body: str, repo: str = None) -> bool:
        """Add a comment to a PR."""
        repo = repo or self.default_repo
        args = ["pr", "comment", str(number), "-R", repo, "-b", body]
        
        result = self._run_gh(args, json_output=False)
        return result is not None
    
    # =========================================================================
    # REPOSITORY MANAGEMENT
    # =========================================================================
    
    async def clone_repo(self, repo: str, path: str = None) -> bool:
        """Clone a repository."""
        args = ["repo", "clone", repo]
        if path:
            args.append(path)
        
        result = self._run_gh(args, json_output=False)
        return result is not None
    
    async def fork_repo(self, repo: str, clone: bool = True) -> bool:
        """Fork a repository."""
        args = ["repo", "fork", repo]
        if clone:
            args.append("--clone")
        
        result = self._run_gh(args, json_output=False)
        return result is not None
    
    async def create_repo(self, name: str, description: str = "",
                         private: bool = False, clone: bool = True) -> bool:
        """Create a new repository."""
        args = ["repo", "create", name, "-d", description]
        
        if private:
            args.append("--private")
        else:
            args.append("--public")
        
        if clone:
            args.append("--clone")
        
        result = self._run_gh(args, json_output=False)
        return result is not None
    
    async def get_repo_info(self, repo: str = None) -> Optional[Dict]:
        """Get repository information."""
        repo = repo or self.default_repo
        args = ["repo", "view", repo, "--json",
                "name,description,url,defaultBranchRef,stargazerCount,forkCount"]
        
        return self._run_gh(args)
    
    # =========================================================================
    # ACTIONS / WORKFLOWS
    # =========================================================================
    
    async def list_workflows(self, repo: str = None) -> List[Dict]:
        """List repository workflows."""
        repo = repo or self.default_repo
        args = ["workflow", "list", "-R", repo]
        
        result = self._run_gh(args)
        return result if result else []
    
    async def run_workflow(self, workflow: str, repo: str = None,
                          ref: str = "main", inputs: Dict = None) -> bool:
        """Trigger a workflow run."""
        repo = repo or self.default_repo
        args = ["workflow", "run", workflow, "-R", repo, "-r", ref]
        
        if inputs:
            for key, value in inputs.items():
                args.extend(["-f", f"{key}={value}"])
        
        result = self._run_gh(args, json_output=False)
        return result is not None
    
    async def list_workflow_runs(self, repo: str = None, workflow: str = None,
                                limit: int = 10) -> List[Dict]:
        """List workflow runs."""
        repo = repo or self.default_repo
        args = ["run", "list", "-R", repo, "-L", str(limit)]
        
        if workflow:
            args.extend(["-w", workflow])
        
        result = self._run_gh(args)
        return result if result else []
    
    # =========================================================================
    # WEBHOOKS
    # =========================================================================
    
    async def create_webhook(self, repo: str, url: str, events: List[str],
                            secret: str = None) -> bool:
        """Create a webhook (requires API token)."""
        # Webhooks require API access, not just gh CLI
        logger.warning("Webhook creation requires GitHub API token")
        return False
    
    # =========================================================================
    # CODE SEARCH
    # =========================================================================
    
    async def search_code(self, query: str, repo: str = None,
                         language: str = None, limit: int = 30) -> List[Dict]:
        """Search code."""
        search_query = query
        if repo:
            search_query += f" repo:{repo}"
        if language:
            search_query += f" language:{language}"
        
        args = ["search", "code", search_query, "-L", str(limit)]
        
        result = self._run_gh(args)
        return result if result else []
    
    # =========================================================================
    # CONVENIENCE METHODS
    # =========================================================================
    
    async def delegate_to_devin(self, task: str, repo: str = None,
                               labels: List[str] = None) -> Optional[Issue]:
        """Create an issue for Devin to work on."""
        labels = labels or ["devin", "automated"]
        
        body = f"""## Task for Devin

{task}

---

*This issue was automatically created by the Mental Models System.*
*Assigned to: @devin-ai*

### Instructions
1. Pull latest from master
2. Create a feature branch
3. Implement the task
4. Create a PR when complete
5. Request review

### Principle
**Improvement = Iteration Speed × Iteration Magnitude**
"""
        
        return await self.create_issue(
            title=f"[Devin] {task[:50]}...",
            body=body,
            repo=repo,
            labels=labels,
            assignees=["devin-ai"]
        )
    
    async def create_improvement_issue(self, improvement: Dict, repo: str = None) -> Optional[Issue]:
        """Create an issue from an improvement suggestion."""
        title = improvement.get("title", "Improvement suggestion")
        
        body = f"""## Improvement Suggestion

**Category:** {improvement.get('category', 'General')}
**Priority:** {improvement.get('priority', 'Medium')}
**Source:** {improvement.get('source', 'Mental Models System')}

### Description
{improvement.get('description', '')}

### Expected Impact
{improvement.get('impact', 'TBD')}

### Implementation Notes
{improvement.get('notes', '')}

---
*Auto-generated by Mental Models System*
"""
        
        return await self.create_issue(
            title=title,
            body=body,
            repo=repo,
            labels=["improvement", "automated"]
        )


# Register connector
from .base import registry
registry.register_class(GitHubConnector)
