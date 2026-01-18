"""
GitHub connector for repository data, issues, PRs, and commits.
"""

from typing import Any, Dict, List, Optional
import aiohttp
import logging

from .base import BaseConnector, ConnectorConfig, ConnectorResult

logger = logging.getLogger(__name__)


class GitHubConnector(BaseConnector):
    """Connector for GitHub API integration."""
    
    BASE_URL = "https://api.github.com"
    
    def __init__(self, config: ConnectorConfig):
        super().__init__(config)
        self._session: Optional[aiohttp.ClientSession] = None
        self._token = config.credentials.get("token", "")
    
    async def connect(self) -> bool:
        """Establish connection to GitHub API."""
        try:
            headers = {
                "Accept": "application/vnd.github.v3+json",
                "User-Agent": "RippleAnalytics-MentalModels"
            }
            if self._token:
                headers["Authorization"] = f"token {self._token}"
            
            self._session = aiohttp.ClientSession(headers=headers)
            
            # Test connection
            async with self._session.get(f"{self.BASE_URL}/user") as resp:
                self._connected = resp.status in [200, 401]  # 401 is ok for public repos
            
            return self._connected
        except Exception as e:
            logger.error(f"Failed to connect to GitHub: {e}")
            return False
    
    async def disconnect(self) -> bool:
        """Close GitHub API connection."""
        if self._session:
            await self._session.close()
            self._session = None
        self._connected = False
        return True
    
    async def fetch(self, query: Dict[str, Any]) -> ConnectorResult:
        """
        Fetch data from GitHub.
        
        Query parameters:
        - resource: "repos", "issues", "pulls", "commits", "contents"
        - owner: Repository owner
        - repo: Repository name
        - path: File path (for contents)
        - state: Issue/PR state (open, closed, all)
        - per_page: Results per page
        - page: Page number
        """
        if not self._session:
            return ConnectorResult(success=False, error="Not connected")
        
        try:
            resource = query.get("resource", "repos")
            owner = query.get("owner")
            repo = query.get("repo")
            
            if resource == "repos":
                url = f"{self.BASE_URL}/repos/{owner}/{repo}"
            elif resource == "issues":
                url = f"{self.BASE_URL}/repos/{owner}/{repo}/issues"
            elif resource == "pulls":
                url = f"{self.BASE_URL}/repos/{owner}/{repo}/pulls"
            elif resource == "commits":
                url = f"{self.BASE_URL}/repos/{owner}/{repo}/commits"
            elif resource == "contents":
                path = query.get("path", "")
                url = f"{self.BASE_URL}/repos/{owner}/{repo}/contents/{path}"
            else:
                return ConnectorResult(success=False, error=f"Unknown resource: {resource}")
            
            params = {}
            if "state" in query:
                params["state"] = query["state"]
            if "per_page" in query:
                params["per_page"] = query["per_page"]
            if "page" in query:
                params["page"] = query["page"]
            
            async with self._session.get(url, params=params) as resp:
                if resp.status == 200:
                    data = await resp.json()
                    return ConnectorResult(
                        success=True,
                        data=data,
                        metadata={"url": url, "status": resp.status}
                    )
                else:
                    error_text = await resp.text()
                    return ConnectorResult(
                        success=False,
                        error=f"GitHub API error: {resp.status} - {error_text}"
                    )
        except Exception as e:
            logger.error(f"GitHub fetch error: {e}")
            return ConnectorResult(success=False, error=str(e))
    
    async def push(self, data: Any) -> ConnectorResult:
        """
        Push data to GitHub (create issues, comments, etc.).
        
        Data parameters:
        - action: "create_issue", "create_comment", "create_pr"
        - owner: Repository owner
        - repo: Repository name
        - title: Issue/PR title
        - body: Issue/PR/comment body
        - issue_number: For comments
        """
        if not self._session:
            return ConnectorResult(success=False, error="Not connected")
        
        try:
            action = data.get("action")
            owner = data.get("owner")
            repo = data.get("repo")
            
            if action == "create_issue":
                url = f"{self.BASE_URL}/repos/{owner}/{repo}/issues"
                payload = {
                    "title": data.get("title"),
                    "body": data.get("body", "")
                }
            elif action == "create_comment":
                issue_number = data.get("issue_number")
                url = f"{self.BASE_URL}/repos/{owner}/{repo}/issues/{issue_number}/comments"
                payload = {"body": data.get("body")}
            else:
                return ConnectorResult(success=False, error=f"Unknown action: {action}")
            
            async with self._session.post(url, json=payload) as resp:
                if resp.status in [200, 201]:
                    result = await resp.json()
                    return ConnectorResult(success=True, data=result)
                else:
                    error_text = await resp.text()
                    return ConnectorResult(
                        success=False,
                        error=f"GitHub API error: {resp.status} - {error_text}"
                    )
        except Exception as e:
            logger.error(f"GitHub push error: {e}")
            return ConnectorResult(success=False, error=str(e))
    
    async def get_repo_info(self, owner: str, repo: str) -> ConnectorResult:
        """Get repository information."""
        return await self.fetch({"resource": "repos", "owner": owner, "repo": repo})
    
    async def list_issues(self, owner: str, repo: str, state: str = "open") -> ConnectorResult:
        """List repository issues."""
        return await self.fetch({
            "resource": "issues",
            "owner": owner,
            "repo": repo,
            "state": state
        })
    
    async def list_pulls(self, owner: str, repo: str, state: str = "open") -> ConnectorResult:
        """List pull requests."""
        return await self.fetch({
            "resource": "pulls",
            "owner": owner,
            "repo": repo,
            "state": state
        })
    
    async def get_file_contents(self, owner: str, repo: str, path: str) -> ConnectorResult:
        """Get file contents from repository."""
        return await self.fetch({
            "resource": "contents",
            "owner": owner,
            "repo": repo,
            "path": path
        })
