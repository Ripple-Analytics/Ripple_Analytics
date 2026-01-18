"""
Generic API connector for REST and GraphQL APIs.
"""

from typing import Any, Dict, List, Optional, Union
import aiohttp
import logging
from enum import Enum

from .base import BaseConnector, ConnectorConfig, ConnectorResult

logger = logging.getLogger(__name__)


class APIMethod(Enum):
    GET = "GET"
    POST = "POST"
    PUT = "PUT"
    PATCH = "PATCH"
    DELETE = "DELETE"


class APIConnector(BaseConnector):
    """Generic connector for REST APIs."""
    
    def __init__(self, config: ConnectorConfig):
        super().__init__(config)
        self._session: Optional[aiohttp.ClientSession] = None
        self._base_url = config.settings.get("base_url", "")
        self._auth_type = config.settings.get("auth_type", "bearer")  # bearer, basic, api_key
        self._api_key = config.credentials.get("api_key", "")
        self._api_key_header = config.settings.get("api_key_header", "X-API-Key")
        self._bearer_token = config.credentials.get("bearer_token", "")
        self._username = config.credentials.get("username", "")
        self._password = config.credentials.get("password", "")
    
    async def connect(self) -> bool:
        """Initialize HTTP session with authentication."""
        try:
            headers = {
                "Accept": "application/json",
                "Content-Type": "application/json"
            }
            
            # Set up authentication
            auth = None
            if self._auth_type == "bearer" and self._bearer_token:
                headers["Authorization"] = f"Bearer {self._bearer_token}"
            elif self._auth_type == "api_key" and self._api_key:
                headers[self._api_key_header] = self._api_key
            elif self._auth_type == "basic" and self._username:
                auth = aiohttp.BasicAuth(self._username, self._password)
            
            timeout = aiohttp.ClientTimeout(total=self.config.timeout)
            self._session = aiohttp.ClientSession(
                headers=headers,
                auth=auth,
                timeout=timeout
            )
            self._connected = True
            return True
        except Exception as e:
            logger.error(f"Failed to initialize API connector: {e}")
            return False
    
    async def disconnect(self) -> bool:
        """Close HTTP session."""
        if self._session:
            await self._session.close()
            self._session = None
        self._connected = False
        return True
    
    async def fetch(self, query: Dict[str, Any]) -> ConnectorResult:
        """
        Fetch data from API (GET request).
        
        Query parameters:
        - endpoint: API endpoint (appended to base_url)
        - params: Query parameters
        - headers: Additional headers
        """
        if not self._session:
            return ConnectorResult(success=False, error="Not connected")
        
        try:
            endpoint = query.get("endpoint", "")
            params = query.get("params", {})
            headers = query.get("headers", {})
            
            url = f"{self._base_url}{endpoint}" if self._base_url else endpoint
            
            async with self._session.get(url, params=params, headers=headers) as resp:
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
                        error=f"API error: {resp.status} - {error_text}"
                    )
        except Exception as e:
            logger.error(f"API fetch error: {e}")
            return ConnectorResult(success=False, error=str(e))
    
    async def push(self, data: Any) -> ConnectorResult:
        """
        Push data to API (POST/PUT/PATCH/DELETE).
        
        Data parameters:
        - method: HTTP method (POST, PUT, PATCH, DELETE)
        - endpoint: API endpoint
        - body: Request body (JSON)
        - headers: Additional headers
        """
        if not self._session:
            return ConnectorResult(success=False, error="Not connected")
        
        try:
            method = data.get("method", "POST").upper()
            endpoint = data.get("endpoint", "")
            body = data.get("body", {})
            headers = data.get("headers", {})
            
            url = f"{self._base_url}{endpoint}" if self._base_url else endpoint
            
            if method == "POST":
                async with self._session.post(url, json=body, headers=headers) as resp:
                    return await self._handle_response(resp, url)
            elif method == "PUT":
                async with self._session.put(url, json=body, headers=headers) as resp:
                    return await self._handle_response(resp, url)
            elif method == "PATCH":
                async with self._session.patch(url, json=body, headers=headers) as resp:
                    return await self._handle_response(resp, url)
            elif method == "DELETE":
                async with self._session.delete(url, headers=headers) as resp:
                    return await self._handle_response(resp, url)
            else:
                return ConnectorResult(success=False, error=f"Unknown method: {method}")
                
        except Exception as e:
            logger.error(f"API push error: {e}")
            return ConnectorResult(success=False, error=str(e))
    
    async def _handle_response(self, resp: aiohttp.ClientResponse, url: str) -> ConnectorResult:
        """Handle API response."""
        if resp.status in [200, 201, 204]:
            try:
                data = await resp.json() if resp.status != 204 else None
            except:
                data = await resp.text()
            return ConnectorResult(
                success=True,
                data=data,
                metadata={"url": url, "status": resp.status}
            )
        else:
            error_text = await resp.text()
            return ConnectorResult(
                success=False,
                error=f"API error: {resp.status} - {error_text}"
            )
    
    async def get(self, endpoint: str, params: Optional[Dict] = None) -> ConnectorResult:
        """Make a GET request."""
        return await self.fetch({"endpoint": endpoint, "params": params or {}})
    
    async def post(self, endpoint: str, body: Dict) -> ConnectorResult:
        """Make a POST request."""
        return await self.push({"method": "POST", "endpoint": endpoint, "body": body})
    
    async def put(self, endpoint: str, body: Dict) -> ConnectorResult:
        """Make a PUT request."""
        return await self.push({"method": "PUT", "endpoint": endpoint, "body": body})
    
    async def delete(self, endpoint: str) -> ConnectorResult:
        """Make a DELETE request."""
        return await self.push({"method": "DELETE", "endpoint": endpoint})


class GraphQLConnector(BaseConnector):
    """Connector for GraphQL APIs."""
    
    def __init__(self, config: ConnectorConfig):
        super().__init__(config)
        self._session: Optional[aiohttp.ClientSession] = None
        self._endpoint = config.settings.get("endpoint", "")
        self._bearer_token = config.credentials.get("bearer_token", "")
    
    async def connect(self) -> bool:
        """Initialize HTTP session."""
        try:
            headers = {
                "Accept": "application/json",
                "Content-Type": "application/json"
            }
            
            if self._bearer_token:
                headers["Authorization"] = f"Bearer {self._bearer_token}"
            
            self._session = aiohttp.ClientSession(headers=headers)
            self._connected = True
            return True
        except Exception as e:
            logger.error(f"Failed to initialize GraphQL connector: {e}")
            return False
    
    async def disconnect(self) -> bool:
        """Close HTTP session."""
        if self._session:
            await self._session.close()
            self._session = None
        self._connected = False
        return True
    
    async def fetch(self, query: Dict[str, Any]) -> ConnectorResult:
        """
        Execute a GraphQL query.
        
        Query parameters:
        - query: GraphQL query string
        - variables: Query variables
        - operation_name: Operation name (optional)
        """
        return await self._execute(query)
    
    async def push(self, data: Any) -> ConnectorResult:
        """
        Execute a GraphQL mutation.
        
        Data parameters:
        - mutation: GraphQL mutation string
        - variables: Mutation variables
        - operation_name: Operation name (optional)
        """
        return await self._execute(data)
    
    async def _execute(self, request: Dict[str, Any]) -> ConnectorResult:
        """Execute a GraphQL request."""
        if not self._session:
            return ConnectorResult(success=False, error="Not connected")
        
        try:
            payload = {
                "query": request.get("query") or request.get("mutation"),
                "variables": request.get("variables", {})
            }
            
            if request.get("operation_name"):
                payload["operationName"] = request["operation_name"]
            
            async with self._session.post(self._endpoint, json=payload) as resp:
                data = await resp.json()
                
                if "errors" in data:
                    return ConnectorResult(
                        success=False,
                        error=str(data["errors"]),
                        data=data.get("data")
                    )
                
                return ConnectorResult(
                    success=True,
                    data=data.get("data"),
                    metadata={"endpoint": self._endpoint}
                )
        except Exception as e:
            logger.error(f"GraphQL error: {e}")
            return ConnectorResult(success=False, error=str(e))
    
    async def query(self, query: str, variables: Optional[Dict] = None) -> ConnectorResult:
        """Execute a GraphQL query."""
        return await self.fetch({"query": query, "variables": variables or {}})
    
    async def mutate(self, mutation: str, variables: Optional[Dict] = None) -> ConnectorResult:
        """Execute a GraphQL mutation."""
        return await self.push({"mutation": mutation, "variables": variables or {}})
