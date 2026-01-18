"""
Google Drive connector for documents, spreadsheets, and files.
"""

from typing import Any, Dict, List, Optional
import aiohttp
import logging
import base64

from .base import BaseConnector, ConnectorConfig, ConnectorResult

logger = logging.getLogger(__name__)


class GoogleDriveConnector(BaseConnector):
    """Connector for Google Drive API integration."""
    
    BASE_URL = "https://www.googleapis.com/drive/v3"
    
    def __init__(self, config: ConnectorConfig):
        super().__init__(config)
        self._session: Optional[aiohttp.ClientSession] = None
        self._access_token = config.credentials.get("access_token", "")
        self._refresh_token = config.credentials.get("refresh_token", "")
        self._client_id = config.credentials.get("client_id", "")
        self._client_secret = config.credentials.get("client_secret", "")
    
    async def connect(self) -> bool:
        """Establish connection to Google Drive API."""
        try:
            headers = {
                "Authorization": f"Bearer {self._access_token}",
                "Accept": "application/json"
            }
            
            self._session = aiohttp.ClientSession(headers=headers)
            
            # Test connection
            async with self._session.get(f"{self.BASE_URL}/about?fields=user") as resp:
                self._connected = resp.status == 200
                if self._connected:
                    data = await resp.json()
                    logger.info(f"Connected to Google Drive as {data.get('user', {}).get('emailAddress')}")
            
            return self._connected
        except Exception as e:
            logger.error(f"Failed to connect to Google Drive: {e}")
            return False
    
    async def disconnect(self) -> bool:
        """Close Google Drive API connection."""
        if self._session:
            await self._session.close()
            self._session = None
        self._connected = False
        return True
    
    async def _refresh_access_token(self) -> bool:
        """Refresh the access token using refresh token."""
        try:
            async with aiohttp.ClientSession() as session:
                async with session.post(
                    "https://oauth2.googleapis.com/token",
                    data={
                        "client_id": self._client_id,
                        "client_secret": self._client_secret,
                        "refresh_token": self._refresh_token,
                        "grant_type": "refresh_token"
                    }
                ) as resp:
                    if resp.status == 200:
                        data = await resp.json()
                        self._access_token = data.get("access_token")
                        # Reconnect with new token
                        await self.disconnect()
                        return await self.connect()
            return False
        except Exception as e:
            logger.error(f"Failed to refresh token: {e}")
            return False
    
    async def fetch(self, query: Dict[str, Any]) -> ConnectorResult:
        """
        Fetch data from Google Drive.
        
        Query parameters:
        - resource: "files", "file", "content"
        - file_id: File ID (for file/content)
        - q: Search query (for files)
        - page_size: Number of results
        - page_token: Pagination token
        - fields: Fields to return
        """
        if not self._session:
            return ConnectorResult(success=False, error="Not connected")
        
        try:
            resource = query.get("resource", "files")
            
            if resource == "files":
                url = f"{self.BASE_URL}/files"
                params = {
                    "pageSize": query.get("page_size", 100),
                    "fields": query.get("fields", "files(id,name,mimeType,modifiedTime,size)")
                }
                if query.get("q"):
                    params["q"] = query["q"]
                if query.get("page_token"):
                    params["pageToken"] = query["page_token"]
            elif resource == "file":
                file_id = query.get("file_id")
                url = f"{self.BASE_URL}/files/{file_id}"
                params = {"fields": query.get("fields", "*")}
            elif resource == "content":
                file_id = query.get("file_id")
                url = f"{self.BASE_URL}/files/{file_id}"
                params = {"alt": "media"}
            else:
                return ConnectorResult(success=False, error=f"Unknown resource: {resource}")
            
            async with self._session.get(url, params=params) as resp:
                if resp.status == 200:
                    if resource == "content":
                        data = await resp.read()
                        return ConnectorResult(
                            success=True,
                            data=data,
                            metadata={"url": url, "content_type": resp.content_type}
                        )
                    else:
                        data = await resp.json()
                        return ConnectorResult(
                            success=True,
                            data=data,
                            metadata={"url": url}
                        )
                elif resp.status == 401:
                    # Try to refresh token
                    if await self._refresh_access_token():
                        return await self.fetch(query)
                    return ConnectorResult(success=False, error="Authentication failed")
                else:
                    error_text = await resp.text()
                    return ConnectorResult(
                        success=False,
                        error=f"Google Drive API error: {resp.status} - {error_text}"
                    )
        except Exception as e:
            logger.error(f"Google Drive fetch error: {e}")
            return ConnectorResult(success=False, error=str(e))
    
    async def push(self, data: Any) -> ConnectorResult:
        """
        Push data to Google Drive (upload files, create folders).
        
        Data parameters:
        - action: "upload", "create_folder", "update"
        - name: File/folder name
        - content: File content (for upload)
        - mime_type: MIME type
        - parent_id: Parent folder ID
        - file_id: File ID (for update)
        """
        if not self._session:
            return ConnectorResult(success=False, error="Not connected")
        
        try:
            action = data.get("action")
            
            if action == "create_folder":
                url = f"{self.BASE_URL}/files"
                metadata = {
                    "name": data.get("name"),
                    "mimeType": "application/vnd.google-apps.folder"
                }
                if data.get("parent_id"):
                    metadata["parents"] = [data["parent_id"]]
                
                async with self._session.post(url, json=metadata) as resp:
                    if resp.status in [200, 201]:
                        result = await resp.json()
                        return ConnectorResult(success=True, data=result)
                    else:
                        error_text = await resp.text()
                        return ConnectorResult(success=False, error=error_text)
            
            elif action == "upload":
                # Simple upload for small files
                url = "https://www.googleapis.com/upload/drive/v3/files?uploadType=multipart"
                
                metadata = {
                    "name": data.get("name"),
                    "mimeType": data.get("mime_type", "text/plain")
                }
                if data.get("parent_id"):
                    metadata["parents"] = [data["parent_id"]]
                
                # For simplicity, using JSON metadata + base64 content
                # In production, use multipart upload
                return ConnectorResult(
                    success=False,
                    error="File upload requires multipart implementation"
                )
            
            else:
                return ConnectorResult(success=False, error=f"Unknown action: {action}")
                
        except Exception as e:
            logger.error(f"Google Drive push error: {e}")
            return ConnectorResult(success=False, error=str(e))
    
    async def search_files(self, query: str, page_size: int = 100) -> ConnectorResult:
        """Search for files matching query."""
        return await self.fetch({
            "resource": "files",
            "q": query,
            "page_size": page_size
        })
    
    async def get_file_metadata(self, file_id: str) -> ConnectorResult:
        """Get file metadata."""
        return await self.fetch({"resource": "file", "file_id": file_id})
    
    async def download_file(self, file_id: str) -> ConnectorResult:
        """Download file content."""
        return await self.fetch({"resource": "content", "file_id": file_id})
    
    async def list_folder(self, folder_id: str = "root") -> ConnectorResult:
        """List files in a folder."""
        return await self.fetch({
            "resource": "files",
            "q": f"'{folder_id}' in parents"
        })
