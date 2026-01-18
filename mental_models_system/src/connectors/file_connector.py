"""
File connector for local and remote file access.
"""

from typing import Any, Dict, List, Optional, Union
import os
import logging
from pathlib import Path
from dataclasses import dataclass
import aiofiles
import aiofiles.os

from .base import BaseConnector, ConnectorConfig, ConnectorResult

logger = logging.getLogger(__name__)


@dataclass
class FileInfo:
    """Information about a file."""
    path: str
    name: str
    size: int
    is_dir: bool
    extension: str
    modified_time: float


class FileConnector(BaseConnector):
    """Connector for local file system access."""
    
    def __init__(self, config: ConnectorConfig):
        super().__init__(config)
        self._base_path = config.settings.get("base_path", "")
        self._allowed_extensions = config.settings.get("allowed_extensions", [])
    
    async def connect(self) -> bool:
        """Verify base path exists."""
        try:
            if self._base_path:
                if not os.path.exists(self._base_path):
                    logger.error(f"Base path does not exist: {self._base_path}")
                    return False
            self._connected = True
            return True
        except Exception as e:
            logger.error(f"Failed to connect to file system: {e}")
            return False
    
    async def disconnect(self) -> bool:
        """No cleanup needed for file system."""
        self._connected = False
        return True
    
    def _resolve_path(self, path: str) -> str:
        """Resolve path relative to base path."""
        if self._base_path:
            return os.path.join(self._base_path, path)
        return path
    
    def _check_extension(self, path: str) -> bool:
        """Check if file extension is allowed."""
        if not self._allowed_extensions:
            return True
        ext = os.path.splitext(path)[1].lower()
        return ext in self._allowed_extensions
    
    async def fetch(self, query: Dict[str, Any]) -> ConnectorResult:
        """
        Fetch file content or directory listing.
        
        Query parameters:
        - path: File or directory path
        - action: "read", "list", "info"
        - encoding: File encoding (default: utf-8)
        - binary: Read as binary (default: False)
        """
        try:
            path = self._resolve_path(query.get("path", ""))
            action = query.get("action", "read")
            
            if action == "list":
                return await self._list_directory(path)
            elif action == "info":
                return await self._get_file_info(path)
            elif action == "read":
                return await self._read_file(
                    path,
                    encoding=query.get("encoding", "utf-8"),
                    binary=query.get("binary", False)
                )
            else:
                return ConnectorResult(success=False, error=f"Unknown action: {action}")
                
        except Exception as e:
            logger.error(f"File fetch error: {e}")
            return ConnectorResult(success=False, error=str(e))
    
    async def push(self, data: Any) -> ConnectorResult:
        """
        Write file content.
        
        Data parameters:
        - path: File path
        - content: Content to write
        - action: "write", "append", "delete", "mkdir"
        - encoding: File encoding (default: utf-8)
        - binary: Write as binary (default: False)
        """
        try:
            path = self._resolve_path(data.get("path", ""))
            action = data.get("action", "write")
            
            if action == "write":
                return await self._write_file(
                    path,
                    data.get("content", ""),
                    encoding=data.get("encoding", "utf-8"),
                    binary=data.get("binary", False)
                )
            elif action == "append":
                return await self._append_file(
                    path,
                    data.get("content", ""),
                    encoding=data.get("encoding", "utf-8")
                )
            elif action == "delete":
                return await self._delete_file(path)
            elif action == "mkdir":
                return await self._create_directory(path)
            else:
                return ConnectorResult(success=False, error=f"Unknown action: {action}")
                
        except Exception as e:
            logger.error(f"File push error: {e}")
            return ConnectorResult(success=False, error=str(e))
    
    async def _read_file(self, path: str, encoding: str = "utf-8", binary: bool = False) -> ConnectorResult:
        """Read file content."""
        if not os.path.exists(path):
            return ConnectorResult(success=False, error=f"File not found: {path}")
        
        if not self._check_extension(path):
            return ConnectorResult(success=False, error=f"Extension not allowed: {path}")
        
        mode = "rb" if binary else "r"
        async with aiofiles.open(path, mode=mode, encoding=None if binary else encoding) as f:
            content = await f.read()
        
        return ConnectorResult(
            success=True,
            data=content,
            metadata={"path": path, "size": len(content)}
        )
    
    async def _write_file(self, path: str, content: Union[str, bytes], encoding: str = "utf-8", binary: bool = False) -> ConnectorResult:
        """Write content to file."""
        # Create parent directories if needed
        parent = os.path.dirname(path)
        if parent and not os.path.exists(parent):
            os.makedirs(parent, exist_ok=True)
        
        mode = "wb" if binary else "w"
        async with aiofiles.open(path, mode=mode, encoding=None if binary else encoding) as f:
            await f.write(content)
        
        return ConnectorResult(
            success=True,
            metadata={"path": path, "size": len(content)}
        )
    
    async def _append_file(self, path: str, content: str, encoding: str = "utf-8") -> ConnectorResult:
        """Append content to file."""
        async with aiofiles.open(path, mode="a", encoding=encoding) as f:
            await f.write(content)
        
        return ConnectorResult(
            success=True,
            metadata={"path": path}
        )
    
    async def _delete_file(self, path: str) -> ConnectorResult:
        """Delete a file."""
        if not os.path.exists(path):
            return ConnectorResult(success=False, error=f"File not found: {path}")
        
        await aiofiles.os.remove(path)
        return ConnectorResult(success=True, metadata={"path": path})
    
    async def _create_directory(self, path: str) -> ConnectorResult:
        """Create a directory."""
        os.makedirs(path, exist_ok=True)
        return ConnectorResult(success=True, metadata={"path": path})
    
    async def _list_directory(self, path: str) -> ConnectorResult:
        """List directory contents."""
        if not os.path.exists(path):
            return ConnectorResult(success=False, error=f"Directory not found: {path}")
        
        if not os.path.isdir(path):
            return ConnectorResult(success=False, error=f"Not a directory: {path}")
        
        files = []
        for entry in os.scandir(path):
            stat = entry.stat()
            files.append(FileInfo(
                path=entry.path,
                name=entry.name,
                size=stat.st_size,
                is_dir=entry.is_dir(),
                extension=os.path.splitext(entry.name)[1].lower(),
                modified_time=stat.st_mtime
            ))
        
        return ConnectorResult(
            success=True,
            data=files,
            metadata={"path": path, "count": len(files)}
        )
    
    async def _get_file_info(self, path: str) -> ConnectorResult:
        """Get file information."""
        if not os.path.exists(path):
            return ConnectorResult(success=False, error=f"File not found: {path}")
        
        stat = os.stat(path)
        info = FileInfo(
            path=path,
            name=os.path.basename(path),
            size=stat.st_size,
            is_dir=os.path.isdir(path),
            extension=os.path.splitext(path)[1].lower(),
            modified_time=stat.st_mtime
        )
        
        return ConnectorResult(success=True, data=info)
    
    # Convenience methods
    
    async def read_text(self, path: str, encoding: str = "utf-8") -> ConnectorResult:
        """Read text file."""
        return await self.fetch({"path": path, "action": "read", "encoding": encoding})
    
    async def read_binary(self, path: str) -> ConnectorResult:
        """Read binary file."""
        return await self.fetch({"path": path, "action": "read", "binary": True})
    
    async def write_text(self, path: str, content: str, encoding: str = "utf-8") -> ConnectorResult:
        """Write text file."""
        return await self.push({"path": path, "content": content, "action": "write", "encoding": encoding})
    
    async def list_files(self, path: str = "") -> ConnectorResult:
        """List files in directory."""
        return await self.fetch({"path": path, "action": "list"})
    
    async def get_info(self, path: str) -> ConnectorResult:
        """Get file info."""
        return await self.fetch({"path": path, "action": "info"})
