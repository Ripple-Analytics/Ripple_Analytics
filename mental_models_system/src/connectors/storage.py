"""
Storage Connectors

File storage integrations:
- Local filesystem
- S3/MinIO (open source S3)
- Google Drive
- Dropbox
"""

import os
import json
import asyncio
import logging
import hashlib
import shutil
from dataclasses import dataclass
from datetime import datetime
from typing import Dict, List, Optional, Any, BinaryIO
from pathlib import Path
import subprocess

from .base import BaseConnector, ConnectorConfig, ConnectorType, ConnectorStatus

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


@dataclass
class StoredFile:
    """A stored file."""
    path: str
    name: str
    size: int
    modified: datetime
    content_type: str
    checksum: str
    
    def to_dict(self) -> Dict:
        return {
            "path": self.path,
            "name": self.name,
            "size": self.size,
            "modified": self.modified.isoformat(),
            "content_type": self.content_type,
            "checksum": self.checksum
        }


# =============================================================================
# LOCAL FILESYSTEM CONNECTOR
# =============================================================================

class LocalConnector(BaseConnector):
    """
    Local filesystem storage.
    
    Simple, fast, no dependencies.
    """
    
    NAME = "local"
    TYPE = ConnectorType.STORAGE
    DESCRIPTION = "Local filesystem storage"
    REQUIRED_CREDENTIALS = []
    OPTIONAL_CREDENTIALS = ["base_path"]
    OPEN_SOURCE = True
    
    def __init__(self, config: ConnectorConfig = None):
        super().__init__(config)
        self._base_path = Path(os.environ.get("LOCAL_STORAGE_PATH", "/home/ubuntu/data"))
    
    async def connect(self) -> bool:
        base_path = self.config.settings.get("base_path")
        if base_path:
            self._base_path = Path(base_path)
        
        self._base_path.mkdir(parents=True, exist_ok=True)
        self.status = ConnectorStatus.CONNECTED
        return True
    
    async def disconnect(self) -> bool:
        self.status = ConnectorStatus.DISCONNECTED
        return True
    
    async def health_check(self) -> bool:
        return self._base_path.exists()
    
    def _get_full_path(self, path: str) -> Path:
        """Get full path, ensuring it's within base path."""
        full_path = (self._base_path / path).resolve()
        if not str(full_path).startswith(str(self._base_path.resolve())):
            raise ValueError("Path traversal not allowed")
        return full_path
    
    def _get_checksum(self, path: Path) -> str:
        """Calculate file checksum."""
        hasher = hashlib.md5()
        with open(path, 'rb') as f:
            for chunk in iter(lambda: f.read(8192), b''):
                hasher.update(chunk)
        return hasher.hexdigest()
    
    async def list_files(self, path: str = "", recursive: bool = False) -> List[StoredFile]:
        """List files in a directory."""
        full_path = self._get_full_path(path)
        
        if not full_path.exists():
            return []
        
        files = []
        pattern = "**/*" if recursive else "*"
        
        for file_path in full_path.glob(pattern):
            if file_path.is_file():
                stat = file_path.stat()
                files.append(StoredFile(
                    path=str(file_path.relative_to(self._base_path)),
                    name=file_path.name,
                    size=stat.st_size,
                    modified=datetime.fromtimestamp(stat.st_mtime),
                    content_type=self._guess_content_type(file_path),
                    checksum=self._get_checksum(file_path)
                ))
        
        return files
    
    def _guess_content_type(self, path: Path) -> str:
        """Guess content type from extension."""
        ext_map = {
            ".txt": "text/plain",
            ".json": "application/json",
            ".pdf": "application/pdf",
            ".md": "text/markdown",
            ".py": "text/x-python",
            ".csv": "text/csv",
        }
        return ext_map.get(path.suffix.lower(), "application/octet-stream")
    
    async def read_file(self, path: str) -> Optional[bytes]:
        """Read a file."""
        full_path = self._get_full_path(path)
        
        if not full_path.exists():
            return None
        
        with open(full_path, 'rb') as f:
            return f.read()
    
    async def read_text(self, path: str, encoding: str = "utf-8") -> Optional[str]:
        """Read a text file."""
        content = await self.read_file(path)
        if content:
            return content.decode(encoding)
        return None
    
    async def write_file(self, path: str, content: bytes) -> bool:
        """Write a file."""
        full_path = self._get_full_path(path)
        full_path.parent.mkdir(parents=True, exist_ok=True)
        
        with open(full_path, 'wb') as f:
            f.write(content)
        
        return True
    
    async def write_text(self, path: str, content: str, encoding: str = "utf-8") -> bool:
        """Write a text file."""
        return await self.write_file(path, content.encode(encoding))
    
    async def delete_file(self, path: str) -> bool:
        """Delete a file."""
        full_path = self._get_full_path(path)
        
        if full_path.exists():
            full_path.unlink()
            return True
        return False
    
    async def copy_file(self, src: str, dst: str) -> bool:
        """Copy a file."""
        src_path = self._get_full_path(src)
        dst_path = self._get_full_path(dst)
        
        if not src_path.exists():
            return False
        
        dst_path.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(src_path, dst_path)
        return True
    
    async def move_file(self, src: str, dst: str) -> bool:
        """Move a file."""
        src_path = self._get_full_path(src)
        dst_path = self._get_full_path(dst)
        
        if not src_path.exists():
            return False
        
        dst_path.parent.mkdir(parents=True, exist_ok=True)
        shutil.move(src_path, dst_path)
        return True


# =============================================================================
# S3/MINIO CONNECTOR
# =============================================================================

class S3Connector(BaseConnector):
    """
    S3-compatible storage (AWS S3, MinIO, etc.).
    
    MinIO is fully open source S3-compatible storage.
    """
    
    NAME = "s3"
    TYPE = ConnectorType.STORAGE
    DESCRIPTION = "S3-compatible storage (AWS S3, MinIO)"
    REQUIRED_CREDENTIALS = ["access_key", "secret_key"]
    OPTIONAL_CREDENTIALS = ["endpoint_url", "bucket", "region"]
    OPEN_SOURCE = True  # MinIO is open source
    
    def __init__(self, config: ConnectorConfig = None):
        super().__init__(config)
        self._client = None
        self._bucket = os.environ.get("S3_BUCKET", "mental-models")
    
    async def connect(self) -> bool:
        try:
            import boto3
            from botocore.config import Config
            
            access_key = self.config.credentials.get("access_key") or os.environ.get("AWS_ACCESS_KEY_ID")
            secret_key = self.config.credentials.get("secret_key") or os.environ.get("AWS_SECRET_ACCESS_KEY")
            endpoint_url = self.config.credentials.get("endpoint_url") or os.environ.get("S3_ENDPOINT_URL")
            region = self.config.credentials.get("region") or os.environ.get("AWS_REGION", "us-east-1")
            
            if not access_key or not secret_key:
                logger.error("S3 credentials not provided")
                self.status = ConnectorStatus.ERROR
                return False
            
            self._client = boto3.client(
                's3',
                aws_access_key_id=access_key,
                aws_secret_access_key=secret_key,
                endpoint_url=endpoint_url,
                region_name=region,
                config=Config(signature_version='s3v4')
            )
            
            # Test connection
            self._client.list_buckets()
            
            self.status = ConnectorStatus.CONNECTED
            return True
            
        except ImportError:
            logger.error("boto3 not installed. Run: pip install boto3")
            self.status = ConnectorStatus.ERROR
            return False
        except Exception as e:
            logger.error(f"S3 connection failed: {e}")
            self.status = ConnectorStatus.ERROR
            return False
    
    async def disconnect(self) -> bool:
        self._client = None
        self.status = ConnectorStatus.DISCONNECTED
        return True
    
    async def health_check(self) -> bool:
        if not self._client:
            return False
        try:
            self._client.list_buckets()
            return True
        except:
            return False
    
    async def list_files(self, prefix: str = "", bucket: str = None) -> List[StoredFile]:
        """List files in bucket."""
        if not self._client:
            return []
        
        bucket = bucket or self._bucket
        
        try:
            response = self._client.list_objects_v2(
                Bucket=bucket,
                Prefix=prefix
            )
            
            files = []
            for obj in response.get('Contents', []):
                files.append(StoredFile(
                    path=obj['Key'],
                    name=obj['Key'].split('/')[-1],
                    size=obj['Size'],
                    modified=obj['LastModified'],
                    content_type="application/octet-stream",
                    checksum=obj.get('ETag', '').strip('"')
                ))
            
            return files
        except Exception as e:
            logger.error(f"S3 list failed: {e}")
            return []
    
    async def read_file(self, path: str, bucket: str = None) -> Optional[bytes]:
        """Read a file from S3."""
        if not self._client:
            return None
        
        bucket = bucket or self._bucket
        
        try:
            response = self._client.get_object(Bucket=bucket, Key=path)
            return response['Body'].read()
        except Exception as e:
            logger.error(f"S3 read failed: {e}")
            return None
    
    async def write_file(self, path: str, content: bytes, bucket: str = None,
                        content_type: str = None) -> bool:
        """Write a file to S3."""
        if not self._client:
            return False
        
        bucket = bucket or self._bucket
        
        try:
            extra_args = {}
            if content_type:
                extra_args['ContentType'] = content_type
            
            self._client.put_object(
                Bucket=bucket,
                Key=path,
                Body=content,
                **extra_args
            )
            return True
        except Exception as e:
            logger.error(f"S3 write failed: {e}")
            return False
    
    async def delete_file(self, path: str, bucket: str = None) -> bool:
        """Delete a file from S3."""
        if not self._client:
            return False
        
        bucket = bucket or self._bucket
        
        try:
            self._client.delete_object(Bucket=bucket, Key=path)
            return True
        except Exception as e:
            logger.error(f"S3 delete failed: {e}")
            return False
    
    async def get_presigned_url(self, path: str, bucket: str = None,
                               expires_in: int = 3600) -> Optional[str]:
        """Get a presigned URL for a file."""
        if not self._client:
            return None
        
        bucket = bucket or self._bucket
        
        try:
            url = self._client.generate_presigned_url(
                'get_object',
                Params={'Bucket': bucket, 'Key': path},
                ExpiresIn=expires_in
            )
            return url
        except Exception as e:
            logger.error(f"Failed to generate presigned URL: {e}")
            return None


# =============================================================================
# GOOGLE DRIVE CONNECTOR
# =============================================================================

class GDriveConnector(BaseConnector):
    """
    Google Drive connector using rclone.
    
    Uses rclone for reliable sync and access.
    """
    
    NAME = "gdrive"
    TYPE = ConnectorType.STORAGE
    DESCRIPTION = "Google Drive via rclone"
    REQUIRED_CREDENTIALS = []
    OPTIONAL_CREDENTIALS = ["config_path", "remote_name"]
    OPEN_SOURCE = True  # rclone is open source
    
    def __init__(self, config: ConnectorConfig = None):
        super().__init__(config)
        self._config_path = "/home/ubuntu/.gdrive-rclone.ini"
        self._remote_name = "manus_google_drive"
    
    async def connect(self) -> bool:
        # Check if rclone config exists
        if Path(self._config_path).exists():
            self.status = ConnectorStatus.CONNECTED
            return True
        
        logger.warning("rclone config not found")
        self.status = ConnectorStatus.ERROR
        return False
    
    async def disconnect(self) -> bool:
        self.status = ConnectorStatus.DISCONNECTED
        return True
    
    async def health_check(self) -> bool:
        return Path(self._config_path).exists()
    
    def _run_rclone(self, args: List[str]) -> Optional[str]:
        """Run rclone command."""
        cmd = ["rclone"] + args + ["--config", self._config_path]
        
        try:
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=300
            )
            
            if result.returncode != 0:
                logger.error(f"rclone error: {result.stderr}")
                return None
            
            return result.stdout
        except Exception as e:
            logger.error(f"rclone command failed: {e}")
            return None
    
    async def list_files(self, path: str = "", recursive: bool = False) -> List[StoredFile]:
        """List files in Google Drive."""
        remote_path = f"{self._remote_name}:{path}"
        
        args = ["lsjson", remote_path]
        if recursive:
            args.append("-R")
        
        output = self._run_rclone(args)
        if not output:
            return []
        
        try:
            items = json.loads(output)
            files = []
            
            for item in items:
                if not item.get("IsDir", False):
                    files.append(StoredFile(
                        path=item.get("Path", ""),
                        name=item.get("Name", ""),
                        size=item.get("Size", 0),
                        modified=datetime.fromisoformat(item.get("ModTime", "").replace("Z", "+00:00")),
                        content_type=item.get("MimeType", ""),
                        checksum=item.get("Hashes", {}).get("MD5", "")
                    ))
            
            return files
        except json.JSONDecodeError:
            return []
    
    async def read_file(self, path: str) -> Optional[bytes]:
        """Download a file from Google Drive."""
        import tempfile
        
        remote_path = f"{self._remote_name}:{path}"
        
        with tempfile.NamedTemporaryFile(delete=False) as tmp:
            tmp_path = tmp.name
        
        try:
            result = self._run_rclone(["copy", remote_path, str(Path(tmp_path).parent)])
            
            if result is not None and Path(tmp_path).exists():
                with open(tmp_path, 'rb') as f:
                    return f.read()
            return None
        finally:
            Path(tmp_path).unlink(missing_ok=True)
    
    async def write_file(self, path: str, content: bytes) -> bool:
        """Upload a file to Google Drive."""
        import tempfile
        
        remote_dir = f"{self._remote_name}:{str(Path(path).parent)}"
        
        with tempfile.NamedTemporaryFile(delete=False, suffix=Path(path).suffix) as tmp:
            tmp.write(content)
            tmp_path = tmp.name
        
        try:
            result = self._run_rclone(["copy", tmp_path, remote_dir])
            return result is not None
        finally:
            Path(tmp_path).unlink(missing_ok=True)
    
    async def delete_file(self, path: str) -> bool:
        """Delete a file from Google Drive."""
        remote_path = f"{self._remote_name}:{path}"
        result = self._run_rclone(["delete", remote_path])
        return result is not None
    
    async def sync_to_local(self, remote_path: str, local_path: str) -> bool:
        """Sync from Google Drive to local."""
        remote = f"{self._remote_name}:{remote_path}"
        result = self._run_rclone(["sync", remote, local_path])
        return result is not None
    
    async def sync_from_local(self, local_path: str, remote_path: str) -> bool:
        """Sync from local to Google Drive."""
        remote = f"{self._remote_name}:{remote_path}"
        result = self._run_rclone(["sync", local_path, remote])
        return result is not None
    
    async def get_link(self, path: str) -> Optional[str]:
        """Get shareable link for a file."""
        remote_path = f"{self._remote_name}:{path}"
        result = self._run_rclone(["link", remote_path])
        return result.strip() if result else None
    
    async def search(self, query: str, path: str = "") -> List[StoredFile]:
        """Search for files."""
        # List all files and filter
        all_files = await self.list_files(path, recursive=True)
        
        query_lower = query.lower()
        return [
            f for f in all_files
            if query_lower in f.name.lower()
        ]


# Register connectors
from .base import registry
registry.register_class(LocalConnector)
registry.register_class(S3Connector)
registry.register_class(GDriveConnector)
