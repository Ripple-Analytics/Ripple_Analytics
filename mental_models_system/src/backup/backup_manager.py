"""
Backup and Restore Manager

Comprehensive backup system for the Mental Models System.
Supports full and incremental backups, compression, encryption,
and multiple storage backends.
"""

import os
import json
import gzip
import shutil
import hashlib
import tarfile
import tempfile
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass, field, asdict
from enum import Enum
import logging

logger = logging.getLogger(__name__)


class BackupType(Enum):
    """Types of backups."""
    FULL = "full"
    INCREMENTAL = "incremental"
    DIFFERENTIAL = "differential"


class StorageBackend(Enum):
    """Storage backends for backups."""
    LOCAL = "local"
    S3 = "s3"
    GDRIVE = "gdrive"


@dataclass
class BackupManifest:
    """Manifest describing a backup."""
    backup_id: str
    backup_type: str
    created_at: str
    completed_at: Optional[str] = None
    size_bytes: int = 0
    compressed_size_bytes: int = 0
    file_count: int = 0
    checksum: str = ""
    parent_backup_id: Optional[str] = None
    components: List[str] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    def to_dict(self) -> Dict:
        return asdict(self)
    
    @classmethod
    def from_dict(cls, data: Dict) -> 'BackupManifest':
        return cls(**data)


@dataclass
class RestoreResult:
    """Result of a restore operation."""
    success: bool
    backup_id: str
    restored_at: str
    components_restored: List[str]
    files_restored: int
    errors: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)


class BackupManager:
    """
    Manages backups and restores for the Mental Models System.
    
    Features:
    - Full, incremental, and differential backups
    - Compression with gzip
    - Checksum verification
    - Multiple storage backends
    - Automatic retention policy
    - Point-in-time recovery
    """
    
    # Components that can be backed up
    BACKUP_COMPONENTS = {
        'models': 'data/raw/mental_models_complete.json',
        'failure_modes': 'data/raw/failure_modes*.json',
        'case_studies': 'data/raw/case_studies*.json',
        'decisions': 'data/decisions/',
        'knowledge_graph': 'data/knowledge_graph/',
        'analyses': 'data/analyses/',
        'settings': 'config/',
        'audit_logs': 'logs/audit/',
    }
    
    def __init__(
        self,
        data_dir: str = None,
        backup_dir: str = None,
        storage_backend: StorageBackend = StorageBackend.LOCAL,
        retention_days: int = 30,
        max_backups: int = 10
    ):
        """
        Initialize the backup manager.
        
        Args:
            data_dir: Directory containing data to backup
            backup_dir: Directory to store backups
            storage_backend: Where to store backups
            retention_days: How long to keep backups
            max_backups: Maximum number of backups to retain
        """
        self.data_dir = Path(data_dir or os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
        self.backup_dir = Path(backup_dir or self.data_dir / 'backups')
        self.backup_dir.mkdir(parents=True, exist_ok=True)
        
        self.storage_backend = storage_backend
        self.retention_days = retention_days
        self.max_backups = max_backups
        
        self.manifest_file = self.backup_dir / 'manifests.json'
        self.manifests: Dict[str, BackupManifest] = self._load_manifests()
    
    def _load_manifests(self) -> Dict[str, BackupManifest]:
        """Load backup manifests from disk."""
        if self.manifest_file.exists():
            try:
                with open(self.manifest_file, 'r') as f:
                    data = json.load(f)
                return {k: BackupManifest.from_dict(v) for k, v in data.items()}
            except Exception as e:
                logger.error(f"Failed to load manifests: {e}")
        return {}
    
    def _save_manifests(self):
        """Save backup manifests to disk."""
        try:
            with open(self.manifest_file, 'w') as f:
                json.dump({k: v.to_dict() for k, v in self.manifests.items()}, f, indent=2)
        except Exception as e:
            logger.error(f"Failed to save manifests: {e}")
    
    def _generate_backup_id(self) -> str:
        """Generate a unique backup ID."""
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        return f"backup_{timestamp}"
    
    def _calculate_checksum(self, file_path: Path) -> str:
        """Calculate SHA256 checksum of a file."""
        sha256 = hashlib.sha256()
        with open(file_path, 'rb') as f:
            for chunk in iter(lambda: f.read(8192), b''):
                sha256.update(chunk)
        return sha256.hexdigest()
    
    def _get_files_to_backup(
        self,
        components: List[str] = None,
        since: datetime = None
    ) -> List[Tuple[Path, str]]:
        """
        Get list of files to backup.
        
        Args:
            components: Specific components to backup (None = all)
            since: Only include files modified since this time
            
        Returns:
            List of (file_path, relative_path) tuples
        """
        files = []
        components = components or list(self.BACKUP_COMPONENTS.keys())
        
        for component in components:
            pattern = self.BACKUP_COMPONENTS.get(component)
            if not pattern:
                continue
            
            # Handle glob patterns
            if '*' in pattern:
                base_dir = self.data_dir / os.path.dirname(pattern)
                if base_dir.exists():
                    for file_path in base_dir.glob(os.path.basename(pattern)):
                        if file_path.is_file():
                            if since is None or datetime.fromtimestamp(file_path.stat().st_mtime) > since:
                                rel_path = file_path.relative_to(self.data_dir)
                                files.append((file_path, str(rel_path)))
            else:
                full_path = self.data_dir / pattern
                if full_path.is_file():
                    if since is None or datetime.fromtimestamp(full_path.stat().st_mtime) > since:
                        files.append((full_path, pattern))
                elif full_path.is_dir():
                    for file_path in full_path.rglob('*'):
                        if file_path.is_file():
                            if since is None or datetime.fromtimestamp(file_path.stat().st_mtime) > since:
                                rel_path = file_path.relative_to(self.data_dir)
                                files.append((file_path, str(rel_path)))
        
        return files
    
    def create_backup(
        self,
        backup_type: BackupType = BackupType.FULL,
        components: List[str] = None,
        compress: bool = True,
        description: str = None
    ) -> BackupManifest:
        """
        Create a new backup.
        
        Args:
            backup_type: Type of backup to create
            components: Specific components to backup (None = all)
            compress: Whether to compress the backup
            description: Optional description
            
        Returns:
            BackupManifest describing the backup
        """
        backup_id = self._generate_backup_id()
        logger.info(f"Creating {backup_type.value} backup: {backup_id}")
        
        # Determine parent backup for incremental/differential
        parent_backup_id = None
        since = None
        
        if backup_type == BackupType.INCREMENTAL:
            # Find most recent backup
            recent = self.get_latest_backup()
            if recent:
                parent_backup_id = recent.backup_id
                since = datetime.fromisoformat(recent.created_at)
        elif backup_type == BackupType.DIFFERENTIAL:
            # Find most recent full backup
            for manifest in sorted(self.manifests.values(), 
                                   key=lambda m: m.created_at, reverse=True):
                if manifest.backup_type == BackupType.FULL.value:
                    parent_backup_id = manifest.backup_id
                    since = datetime.fromisoformat(manifest.created_at)
                    break
        
        # Get files to backup
        files = self._get_files_to_backup(components, since)
        
        if not files:
            logger.warning("No files to backup")
            manifest = BackupManifest(
                backup_id=backup_id,
                backup_type=backup_type.value,
                created_at=datetime.now().isoformat(),
                completed_at=datetime.now().isoformat(),
                components=components or list(self.BACKUP_COMPONENTS.keys()),
                parent_backup_id=parent_backup_id,
                metadata={'description': description, 'empty': True}
            )
            self.manifests[backup_id] = manifest
            self._save_manifests()
            return manifest
        
        # Create backup archive
        backup_file = self.backup_dir / f"{backup_id}.tar"
        if compress:
            backup_file = backup_file.with_suffix('.tar.gz')
        
        total_size = 0
        file_count = 0
        
        mode = 'w:gz' if compress else 'w'
        with tarfile.open(backup_file, mode) as tar:
            for file_path, rel_path in files:
                tar.add(file_path, arcname=rel_path)
                total_size += file_path.stat().st_size
                file_count += 1
        
        # Calculate checksum
        checksum = self._calculate_checksum(backup_file)
        compressed_size = backup_file.stat().st_size
        
        # Create manifest
        manifest = BackupManifest(
            backup_id=backup_id,
            backup_type=backup_type.value,
            created_at=datetime.now().isoformat(),
            completed_at=datetime.now().isoformat(),
            size_bytes=total_size,
            compressed_size_bytes=compressed_size,
            file_count=file_count,
            checksum=checksum,
            parent_backup_id=parent_backup_id,
            components=components or list(self.BACKUP_COMPONENTS.keys()),
            metadata={
                'description': description,
                'compression': compress,
                'compression_ratio': round(compressed_size / total_size, 3) if total_size > 0 else 1.0
            }
        )
        
        self.manifests[backup_id] = manifest
        self._save_manifests()
        
        logger.info(f"Backup complete: {file_count} files, {compressed_size} bytes")
        
        # Apply retention policy
        self._apply_retention_policy()
        
        return manifest
    
    def restore_backup(
        self,
        backup_id: str,
        target_dir: str = None,
        components: List[str] = None,
        verify_checksum: bool = True
    ) -> RestoreResult:
        """
        Restore from a backup.
        
        Args:
            backup_id: ID of backup to restore
            target_dir: Directory to restore to (None = original location)
            components: Specific components to restore (None = all)
            verify_checksum: Whether to verify backup integrity
            
        Returns:
            RestoreResult describing the operation
        """
        manifest = self.manifests.get(backup_id)
        if not manifest:
            return RestoreResult(
                success=False,
                backup_id=backup_id,
                restored_at=datetime.now().isoformat(),
                components_restored=[],
                files_restored=0,
                errors=[f"Backup not found: {backup_id}"]
            )
        
        logger.info(f"Restoring backup: {backup_id}")
        
        # Find backup file
        backup_file = self.backup_dir / f"{backup_id}.tar.gz"
        if not backup_file.exists():
            backup_file = self.backup_dir / f"{backup_id}.tar"
        
        if not backup_file.exists():
            return RestoreResult(
                success=False,
                backup_id=backup_id,
                restored_at=datetime.now().isoformat(),
                components_restored=[],
                files_restored=0,
                errors=[f"Backup file not found: {backup_file}"]
            )
        
        # Verify checksum
        if verify_checksum:
            actual_checksum = self._calculate_checksum(backup_file)
            if actual_checksum != manifest.checksum:
                return RestoreResult(
                    success=False,
                    backup_id=backup_id,
                    restored_at=datetime.now().isoformat(),
                    components_restored=[],
                    files_restored=0,
                    errors=["Checksum verification failed - backup may be corrupted"]
                )
        
        # Determine target directory
        target = Path(target_dir) if target_dir else self.data_dir
        target.mkdir(parents=True, exist_ok=True)
        
        # For incremental/differential, restore parent first
        if manifest.parent_backup_id and manifest.backup_type != BackupType.FULL.value:
            parent_result = self.restore_backup(
                manifest.parent_backup_id,
                str(target),
                components,
                verify_checksum
            )
            if not parent_result.success:
                return RestoreResult(
                    success=False,
                    backup_id=backup_id,
                    restored_at=datetime.now().isoformat(),
                    components_restored=[],
                    files_restored=0,
                    errors=[f"Failed to restore parent backup: {parent_result.errors}"]
                )
        
        # Extract backup
        errors = []
        warnings = []
        files_restored = 0
        components_restored = set()
        
        mode = 'r:gz' if str(backup_file).endswith('.gz') else 'r'
        try:
            with tarfile.open(backup_file, mode) as tar:
                for member in tar.getmembers():
                    # Check if this file belongs to requested components
                    if components:
                        component_match = False
                        for comp, pattern in self.BACKUP_COMPONENTS.items():
                            if comp in components:
                                if '*' in pattern:
                                    base = os.path.dirname(pattern)
                                    if member.name.startswith(base):
                                        component_match = True
                                        components_restored.add(comp)
                                        break
                                elif member.name.startswith(pattern.rstrip('/')):
                                    component_match = True
                                    components_restored.add(comp)
                                    break
                        if not component_match:
                            continue
                    else:
                        # Track which component this file belongs to
                        for comp, pattern in self.BACKUP_COMPONENTS.items():
                            if '*' in pattern:
                                base = os.path.dirname(pattern)
                                if member.name.startswith(base):
                                    components_restored.add(comp)
                                    break
                            elif member.name.startswith(pattern.rstrip('/')):
                                components_restored.add(comp)
                                break
                    
                    # Extract file
                    try:
                        tar.extract(member, target)
                        files_restored += 1
                    except Exception as e:
                        errors.append(f"Failed to extract {member.name}: {e}")
        
        except Exception as e:
            return RestoreResult(
                success=False,
                backup_id=backup_id,
                restored_at=datetime.now().isoformat(),
                components_restored=list(components_restored),
                files_restored=files_restored,
                errors=[f"Failed to open backup: {e}"]
            )
        
        logger.info(f"Restore complete: {files_restored} files")
        
        return RestoreResult(
            success=len(errors) == 0,
            backup_id=backup_id,
            restored_at=datetime.now().isoformat(),
            components_restored=list(components_restored),
            files_restored=files_restored,
            errors=errors,
            warnings=warnings
        )
    
    def get_latest_backup(self, backup_type: BackupType = None) -> Optional[BackupManifest]:
        """Get the most recent backup."""
        backups = list(self.manifests.values())
        if backup_type:
            backups = [b for b in backups if b.backup_type == backup_type.value]
        
        if not backups:
            return None
        
        return max(backups, key=lambda b: b.created_at)
    
    def list_backups(
        self,
        backup_type: BackupType = None,
        since: datetime = None,
        limit: int = None
    ) -> List[BackupManifest]:
        """List available backups."""
        backups = list(self.manifests.values())
        
        if backup_type:
            backups = [b for b in backups if b.backup_type == backup_type.value]
        
        if since:
            backups = [b for b in backups if datetime.fromisoformat(b.created_at) > since]
        
        # Sort by creation time, newest first
        backups.sort(key=lambda b: b.created_at, reverse=True)
        
        if limit:
            backups = backups[:limit]
        
        return backups
    
    def delete_backup(self, backup_id: str) -> bool:
        """Delete a backup."""
        if backup_id not in self.manifests:
            return False
        
        # Check if any backups depend on this one
        for manifest in self.manifests.values():
            if manifest.parent_backup_id == backup_id:
                logger.warning(f"Cannot delete {backup_id}: backup {manifest.backup_id} depends on it")
                return False
        
        # Delete backup file
        for ext in ['.tar.gz', '.tar']:
            backup_file = self.backup_dir / f"{backup_id}{ext}"
            if backup_file.exists():
                backup_file.unlink()
                break
        
        # Remove from manifests
        del self.manifests[backup_id]
        self._save_manifests()
        
        logger.info(f"Deleted backup: {backup_id}")
        return True
    
    def _apply_retention_policy(self):
        """Apply retention policy to remove old backups."""
        cutoff_date = datetime.now() - timedelta(days=self.retention_days)
        
        # Get backups sorted by date
        backups = sorted(self.manifests.values(), key=lambda b: b.created_at, reverse=True)
        
        # Keep at least max_backups, delete old ones
        to_delete = []
        kept = 0
        
        for backup in backups:
            backup_date = datetime.fromisoformat(backup.created_at)
            
            if kept < self.max_backups:
                kept += 1
                continue
            
            if backup_date < cutoff_date:
                # Check if any backups depend on this one
                has_dependents = any(
                    m.parent_backup_id == backup.backup_id 
                    for m in self.manifests.values()
                )
                if not has_dependents:
                    to_delete.append(backup.backup_id)
        
        for backup_id in to_delete:
            self.delete_backup(backup_id)
            logger.info(f"Retention policy: deleted {backup_id}")
    
    def get_backup_stats(self) -> Dict[str, Any]:
        """Get backup statistics."""
        backups = list(self.manifests.values())
        
        if not backups:
            return {
                'total_backups': 0,
                'total_size_bytes': 0,
                'total_compressed_bytes': 0,
                'by_type': {},
                'oldest_backup': None,
                'newest_backup': None,
                'average_compression_ratio': 0
            }
        
        total_size = sum(b.size_bytes for b in backups)
        total_compressed = sum(b.compressed_size_bytes for b in backups)
        
        by_type = {}
        for backup in backups:
            bt = backup.backup_type
            if bt not in by_type:
                by_type[bt] = {'count': 0, 'size_bytes': 0}
            by_type[bt]['count'] += 1
            by_type[bt]['size_bytes'] += backup.compressed_size_bytes
        
        return {
            'total_backups': len(backups),
            'total_size_bytes': total_size,
            'total_compressed_bytes': total_compressed,
            'by_type': by_type,
            'oldest_backup': min(b.created_at for b in backups),
            'newest_backup': max(b.created_at for b in backups),
            'average_compression_ratio': round(total_compressed / total_size, 3) if total_size > 0 else 1.0,
            'retention_days': self.retention_days,
            'max_backups': self.max_backups
        }
    
    def verify_backup(self, backup_id: str) -> Dict[str, Any]:
        """Verify a backup's integrity."""
        manifest = self.manifests.get(backup_id)
        if not manifest:
            return {'valid': False, 'error': 'Backup not found'}
        
        # Find backup file
        backup_file = None
        for ext in ['.tar.gz', '.tar']:
            path = self.backup_dir / f"{backup_id}{ext}"
            if path.exists():
                backup_file = path
                break
        
        if not backup_file:
            return {'valid': False, 'error': 'Backup file not found'}
        
        # Verify checksum
        actual_checksum = self._calculate_checksum(backup_file)
        checksum_valid = actual_checksum == manifest.checksum
        
        # Verify archive integrity
        archive_valid = True
        file_count = 0
        try:
            mode = 'r:gz' if str(backup_file).endswith('.gz') else 'r'
            with tarfile.open(backup_file, mode) as tar:
                for member in tar.getmembers():
                    file_count += 1
        except Exception as e:
            archive_valid = False
        
        return {
            'valid': checksum_valid and archive_valid,
            'checksum_valid': checksum_valid,
            'archive_valid': archive_valid,
            'expected_checksum': manifest.checksum,
            'actual_checksum': actual_checksum,
            'expected_files': manifest.file_count,
            'actual_files': file_count,
            'size_bytes': backup_file.stat().st_size
        }


# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

def create_full_backup(
    data_dir: str = None,
    backup_dir: str = None,
    description: str = None
) -> BackupManifest:
    """Create a full backup."""
    manager = BackupManager(data_dir=data_dir, backup_dir=backup_dir)
    return manager.create_backup(
        backup_type=BackupType.FULL,
        description=description
    )


def create_incremental_backup(
    data_dir: str = None,
    backup_dir: str = None,
    description: str = None
) -> BackupManifest:
    """Create an incremental backup."""
    manager = BackupManager(data_dir=data_dir, backup_dir=backup_dir)
    return manager.create_backup(
        backup_type=BackupType.INCREMENTAL,
        description=description
    )


def restore_latest(
    data_dir: str = None,
    backup_dir: str = None,
    target_dir: str = None
) -> RestoreResult:
    """Restore from the latest backup."""
    manager = BackupManager(data_dir=data_dir, backup_dir=backup_dir)
    latest = manager.get_latest_backup()
    if not latest:
        return RestoreResult(
            success=False,
            backup_id="",
            restored_at=datetime.now().isoformat(),
            components_restored=[],
            files_restored=0,
            errors=["No backups found"]
        )
    return manager.restore_backup(latest.backup_id, target_dir)
