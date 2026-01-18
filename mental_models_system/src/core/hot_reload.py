import os
import sys
import json
import logging
import asyncio
import importlib
import importlib.util
import threading
import time
import hashlib
import shutil
from pathlib import Path
from dataclasses import dataclass, field
from typing import Dict, Any, Optional, List, Callable, Set
from datetime import datetime
from enum import Enum
import subprocess

logger = logging.getLogger(__name__)


class VersionStatus(Enum):
    ACTIVE = "active"
    STANDBY = "standby"
    UPGRADING = "upgrading"
    FAILED = "failed"
    ROLLED_BACK = "rolled_back"


@dataclass
class CodeVersion:
    version_id: str
    version_number: str
    path: str
    status: VersionStatus = VersionStatus.STANDBY
    created_at: datetime = field(default_factory=datetime.now)
    activated_at: Optional[datetime] = None
    checksum: str = ""
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class UpgradeResult:
    success: bool
    old_version: str
    new_version: str
    error_message: str = ""
    rollback_performed: bool = False
    duration_seconds: float = 0.0


class ModuleWatcher:
    def __init__(
        self,
        watch_paths: List[str],
        callback: Callable[[str, str], None] = None,
        poll_interval: float = 1.0,
    ):
        self.watch_paths = [Path(p) for p in watch_paths]
        self.callback = callback
        self.poll_interval = poll_interval
        self._file_hashes: Dict[str, str] = {}
        self._running = False
        self._thread: Optional[threading.Thread] = None

    def _compute_hash(self, path: Path) -> str:
        try:
            with open(path, "rb") as f:
                return hashlib.md5(f.read()).hexdigest()
        except Exception:
            return ""

    def _scan_files(self) -> Dict[str, str]:
        hashes = {}
        for watch_path in self.watch_paths:
            if watch_path.is_file():
                hashes[str(watch_path)] = self._compute_hash(watch_path)
            elif watch_path.is_dir():
                for py_file in watch_path.rglob("*.py"):
                    if "__pycache__" not in str(py_file):
                        hashes[str(py_file)] = self._compute_hash(py_file)
        return hashes

    def _watch_loop(self):
        self._file_hashes = self._scan_files()
        
        while self._running:
            time.sleep(self.poll_interval)
            
            current_hashes = self._scan_files()
            
            for path, new_hash in current_hashes.items():
                old_hash = self._file_hashes.get(path)
                if old_hash and old_hash != new_hash:
                    logger.info(f"File changed: {path}")
                    if self.callback:
                        self.callback(path, "modified")
            
            for path in set(current_hashes.keys()) - set(self._file_hashes.keys()):
                logger.info(f"File added: {path}")
                if self.callback:
                    self.callback(path, "added")
            
            for path in set(self._file_hashes.keys()) - set(current_hashes.keys()):
                logger.info(f"File removed: {path}")
                if self.callback:
                    self.callback(path, "removed")
            
            self._file_hashes = current_hashes

    def start(self):
        if self._running:
            return
        self._running = True
        self._thread = threading.Thread(target=self._watch_loop, daemon=True)
        self._thread.start()
        logger.info(f"Module watcher started for {len(self.watch_paths)} paths")

    def stop(self):
        self._running = False
        if self._thread:
            self._thread.join(timeout=5)
        logger.info("Module watcher stopped")


class HotReloader:
    def __init__(
        self,
        base_path: str = None,
        versions_dir: str = None,
        auto_reload: bool = True,
        health_check_callback: Callable[[], bool] = None,
        rollback_on_error: bool = True,
    ):
        self.base_path = Path(base_path or os.getcwd())
        self.versions_dir = Path(versions_dir or os.path.expanduser("~/.mental_models/versions"))
        self.versions_dir.mkdir(parents=True, exist_ok=True)
        
        self.auto_reload = auto_reload
        self.health_check_callback = health_check_callback
        self.rollback_on_error = rollback_on_error
        
        self._versions: Dict[str, CodeVersion] = {}
        self._active_version: Optional[str] = None
        self._loaded_modules: Dict[str, Any] = {}
        self._module_watcher: Optional[ModuleWatcher] = None
        self._lock = threading.RLock()
        self._reload_callbacks: List[Callable[[str], None]] = []

    def _generate_version_id(self) -> str:
        return f"v{int(time.time())}-{hashlib.md5(str(time.time()).encode()).hexdigest()[:8]}"

    def _compute_directory_checksum(self, path: Path) -> str:
        checksums = []
        for py_file in sorted(path.rglob("*.py")):
            if "__pycache__" not in str(py_file):
                try:
                    with open(py_file, "rb") as f:
                        checksums.append(hashlib.md5(f.read()).hexdigest())
                except Exception:
                    pass
        return hashlib.md5("".join(checksums).encode()).hexdigest()

    def create_version(
        self,
        source_path: str = None,
        version_number: str = None,
    ) -> Optional[CodeVersion]:
        with self._lock:
            source = Path(source_path or self.base_path)
            version_id = self._generate_version_id()
            version_number = version_number or version_id
            
            version_path = self.versions_dir / version_id
            
            try:
                shutil.copytree(
                    source,
                    version_path,
                    ignore=shutil.ignore_patterns(
                        "__pycache__", "*.pyc", ".git", ".env",
                        "node_modules", "venv", "*.egg-info"
                    )
                )
                
                checksum = self._compute_directory_checksum(version_path)
                
                version = CodeVersion(
                    version_id=version_id,
                    version_number=version_number,
                    path=str(version_path),
                    checksum=checksum,
                )
                
                self._versions[version_id] = version
                
                manifest = {
                    "version_id": version_id,
                    "version_number": version_number,
                    "checksum": checksum,
                    "created_at": version.created_at.isoformat(),
                }
                with open(version_path / "version.json", "w") as f:
                    json.dump(manifest, f, indent=2)
                
                logger.info(f"Created version {version_id} at {version_path}")
                return version
                
            except Exception as e:
                logger.error(f"Failed to create version: {e}")
                if version_path.exists():
                    shutil.rmtree(version_path)
                return None

    def activate_version(self, version_id: str) -> UpgradeResult:
        with self._lock:
            start_time = time.time()
            old_version = self._active_version or "none"
            
            if version_id not in self._versions:
                return UpgradeResult(
                    success=False,
                    old_version=old_version,
                    new_version=version_id,
                    error_message=f"Version {version_id} not found",
                )
            
            version = self._versions[version_id]
            version.status = VersionStatus.UPGRADING
            
            try:
                version_path = Path(version.path)
                if str(version_path) not in sys.path:
                    sys.path.insert(0, str(version_path))
                
                if self.health_check_callback:
                    if not self.health_check_callback():
                        raise Exception("Health check failed after activation")
                
                if self._active_version and self._active_version in self._versions:
                    self._versions[self._active_version].status = VersionStatus.STANDBY
                
                version.status = VersionStatus.ACTIVE
                version.activated_at = datetime.now()
                self._active_version = version_id
                
                for callback in self._reload_callbacks:
                    try:
                        callback(version_id)
                    except Exception as e:
                        logger.warning(f"Reload callback failed: {e}")
                
                duration = time.time() - start_time
                logger.info(f"Activated version {version_id} in {duration:.2f}s")
                
                return UpgradeResult(
                    success=True,
                    old_version=old_version,
                    new_version=version_id,
                    duration_seconds=duration,
                )
                
            except Exception as e:
                logger.error(f"Failed to activate version {version_id}: {e}")
                version.status = VersionStatus.FAILED
                
                if self.rollback_on_error and old_version != "none":
                    logger.info(f"Rolling back to {old_version}")
                    rollback_result = self.activate_version(old_version)
                    
                    return UpgradeResult(
                        success=False,
                        old_version=old_version,
                        new_version=version_id,
                        error_message=str(e),
                        rollback_performed=rollback_result.success,
                        duration_seconds=time.time() - start_time,
                    )
                
                return UpgradeResult(
                    success=False,
                    old_version=old_version,
                    new_version=version_id,
                    error_message=str(e),
                    duration_seconds=time.time() - start_time,
                )

    def reload_module(self, module_name: str) -> bool:
        with self._lock:
            try:
                if module_name in sys.modules:
                    module = sys.modules[module_name]
                    importlib.reload(module)
                    self._loaded_modules[module_name] = module
                    logger.info(f"Reloaded module: {module_name}")
                    return True
                else:
                    module = importlib.import_module(module_name)
                    self._loaded_modules[module_name] = module
                    logger.info(f"Loaded module: {module_name}")
                    return True
                    
            except Exception as e:
                logger.error(f"Failed to reload module {module_name}: {e}")
                return False

    def reload_all_modules(self) -> Dict[str, bool]:
        results = {}
        for module_name in list(self._loaded_modules.keys()):
            results[module_name] = self.reload_module(module_name)
        return results

    def _on_file_change(self, path: str, change_type: str):
        if not self.auto_reload:
            return
        
        try:
            rel_path = Path(path).relative_to(self.base_path)
            module_path = str(rel_path).replace("/", ".").replace("\\", ".")
            if module_path.endswith(".py"):
                module_path = module_path[:-3]
            
            self.reload_module(module_path)
            
        except Exception as e:
            logger.warning(f"Auto-reload failed for {path}: {e}")

    def start_watching(self, watch_paths: List[str] = None):
        paths = watch_paths or [str(self.base_path)]
        self._module_watcher = ModuleWatcher(
            watch_paths=paths,
            callback=self._on_file_change,
        )
        self._module_watcher.start()

    def stop_watching(self):
        if self._module_watcher:
            self._module_watcher.stop()
            self._module_watcher = None

    def add_reload_callback(self, callback: Callable[[str], None]):
        self._reload_callbacks.append(callback)

    def get_active_version(self) -> Optional[CodeVersion]:
        if self._active_version:
            return self._versions.get(self._active_version)
        return None

    def list_versions(self) -> List[CodeVersion]:
        return list(self._versions.values())

    def cleanup_old_versions(self, keep_count: int = 5) -> int:
        with self._lock:
            versions = sorted(
                self._versions.values(),
                key=lambda v: v.created_at,
                reverse=True
            )
            
            to_remove = versions[keep_count:]
            removed = 0
            
            for version in to_remove:
                if version.status == VersionStatus.ACTIVE:
                    continue
                
                try:
                    shutil.rmtree(version.path)
                    del self._versions[version.version_id]
                    removed += 1
                    logger.info(f"Removed old version: {version.version_id}")
                except Exception as e:
                    logger.warning(f"Failed to remove version {version.version_id}: {e}")
            
            return removed


class ZeroDowntimeUpgrader:
    def __init__(
        self,
        hot_reloader: HotReloader,
        pre_upgrade_callback: Callable[[], bool] = None,
        post_upgrade_callback: Callable[[bool], None] = None,
        drain_timeout: float = 30.0,
    ):
        self.hot_reloader = hot_reloader
        self.pre_upgrade_callback = pre_upgrade_callback
        self.post_upgrade_callback = post_upgrade_callback
        self.drain_timeout = drain_timeout
        self._upgrading = False
        self._active_requests = 0
        self._lock = threading.Lock()

    def request_started(self):
        with self._lock:
            self._active_requests += 1

    def request_finished(self):
        with self._lock:
            self._active_requests = max(0, self._active_requests - 1)

    def _drain_requests(self) -> bool:
        start_time = time.time()
        
        while time.time() - start_time < self.drain_timeout:
            with self._lock:
                if self._active_requests == 0:
                    return True
            time.sleep(0.1)
        
        logger.warning(f"Drain timeout reached with {self._active_requests} active requests")
        return False

    async def upgrade(
        self,
        source_path: str = None,
        version_number: str = None,
        force: bool = False,
    ) -> UpgradeResult:
        if self._upgrading and not force:
            return UpgradeResult(
                success=False,
                old_version=self.hot_reloader._active_version or "none",
                new_version="pending",
                error_message="Upgrade already in progress",
            )
        
        self._upgrading = True
        
        try:
            if self.pre_upgrade_callback:
                if not self.pre_upgrade_callback():
                    return UpgradeResult(
                        success=False,
                        old_version=self.hot_reloader._active_version or "none",
                        new_version="pending",
                        error_message="Pre-upgrade check failed",
                    )
            
            logger.info("Draining active requests...")
            drained = self._drain_requests()
            if not drained:
                logger.warning("Proceeding with upgrade despite active requests")
            
            version = self.hot_reloader.create_version(source_path, version_number)
            if not version:
                return UpgradeResult(
                    success=False,
                    old_version=self.hot_reloader._active_version or "none",
                    new_version="failed",
                    error_message="Failed to create new version",
                )
            
            result = self.hot_reloader.activate_version(version.version_id)
            
            if self.post_upgrade_callback:
                self.post_upgrade_callback(result.success)
            
            return result
            
        finally:
            self._upgrading = False

    def get_status(self) -> Dict[str, Any]:
        active_version = self.hot_reloader.get_active_version()
        
        return {
            "upgrading": self._upgrading,
            "active_requests": self._active_requests,
            "active_version": active_version.version_id if active_version else None,
            "active_version_number": active_version.version_number if active_version else None,
            "available_versions": len(self.hot_reloader.list_versions()),
        }


class GitBasedUpgrader:
    def __init__(
        self,
        repo_path: str,
        hot_reloader: HotReloader,
        remote: str = "origin",
        branch: str = "main",
    ):
        self.repo_path = Path(repo_path)
        self.hot_reloader = hot_reloader
        self.remote = remote
        self.branch = branch

    def _run_git(self, *args) -> tuple:
        try:
            result = subprocess.run(
                ["git"] + list(args),
                cwd=self.repo_path,
                capture_output=True,
                text=True,
            )
            return result.returncode == 0, result.stdout.strip(), result.stderr.strip()
        except Exception as e:
            return False, "", str(e)

    def check_for_updates(self) -> Optional[str]:
        success, _, _ = self._run_git("fetch", self.remote)
        if not success:
            return None
        
        success, local_hash, _ = self._run_git("rev-parse", "HEAD")
        if not success:
            return None
        
        success, remote_hash, _ = self._run_git("rev-parse", f"{self.remote}/{self.branch}")
        if not success:
            return None
        
        if local_hash != remote_hash:
            return remote_hash
        
        return None

    async def pull_and_upgrade(self) -> UpgradeResult:
        success, _, error = self._run_git("pull", self.remote, self.branch)
        
        if not success:
            return UpgradeResult(
                success=False,
                old_version=self.hot_reloader._active_version or "none",
                new_version="failed",
                error_message=f"Git pull failed: {error}",
            )
        
        success, commit_hash, _ = self._run_git("rev-parse", "--short", "HEAD")
        version_number = commit_hash if success else None
        
        version = self.hot_reloader.create_version(
            str(self.repo_path),
            version_number,
        )
        
        if not version:
            return UpgradeResult(
                success=False,
                old_version=self.hot_reloader._active_version or "none",
                new_version="failed",
                error_message="Failed to create version from pulled code",
            )
        
        return self.hot_reloader.activate_version(version.version_id)

    async def auto_upgrade_loop(self, check_interval: float = 60.0):
        while True:
            try:
                new_commit = self.check_for_updates()
                
                if new_commit:
                    logger.info(f"New version available: {new_commit}")
                    result = await self.pull_and_upgrade()
                    
                    if result.success:
                        logger.info(f"Successfully upgraded to {result.new_version}")
                    else:
                        logger.error(f"Upgrade failed: {result.error_message}")
                
            except Exception as e:
                logger.error(f"Auto-upgrade check failed: {e}")
            
            await asyncio.sleep(check_interval)
