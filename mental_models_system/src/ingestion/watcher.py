import os
import time
import hashlib
import threading
from pathlib import Path
from dataclasses import dataclass, field
from typing import Callable, Optional, Set, Dict, List
from datetime import datetime
import logging

logger = logging.getLogger(__name__)


@dataclass
class FileEvent:
    path: Path
    event_type: str
    timestamp: datetime
    file_hash: Optional[str] = None
    file_size: int = 0


class FolderWatcher:
    def __init__(
        self,
        watch_paths: List[str],
        extensions: Optional[List[str]] = None,
        recursive: bool = True,
        poll_interval: float = 1.0,
        batch_size: int = 100,
        batch_timeout: float = 5.0,
    ):
        self.watch_paths = [Path(p) for p in watch_paths]
        self.extensions = set(extensions) if extensions else {".txt"}
        self.recursive = recursive
        self.poll_interval = poll_interval
        self.batch_size = batch_size
        self.batch_timeout = batch_timeout
        
        self._file_hashes: Dict[Path, str] = {}
        self._file_mtimes: Dict[Path, float] = {}
        self._callbacks: List[Callable[[List[FileEvent]], None]] = []
        self._running = False
        self._thread: Optional[threading.Thread] = None
        self._lock = threading.Lock()
        self._pending_events: List[FileEvent] = []
        self._last_batch_time = time.time()

    def add_callback(self, callback: Callable[[List[FileEvent]], None]) -> None:
        self._callbacks.append(callback)

    def _compute_hash(self, path: Path) -> str:
        hasher = hashlib.md5()
        try:
            with open(path, "rb") as f:
                for chunk in iter(lambda: f.read(65536), b""):
                    hasher.update(chunk)
            return hasher.hexdigest()
        except Exception as e:
            logger.warning(f"Failed to hash {path}: {e}")
            return ""

    def _scan_directory(self, directory: Path) -> Set[Path]:
        files = set()
        try:
            if self.recursive:
                for ext in self.extensions:
                    files.update(directory.rglob(f"*{ext}"))
            else:
                for ext in self.extensions:
                    files.update(directory.glob(f"*{ext}"))
        except Exception as e:
            logger.error(f"Error scanning {directory}: {e}")
        return files

    def _check_for_changes(self) -> List[FileEvent]:
        events = []
        current_files: Set[Path] = set()
        
        for watch_path in self.watch_paths:
            if watch_path.is_dir():
                current_files.update(self._scan_directory(watch_path))
            elif watch_path.is_file() and watch_path.suffix in self.extensions:
                current_files.add(watch_path)
        
        known_files = set(self._file_hashes.keys())
        
        new_files = current_files - known_files
        for path in new_files:
            try:
                file_hash = self._compute_hash(path)
                file_size = path.stat().st_size
                mtime = path.stat().st_mtime
                
                self._file_hashes[path] = file_hash
                self._file_mtimes[path] = mtime
                
                events.append(FileEvent(
                    path=path,
                    event_type="created",
                    timestamp=datetime.now(),
                    file_hash=file_hash,
                    file_size=file_size,
                ))
            except Exception as e:
                logger.warning(f"Error processing new file {path}: {e}")
        
        deleted_files = known_files - current_files
        for path in deleted_files:
            events.append(FileEvent(
                path=path,
                event_type="deleted",
                timestamp=datetime.now(),
                file_hash=self._file_hashes.get(path),
            ))
            self._file_hashes.pop(path, None)
            self._file_mtimes.pop(path, None)
        
        for path in current_files & known_files:
            try:
                current_mtime = path.stat().st_mtime
                if current_mtime != self._file_mtimes.get(path):
                    new_hash = self._compute_hash(path)
                    if new_hash != self._file_hashes.get(path):
                        file_size = path.stat().st_size
                        
                        self._file_hashes[path] = new_hash
                        self._file_mtimes[path] = current_mtime
                        
                        events.append(FileEvent(
                            path=path,
                            event_type="modified",
                            timestamp=datetime.now(),
                            file_hash=new_hash,
                            file_size=file_size,
                        ))
            except Exception as e:
                logger.warning(f"Error checking file {path}: {e}")
        
        return events

    def _process_batch(self, events: List[FileEvent]) -> None:
        if not events:
            return
        
        for callback in self._callbacks:
            try:
                callback(events)
            except Exception as e:
                logger.error(f"Callback error: {e}")

    def _watch_loop(self) -> None:
        while self._running:
            try:
                events = self._check_for_changes()
                
                with self._lock:
                    self._pending_events.extend(events)
                    
                    should_process = (
                        len(self._pending_events) >= self.batch_size or
                        (self._pending_events and 
                         time.time() - self._last_batch_time >= self.batch_timeout)
                    )
                    
                    if should_process:
                        batch = self._pending_events[:]
                        self._pending_events = []
                        self._last_batch_time = time.time()
                
                if should_process:
                    self._process_batch(batch)
                
                time.sleep(self.poll_interval)
                
            except Exception as e:
                logger.error(f"Watch loop error: {e}")
                time.sleep(self.poll_interval)

    def start(self) -> None:
        if self._running:
            return
        
        self._running = True
        self._thread = threading.Thread(target=self._watch_loop, daemon=True)
        self._thread.start()
        logger.info(f"Started watching: {self.watch_paths}")

    def stop(self) -> None:
        self._running = False
        if self._thread:
            self._thread.join(timeout=5.0)
            self._thread = None
        logger.info("Stopped watching")

    def scan_existing(self) -> List[FileEvent]:
        events = []
        for watch_path in self.watch_paths:
            if watch_path.is_dir():
                files = self._scan_directory(watch_path)
            elif watch_path.is_file():
                files = {watch_path}
            else:
                continue
            
            for path in files:
                try:
                    file_hash = self._compute_hash(path)
                    file_size = path.stat().st_size
                    mtime = path.stat().st_mtime
                    
                    self._file_hashes[path] = file_hash
                    self._file_mtimes[path] = mtime
                    
                    events.append(FileEvent(
                        path=path,
                        event_type="existing",
                        timestamp=datetime.now(),
                        file_hash=file_hash,
                        file_size=file_size,
                    ))
                except Exception as e:
                    logger.warning(f"Error scanning {path}: {e}")
        
        return events

    def __enter__(self):
        self.start()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.stop()
        return False
