import os
import json
import logging
import asyncio
import hashlib
import sqlite3
from pathlib import Path
from dataclasses import dataclass, field
from typing import List, Optional, Dict, Any, Callable, Set
from datetime import datetime
from enum import Enum
from concurrent.futures import ThreadPoolExecutor, ProcessPoolExecutor
import threading
import queue
import time
import uuid

logger = logging.getLogger(__name__)


class ProcessingStatus(Enum):
    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    FAILED = "failed"
    SKIPPED = "skipped"


class FileType(Enum):
    TXT = "txt"
    PDF = "pdf"
    DOC = "doc"
    DOCX = "docx"
    RTF = "rtf"
    MD = "md"
    HTML = "html"
    JSON = "json"
    CSV = "csv"
    UNKNOWN = "unknown"


@dataclass
class FileTask:
    id: str
    path: str
    file_type: FileType
    size_bytes: int
    status: ProcessingStatus = ProcessingStatus.PENDING
    priority: int = 0
    retry_count: int = 0
    max_retries: int = 3
    error_message: str = ""
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None
    worker_id: Optional[str] = None
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class BatchProgress:
    total_files: int = 0
    total_bytes: int = 0
    processed_files: int = 0
    processed_bytes: int = 0
    failed_files: int = 0
    skipped_files: int = 0
    in_progress_files: int = 0
    start_time: Optional[datetime] = None
    estimated_completion: Optional[datetime] = None
    current_rate_files_per_sec: float = 0.0
    current_rate_bytes_per_sec: float = 0.0


@dataclass
class WorkerStatus:
    worker_id: str
    hostname: str
    status: str = "idle"
    current_task_id: Optional[str] = None
    tasks_completed: int = 0
    tasks_failed: int = 0
    last_heartbeat: datetime = field(default_factory=datetime.now)
    metadata: Dict[str, Any] = field(default_factory=dict)


class ProgressTracker:
    def __init__(self, db_path: str = None):
        self.db_path = db_path or os.path.expanduser("~/.mental_models/progress.db")
        os.makedirs(os.path.dirname(self.db_path), exist_ok=True)
        self._init_db()
        self._lock = threading.Lock()

    def _init_db(self):
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS file_tasks (
                id TEXT PRIMARY KEY,
                path TEXT UNIQUE NOT NULL,
                file_type TEXT NOT NULL,
                size_bytes INTEGER NOT NULL,
                status TEXT NOT NULL DEFAULT 'pending',
                priority INTEGER DEFAULT 0,
                retry_count INTEGER DEFAULT 0,
                max_retries INTEGER DEFAULT 3,
                error_message TEXT DEFAULT '',
                started_at TEXT,
                completed_at TEXT,
                worker_id TEXT,
                metadata TEXT DEFAULT '{}',
                created_at TEXT DEFAULT CURRENT_TIMESTAMP,
                updated_at TEXT DEFAULT CURRENT_TIMESTAMP
            )
        """)
        
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS workers (
                worker_id TEXT PRIMARY KEY,
                hostname TEXT NOT NULL,
                status TEXT DEFAULT 'idle',
                current_task_id TEXT,
                tasks_completed INTEGER DEFAULT 0,
                tasks_failed INTEGER DEFAULT 0,
                last_heartbeat TEXT DEFAULT CURRENT_TIMESTAMP,
                metadata TEXT DEFAULT '{}'
            )
        """)
        
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS batch_runs (
                id TEXT PRIMARY KEY,
                name TEXT,
                source_paths TEXT NOT NULL,
                status TEXT DEFAULT 'running',
                total_files INTEGER DEFAULT 0,
                processed_files INTEGER DEFAULT 0,
                failed_files INTEGER DEFAULT 0,
                started_at TEXT DEFAULT CURRENT_TIMESTAMP,
                completed_at TEXT,
                metadata TEXT DEFAULT '{}'
            )
        """)
        
        cursor.execute("CREATE INDEX IF NOT EXISTS idx_tasks_status ON file_tasks(status)")
        cursor.execute("CREATE INDEX IF NOT EXISTS idx_tasks_path ON file_tasks(path)")
        cursor.execute("CREATE INDEX IF NOT EXISTS idx_workers_status ON workers(status)")
        
        conn.commit()
        conn.close()

    def add_task(self, task: FileTask) -> bool:
        with self._lock:
            try:
                conn = sqlite3.connect(self.db_path)
                cursor = conn.cursor()
                
                cursor.execute("""
                    INSERT OR REPLACE INTO file_tasks 
                    (id, path, file_type, size_bytes, status, priority, retry_count, 
                     max_retries, error_message, started_at, completed_at, worker_id, metadata)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """, (
                    task.id, task.path, task.file_type.value, task.size_bytes,
                    task.status.value, task.priority, task.retry_count, task.max_retries,
                    task.error_message,
                    task.started_at.isoformat() if task.started_at else None,
                    task.completed_at.isoformat() if task.completed_at else None,
                    task.worker_id, json.dumps(task.metadata)
                ))
                
                conn.commit()
                conn.close()
                return True
                
            except Exception as e:
                logger.error(f"Failed to add task: {e}")
                return False

    def get_next_task(self, worker_id: str) -> Optional[FileTask]:
        with self._lock:
            try:
                conn = sqlite3.connect(self.db_path)
                cursor = conn.cursor()
                
                cursor.execute("""
                    SELECT id, path, file_type, size_bytes, status, priority, retry_count,
                           max_retries, error_message, started_at, completed_at, worker_id, metadata
                    FROM file_tasks
                    WHERE status = 'pending'
                    ORDER BY priority DESC, size_bytes ASC
                    LIMIT 1
                """)
                
                row = cursor.fetchone()
                if not row:
                    conn.close()
                    return None
                
                task = FileTask(
                    id=row[0], path=row[1], file_type=FileType(row[2]),
                    size_bytes=row[3], status=ProcessingStatus(row[4]),
                    priority=row[5], retry_count=row[6], max_retries=row[7],
                    error_message=row[8],
                    started_at=datetime.fromisoformat(row[9]) if row[9] else None,
                    completed_at=datetime.fromisoformat(row[10]) if row[10] else None,
                    worker_id=row[11],
                    metadata=json.loads(row[12]) if row[12] else {}
                )
                
                cursor.execute("""
                    UPDATE file_tasks 
                    SET status = 'in_progress', worker_id = ?, started_at = ?, updated_at = CURRENT_TIMESTAMP
                    WHERE id = ?
                """, (worker_id, datetime.now().isoformat(), task.id))
                
                conn.commit()
                conn.close()
                
                task.status = ProcessingStatus.IN_PROGRESS
                task.worker_id = worker_id
                task.started_at = datetime.now()
                
                return task
                
            except Exception as e:
                logger.error(f"Failed to get next task: {e}")
                return None

    def complete_task(self, task_id: str, success: bool, error_message: str = "") -> bool:
        with self._lock:
            try:
                conn = sqlite3.connect(self.db_path)
                cursor = conn.cursor()
                
                status = "completed" if success else "failed"
                
                cursor.execute("""
                    UPDATE file_tasks 
                    SET status = ?, completed_at = ?, error_message = ?, updated_at = CURRENT_TIMESTAMP
                    WHERE id = ?
                """, (status, datetime.now().isoformat(), error_message, task_id))
                
                conn.commit()
                conn.close()
                return True
                
            except Exception as e:
                logger.error(f"Failed to complete task: {e}")
                return False

    def retry_task(self, task_id: str) -> bool:
        with self._lock:
            try:
                conn = sqlite3.connect(self.db_path)
                cursor = conn.cursor()
                
                cursor.execute("""
                    UPDATE file_tasks 
                    SET status = 'pending', retry_count = retry_count + 1, 
                        worker_id = NULL, started_at = NULL, updated_at = CURRENT_TIMESTAMP
                    WHERE id = ? AND retry_count < max_retries
                """, (task_id,))
                
                affected = cursor.rowcount
                conn.commit()
                conn.close()
                return affected > 0
                
            except Exception as e:
                logger.error(f"Failed to retry task: {e}")
                return False

    def get_progress(self) -> BatchProgress:
        try:
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()
            
            cursor.execute("SELECT COUNT(*), COALESCE(SUM(size_bytes), 0) FROM file_tasks")
            total_files, total_bytes = cursor.fetchone()
            
            cursor.execute("""
                SELECT COUNT(*), COALESCE(SUM(size_bytes), 0) 
                FROM file_tasks WHERE status = 'completed'
            """)
            processed_files, processed_bytes = cursor.fetchone()
            
            cursor.execute("SELECT COUNT(*) FROM file_tasks WHERE status = 'failed'")
            failed_files = cursor.fetchone()[0]
            
            cursor.execute("SELECT COUNT(*) FROM file_tasks WHERE status = 'skipped'")
            skipped_files = cursor.fetchone()[0]
            
            cursor.execute("SELECT COUNT(*) FROM file_tasks WHERE status = 'in_progress'")
            in_progress_files = cursor.fetchone()[0]
            
            cursor.execute("SELECT MIN(started_at) FROM file_tasks WHERE started_at IS NOT NULL")
            start_time_str = cursor.fetchone()[0]
            start_time = datetime.fromisoformat(start_time_str) if start_time_str else None
            
            conn.close()
            
            progress = BatchProgress(
                total_files=total_files,
                total_bytes=total_bytes,
                processed_files=processed_files,
                processed_bytes=processed_bytes,
                failed_files=failed_files,
                skipped_files=skipped_files,
                in_progress_files=in_progress_files,
                start_time=start_time,
            )
            
            if start_time and processed_files > 0:
                elapsed = (datetime.now() - start_time).total_seconds()
                if elapsed > 0:
                    progress.current_rate_files_per_sec = processed_files / elapsed
                    progress.current_rate_bytes_per_sec = processed_bytes / elapsed
                    
                    remaining_files = total_files - processed_files - failed_files - skipped_files
                    if progress.current_rate_files_per_sec > 0:
                        remaining_seconds = remaining_files / progress.current_rate_files_per_sec
                        progress.estimated_completion = datetime.now() + \
                            __import__('datetime').timedelta(seconds=remaining_seconds)
            
            return progress
            
        except Exception as e:
            logger.error(f"Failed to get progress: {e}")
            return BatchProgress()

    def get_failed_tasks(self) -> List[FileTask]:
        try:
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()
            
            cursor.execute("""
                SELECT id, path, file_type, size_bytes, status, priority, retry_count,
                       max_retries, error_message, started_at, completed_at, worker_id, metadata
                FROM file_tasks
                WHERE status = 'failed'
            """)
            
            tasks = []
            for row in cursor.fetchall():
                tasks.append(FileTask(
                    id=row[0], path=row[1], file_type=FileType(row[2]),
                    size_bytes=row[3], status=ProcessingStatus(row[4]),
                    priority=row[5], retry_count=row[6], max_retries=row[7],
                    error_message=row[8],
                    started_at=datetime.fromisoformat(row[9]) if row[9] else None,
                    completed_at=datetime.fromisoformat(row[10]) if row[10] else None,
                    worker_id=row[11],
                    metadata=json.loads(row[12]) if row[12] else {}
                ))
            
            conn.close()
            return tasks
            
        except Exception as e:
            logger.error(f"Failed to get failed tasks: {e}")
            return []

    def reset_in_progress_tasks(self) -> int:
        with self._lock:
            try:
                conn = sqlite3.connect(self.db_path)
                cursor = conn.cursor()
                
                cursor.execute("""
                    UPDATE file_tasks 
                    SET status = 'pending', worker_id = NULL, started_at = NULL
                    WHERE status = 'in_progress'
                """)
                
                affected = cursor.rowcount
                conn.commit()
                conn.close()
                return affected
                
            except Exception as e:
                logger.error(f"Failed to reset in-progress tasks: {e}")
                return 0

    def clear_all(self) -> bool:
        with self._lock:
            try:
                conn = sqlite3.connect(self.db_path)
                cursor = conn.cursor()
                cursor.execute("DELETE FROM file_tasks")
                conn.commit()
                conn.close()
                return True
            except Exception as e:
                logger.error(f"Failed to clear tasks: {e}")
                return False


class FileScanner:
    SUPPORTED_EXTENSIONS = {
        ".txt": FileType.TXT,
        ".pdf": FileType.PDF,
        ".doc": FileType.DOC,
        ".docx": FileType.DOCX,
        ".rtf": FileType.RTF,
        ".md": FileType.MD,
        ".html": FileType.HTML,
        ".htm": FileType.HTML,
        ".json": FileType.JSON,
        ".csv": FileType.CSV,
    }

    def __init__(
        self,
        extensions: List[str] = None,
        min_size: int = 0,
        max_size: int = None,
        exclude_patterns: List[str] = None,
    ):
        self.extensions = extensions or list(self.SUPPORTED_EXTENSIONS.keys())
        self.min_size = min_size
        self.max_size = max_size
        self.exclude_patterns = exclude_patterns or [
            ".*", "__pycache__", "node_modules", ".git", ".svn",
            "venv", "env", ".env", "build", "dist"
        ]

    def _should_exclude(self, path: Path) -> bool:
        for pattern in self.exclude_patterns:
            if pattern in str(path):
                return True
        return False

    def _get_file_type(self, path: Path) -> FileType:
        ext = path.suffix.lower()
        return self.SUPPORTED_EXTENSIONS.get(ext, FileType.UNKNOWN)

    def scan_directory(
        self,
        directory: str,
        recursive: bool = True,
    ) -> List[FileTask]:
        tasks = []
        dir_path = Path(directory)
        
        if not dir_path.exists():
            logger.warning(f"Directory does not exist: {directory}")
            return tasks
        
        pattern = "**/*" if recursive else "*"
        
        for file_path in dir_path.glob(pattern):
            if not file_path.is_file():
                continue
            
            if self._should_exclude(file_path):
                continue
            
            ext = file_path.suffix.lower()
            if ext not in self.extensions:
                continue
            
            try:
                size = file_path.stat().st_size
                
                if size < self.min_size:
                    continue
                if self.max_size and size > self.max_size:
                    continue
                
                file_type = self._get_file_type(file_path)
                task_id = hashlib.sha256(str(file_path).encode()).hexdigest()[:32]
                
                tasks.append(FileTask(
                    id=task_id,
                    path=str(file_path.absolute()),
                    file_type=file_type,
                    size_bytes=size,
                    metadata={
                        "filename": file_path.name,
                        "extension": ext,
                        "directory": str(file_path.parent),
                        "modified_time": file_path.stat().st_mtime,
                    }
                ))
                
            except Exception as e:
                logger.warning(f"Failed to scan file {file_path}: {e}")
        
        return tasks

    def scan_multiple_directories(
        self,
        directories: List[str],
        recursive: bool = True,
    ) -> List[FileTask]:
        all_tasks = []
        seen_paths: Set[str] = set()
        
        for directory in directories:
            tasks = self.scan_directory(directory, recursive)
            for task in tasks:
                if task.path not in seen_paths:
                    all_tasks.append(task)
                    seen_paths.add(task.path)
        
        return all_tasks


class DistributedBatchProcessor:
    def __init__(
        self,
        tracker: ProgressTracker = None,
        worker_id: str = None,
        max_workers: int = 4,
        process_callback: Callable[[FileTask], bool] = None,
    ):
        self.tracker = tracker or ProgressTracker()
        self.worker_id = worker_id or f"worker-{uuid.uuid4().hex[:8]}"
        self.max_workers = max_workers
        self.process_callback = process_callback
        self._running = False
        self._workers: List[threading.Thread] = []
        self._task_queue: queue.Queue = queue.Queue()

    def add_directory(
        self,
        directory: str,
        recursive: bool = True,
        extensions: List[str] = None,
    ) -> int:
        scanner = FileScanner(extensions=extensions)
        tasks = scanner.scan_directory(directory, recursive)
        
        added = 0
        for task in tasks:
            if self.tracker.add_task(task):
                added += 1
        
        logger.info(f"Added {added} files from {directory}")
        return added

    def add_directories(
        self,
        directories: List[str],
        recursive: bool = True,
        extensions: List[str] = None,
    ) -> int:
        scanner = FileScanner(extensions=extensions)
        tasks = scanner.scan_multiple_directories(directories, recursive)
        
        added = 0
        for task in tasks:
            if self.tracker.add_task(task):
                added += 1
        
        logger.info(f"Added {added} files from {len(directories)} directories")
        return added

    def _worker_loop(self, worker_num: int):
        worker_id = f"{self.worker_id}-{worker_num}"
        logger.info(f"Worker {worker_id} started")
        
        while self._running:
            task = self.tracker.get_next_task(worker_id)
            
            if not task:
                time.sleep(1)
                continue
            
            try:
                if self.process_callback:
                    success = self.process_callback(task)
                else:
                    success = self._default_process(task)
                
                self.tracker.complete_task(task.id, success)
                
                if not success and task.retry_count < task.max_retries:
                    self.tracker.retry_task(task.id)
                    
            except Exception as e:
                logger.error(f"Worker {worker_id} failed on task {task.id}: {e}")
                self.tracker.complete_task(task.id, False, str(e))
                
                if task.retry_count < task.max_retries:
                    self.tracker.retry_task(task.id)
        
        logger.info(f"Worker {worker_id} stopped")

    def _default_process(self, task: FileTask) -> bool:
        logger.info(f"Processing: {task.path}")
        return True

    def start(self):
        if self._running:
            return
        
        self._running = True
        reset_count = self.tracker.reset_in_progress_tasks()
        if reset_count > 0:
            logger.info(f"Reset {reset_count} in-progress tasks from previous run")
        
        for i in range(self.max_workers):
            worker = threading.Thread(target=self._worker_loop, args=(i,), daemon=True)
            worker.start()
            self._workers.append(worker)
        
        logger.info(f"Started {self.max_workers} workers")

    def stop(self):
        self._running = False
        
        for worker in self._workers:
            worker.join(timeout=5)
        
        self._workers.clear()
        logger.info("All workers stopped")

    def wait_for_completion(self, poll_interval: float = 5.0):
        while self._running:
            progress = self.tracker.get_progress()
            
            remaining = progress.total_files - progress.processed_files - \
                       progress.failed_files - progress.skipped_files
            
            if remaining <= 0 and progress.in_progress_files == 0:
                break
            
            logger.info(
                f"Progress: {progress.processed_files}/{progress.total_files} "
                f"({progress.failed_files} failed, {progress.in_progress_files} in progress)"
            )
            
            time.sleep(poll_interval)

    def get_progress(self) -> BatchProgress:
        return self.tracker.get_progress()

    def get_failed_tasks(self) -> List[FileTask]:
        return self.tracker.get_failed_tasks()

    def retry_all_failed(self) -> int:
        failed_tasks = self.get_failed_tasks()
        retried = 0
        
        for task in failed_tasks:
            if self.tracker.retry_task(task.id):
                retried += 1
        
        return retried


class ClusterCoordinator:
    def __init__(
        self,
        coordinator_url: str = None,
        node_id: str = None,
        is_coordinator: bool = False,
    ):
        self.coordinator_url = coordinator_url
        self.node_id = node_id or f"node-{uuid.uuid4().hex[:8]}"
        self.is_coordinator = is_coordinator
        self._workers: Dict[str, WorkerStatus] = {}
        self._running = False

    def register_worker(self, worker: WorkerStatus) -> bool:
        self._workers[worker.worker_id] = worker
        logger.info(f"Registered worker: {worker.worker_id} on {worker.hostname}")
        return True

    def unregister_worker(self, worker_id: str) -> bool:
        if worker_id in self._workers:
            del self._workers[worker_id]
            logger.info(f"Unregistered worker: {worker_id}")
            return True
        return False

    def heartbeat(self, worker_id: str) -> bool:
        if worker_id in self._workers:
            self._workers[worker_id].last_heartbeat = datetime.now()
            return True
        return False

    def get_cluster_status(self) -> Dict[str, Any]:
        active_workers = [
            w for w in self._workers.values()
            if (datetime.now() - w.last_heartbeat).total_seconds() < 60
        ]
        
        return {
            "total_workers": len(self._workers),
            "active_workers": len(active_workers),
            "workers": [
                {
                    "worker_id": w.worker_id,
                    "hostname": w.hostname,
                    "status": w.status,
                    "tasks_completed": w.tasks_completed,
                    "tasks_failed": w.tasks_failed,
                    "last_heartbeat": w.last_heartbeat.isoformat(),
                }
                for w in self._workers.values()
            ]
        }

    def distribute_work(
        self,
        directories: List[str],
        tracker: ProgressTracker,
    ) -> Dict[str, int]:
        scanner = FileScanner()
        all_tasks = scanner.scan_multiple_directories(directories)
        
        for task in all_tasks:
            tracker.add_task(task)
        
        return {
            "total_files": len(all_tasks),
            "total_bytes": sum(t.size_bytes for t in all_tasks),
        }
