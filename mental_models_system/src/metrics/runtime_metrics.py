import time
import threading
import functools
import traceback
import psutil
import os
from dataclasses import dataclass, field
from typing import Dict, Any, List, Optional, Callable
from datetime import datetime, timedelta
from collections import defaultdict
from enum import Enum
import logging
import json

logger = logging.getLogger(__name__)


class MetricType(Enum):
    COUNTER = "counter"
    GAUGE = "gauge"
    HISTOGRAM = "histogram"
    SUMMARY = "summary"
    TIMER = "timer"


@dataclass
class TimerMetric:
    name: str
    start_time: float
    end_time: Optional[float] = None
    duration_ms: Optional[float] = None
    success: bool = True
    error: Optional[str] = None
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    def stop(self, success: bool = True, error: Optional[str] = None):
        self.end_time = time.time()
        self.duration_ms = (self.end_time - self.start_time) * 1000
        self.success = success
        self.error = error
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "start_time": self.start_time,
            "end_time": self.end_time,
            "duration_ms": self.duration_ms,
            "success": self.success,
            "error": self.error,
            "metadata": self.metadata
        }


@dataclass
class HistogramBucket:
    le: float
    count: int = 0


class Histogram:
    def __init__(self, name: str, buckets: List[float] = None):
        self.name = name
        self.buckets = buckets or [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10]
        self.bucket_counts: Dict[float, int] = {b: 0 for b in self.buckets}
        self.bucket_counts[float('inf')] = 0
        self.sum = 0.0
        self.count = 0
        self._lock = threading.Lock()
    
    def observe(self, value: float):
        with self._lock:
            self.sum += value
            self.count += 1
            for bucket in sorted(self.buckets):
                if value <= bucket:
                    self.bucket_counts[bucket] += 1
            self.bucket_counts[float('inf')] += 1
    
    def get_percentile(self, p: float) -> float:
        if self.count == 0:
            return 0
        
        target = self.count * p
        cumulative = 0
        prev_bucket = 0
        
        for bucket in sorted(self.buckets):
            cumulative += self.bucket_counts[bucket]
            if cumulative >= target:
                return bucket
            prev_bucket = bucket
        
        return self.buckets[-1] if self.buckets else 0
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "count": self.count,
            "sum": self.sum,
            "avg": self.sum / self.count if self.count > 0 else 0,
            "p50": self.get_percentile(0.5),
            "p90": self.get_percentile(0.9),
            "p95": self.get_percentile(0.95),
            "p99": self.get_percentile(0.99),
            "buckets": {str(k): v for k, v in self.bucket_counts.items()}
        }


class Counter:
    def __init__(self, name: str, labels: List[str] = None):
        self.name = name
        self.labels = labels or []
        self._values: Dict[tuple, float] = defaultdict(float)
        self._lock = threading.Lock()
    
    def inc(self, value: float = 1, **label_values):
        key = tuple(label_values.get(l, "") for l in self.labels)
        with self._lock:
            self._values[key] += value
    
    def get(self, **label_values) -> float:
        key = tuple(label_values.get(l, "") for l in self.labels)
        return self._values.get(key, 0)
    
    def total(self) -> float:
        return sum(self._values.values())
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "total": self.total(),
            "by_labels": {str(k): v for k, v in self._values.items()} if self.labels else {}
        }


class Gauge:
    def __init__(self, name: str):
        self.name = name
        self._value = 0.0
        self._lock = threading.Lock()
        self._history: List[tuple] = []
        self._max_history = 1000
    
    def set(self, value: float):
        with self._lock:
            self._value = value
            self._history.append((time.time(), value))
            if len(self._history) > self._max_history:
                self._history = self._history[-self._max_history:]
    
    def inc(self, value: float = 1):
        with self._lock:
            self._value += value
    
    def dec(self, value: float = 1):
        with self._lock:
            self._value -= value
    
    def get(self) -> float:
        return self._value
    
    def get_history(self, seconds: int = 60) -> List[tuple]:
        cutoff = time.time() - seconds
        return [(t, v) for t, v in self._history if t >= cutoff]
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "value": self._value,
            "history_points": len(self._history)
        }


class RuntimeMetricsCollector:
    _instance = None
    _lock = threading.Lock()
    
    def __new__(cls):
        if cls._instance is None:
            with cls._lock:
                if cls._instance is None:
                    cls._instance = super().__new__(cls)
                    cls._instance._initialized = False
        return cls._instance
    
    def __init__(self):
        if self._initialized:
            return
        
        self._initialized = True
        self.counters: Dict[str, Counter] = {}
        self.gauges: Dict[str, Gauge] = {}
        self.histograms: Dict[str, Histogram] = {}
        self.timers: List[TimerMetric] = []
        self.errors: List[Dict[str, Any]] = []
        self.start_time = time.time()
        self._collection_thread = None
        self._running = False
        self._system_metrics_history: List[Dict[str, Any]] = []
        
        self._init_default_metrics()
    
    def _init_default_metrics(self):
        self.counters["requests_total"] = Counter("requests_total", ["method", "endpoint", "status"])
        self.counters["errors_total"] = Counter("errors_total", ["type", "module"])
        self.counters["function_calls"] = Counter("function_calls", ["function", "module"])
        self.counters["db_queries"] = Counter("db_queries", ["operation", "table"])
        self.counters["cache_hits"] = Counter("cache_hits", ["cache_name"])
        self.counters["cache_misses"] = Counter("cache_misses", ["cache_name"])
        self.counters["api_calls"] = Counter("api_calls", ["service", "endpoint"])
        self.counters["file_operations"] = Counter("file_operations", ["operation", "type"])
        self.counters["network_requests"] = Counter("network_requests", ["method", "host"])
        self.counters["events_processed"] = Counter("events_processed", ["event_type"])
        
        self.gauges["active_connections"] = Gauge("active_connections")
        self.gauges["queue_size"] = Gauge("queue_size")
        self.gauges["memory_usage_mb"] = Gauge("memory_usage_mb")
        self.gauges["cpu_percent"] = Gauge("cpu_percent")
        self.gauges["disk_usage_percent"] = Gauge("disk_usage_percent")
        self.gauges["open_files"] = Gauge("open_files")
        self.gauges["thread_count"] = Gauge("thread_count")
        self.gauges["active_tasks"] = Gauge("active_tasks")
        self.gauges["pending_tasks"] = Gauge("pending_tasks")
        self.gauges["cache_size"] = Gauge("cache_size")
        
        self.histograms["request_duration_ms"] = Histogram("request_duration_ms")
        self.histograms["db_query_duration_ms"] = Histogram("db_query_duration_ms")
        self.histograms["function_duration_ms"] = Histogram("function_duration_ms")
        self.histograms["api_call_duration_ms"] = Histogram("api_call_duration_ms")
        self.histograms["file_operation_duration_ms"] = Histogram("file_operation_duration_ms")
        self.histograms["response_size_bytes"] = Histogram("response_size_bytes", 
            [100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000])
        self.histograms["batch_size"] = Histogram("batch_size",
            [1, 5, 10, 25, 50, 100, 250, 500, 1000])
    
    def start_collection(self, interval_seconds: float = 5.0):
        if self._running:
            return
        
        self._running = True
        self._collection_thread = threading.Thread(
            target=self._collect_system_metrics,
            args=(interval_seconds,),
            daemon=True
        )
        self._collection_thread.start()
    
    def stop_collection(self):
        self._running = False
        if self._collection_thread:
            self._collection_thread.join(timeout=5)
    
    def _collect_system_metrics(self, interval: float):
        while self._running:
            try:
                process = psutil.Process(os.getpid())
                
                memory_info = process.memory_info()
                self.gauges["memory_usage_mb"].set(memory_info.rss / 1024 / 1024)
                
                cpu_percent = process.cpu_percent()
                self.gauges["cpu_percent"].set(cpu_percent)
                
                try:
                    disk = psutil.disk_usage("/")
                    self.gauges["disk_usage_percent"].set(disk.percent)
                except Exception:
                    pass
                
                try:
                    open_files = len(process.open_files())
                    self.gauges["open_files"].set(open_files)
                except Exception:
                    pass
                
                self.gauges["thread_count"].set(threading.active_count())
                
                snapshot = {
                    "timestamp": time.time(),
                    "memory_mb": memory_info.rss / 1024 / 1024,
                    "cpu_percent": cpu_percent,
                    "thread_count": threading.active_count(),
                    "uptime_seconds": time.time() - self.start_time
                }
                self._system_metrics_history.append(snapshot)
                
                if len(self._system_metrics_history) > 10000:
                    self._system_metrics_history = self._system_metrics_history[-10000:]
                
            except Exception as e:
                logger.warning(f"Error collecting system metrics: {e}")
            
            time.sleep(interval)
    
    def start_timer(self, name: str, **metadata) -> TimerMetric:
        timer = TimerMetric(
            name=name,
            start_time=time.time(),
            metadata=metadata
        )
        return timer
    
    def record_timer(self, timer: TimerMetric):
        if timer.duration_ms is None:
            timer.stop()
        
        self.timers.append(timer)
        self.histograms["function_duration_ms"].observe(timer.duration_ms)
        
        if len(self.timers) > 10000:
            self.timers = self.timers[-10000:]
    
    def record_error(self, error: Exception, context: Dict[str, Any] = None):
        error_record = {
            "timestamp": time.time(),
            "type": type(error).__name__,
            "message": str(error),
            "traceback": traceback.format_exc(),
            "context": context or {}
        }
        self.errors.append(error_record)
        self.counters["errors_total"].inc(type=type(error).__name__, module=context.get("module", "unknown") if context else "unknown")
        
        if len(self.errors) > 1000:
            self.errors = self.errors[-1000:]
    
    def get_counter(self, name: str) -> Counter:
        if name not in self.counters:
            self.counters[name] = Counter(name)
        return self.counters[name]
    
    def get_gauge(self, name: str) -> Gauge:
        if name not in self.gauges:
            self.gauges[name] = Gauge(name)
        return self.gauges[name]
    
    def get_histogram(self, name: str) -> Histogram:
        if name not in self.histograms:
            self.histograms[name] = Histogram(name)
        return self.histograms[name]
    
    def get_full_report(self) -> Dict[str, Any]:
        uptime = time.time() - self.start_time
        
        recent_timers = self.timers[-100:] if self.timers else []
        avg_duration = sum(t.duration_ms or 0 for t in recent_timers) / len(recent_timers) if recent_timers else 0
        success_rate = sum(1 for t in recent_timers if t.success) / len(recent_timers) * 100 if recent_timers else 100
        
        return {
            "summary": {
                "uptime_seconds": uptime,
                "uptime_formatted": str(timedelta(seconds=int(uptime))),
                "total_requests": self.counters["requests_total"].total(),
                "total_errors": self.counters["errors_total"].total(),
                "error_rate": self.counters["errors_total"].total() / max(self.counters["requests_total"].total(), 1) * 100,
                "avg_response_time_ms": avg_duration,
                "success_rate": success_rate,
                "memory_usage_mb": self.gauges["memory_usage_mb"].get(),
                "cpu_percent": self.gauges["cpu_percent"].get(),
                "thread_count": self.gauges["thread_count"].get(),
            },
            "counters": {name: c.to_dict() for name, c in self.counters.items()},
            "gauges": {name: g.to_dict() for name, g in self.gauges.items()},
            "histograms": {name: h.to_dict() for name, h in self.histograms.items()},
            "recent_timers": [t.to_dict() for t in recent_timers],
            "recent_errors": self.errors[-20:],
            "system_metrics_history": self._system_metrics_history[-100:],
            "timestamp": datetime.now().isoformat()
        }
    
    def get_prometheus_format(self) -> str:
        lines = []
        
        for name, counter in self.counters.items():
            lines.append(f"# TYPE {name} counter")
            if counter.labels:
                for labels, value in counter._values.items():
                    label_str = ",".join(f'{l}="{v}"' for l, v in zip(counter.labels, labels))
                    lines.append(f"{name}{{{label_str}}} {value}")
            else:
                lines.append(f"{name} {counter.total()}")
        
        for name, gauge in self.gauges.items():
            lines.append(f"# TYPE {name} gauge")
            lines.append(f"{name} {gauge.get()}")
        
        for name, histogram in self.histograms.items():
            lines.append(f"# TYPE {name} histogram")
            for bucket, count in sorted(histogram.bucket_counts.items()):
                le = "+Inf" if bucket == float('inf') else bucket
                lines.append(f'{name}_bucket{{le="{le}"}} {count}')
            lines.append(f"{name}_sum {histogram.sum}")
            lines.append(f"{name}_count {histogram.count}")
        
        return "\n".join(lines)


def timed(name: str = None):
    def decorator(func: Callable):
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            collector = RuntimeMetricsCollector()
            timer_name = name or f"{func.__module__}.{func.__name__}"
            timer = collector.start_timer(timer_name, function=func.__name__)
            
            collector.counters["function_calls"].inc(
                function=func.__name__,
                module=func.__module__
            )
            
            try:
                result = func(*args, **kwargs)
                timer.stop(success=True)
                collector.record_timer(timer)
                return result
            except Exception as e:
                timer.stop(success=False, error=str(e))
                collector.record_timer(timer)
                collector.record_error(e, {"function": func.__name__, "module": func.__module__})
                raise
        
        return wrapper
    return decorator


def counted(counter_name: str, **labels):
    def decorator(func: Callable):
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            collector = RuntimeMetricsCollector()
            collector.get_counter(counter_name).inc(**labels)
            return func(*args, **kwargs)
        return wrapper
    return decorator


class MetricsMiddleware:
    def __init__(self, app):
        self.app = app
        self.collector = RuntimeMetricsCollector()
    
    async def __call__(self, scope, receive, send):
        if scope["type"] != "http":
            await self.app(scope, receive, send)
            return
        
        method = scope.get("method", "UNKNOWN")
        path = scope.get("path", "/")
        
        timer = self.collector.start_timer(
            "http_request",
            method=method,
            path=path
        )
        
        status_code = 200
        
        async def send_wrapper(message):
            nonlocal status_code
            if message["type"] == "http.response.start":
                status_code = message.get("status", 200)
            await send(message)
        
        try:
            await self.app(scope, receive, send_wrapper)
            timer.stop(success=status_code < 400)
        except Exception as e:
            timer.stop(success=False, error=str(e))
            self.collector.record_error(e, {"path": path, "method": method})
            raise
        finally:
            self.collector.record_timer(timer)
            self.collector.counters["requests_total"].inc(
                method=method,
                endpoint=path,
                status=str(status_code)
            )
            self.collector.histograms["request_duration_ms"].observe(timer.duration_ms or 0)


def get_metrics_collector() -> RuntimeMetricsCollector:
    return RuntimeMetricsCollector()


def start_metrics_collection(interval: float = 5.0):
    collector = RuntimeMetricsCollector()
    collector.start_collection(interval)
    return collector


def get_metrics_report() -> Dict[str, Any]:
    return RuntimeMetricsCollector().get_full_report()


def get_prometheus_metrics() -> str:
    return RuntimeMetricsCollector().get_prometheus_format()
