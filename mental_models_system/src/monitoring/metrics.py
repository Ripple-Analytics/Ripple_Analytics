"""
Prometheus metrics exporter for Mental Models System.

Exposes metrics for:
- API performance
- LLM inference
- Mental model analysis
- Decision tracking
- Signal harvesting
"""

import time
from functools import wraps
from typing import Callable, Dict, Any, Optional
from dataclasses import dataclass, field
from collections import defaultdict
import threading


@dataclass
class MetricValue:
    """A single metric value."""
    value: float
    labels: Dict[str, str] = field(default_factory=dict)
    timestamp: float = field(default_factory=time.time)


class Counter:
    """Prometheus-style counter metric."""
    
    def __init__(self, name: str, description: str, labels: list = None):
        self.name = name
        self.description = description
        self.label_names = labels or []
        self._values: Dict[tuple, float] = defaultdict(float)
        self._lock = threading.Lock()
    
    def inc(self, value: float = 1, **labels):
        """Increment counter."""
        label_values = tuple(labels.get(l, "") for l in self.label_names)
        with self._lock:
            self._values[label_values] += value
    
    def get(self, **labels) -> float:
        """Get current value."""
        label_values = tuple(labels.get(l, "") for l in self.label_names)
        return self._values[label_values]
    
    def collect(self) -> list:
        """Collect all values for export."""
        result = []
        for label_values, value in self._values.items():
            labels = dict(zip(self.label_names, label_values))
            result.append(MetricValue(value=value, labels=labels))
        return result


class Gauge:
    """Prometheus-style gauge metric."""
    
    def __init__(self, name: str, description: str, labels: list = None):
        self.name = name
        self.description = description
        self.label_names = labels or []
        self._values: Dict[tuple, float] = {}
        self._lock = threading.Lock()
    
    def set(self, value: float, **labels):
        """Set gauge value."""
        label_values = tuple(labels.get(l, "") for l in self.label_names)
        with self._lock:
            self._values[label_values] = value
    
    def inc(self, value: float = 1, **labels):
        """Increment gauge."""
        label_values = tuple(labels.get(l, "") for l in self.label_names)
        with self._lock:
            self._values[label_values] = self._values.get(label_values, 0) + value
    
    def dec(self, value: float = 1, **labels):
        """Decrement gauge."""
        self.inc(-value, **labels)
    
    def get(self, **labels) -> float:
        """Get current value."""
        label_values = tuple(labels.get(l, "") for l in self.label_names)
        return self._values.get(label_values, 0)
    
    def collect(self) -> list:
        """Collect all values for export."""
        result = []
        for label_values, value in self._values.items():
            labels = dict(zip(self.label_names, label_values))
            result.append(MetricValue(value=value, labels=labels))
        return result


class Histogram:
    """Prometheus-style histogram metric."""
    
    DEFAULT_BUCKETS = (0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10, float('inf'))
    
    def __init__(self, name: str, description: str, labels: list = None, buckets: tuple = None):
        self.name = name
        self.description = description
        self.label_names = labels or []
        self.buckets = buckets or self.DEFAULT_BUCKETS
        self._counts: Dict[tuple, Dict[float, int]] = defaultdict(lambda: defaultdict(int))
        self._sums: Dict[tuple, float] = defaultdict(float)
        self._lock = threading.Lock()
    
    def observe(self, value: float, **labels):
        """Observe a value."""
        label_values = tuple(labels.get(l, "") for l in self.label_names)
        with self._lock:
            self._sums[label_values] += value
            for bucket in self.buckets:
                if value <= bucket:
                    self._counts[label_values][bucket] += 1
    
    def time(self, **labels):
        """Context manager for timing."""
        return HistogramTimer(self, labels)
    
    def collect(self) -> list:
        """Collect all values for export."""
        result = []
        for label_values in self._counts:
            labels = dict(zip(self.label_names, label_values))
            
            # Bucket values
            cumulative = 0
            for bucket in self.buckets:
                cumulative += self._counts[label_values][bucket]
                bucket_labels = {**labels, "le": str(bucket)}
                result.append(MetricValue(value=cumulative, labels=bucket_labels))
            
            # Sum
            result.append(MetricValue(
                value=self._sums[label_values],
                labels={**labels, "type": "sum"}
            ))
            
            # Count
            result.append(MetricValue(
                value=cumulative,
                labels={**labels, "type": "count"}
            ))
        
        return result


class HistogramTimer:
    """Context manager for timing histogram observations."""
    
    def __init__(self, histogram: Histogram, labels: dict):
        self.histogram = histogram
        self.labels = labels
        self.start_time = None
    
    def __enter__(self):
        self.start_time = time.time()
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        duration = time.time() - self.start_time
        self.histogram.observe(duration, **self.labels)


# =============================================================================
# MENTAL MODELS SYSTEM METRICS
# =============================================================================

# API Metrics
http_requests_total = Counter(
    "http_requests_total",
    "Total HTTP requests",
    labels=["method", "endpoint", "status"]
)

http_request_duration_seconds = Histogram(
    "http_request_duration_seconds",
    "HTTP request duration in seconds",
    labels=["method", "endpoint"],
    buckets=(0.01, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10)
)

# LLM Metrics
llm_requests_total = Counter(
    "llm_requests_total",
    "Total LLM requests",
    labels=["backend", "model", "status"]
)

llm_request_duration_seconds = Histogram(
    "llm_request_duration_seconds",
    "LLM request duration in seconds",
    labels=["backend", "model"],
    buckets=(0.5, 1, 2.5, 5, 10, 30, 60, 120)
)

llm_tokens_total = Counter(
    "llm_tokens_total",
    "Total LLM tokens processed",
    labels=["backend", "model", "type"]  # type: input/output
)

# Analysis Metrics
analysis_total = Counter(
    "mental_models_analysis_total",
    "Total document analyses",
    labels=["status"]
)

analysis_duration_seconds = Histogram(
    "mental_models_analysis_duration_seconds",
    "Analysis duration in seconds",
    labels=["analysis_type"],
    buckets=(0.1, 0.5, 1, 2.5, 5, 10, 30, 60)
)

models_detected = Counter(
    "mental_models_detected_total",
    "Mental models detected in analyses",
    labels=["model_id", "model_name", "category"]
)

lollapalooza_score = Gauge(
    "mental_models_lollapalooza_score",
    "Lollapalooza score for documents",
    labels=["document_id"]
)

analysis_queue_size = Gauge(
    "mental_models_analysis_queue_size",
    "Number of documents waiting for analysis"
)

# Decision Metrics
decisions_total = Counter(
    "mental_models_decisions_total",
    "Total decisions tracked",
    labels=["domain", "outcome"]
)

decision_risk_score = Gauge(
    "mental_models_decision_risk_score",
    "Risk score for decisions",
    labels=["decision_id"]
)

model_effectiveness = Gauge(
    "mental_models_model_effectiveness",
    "Effectiveness score for mental models",
    labels=["model_id", "model_name"]
)

# Signal Harvester Metrics
signals_harvested_total = Counter(
    "mental_models_signals_harvested_total",
    "Total signals harvested",
    labels=["source", "priority"]
)

signal_priority = Gauge(
    "mental_models_signal_priority",
    "Priority level of detected signals",
    labels=["signal_id", "signal_title"]
)

harvester_errors_total = Counter(
    "harvester_errors_total",
    "Total harvester errors",
    labels=["error_type"]
)

# Knowledge Graph Metrics
knowledge_graph_nodes = Gauge(
    "mental_models_knowledge_graph_nodes",
    "Number of nodes in knowledge graph",
    labels=["node_type"]
)

knowledge_graph_edges = Gauge(
    "mental_models_knowledge_graph_edges",
    "Number of edges in knowledge graph"
)

# Improvement Tracking Metrics
improvement_suggestions_pending = Gauge(
    "improvement_suggestions_pending_total",
    "Total number of pending improvement suggestions"
)

improvement_suggestions_completed = Counter(
    "improvement_suggestions_completed_total",
    "Total number of completed improvement suggestions"
)

improvement_suggestions_generated = Counter(
    "improvement_suggestions_generated_total",
    "Total number of improvement suggestions generated"
)

improvement_suggestions_by_priority = Gauge(
    "improvement_suggestions_by_priority",
    "Improvement suggestions by priority level",
    labels=["priority"]
)

improvement_suggestions_by_category = Gauge(
    "improvement_suggestions_by_category",
    "Improvement suggestions by category",
    labels=["category"]
)

failure_mode_coverage = Gauge(
    "failure_mode_coverage_ratio",
    "Ratio of models with adequate failure modes (5+)"
)

failure_modes_per_model = Gauge(
    "failure_modes_per_model",
    "Number of failure modes per model",
    labels=["model"]
)

decisions_recorded = Counter(
    "decisions_recorded_total",
    "Total number of decisions recorded"
)

decisions_with_outcomes = Counter(
    "decisions_with_outcomes_total",
    "Total number of decisions with recorded outcomes"
)

mental_models_total = Gauge(
    "mental_models_total",
    "Total number of mental models in the system"
)

improvement_suggestion_impact = Gauge(
    "improvement_suggestion_impact",
    "Estimated impact of improvement suggestions",
    labels=["suggestion_id", "title"]
)

# Audit Event Metrics
audit_events_total = Counter(
    "audit_events_total",
    "Total audit events logged",
    labels=["event_type", "outcome"]
)

audit_events_by_actor = Counter(
    "audit_events_by_actor_total",
    "Audit events by actor",
    labels=["actor", "event_type"]
)

audit_events_by_resource = Counter(
    "audit_events_by_resource_total",
    "Audit events by resource type",
    labels=["resource_type", "action"]
)

audit_api_requests_total = Counter(
    "audit_api_requests_total",
    "Total API requests logged in audit",
    labels=["method", "path", "status_code"]
)

audit_api_request_duration = Histogram(
    "audit_api_request_duration_seconds",
    "API request duration from audit logs",
    labels=["method", "path"],
    buckets=(0.01, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10)
)

audit_security_events_total = Counter(
    "audit_security_events_total",
    "Security-related audit events",
    labels=["event_type", "severity"]
)

audit_failed_operations_total = Counter(
    "audit_failed_operations_total",
    "Failed operations logged in audit",
    labels=["operation", "error_type"]
)

audit_buffer_size = Gauge(
    "audit_buffer_size",
    "Current size of audit event buffer"
)

audit_events_flushed_total = Counter(
    "audit_events_flushed_total",
    "Total audit events flushed to disk"
)

# Security Metrics
rate_limit_hits_total = Counter(
    "rate_limit_hits_total",
    "Total rate limit hits",
    labels=["identifier", "endpoint"]
)

rate_limit_blocked_total = Counter(
    "rate_limit_blocked_total",
    "Total requests blocked by rate limiting",
    labels=["identifier", "endpoint"]
)

api_key_validations_total = Counter(
    "api_key_validations_total",
    "Total API key validations",
    labels=["status"]  # success, invalid, expired, revoked
)

api_key_creations_total = Counter(
    "api_key_creations_total",
    "Total API keys created"
)

api_key_revocations_total = Counter(
    "api_key_revocations_total",
    "Total API keys revoked"
)

blocked_ips_total = Gauge(
    "blocked_ips_total",
    "Total number of blocked IPs"
)

active_api_keys_total = Gauge(
    "active_api_keys_total",
    "Total number of active API keys"
)


# =============================================================================
# METRICS REGISTRY
# =============================================================================

class MetricsRegistry:
    """Registry for all metrics."""
    
    _instance = None
    
    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
            cls._instance._metrics = {}
        return cls._instance
    
    def register(self, metric):
        """Register a metric."""
        self._metrics[metric.name] = metric
    
    def get(self, name: str):
        """Get a metric by name."""
        return self._metrics.get(name)
    
    def collect_all(self) -> Dict[str, list]:
        """Collect all metrics."""
        result = {}
        for name, metric in self._metrics.items():
            result[name] = {
                "description": metric.description,
                "type": type(metric).__name__.lower(),
                "values": metric.collect()
            }
        return result
    
    def export_prometheus(self) -> str:
        """Export metrics in Prometheus format."""
        lines = []
        
        for name, metric in self._metrics.items():
            lines.append(f"# HELP {name} {metric.description}")
            lines.append(f"# TYPE {name} {type(metric).__name__.lower()}")
            
            for value in metric.collect():
                label_str = ""
                if value.labels:
                    label_parts = [f'{k}="{v}"' for k, v in value.labels.items()]
                    label_str = "{" + ",".join(label_parts) + "}"
                
                lines.append(f"{name}{label_str} {value.value}")
        
        return "\n".join(lines)


# Initialize registry with all metrics
registry = MetricsRegistry()
registry.register(http_requests_total)
registry.register(http_request_duration_seconds)
registry.register(llm_requests_total)
registry.register(llm_request_duration_seconds)
registry.register(llm_tokens_total)
registry.register(analysis_total)
registry.register(analysis_duration_seconds)
registry.register(models_detected)
registry.register(lollapalooza_score)
registry.register(analysis_queue_size)
registry.register(decisions_total)
registry.register(decision_risk_score)
registry.register(model_effectiveness)
registry.register(signals_harvested_total)
registry.register(signal_priority)
registry.register(harvester_errors_total)
registry.register(knowledge_graph_nodes)
registry.register(knowledge_graph_edges)
registry.register(improvement_suggestions_pending)
registry.register(improvement_suggestions_completed)
registry.register(improvement_suggestions_generated)
registry.register(improvement_suggestions_by_priority)
registry.register(improvement_suggestions_by_category)
registry.register(failure_mode_coverage)
registry.register(failure_modes_per_model)
registry.register(decisions_recorded)
registry.register(decisions_with_outcomes)
registry.register(mental_models_total)
registry.register(improvement_suggestion_impact)

# Register audit metrics
registry.register(audit_events_total)
registry.register(audit_events_by_actor)
registry.register(audit_events_by_resource)
registry.register(audit_api_requests_total)
registry.register(audit_api_request_duration)
registry.register(audit_security_events_total)
registry.register(audit_failed_operations_total)
registry.register(audit_buffer_size)
registry.register(audit_events_flushed_total)

# Register security metrics
registry.register(rate_limit_hits_total)
registry.register(rate_limit_blocked_total)
registry.register(api_key_validations_total)
registry.register(api_key_creations_total)
registry.register(api_key_revocations_total)
registry.register(blocked_ips_total)
registry.register(active_api_keys_total)


# =============================================================================
# DECORATORS
# =============================================================================

def track_request(endpoint: str):
    """Decorator to track HTTP request metrics."""
    def decorator(func: Callable):
        @wraps(func)
        async def wrapper(*args, **kwargs):
            method = kwargs.get("method", "GET")
            
            with http_request_duration_seconds.time(method=method, endpoint=endpoint):
                try:
                    result = await func(*args, **kwargs)
                    http_requests_total.inc(method=method, endpoint=endpoint, status="200")
                    return result
                except Exception as e:
                    http_requests_total.inc(method=method, endpoint=endpoint, status="500")
                    raise
        
        return wrapper
    return decorator


def track_llm_request(backend: str, model: str):
    """Decorator to track LLM request metrics."""
    def decorator(func: Callable):
        @wraps(func)
        async def wrapper(*args, **kwargs):
            with llm_request_duration_seconds.time(backend=backend, model=model):
                try:
                    result = await func(*args, **kwargs)
                    llm_requests_total.inc(backend=backend, model=model, status="success")
                    
                    # Track tokens if available
                    if hasattr(result, "tokens_used"):
                        llm_tokens_total.inc(
                            result.tokens_used,
                            backend=backend,
                            model=model,
                            type="total"
                        )
                    
                    return result
                except Exception as e:
                    llm_requests_total.inc(backend=backend, model=model, status="error")
                    raise
        
        return wrapper
    return decorator


def track_analysis():
    """Decorator to track analysis metrics."""
    def decorator(func: Callable):
        @wraps(func)
        async def wrapper(*args, **kwargs):
            with analysis_duration_seconds.time(analysis_type="full"):
                try:
                    result = await func(*args, **kwargs)
                    analysis_total.inc(status="success")
                    
                    # Track detected models
                    if hasattr(result, "models"):
                        for model in result.models:
                            models_detected.inc(
                                model_id=str(model.get("id", "")),
                                model_name=model.get("name", ""),
                                category=model.get("category", "")
                            )
                    
                    # Track Lollapalooza score
                    if hasattr(result, "lollapalooza_score"):
                        lollapalooza_score.set(
                            result.lollapalooza_score,
                            document_id=str(getattr(result, "document_id", ""))
                        )
                    
                    return result
                except Exception as e:
                    analysis_total.inc(status="error")
                    raise
        
        return wrapper
    return decorator


# =============================================================================
# FASTAPI INTEGRATION
# =============================================================================

def create_metrics_endpoint():
    """Create FastAPI endpoint for Prometheus metrics."""
    from fastapi import APIRouter, Response
    
    router = APIRouter()
    
    @router.get("/metrics")
    async def metrics():
        """Prometheus metrics endpoint."""
        return Response(
            content=registry.export_prometheus(),
            media_type="text/plain"
        )
    
    return router
