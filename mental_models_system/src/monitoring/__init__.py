"""
Monitoring module for Mental Models System.

Provides Prometheus metrics, alerting, and observability.
"""

from .metrics import (
    # Metrics
    http_requests_total,
    http_request_duration_seconds,
    llm_requests_total,
    llm_request_duration_seconds,
    llm_tokens_total,
    analysis_total,
    analysis_duration_seconds,
    models_detected,
    lollapalooza_score,
    analysis_queue_size,
    decisions_total,
    decision_risk_score,
    model_effectiveness,
    signals_harvested_total,
    signal_priority,
    harvester_errors_total,
    knowledge_graph_nodes,
    knowledge_graph_edges,
    # Registry
    registry,
    MetricsRegistry,
    # Decorators
    track_request,
    track_llm_request,
    track_analysis,
    # FastAPI integration
    create_metrics_endpoint,
)

__all__ = [
    "http_requests_total",
    "http_request_duration_seconds",
    "llm_requests_total",
    "llm_request_duration_seconds",
    "llm_tokens_total",
    "analysis_total",
    "analysis_duration_seconds",
    "models_detected",
    "lollapalooza_score",
    "analysis_queue_size",
    "decisions_total",
    "decision_risk_score",
    "model_effectiveness",
    "signals_harvested_total",
    "signal_priority",
    "harvester_errors_total",
    "knowledge_graph_nodes",
    "knowledge_graph_edges",
    "registry",
    "MetricsRegistry",
    "track_request",
    "track_llm_request",
    "track_analysis",
    "create_metrics_endpoint",
]
