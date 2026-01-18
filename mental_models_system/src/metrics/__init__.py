"""
Comprehensive Metrics System for Mental Models.

Measures thousands of code quality, development, and runtime metrics
to better direct development and improvement cycles.

Categories:
- Code Quality: Complexity, maintainability, security, duplication, documentation
- Development: Git metrics, velocity, iteration speed, contributor analysis
- Runtime: Counters, gauges, histograms, timers, system metrics

Usage:
    from metrics import (
        analyze_project,
        collect_development_metrics,
        get_metrics_collector,
        start_metrics_collection,
    )
    
    # Analyze code quality
    report = analyze_project("/path/to/project")
    
    # Collect git/development metrics
    dev_metrics = collect_development_metrics("/path/to/repo")
    
    # Start runtime metrics collection
    collector = start_metrics_collection(interval=5.0)
"""

from .code_quality_metrics import (
    CodeQualityMetrics,
    ComplexityAnalyzer,
    HalsteadMetrics,
    MaintainabilityIndex,
    MetricCategory,
    MetricValue,
    CodeIssue,
    Severity,
    analyze_project,
    get_metric_summary,
)

from .development_metrics import (
    GitMetricsCollector,
    DevelopmentVelocityTracker,
    PRMetricsCollector,
    CommitMetrics,
    AuthorMetrics,
    FileChurnMetrics,
    VelocityPeriod,
    collect_development_metrics,
)

from .runtime_metrics import (
    RuntimeMetricsCollector,
    Counter,
    Gauge,
    Histogram,
    TimerMetric,
    MetricType,
    MetricsMiddleware,
    timed,
    counted,
    get_metrics_collector,
    start_metrics_collection,
    get_metrics_report,
    get_prometheus_metrics,
)

__all__ = [
    "CodeQualityMetrics",
    "ComplexityAnalyzer",
    "HalsteadMetrics",
    "MaintainabilityIndex",
    "MetricCategory",
    "MetricValue",
    "CodeIssue",
    "Severity",
    "analyze_project",
    "get_metric_summary",
    "GitMetricsCollector",
    "DevelopmentVelocityTracker",
    "PRMetricsCollector",
    "CommitMetrics",
    "AuthorMetrics",
    "FileChurnMetrics",
    "VelocityPeriod",
    "collect_development_metrics",
    "RuntimeMetricsCollector",
    "Counter",
    "Gauge",
    "Histogram",
    "TimerMetric",
    "MetricType",
    "MetricsMiddleware",
    "timed",
    "counted",
    "get_metrics_collector",
    "start_metrics_collection",
    "get_metrics_report",
    "get_prometheus_metrics",
]


METRIC_CATEGORIES = {
    "code_quality": {
        "complexity": [
            "cyclomatic_complexity",
            "cognitive_complexity",
            "nesting_depth",
            "branches",
            "loops",
            "conditions",
            "boolean_operators",
        ],
        "maintainability": [
            "maintainability_index",
            "halstead_volume",
            "halstead_difficulty",
            "halstead_effort",
            "lines_per_function",
            "parameters_per_function",
        ],
        "size": [
            "total_lines",
            "code_lines",
            "comment_lines",
            "blank_lines",
            "functions",
            "classes",
            "methods",
        ],
        "documentation": [
            "docstring_coverage",
            "comment_ratio",
            "readme_exists",
            "api_docs_coverage",
        ],
        "security": [
            "hardcoded_secrets",
            "sql_injection_risk",
            "eval_usage",
            "unsafe_deserialization",
        ],
        "duplication": [
            "duplicate_blocks",
            "duplicate_lines",
            "duplication_ratio",
        ],
        "coupling": [
            "afferent_coupling",
            "efferent_coupling",
            "instability",
            "abstractness",
        ],
        "testing": [
            "test_count",
            "assertion_count",
            "test_coverage",
            "mutation_score",
        ],
    },
    "development": {
        "velocity": [
            "commits_per_day",
            "lines_per_commit",
            "files_per_commit",
            "iteration_speed",
        ],
        "contributors": [
            "active_contributors",
            "bus_factor",
            "knowledge_distribution",
        ],
        "churn": [
            "file_churn_rate",
            "hotspot_count",
            "refactoring_frequency",
        ],
        "branches": [
            "active_branches",
            "stale_branches",
            "merge_frequency",
        ],
    },
    "runtime": {
        "performance": [
            "response_time_p50",
            "response_time_p95",
            "response_time_p99",
            "throughput",
        ],
        "reliability": [
            "error_rate",
            "success_rate",
            "availability",
            "mtbf",
        ],
        "resources": [
            "memory_usage",
            "cpu_usage",
            "disk_usage",
            "network_io",
        ],
        "capacity": [
            "active_connections",
            "queue_depth",
            "thread_count",
            "cache_hit_rate",
        ],
    },
}


def get_total_metric_count() -> int:
    count = 0
    for category in METRIC_CATEGORIES.values():
        for subcategory in category.values():
            count += len(subcategory)
    return count


def list_all_metrics() -> list:
    metrics = []
    for category, subcategories in METRIC_CATEGORIES.items():
        for subcategory, metric_names in subcategories.items():
            for name in metric_names:
                metrics.append({
                    "category": category,
                    "subcategory": subcategory,
                    "name": name,
                    "full_name": f"{category}.{subcategory}.{name}"
                })
    return metrics
