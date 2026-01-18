"""
Comprehensive Metrics Collection System

Implements metric-driven development by collecting and analyzing:
- Code quality metrics
- Performance metrics
- Usage patterns
- Test coverage
- Development velocity
- System health

"What gets measured gets improved." - Peter Drucker
"""

import json
import time
import logging
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, field, asdict
from enum import Enum
import subprocess

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class MetricCategory(Enum):
    """Categories of metrics."""
    CODE_QUALITY = "code_quality"
    PERFORMANCE = "performance"
    USAGE = "usage"
    TEST_COVERAGE = "test_coverage"
    DEVELOPMENT = "development"
    SYSTEM_HEALTH = "system_health"


@dataclass
class Metric:
    """A single metric measurement."""
    name: str
    value: float
    unit: str
    category: MetricCategory
    timestamp: datetime = field(default_factory=datetime.now)
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    def to_dict(self) -> Dict:
        return {
            "name": self.name,
            "value": self.value,
            "unit": self.unit,
            "category": self.category.value,
            "timestamp": self.timestamp.isoformat(),
            "metadata": self.metadata
        }


@dataclass
class MetricSnapshot:
    """A snapshot of all metrics at a point in time."""
    timestamp: datetime
    metrics: List[Metric]
    summary: Dict[str, Any] = field(default_factory=dict)
    
    def to_dict(self) -> Dict:
        return {
            "timestamp": self.timestamp.isoformat(),
            "metrics": [m.to_dict() for m in self.metrics],
            "summary": self.summary
        }


class MetricsCollector:
    """
    Collects and tracks comprehensive system metrics.
    
    Usage:
        collector = MetricsCollector()
        
        # Collect all metrics
        snapshot = collector.collect_all()
        
        # Get specific metrics
        code_metrics = collector.collect_code_quality_metrics()
        perf_metrics = collector.collect_performance_metrics()
        
        # Save metrics
        collector.save_snapshot(snapshot)
        
        # Get trends
        trends = collector.get_trends(days=30)
    """
    
    def __init__(self, project_root: Path = None, data_dir: Path = None):
        self.project_root = project_root or Path("/home/ubuntu/Ripple_Analytics/mental_models_system")
        self.data_dir = data_dir or (self.project_root / "data" / "metrics")
        self.data_dir.mkdir(parents=True, exist_ok=True)
        
        self.metrics_history: List[MetricSnapshot] = []
        self._load_history()
    
    def _load_history(self):
        """Load historical metrics."""
        history_file = self.data_dir / "metrics_history.json"
        if history_file.exists():
            try:
                with open(history_file) as f:
                    data = json.load(f)
                    # Load only recent history (last 1000 snapshots)
                    for item in data[-1000:]:
                        snapshot = MetricSnapshot(
                            timestamp=datetime.fromisoformat(item["timestamp"]),
                            metrics=[],
                            summary=item.get("summary", {})
                        )
                        self.metrics_history.append(snapshot)
            except Exception as e:
                logger.error(f"Error loading metrics history: {e}")
    
    def collect_all(self) -> MetricSnapshot:
        """Collect all metrics."""
        metrics = []
        
        # Collect from all categories
        metrics.extend(self.collect_code_quality_metrics())
        metrics.extend(self.collect_performance_metrics())
        metrics.extend(self.collect_test_coverage_metrics())
        metrics.extend(self.collect_development_metrics())
        metrics.extend(self.collect_system_health_metrics())
        
        # Create summary
        summary = self._create_summary(metrics)
        
        snapshot = MetricSnapshot(
            timestamp=datetime.now(),
            metrics=metrics,
            summary=summary
        )
        
        self.metrics_history.append(snapshot)
        self.save_snapshot(snapshot)
        
        return snapshot
    
    def collect_code_quality_metrics(self) -> List[Metric]:
        """Collect code quality metrics."""
        metrics = []
        
        # Lines of code
        loc = self._count_lines_of_code()
        metrics.append(Metric(
            name="lines_of_code",
            value=loc,
            unit="lines",
            category=MetricCategory.CODE_QUALITY
        ))
        
        # Number of files
        num_files = len(list((self.project_root / "src").rglob("*.py")))
        metrics.append(Metric(
            name="python_files",
            value=num_files,
            unit="files",
            category=MetricCategory.CODE_QUALITY
        ))
        
        # Average file size
        avg_file_size = loc / num_files if num_files > 0 else 0
        metrics.append(Metric(
            name="avg_file_size",
            value=avg_file_size,
            unit="lines",
            category=MetricCategory.CODE_QUALITY
        ))
        
        # Docstring coverage (approximate)
        docstring_coverage = self._estimate_docstring_coverage()
        metrics.append(Metric(
            name="docstring_coverage",
            value=docstring_coverage,
            unit="percent",
            category=MetricCategory.CODE_QUALITY
        ))
        
        return metrics
    
    def collect_performance_metrics(self) -> List[Metric]:
        """Collect performance metrics."""
        metrics = []
        
        # Test execution time
        test_time = self._measure_test_execution_time()
        if test_time:
            metrics.append(Metric(
                name="test_execution_time",
                value=test_time,
                unit="seconds",
                category=MetricCategory.PERFORMANCE
            ))
        
        # Import time
        import_time = self._measure_import_time()
        metrics.append(Metric(
            name="import_time",
            value=import_time,
            unit="seconds",
            category=MetricCategory.PERFORMANCE
        ))
        
        return metrics
    
    def collect_test_coverage_metrics(self) -> List[Metric]:
        """Collect test coverage metrics."""
        metrics = []
        
        # Number of tests
        num_tests = self._count_tests()
        metrics.append(Metric(
            name="total_tests",
            value=num_tests,
            unit="tests",
            category=MetricCategory.TEST_COVERAGE
        ))
        
        # Test files
        test_files = len(list((self.project_root / "tests").rglob("test_*.py")))
        metrics.append(Metric(
            name="test_files",
            value=test_files,
            unit="files",
            category=MetricCategory.TEST_COVERAGE
        ))
        
        # Tests per source file ratio
        src_files = len(list((self.project_root / "src").rglob("*.py")))
        test_ratio = num_tests / src_files if src_files > 0 else 0
        metrics.append(Metric(
            name="tests_per_source_file",
            value=test_ratio,
            unit="ratio",
            category=MetricCategory.TEST_COVERAGE
        ))
        
        return metrics
    
    def collect_development_metrics(self) -> List[Metric]:
        """Collect development velocity metrics."""
        metrics = []
        
        # Git commits (last 7 days)
        commits = self._count_recent_commits(days=7)
        metrics.append(Metric(
            name="commits_last_7_days",
            value=commits,
            unit="commits",
            category=MetricCategory.DEVELOPMENT
        ))
        
        # Files changed (last 7 days)
        files_changed = self._count_files_changed(days=7)
        metrics.append(Metric(
            name="files_changed_last_7_days",
            value=files_changed,
            unit="files",
            category=MetricCategory.DEVELOPMENT
        ))
        
        return metrics
    
    def collect_system_health_metrics(self) -> List[Metric]:
        """Collect system health metrics."""
        metrics = []
        
        # Test pass rate
        pass_rate = self._get_test_pass_rate()
        if pass_rate is not None:
            metrics.append(Metric(
                name="test_pass_rate",
                value=pass_rate,
                unit="percent",
                category=MetricCategory.SYSTEM_HEALTH
            ))
        
        # Number of connectors
        num_connectors = self._count_connectors()
        metrics.append(Metric(
            name="total_connectors",
            value=num_connectors,
            unit="connectors",
            category=MetricCategory.SYSTEM_HEALTH
        ))
        
        # Mental models count
        num_models = self._count_mental_models()
        metrics.append(Metric(
            name="mental_models",
            value=num_models,
            unit="models",
            category=MetricCategory.SYSTEM_HEALTH
        ))
        
        return metrics
    
    def _count_lines_of_code(self) -> int:
        """Count total lines of Python code."""
        total = 0
        for py_file in (self.project_root / "src").rglob("*.py"):
            try:
                with open(py_file) as f:
                    total += len(f.readlines())
            except:
                pass
        return total
    
    def _estimate_docstring_coverage(self) -> float:
        """Estimate docstring coverage."""
        total_functions = 0
        documented_functions = 0
        
        for py_file in (self.project_root / "src").rglob("*.py"):
            try:
                with open(py_file) as f:
                    lines = f.readlines()
                    in_function = False
                    has_docstring = False
                    
                    for line in lines:
                        stripped = line.strip()
                        if stripped.startswith("def ") or stripped.startswith("async def "):
                            if in_function:
                                total_functions += 1
                                if has_docstring:
                                    documented_functions += 1
                            in_function = True
                            has_docstring = False
                        elif in_function and (stripped.startswith('"""') or stripped.startswith("'''")):
                            has_docstring = True
                    
                    if in_function:
                        total_functions += 1
                        if has_docstring:
                            documented_functions += 1
            except:
                pass
        
        return (documented_functions / total_functions * 100) if total_functions > 0 else 0
    
    def _measure_test_execution_time(self) -> Optional[float]:
        """Measure test execution time."""
        try:
            start = time.time()
            result = subprocess.run(
                ["python3.11", "-m", "pytest", "tests/", "-q", "--tb=no"],
                cwd=self.project_root,
                capture_output=True,
                timeout=300
            )
            return time.time() - start
        except:
            return None
    
    def _measure_import_time(self) -> float:
        """Measure time to import main modules."""
        start = time.time()
        try:
            import sys
            sys.path.insert(0, str(self.project_root / "src"))
            import connectors.base
            import analysis.mental_model_analyzer
        except:
            pass
        return time.time() - start
    
    def _count_tests(self) -> int:
        """Count total number of tests."""
        try:
            result = subprocess.run(
                ["python3.11", "-m", "pytest", "--collect-only", "-q"],
                cwd=self.project_root,
                capture_output=True,
                text=True,
                timeout=30
            )
            # Parse output to count tests
            output = result.stdout
            for line in output.split('\n'):
                if 'test' in line.lower() and 'selected' in line.lower():
                    parts = line.split()
                    for part in parts:
                        if part.isdigit():
                            return int(part)
        except:
            pass
        return 0
    
    def _count_recent_commits(self, days: int = 7) -> int:
        """Count commits in the last N days."""
        try:
            result = subprocess.run(
                ["git", "log", f"--since={days} days ago", "--oneline"],
                cwd=self.project_root,
                capture_output=True,
                text=True,
                timeout=10
            )
            return len(result.stdout.strip().split('\n')) if result.stdout.strip() else 0
        except:
            return 0
    
    def _count_files_changed(self, days: int = 7) -> int:
        """Count files changed in the last N days."""
        try:
            result = subprocess.run(
                ["git", "log", f"--since={days} days ago", "--name-only", "--pretty=format:"],
                cwd=self.project_root,
                capture_output=True,
                text=True,
                timeout=10
            )
            files = set(line for line in result.stdout.strip().split('\n') if line)
            return len(files)
        except:
            return 0
    
    def _get_test_pass_rate(self) -> Optional[float]:
        """Get test pass rate."""
        try:
            result = subprocess.run(
                ["python3.11", "-m", "pytest", "tests/", "-q", "--tb=no"],
                cwd=self.project_root,
                capture_output=True,
                text=True,
                timeout=300
            )
            output = result.stdout
            # Parse output like "333 passed, 9 failed"
            passed = 0
            failed = 0
            for line in output.split('\n'):
                if 'passed' in line:
                    parts = line.split()
                    for i, part in enumerate(parts):
                        if 'passed' in part and i > 0:
                            try:
                                passed = int(parts[i-1])
                            except:
                                pass
                        if 'failed' in part and i > 0:
                            try:
                                failed = int(parts[i-1])
                            except:
                                pass
            
            total = passed + failed
            return (passed / total * 100) if total > 0 else None
        except:
            return None
    
    def _count_connectors(self) -> int:
        """Count number of connectors."""
        connector_files = list((self.project_root / "src" / "connectors").glob("*.py"))
        # Exclude base.py and __init__.py
        return len([f for f in connector_files if f.name not in ["base.py", "__init__.py"]])
    
    def _count_mental_models(self) -> int:
        """Count number of mental models."""
        try:
            models_file = self.project_root / "data" / "raw" / "mental_models_complete.json"
            if models_file.exists():
                with open(models_file) as f:
                    data = json.load(f)
                    return len(data.get("mental_models", []))
        except:
            pass
        return 0
    
    def _create_summary(self, metrics: List[Metric]) -> Dict[str, Any]:
        """Create summary of metrics."""
        summary = {
            "total_metrics": len(metrics),
            "by_category": {}
        }
        
        for category in MetricCategory:
            category_metrics = [m for m in metrics if m.category == category]
            summary["by_category"][category.value] = {
                "count": len(category_metrics),
                "metrics": {m.name: m.value for m in category_metrics}
            }
        
        return summary
    
    def save_snapshot(self, snapshot: MetricSnapshot):
        """Save metrics snapshot."""
        # Save individual snapshot
        snapshot_file = self.data_dir / f"snapshot_{snapshot.timestamp.strftime('%Y%m%d_%H%M%S')}.json"
        with open(snapshot_file, 'w') as f:
            json.dump(snapshot.to_dict(), f, indent=2)
        
        # Update history file
        history_file = self.data_dir / "metrics_history.json"
        history = []
        if history_file.exists():
            with open(history_file) as f:
                history = json.load(f)
        
        history.append(snapshot.to_dict())
        
        # Keep only last 1000 snapshots
        history = history[-1000:]
        
        with open(history_file, 'w') as f:
            json.dump(history, f, indent=2)
    
    def get_trends(self, days: int = 30) -> Dict[str, Any]:
        """Get metric trends over time."""
        cutoff = datetime.now().timestamp() - (days * 24 * 60 * 60)
        recent_snapshots = [
            s for s in self.metrics_history
            if s.timestamp.timestamp() > cutoff
        ]
        
        if not recent_snapshots:
            return {"error": "No historical data available"}
        
        trends = {}
        
        # Calculate trends for each metric
        metric_names = set()
        for snapshot in recent_snapshots:
            for metric in snapshot.metrics:
                metric_names.add(metric.name)
        
        for name in metric_names:
            values = []
            timestamps = []
            for snapshot in recent_snapshots:
                for metric in snapshot.metrics:
                    if metric.name == name:
                        values.append(metric.value)
                        timestamps.append(snapshot.timestamp)
            
            if len(values) >= 2:
                change = values[-1] - values[0]
                percent_change = (change / values[0] * 100) if values[0] != 0 else 0
                trends[name] = {
                    "current": values[-1],
                    "start": values[0],
                    "change": change,
                    "percent_change": percent_change,
                    "direction": "up" if change > 0 else "down" if change < 0 else "stable"
                }
        
        return trends
    
    def generate_report(self) -> str:
        """Generate a human-readable metrics report."""
        snapshot = self.collect_all()
        
        report = []
        report.append("=" * 80)
        report.append("MENTAL MODELS SYSTEM - METRICS REPORT")
        report.append(f"Generated: {snapshot.timestamp.strftime('%Y-%m-%d %H:%M:%S')}")
        report.append("=" * 80)
        report.append("")
        
        # Group by category
        for category in MetricCategory:
            category_metrics = [m for m in snapshot.metrics if m.category == category]
            if category_metrics:
                report.append(f"\n{category.value.upper().replace('_', ' ')}")
                report.append("-" * 40)
                for metric in category_metrics:
                    report.append(f"  {metric.name:30s}: {metric.value:10.2f} {metric.unit}")
        
        report.append("\n" + "=" * 80)
        
        return "\n".join(report)


if __name__ == "__main__":
    collector = MetricsCollector()
    print(collector.generate_report())
