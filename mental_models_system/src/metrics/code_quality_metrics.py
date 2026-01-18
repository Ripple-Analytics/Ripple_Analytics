import ast
import os
import re
import json
import hashlib
import subprocess
from dataclasses import dataclass, field
from typing import Dict, Any, List, Optional, Set, Tuple
from datetime import datetime
from pathlib import Path
from enum import Enum
from collections import defaultdict
import logging

logger = logging.getLogger(__name__)


class MetricCategory(Enum):
    COMPLEXITY = "complexity"
    MAINTAINABILITY = "maintainability"
    RELIABILITY = "reliability"
    SECURITY = "security"
    PERFORMANCE = "performance"
    DOCUMENTATION = "documentation"
    TESTING = "testing"
    ARCHITECTURE = "architecture"
    DEPENDENCIES = "dependencies"
    CODE_STYLE = "code_style"
    DUPLICATION = "duplication"
    SIZE = "size"
    COUPLING = "coupling"
    COHESION = "cohesion"
    TECHNICAL_DEBT = "technical_debt"


class Severity(Enum):
    INFO = "info"
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    CRITICAL = "critical"


@dataclass
class MetricValue:
    name: str
    value: float
    category: MetricCategory
    unit: str = ""
    threshold_warning: Optional[float] = None
    threshold_critical: Optional[float] = None
    description: str = ""
    file_path: Optional[str] = None
    line_number: Optional[int] = None
    timestamp: datetime = field(default_factory=datetime.now)
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "value": self.value,
            "category": self.category.value,
            "unit": self.unit,
            "threshold_warning": self.threshold_warning,
            "threshold_critical": self.threshold_critical,
            "description": self.description,
            "file_path": self.file_path,
            "line_number": self.line_number,
            "timestamp": self.timestamp.isoformat(),
            "status": self._get_status()
        }
    
    def _get_status(self) -> str:
        if self.threshold_critical and self.value >= self.threshold_critical:
            return "critical"
        if self.threshold_warning and self.value >= self.threshold_warning:
            return "warning"
        return "ok"


@dataclass
class CodeIssue:
    title: str
    description: str
    severity: Severity
    category: MetricCategory
    file_path: str
    line_number: int
    column: int = 0
    suggestion: str = ""
    effort_minutes: int = 5
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "title": self.title,
            "description": self.description,
            "severity": self.severity.value,
            "category": self.category.value,
            "file_path": self.file_path,
            "line_number": self.line_number,
            "column": self.column,
            "suggestion": self.suggestion,
            "effort_minutes": self.effort_minutes
        }


class ComplexityAnalyzer(ast.NodeVisitor):
    def __init__(self):
        self.complexity = 1
        self.nesting_depth = 0
        self.max_nesting = 0
        self.branches = 0
        self.loops = 0
        self.conditions = 0
        self.boolean_operators = 0
        self.function_calls = 0
        self.assignments = 0
        self.comparisons = 0
        self.exceptions = 0
        self.returns = 0
        self.yields = 0
        self.lambdas = 0
        self.comprehensions = 0
        self.decorators = 0
        self.assertions = 0
        self.global_vars = 0
        self.nonlocal_vars = 0
        
    def visit_If(self, node):
        self.complexity += 1
        self.branches += 1
        self.conditions += 1
        self.nesting_depth += 1
        self.max_nesting = max(self.max_nesting, self.nesting_depth)
        self.generic_visit(node)
        self.nesting_depth -= 1
        
    def visit_For(self, node):
        self.complexity += 1
        self.loops += 1
        self.nesting_depth += 1
        self.max_nesting = max(self.max_nesting, self.nesting_depth)
        self.generic_visit(node)
        self.nesting_depth -= 1
        
    def visit_While(self, node):
        self.complexity += 1
        self.loops += 1
        self.nesting_depth += 1
        self.max_nesting = max(self.max_nesting, self.nesting_depth)
        self.generic_visit(node)
        self.nesting_depth -= 1
        
    def visit_Try(self, node):
        self.complexity += len(node.handlers)
        self.exceptions += 1
        self.generic_visit(node)
        
    def visit_ExceptHandler(self, node):
        self.complexity += 1
        self.generic_visit(node)
        
    def visit_With(self, node):
        self.complexity += 1
        self.generic_visit(node)
        
    def visit_BoolOp(self, node):
        self.complexity += len(node.values) - 1
        self.boolean_operators += len(node.values) - 1
        self.generic_visit(node)
        
    def visit_comprehension(self, node):
        self.complexity += 1
        self.comprehensions += 1
        self.generic_visit(node)
        
    def visit_IfExp(self, node):
        self.complexity += 1
        self.conditions += 1
        self.generic_visit(node)
        
    def visit_Call(self, node):
        self.function_calls += 1
        self.generic_visit(node)
        
    def visit_Assign(self, node):
        self.assignments += 1
        self.generic_visit(node)
        
    def visit_AugAssign(self, node):
        self.assignments += 1
        self.generic_visit(node)
        
    def visit_Compare(self, node):
        self.comparisons += len(node.ops)
        self.generic_visit(node)
        
    def visit_Return(self, node):
        self.returns += 1
        self.generic_visit(node)
        
    def visit_Yield(self, node):
        self.yields += 1
        self.generic_visit(node)
        
    def visit_YieldFrom(self, node):
        self.yields += 1
        self.generic_visit(node)
        
    def visit_Lambda(self, node):
        self.lambdas += 1
        self.generic_visit(node)
        
    def visit_FunctionDef(self, node):
        self.decorators += len(node.decorator_list)
        self.generic_visit(node)
        
    def visit_AsyncFunctionDef(self, node):
        self.decorators += len(node.decorator_list)
        self.generic_visit(node)
        
    def visit_ClassDef(self, node):
        self.decorators += len(node.decorator_list)
        self.generic_visit(node)
        
    def visit_Assert(self, node):
        self.assertions += 1
        self.generic_visit(node)
        
    def visit_Global(self, node):
        self.global_vars += len(node.names)
        self.generic_visit(node)
        
    def visit_Nonlocal(self, node):
        self.nonlocal_vars += len(node.names)
        self.generic_visit(node)


class HalsteadMetrics:
    def __init__(self):
        self.operators: Set[str] = set()
        self.operands: Set[str] = set()
        self.operator_count = 0
        self.operand_count = 0
        
    def analyze(self, tree: ast.AST):
        for node in ast.walk(tree):
            if isinstance(node, ast.BinOp):
                self.operators.add(type(node.op).__name__)
                self.operator_count += 1
            elif isinstance(node, ast.UnaryOp):
                self.operators.add(type(node.op).__name__)
                self.operator_count += 1
            elif isinstance(node, ast.Compare):
                for op in node.ops:
                    self.operators.add(type(op).__name__)
                    self.operator_count += 1
            elif isinstance(node, ast.BoolOp):
                self.operators.add(type(node.op).__name__)
                self.operator_count += 1
            elif isinstance(node, ast.Name):
                self.operands.add(node.id)
                self.operand_count += 1
            elif isinstance(node, ast.Constant):
                self.operands.add(str(node.value))
                self.operand_count += 1
    
    @property
    def n1(self) -> int:
        return len(self.operators)
    
    @property
    def n2(self) -> int:
        return len(self.operands)
    
    @property
    def N1(self) -> int:
        return self.operator_count
    
    @property
    def N2(self) -> int:
        return self.operand_count
    
    @property
    def vocabulary(self) -> int:
        return self.n1 + self.n2
    
    @property
    def length(self) -> int:
        return self.N1 + self.N2
    
    @property
    def calculated_length(self) -> float:
        import math
        if self.n1 == 0 or self.n2 == 0:
            return 0
        return self.n1 * math.log2(self.n1) + self.n2 * math.log2(self.n2)
    
    @property
    def volume(self) -> float:
        import math
        if self.vocabulary == 0:
            return 0
        return self.length * math.log2(self.vocabulary)
    
    @property
    def difficulty(self) -> float:
        if self.n2 == 0:
            return 0
        return (self.n1 / 2) * (self.N2 / self.n2)
    
    @property
    def effort(self) -> float:
        return self.difficulty * self.volume
    
    @property
    def time_to_program(self) -> float:
        return self.effort / 18
    
    @property
    def bugs_delivered(self) -> float:
        return self.volume / 3000


class MaintainabilityIndex:
    @staticmethod
    def calculate(
        halstead_volume: float,
        cyclomatic_complexity: float,
        lines_of_code: int,
        comment_percentage: float = 0
    ) -> float:
        import math
        
        if lines_of_code == 0:
            return 100
        
        mi = 171 - 5.2 * math.log(halstead_volume + 1) - 0.23 * cyclomatic_complexity - 16.2 * math.log(lines_of_code)
        
        if comment_percentage > 0:
            mi += 50 * math.sin(math.sqrt(2.4 * comment_percentage))
        
        return max(0, min(100, mi))


class CodeQualityMetrics:
    def __init__(self, project_root: str):
        self.project_root = Path(project_root)
        self.metrics: List[MetricValue] = []
        self.issues: List[CodeIssue] = []
        self.file_metrics: Dict[str, Dict[str, Any]] = {}
        self.function_metrics: Dict[str, Dict[str, Any]] = {}
        self.class_metrics: Dict[str, Dict[str, Any]] = {}
        self.module_metrics: Dict[str, Dict[str, Any]] = {}
        self.project_metrics: Dict[str, Any] = {}
        
    def analyze_project(self) -> Dict[str, Any]:
        python_files = list(self.project_root.rglob("*.py"))
        
        total_lines = 0
        total_code_lines = 0
        total_comment_lines = 0
        total_blank_lines = 0
        total_docstring_lines = 0
        total_functions = 0
        total_classes = 0
        total_methods = 0
        total_imports = 0
        total_complexity = 0
        all_dependencies: Set[str] = set()
        
        for py_file in python_files:
            try:
                file_metrics = self._analyze_file(py_file)
                self.file_metrics[str(py_file)] = file_metrics
                
                total_lines += file_metrics.get("total_lines", 0)
                total_code_lines += file_metrics.get("code_lines", 0)
                total_comment_lines += file_metrics.get("comment_lines", 0)
                total_blank_lines += file_metrics.get("blank_lines", 0)
                total_docstring_lines += file_metrics.get("docstring_lines", 0)
                total_functions += file_metrics.get("function_count", 0)
                total_classes += file_metrics.get("class_count", 0)
                total_methods += file_metrics.get("method_count", 0)
                total_imports += file_metrics.get("import_count", 0)
                total_complexity += file_metrics.get("cyclomatic_complexity", 0)
                all_dependencies.update(file_metrics.get("dependencies", []))
                
            except Exception as e:
                logger.warning(f"Error analyzing {py_file}: {e}")
        
        self.project_metrics = {
            "total_files": len(python_files),
            "total_lines": total_lines,
            "total_code_lines": total_code_lines,
            "total_comment_lines": total_comment_lines,
            "total_blank_lines": total_blank_lines,
            "total_docstring_lines": total_docstring_lines,
            "total_functions": total_functions,
            "total_classes": total_classes,
            "total_methods": total_methods,
            "total_imports": total_imports,
            "total_complexity": total_complexity,
            "average_complexity": total_complexity / max(total_functions, 1),
            "comment_ratio": total_comment_lines / max(total_code_lines, 1),
            "docstring_ratio": total_docstring_lines / max(total_code_lines, 1),
            "dependency_count": len(all_dependencies),
            "dependencies": list(all_dependencies),
            "lines_per_file": total_lines / max(len(python_files), 1),
            "functions_per_file": total_functions / max(len(python_files), 1),
            "classes_per_file": total_classes / max(len(python_files), 1),
        }
        
        self._add_project_metrics()
        self._analyze_architecture()
        self._analyze_duplication()
        self._analyze_security()
        self._analyze_performance_patterns()
        self._analyze_testing()
        self._analyze_documentation()
        self._analyze_technical_debt()
        
        return self.get_full_report()
    
    def _analyze_file(self, file_path: Path) -> Dict[str, Any]:
        with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
            content = f.read()
            lines = content.split("\n")
        
        try:
            tree = ast.parse(content)
        except SyntaxError:
            return {"error": "syntax_error", "total_lines": len(lines)}
        
        total_lines = len(lines)
        blank_lines = sum(1 for line in lines if not line.strip())
        comment_lines = sum(1 for line in lines if line.strip().startswith("#"))
        
        docstring_lines = 0
        for node in ast.walk(tree):
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef, ast.Module)):
                docstring = ast.get_docstring(node)
                if docstring:
                    docstring_lines += len(docstring.split("\n"))
        
        code_lines = total_lines - blank_lines - comment_lines
        
        functions = [n for n in ast.walk(tree) if isinstance(n, (ast.FunctionDef, ast.AsyncFunctionDef))]
        classes = [n for n in ast.walk(tree) if isinstance(n, ast.ClassDef)]
        
        methods = []
        for cls in classes:
            for item in cls.body:
                if isinstance(item, (ast.FunctionDef, ast.AsyncFunctionDef)):
                    methods.append(item)
        
        imports = []
        for node in ast.walk(tree):
            if isinstance(node, ast.Import):
                for alias in node.names:
                    imports.append(alias.name.split(".")[0])
            elif isinstance(node, ast.ImportFrom):
                if node.module:
                    imports.append(node.module.split(".")[0])
        
        complexity_analyzer = ComplexityAnalyzer()
        complexity_analyzer.visit(tree)
        
        halstead = HalsteadMetrics()
        halstead.analyze(tree)
        
        mi = MaintainabilityIndex.calculate(
            halstead.volume,
            complexity_analyzer.complexity,
            code_lines,
            (comment_lines + docstring_lines) / max(code_lines, 1) * 100
        )
        
        function_metrics = []
        for func in functions:
            func_analyzer = ComplexityAnalyzer()
            func_analyzer.visit(func)
            
            func_lines = func.end_lineno - func.lineno + 1 if hasattr(func, "end_lineno") else 0
            param_count = len(func.args.args) + len(func.args.posonlyargs) + len(func.args.kwonlyargs)
            if func.args.vararg:
                param_count += 1
            if func.args.kwarg:
                param_count += 1
            
            func_metric = {
                "name": func.name,
                "line": func.lineno,
                "lines": func_lines,
                "complexity": func_analyzer.complexity,
                "parameters": param_count,
                "returns": func_analyzer.returns,
                "branches": func_analyzer.branches,
                "loops": func_analyzer.loops,
                "nesting_depth": func_analyzer.max_nesting,
                "function_calls": func_analyzer.function_calls,
                "has_docstring": ast.get_docstring(func) is not None,
            }
            function_metrics.append(func_metric)
            
            self.function_metrics[f"{file_path}:{func.name}"] = func_metric
            
            if func_analyzer.complexity > 10:
                self.issues.append(CodeIssue(
                    title=f"High complexity in {func.name}",
                    description=f"Function has cyclomatic complexity of {func_analyzer.complexity}",
                    severity=Severity.HIGH if func_analyzer.complexity > 20 else Severity.MEDIUM,
                    category=MetricCategory.COMPLEXITY,
                    file_path=str(file_path),
                    line_number=func.lineno,
                    suggestion="Consider breaking this function into smaller functions",
                    effort_minutes=30
                ))
            
            if func_lines > 50:
                self.issues.append(CodeIssue(
                    title=f"Long function: {func.name}",
                    description=f"Function has {func_lines} lines",
                    severity=Severity.MEDIUM,
                    category=MetricCategory.MAINTAINABILITY,
                    file_path=str(file_path),
                    line_number=func.lineno,
                    suggestion="Consider breaking this function into smaller functions",
                    effort_minutes=20
                ))
            
            if param_count > 5:
                self.issues.append(CodeIssue(
                    title=f"Too many parameters in {func.name}",
                    description=f"Function has {param_count} parameters",
                    severity=Severity.LOW,
                    category=MetricCategory.MAINTAINABILITY,
                    file_path=str(file_path),
                    line_number=func.lineno,
                    suggestion="Consider using a configuration object or dataclass",
                    effort_minutes=15
                ))
        
        class_metrics = []
        for cls in classes:
            cls_methods = [n for n in cls.body if isinstance(n, (ast.FunctionDef, ast.AsyncFunctionDef))]
            cls_attrs = [n for n in cls.body if isinstance(n, ast.Assign)]
            
            cls_metric = {
                "name": cls.name,
                "line": cls.lineno,
                "method_count": len(cls_methods),
                "attribute_count": len(cls_attrs),
                "has_docstring": ast.get_docstring(cls) is not None,
                "base_classes": len(cls.bases),
                "decorators": len(cls.decorator_list),
            }
            class_metrics.append(cls_metric)
            
            self.class_metrics[f"{file_path}:{cls.name}"] = cls_metric
            
            if len(cls_methods) > 20:
                self.issues.append(CodeIssue(
                    title=f"Large class: {cls.name}",
                    description=f"Class has {len(cls_methods)} methods",
                    severity=Severity.MEDIUM,
                    category=MetricCategory.MAINTAINABILITY,
                    file_path=str(file_path),
                    line_number=cls.lineno,
                    suggestion="Consider splitting into smaller classes",
                    effort_minutes=60
                ))
        
        return {
            "total_lines": total_lines,
            "code_lines": code_lines,
            "blank_lines": blank_lines,
            "comment_lines": comment_lines,
            "docstring_lines": docstring_lines,
            "function_count": len(functions),
            "class_count": len(classes),
            "method_count": len(methods),
            "import_count": len(imports),
            "dependencies": list(set(imports)),
            "cyclomatic_complexity": complexity_analyzer.complexity,
            "max_nesting_depth": complexity_analyzer.max_nesting,
            "halstead_volume": halstead.volume,
            "halstead_difficulty": halstead.difficulty,
            "halstead_effort": halstead.effort,
            "halstead_bugs": halstead.bugs_delivered,
            "maintainability_index": mi,
            "functions": function_metrics,
            "classes": class_metrics,
            "branches": complexity_analyzer.branches,
            "loops": complexity_analyzer.loops,
            "conditions": complexity_analyzer.conditions,
            "boolean_operators": complexity_analyzer.boolean_operators,
            "function_calls": complexity_analyzer.function_calls,
            "assignments": complexity_analyzer.assignments,
            "comparisons": complexity_analyzer.comparisons,
            "exceptions": complexity_analyzer.exceptions,
            "returns": complexity_analyzer.returns,
            "yields": complexity_analyzer.yields,
            "lambdas": complexity_analyzer.lambdas,
            "comprehensions": complexity_analyzer.comprehensions,
            "decorators": complexity_analyzer.decorators,
            "assertions": complexity_analyzer.assertions,
            "global_vars": complexity_analyzer.global_vars,
            "nonlocal_vars": complexity_analyzer.nonlocal_vars,
        }
    
    def _add_project_metrics(self):
        self.metrics.append(MetricValue(
            name="total_lines_of_code",
            value=self.project_metrics["total_lines"],
            category=MetricCategory.SIZE,
            unit="lines",
            description="Total lines of code in the project"
        ))
        
        self.metrics.append(MetricValue(
            name="code_lines",
            value=self.project_metrics["total_code_lines"],
            category=MetricCategory.SIZE,
            unit="lines",
            description="Lines of actual code (excluding blanks and comments)"
        ))
        
        self.metrics.append(MetricValue(
            name="comment_lines",
            value=self.project_metrics["total_comment_lines"],
            category=MetricCategory.DOCUMENTATION,
            unit="lines",
            description="Lines of comments"
        ))
        
        self.metrics.append(MetricValue(
            name="comment_ratio",
            value=self.project_metrics["comment_ratio"] * 100,
            category=MetricCategory.DOCUMENTATION,
            unit="%",
            threshold_warning=5,
            threshold_critical=2,
            description="Ratio of comments to code"
        ))
        
        self.metrics.append(MetricValue(
            name="total_functions",
            value=self.project_metrics["total_functions"],
            category=MetricCategory.SIZE,
            unit="functions",
            description="Total number of functions"
        ))
        
        self.metrics.append(MetricValue(
            name="total_classes",
            value=self.project_metrics["total_classes"],
            category=MetricCategory.SIZE,
            unit="classes",
            description="Total number of classes"
        ))
        
        self.metrics.append(MetricValue(
            name="average_complexity",
            value=self.project_metrics["average_complexity"],
            category=MetricCategory.COMPLEXITY,
            unit="",
            threshold_warning=10,
            threshold_critical=20,
            description="Average cyclomatic complexity per function"
        ))
        
        self.metrics.append(MetricValue(
            name="total_complexity",
            value=self.project_metrics["total_complexity"],
            category=MetricCategory.COMPLEXITY,
            unit="",
            description="Total cyclomatic complexity"
        ))
        
        self.metrics.append(MetricValue(
            name="dependency_count",
            value=self.project_metrics["dependency_count"],
            category=MetricCategory.DEPENDENCIES,
            unit="packages",
            threshold_warning=50,
            threshold_critical=100,
            description="Number of external dependencies"
        ))
        
        self.metrics.append(MetricValue(
            name="lines_per_file",
            value=self.project_metrics["lines_per_file"],
            category=MetricCategory.SIZE,
            unit="lines",
            threshold_warning=500,
            threshold_critical=1000,
            description="Average lines per file"
        ))
        
        self.metrics.append(MetricValue(
            name="functions_per_file",
            value=self.project_metrics["functions_per_file"],
            category=MetricCategory.SIZE,
            unit="functions",
            threshold_warning=20,
            threshold_critical=50,
            description="Average functions per file"
        ))
    
    def _analyze_architecture(self):
        import_graph: Dict[str, Set[str]] = defaultdict(set)
        
        for file_path, metrics in self.file_metrics.items():
            if "dependencies" in metrics:
                module_name = Path(file_path).stem
                for dep in metrics["dependencies"]:
                    import_graph[module_name].add(dep)
        
        afferent_coupling: Dict[str, int] = defaultdict(int)
        efferent_coupling: Dict[str, int] = defaultdict(int)
        
        for module, deps in import_graph.items():
            efferent_coupling[module] = len(deps)
            for dep in deps:
                afferent_coupling[dep] += 1
        
        for module in import_graph:
            ca = afferent_coupling[module]
            ce = efferent_coupling[module]
            
            self.metrics.append(MetricValue(
                name=f"afferent_coupling_{module}",
                value=ca,
                category=MetricCategory.COUPLING,
                description=f"Number of modules that depend on {module}"
            ))
            
            self.metrics.append(MetricValue(
                name=f"efferent_coupling_{module}",
                value=ce,
                category=MetricCategory.COUPLING,
                description=f"Number of modules that {module} depends on"
            ))
            
            if ca + ce > 0:
                instability = ce / (ca + ce)
                self.metrics.append(MetricValue(
                    name=f"instability_{module}",
                    value=instability,
                    category=MetricCategory.ARCHITECTURE,
                    threshold_warning=0.8,
                    threshold_critical=0.95,
                    description=f"Instability metric for {module} (0=stable, 1=unstable)"
                ))
        
        total_ca = sum(afferent_coupling.values())
        total_ce = sum(efferent_coupling.values())
        
        self.metrics.append(MetricValue(
            name="total_afferent_coupling",
            value=total_ca,
            category=MetricCategory.COUPLING,
            description="Total afferent coupling across all modules"
        ))
        
        self.metrics.append(MetricValue(
            name="total_efferent_coupling",
            value=total_ce,
            category=MetricCategory.COUPLING,
            description="Total efferent coupling across all modules"
        ))
    
    def _analyze_duplication(self):
        code_hashes: Dict[str, List[Tuple[str, int]]] = defaultdict(list)
        
        for file_path, metrics in self.file_metrics.items():
            if "error" in metrics:
                continue
            
            try:
                with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
                    lines = f.readlines()
                
                window_size = 6
                for i in range(len(lines) - window_size + 1):
                    window = "".join(lines[i:i + window_size]).strip()
                    if len(window) > 50:
                        hash_val = hashlib.md5(window.encode()).hexdigest()
                        code_hashes[hash_val].append((file_path, i + 1))
            except Exception:
                pass
        
        duplicate_blocks = 0
        duplicate_lines = 0
        
        for hash_val, locations in code_hashes.items():
            if len(locations) > 1:
                duplicate_blocks += 1
                duplicate_lines += len(locations) * 6
                
                if len(locations) <= 5:
                    for file_path, line in locations:
                        self.issues.append(CodeIssue(
                            title="Duplicate code block",
                            description=f"Found {len(locations)} copies of this code block",
                            severity=Severity.LOW,
                            category=MetricCategory.DUPLICATION,
                            file_path=file_path,
                            line_number=line,
                            suggestion="Consider extracting to a shared function",
                            effort_minutes=15
                        ))
        
        self.metrics.append(MetricValue(
            name="duplicate_blocks",
            value=duplicate_blocks,
            category=MetricCategory.DUPLICATION,
            unit="blocks",
            threshold_warning=10,
            threshold_critical=50,
            description="Number of duplicate code blocks"
        ))
        
        self.metrics.append(MetricValue(
            name="duplicate_lines",
            value=duplicate_lines,
            category=MetricCategory.DUPLICATION,
            unit="lines",
            description="Estimated lines of duplicate code"
        ))
        
        total_lines = self.project_metrics.get("total_code_lines", 1)
        duplication_ratio = duplicate_lines / max(total_lines, 1) * 100
        
        self.metrics.append(MetricValue(
            name="duplication_ratio",
            value=duplication_ratio,
            category=MetricCategory.DUPLICATION,
            unit="%",
            threshold_warning=5,
            threshold_critical=15,
            description="Percentage of duplicate code"
        ))
    
    def _analyze_security(self):
        security_patterns = [
            (r"eval\s*\(", "Use of eval()", Severity.CRITICAL),
            (r"exec\s*\(", "Use of exec()", Severity.CRITICAL),
            (r"__import__\s*\(", "Dynamic import", Severity.HIGH),
            (r"subprocess\.call\s*\(.*shell\s*=\s*True", "Shell injection risk", Severity.CRITICAL),
            (r"os\.system\s*\(", "Use of os.system()", Severity.HIGH),
            (r"pickle\.load", "Unsafe pickle deserialization", Severity.HIGH),
            (r"yaml\.load\s*\([^,]+\)", "Unsafe YAML load", Severity.HIGH),
            (r"password\s*=\s*['\"][^'\"]+['\"]", "Hardcoded password", Severity.CRITICAL),
            (r"api_key\s*=\s*['\"][^'\"]+['\"]", "Hardcoded API key", Severity.CRITICAL),
            (r"secret\s*=\s*['\"][^'\"]+['\"]", "Hardcoded secret", Severity.CRITICAL),
            (r"token\s*=\s*['\"][^'\"]+['\"]", "Hardcoded token", Severity.HIGH),
            (r"SELECT\s+.*\s+FROM\s+.*\s+WHERE\s+.*%s", "Potential SQL injection", Severity.HIGH),
            (r"\.format\s*\(.*\)\s*$", "String formatting (potential injection)", Severity.LOW),
            (r"assert\s+", "Assert in production code", Severity.LOW),
            (r"DEBUG\s*=\s*True", "Debug mode enabled", Severity.MEDIUM),
            (r"verify\s*=\s*False", "SSL verification disabled", Severity.HIGH),
            (r"random\.", "Use of random (not cryptographically secure)", Severity.LOW),
            (r"md5\(", "Use of MD5 (weak hash)", Severity.MEDIUM),
            (r"sha1\(", "Use of SHA1 (weak hash)", Severity.LOW),
            (r"tempfile\.mktemp", "Insecure temp file creation", Severity.MEDIUM),
        ]
        
        security_issues = 0
        critical_issues = 0
        high_issues = 0
        
        for file_path in self.file_metrics:
            try:
                with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
                    content = f.read()
                    lines = content.split("\n")
                
                for pattern, description, severity in security_patterns:
                    for i, line in enumerate(lines, 1):
                        if re.search(pattern, line, re.IGNORECASE):
                            security_issues += 1
                            if severity == Severity.CRITICAL:
                                critical_issues += 1
                            elif severity == Severity.HIGH:
                                high_issues += 1
                            
                            self.issues.append(CodeIssue(
                                title=f"Security: {description}",
                                description=f"Potential security issue: {description}",
                                severity=severity,
                                category=MetricCategory.SECURITY,
                                file_path=file_path,
                                line_number=i,
                                suggestion="Review and fix this security concern",
                                effort_minutes=30 if severity in [Severity.CRITICAL, Severity.HIGH] else 10
                            ))
            except Exception:
                pass
        
        self.metrics.append(MetricValue(
            name="security_issues",
            value=security_issues,
            category=MetricCategory.SECURITY,
            unit="issues",
            threshold_warning=5,
            threshold_critical=10,
            description="Total security issues found"
        ))
        
        self.metrics.append(MetricValue(
            name="critical_security_issues",
            value=critical_issues,
            category=MetricCategory.SECURITY,
            unit="issues",
            threshold_warning=1,
            threshold_critical=3,
            description="Critical security issues"
        ))
        
        self.metrics.append(MetricValue(
            name="high_security_issues",
            value=high_issues,
            category=MetricCategory.SECURITY,
            unit="issues",
            threshold_warning=3,
            threshold_critical=10,
            description="High severity security issues"
        ))
    
    def _analyze_performance_patterns(self):
        performance_patterns = [
            (r"for\s+.*\s+in\s+range\s*\(\s*len\s*\(", "Inefficient iteration pattern", Severity.LOW),
            (r"\+\s*=\s*['\"]", "String concatenation in loop", Severity.LOW),
            (r"\.append\s*\(.*\)\s*$", "List append (consider list comprehension)", Severity.INFO),
            (r"time\.sleep\s*\(", "Blocking sleep call", Severity.LOW),
            (r"\.read\s*\(\s*\)", "Reading entire file into memory", Severity.LOW),
            (r"global\s+", "Global variable usage", Severity.LOW),
            (r"import\s+\*", "Wildcard import", Severity.MEDIUM),
            (r"except\s*:", "Bare except clause", Severity.MEDIUM),
            (r"except\s+Exception\s*:", "Catching generic Exception", Severity.LOW),
            (r"\.keys\s*\(\s*\)\s*\)", "Unnecessary .keys() call", Severity.INFO),
            (r"if\s+.*\s+==\s+True", "Redundant == True comparison", Severity.INFO),
            (r"if\s+.*\s+==\s+False", "Redundant == False comparison", Severity.INFO),
            (r"if\s+.*\s+==\s+None", "Use 'is None' instead", Severity.INFO),
            (r"if\s+.*\s+!=\s+None", "Use 'is not None' instead", Severity.INFO),
            (r"lambda\s+.*:\s+.*\(.*\)", "Lambda that could be a function reference", Severity.INFO),
        ]
        
        performance_issues = 0
        
        for file_path in self.file_metrics:
            try:
                with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
                    lines = f.readlines()
                
                for pattern, description, severity in performance_patterns:
                    for i, line in enumerate(lines, 1):
                        if re.search(pattern, line):
                            performance_issues += 1
                            
                            self.issues.append(CodeIssue(
                                title=f"Performance: {description}",
                                description=f"Potential performance issue: {description}",
                                severity=severity,
                                category=MetricCategory.PERFORMANCE,
                                file_path=file_path,
                                line_number=i,
                                suggestion="Consider optimizing this pattern",
                                effort_minutes=10
                            ))
            except Exception:
                pass
        
        self.metrics.append(MetricValue(
            name="performance_issues",
            value=performance_issues,
            category=MetricCategory.PERFORMANCE,
            unit="issues",
            threshold_warning=20,
            threshold_critical=50,
            description="Potential performance issues found"
        ))
    
    def _analyze_testing(self):
        test_files = list(self.project_root.rglob("test_*.py")) + list(self.project_root.rglob("*_test.py"))
        test_files += list(self.project_root.rglob("tests/*.py"))
        
        test_count = 0
        assertion_count = 0
        
        for test_file in test_files:
            try:
                with open(test_file, "r", encoding="utf-8", errors="ignore") as f:
                    content = f.read()
                
                test_count += len(re.findall(r"def\s+test_", content))
                assertion_count += len(re.findall(r"assert\s+", content))
                assertion_count += len(re.findall(r"self\.assert", content))
                assertion_count += len(re.findall(r"pytest\.raises", content))
            except Exception:
                pass
        
        total_functions = self.project_metrics.get("total_functions", 1)
        test_ratio = test_count / max(total_functions, 1) * 100
        
        self.metrics.append(MetricValue(
            name="test_count",
            value=test_count,
            category=MetricCategory.TESTING,
            unit="tests",
            description="Number of test functions"
        ))
        
        self.metrics.append(MetricValue(
            name="assertion_count",
            value=assertion_count,
            category=MetricCategory.TESTING,
            unit="assertions",
            description="Number of assertions in tests"
        ))
        
        self.metrics.append(MetricValue(
            name="test_ratio",
            value=test_ratio,
            category=MetricCategory.TESTING,
            unit="%",
            threshold_warning=20,
            threshold_critical=10,
            description="Ratio of tests to functions"
        ))
        
        self.metrics.append(MetricValue(
            name="assertions_per_test",
            value=assertion_count / max(test_count, 1),
            category=MetricCategory.TESTING,
            unit="",
            description="Average assertions per test"
        ))
    
    def _analyze_documentation(self):
        documented_functions = 0
        undocumented_functions = 0
        documented_classes = 0
        undocumented_classes = 0
        
        for file_path, metrics in self.file_metrics.items():
            if "functions" in metrics:
                for func in metrics["functions"]:
                    if func.get("has_docstring"):
                        documented_functions += 1
                    else:
                        undocumented_functions += 1
                        
                        self.issues.append(CodeIssue(
                            title=f"Missing docstring: {func['name']}",
                            description="Function lacks documentation",
                            severity=Severity.INFO,
                            category=MetricCategory.DOCUMENTATION,
                            file_path=file_path,
                            line_number=func["line"],
                            suggestion="Add a docstring describing the function's purpose",
                            effort_minutes=5
                        ))
            
            if "classes" in metrics:
                for cls in metrics["classes"]:
                    if cls.get("has_docstring"):
                        documented_classes += 1
                    else:
                        undocumented_classes += 1
        
        total_functions = documented_functions + undocumented_functions
        total_classes = documented_classes + undocumented_classes
        
        function_doc_ratio = documented_functions / max(total_functions, 1) * 100
        class_doc_ratio = documented_classes / max(total_classes, 1) * 100
        
        self.metrics.append(MetricValue(
            name="documented_functions",
            value=documented_functions,
            category=MetricCategory.DOCUMENTATION,
            unit="functions",
            description="Functions with docstrings"
        ))
        
        self.metrics.append(MetricValue(
            name="undocumented_functions",
            value=undocumented_functions,
            category=MetricCategory.DOCUMENTATION,
            unit="functions",
            description="Functions without docstrings"
        ))
        
        self.metrics.append(MetricValue(
            name="function_documentation_ratio",
            value=function_doc_ratio,
            category=MetricCategory.DOCUMENTATION,
            unit="%",
            threshold_warning=50,
            threshold_critical=25,
            description="Percentage of functions with docstrings"
        ))
        
        self.metrics.append(MetricValue(
            name="class_documentation_ratio",
            value=class_doc_ratio,
            category=MetricCategory.DOCUMENTATION,
            unit="%",
            threshold_warning=50,
            threshold_critical=25,
            description="Percentage of classes with docstrings"
        ))
    
    def _analyze_technical_debt(self):
        todo_count = 0
        fixme_count = 0
        hack_count = 0
        xxx_count = 0
        
        debt_patterns = [
            (r"#\s*TODO", "todo"),
            (r"#\s*FIXME", "fixme"),
            (r"#\s*HACK", "hack"),
            (r"#\s*XXX", "xxx"),
            (r"#\s*BUG", "bug"),
            (r"#\s*OPTIMIZE", "optimize"),
            (r"#\s*REFACTOR", "refactor"),
        ]
        
        for file_path in self.file_metrics:
            try:
                with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
                    content = f.read()
                
                todo_count += len(re.findall(r"#\s*TODO", content, re.IGNORECASE))
                fixme_count += len(re.findall(r"#\s*FIXME", content, re.IGNORECASE))
                hack_count += len(re.findall(r"#\s*HACK", content, re.IGNORECASE))
                xxx_count += len(re.findall(r"#\s*XXX", content, re.IGNORECASE))
            except Exception:
                pass
        
        total_debt_markers = todo_count + fixme_count + hack_count + xxx_count
        
        total_effort = sum(issue.effort_minutes for issue in self.issues)
        
        self.metrics.append(MetricValue(
            name="todo_count",
            value=todo_count,
            category=MetricCategory.TECHNICAL_DEBT,
            unit="items",
            description="Number of TODO comments"
        ))
        
        self.metrics.append(MetricValue(
            name="fixme_count",
            value=fixme_count,
            category=MetricCategory.TECHNICAL_DEBT,
            unit="items",
            description="Number of FIXME comments"
        ))
        
        self.metrics.append(MetricValue(
            name="hack_count",
            value=hack_count,
            category=MetricCategory.TECHNICAL_DEBT,
            unit="items",
            description="Number of HACK comments"
        ))
        
        self.metrics.append(MetricValue(
            name="total_debt_markers",
            value=total_debt_markers,
            category=MetricCategory.TECHNICAL_DEBT,
            unit="items",
            threshold_warning=20,
            threshold_critical=50,
            description="Total technical debt markers"
        ))
        
        self.metrics.append(MetricValue(
            name="estimated_debt_hours",
            value=total_effort / 60,
            category=MetricCategory.TECHNICAL_DEBT,
            unit="hours",
            description="Estimated hours to fix all issues"
        ))
        
        self.metrics.append(MetricValue(
            name="total_issues",
            value=len(self.issues),
            category=MetricCategory.TECHNICAL_DEBT,
            unit="issues",
            description="Total code issues found"
        ))
    
    def get_full_report(self) -> Dict[str, Any]:
        metrics_by_category: Dict[str, List[Dict]] = defaultdict(list)
        for metric in self.metrics:
            metrics_by_category[metric.category.value].append(metric.to_dict())
        
        issues_by_severity: Dict[str, List[Dict]] = defaultdict(list)
        for issue in self.issues:
            issues_by_severity[issue.severity.value].append(issue.to_dict())
        
        return {
            "summary": {
                "total_metrics": len(self.metrics),
                "total_issues": len(self.issues),
                "critical_issues": len([i for i in self.issues if i.severity == Severity.CRITICAL]),
                "high_issues": len([i for i in self.issues if i.severity == Severity.HIGH]),
                "medium_issues": len([i for i in self.issues if i.severity == Severity.MEDIUM]),
                "low_issues": len([i for i in self.issues if i.severity == Severity.LOW]),
                "info_issues": len([i for i in self.issues if i.severity == Severity.INFO]),
                "files_analyzed": len(self.file_metrics),
                "functions_analyzed": len(self.function_metrics),
                "classes_analyzed": len(self.class_metrics),
            },
            "project_metrics": self.project_metrics,
            "metrics_by_category": dict(metrics_by_category),
            "issues_by_severity": dict(issues_by_severity),
            "all_metrics": [m.to_dict() for m in self.metrics],
            "all_issues": [i.to_dict() for i in self.issues],
            "file_metrics": self.file_metrics,
            "function_metrics": self.function_metrics,
            "class_metrics": self.class_metrics,
            "timestamp": datetime.now().isoformat(),
        }
    
    def get_metric_count(self) -> int:
        return len(self.metrics)
    
    def get_issue_count(self) -> int:
        return len(self.issues)


def analyze_project(project_root: str) -> Dict[str, Any]:
    analyzer = CodeQualityMetrics(project_root)
    return analyzer.analyze_project()


def get_metric_summary(report: Dict[str, Any]) -> str:
    summary = report.get("summary", {})
    project = report.get("project_metrics", {})
    
    lines = [
        f"Code Quality Report",
        f"==================",
        f"",
        f"Files Analyzed: {summary.get('files_analyzed', 0)}",
        f"Total Lines: {project.get('total_lines', 0):,}",
        f"Code Lines: {project.get('total_code_lines', 0):,}",
        f"Functions: {project.get('total_functions', 0):,}",
        f"Classes: {project.get('total_classes', 0):,}",
        f"",
        f"Metrics Collected: {summary.get('total_metrics', 0)}",
        f"Issues Found: {summary.get('total_issues', 0)}",
        f"  - Critical: {summary.get('critical_issues', 0)}",
        f"  - High: {summary.get('high_issues', 0)}",
        f"  - Medium: {summary.get('medium_issues', 0)}",
        f"  - Low: {summary.get('low_issues', 0)}",
        f"  - Info: {summary.get('info_issues', 0)}",
        f"",
        f"Average Complexity: {project.get('average_complexity', 0):.2f}",
        f"Comment Ratio: {project.get('comment_ratio', 0) * 100:.1f}%",
        f"Dependencies: {project.get('dependency_count', 0)}",
    ]
    
    return "\n".join(lines)
