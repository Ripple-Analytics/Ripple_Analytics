import logging
import math
import json
from dataclasses import dataclass, field
from typing import Dict, Any, List, Optional, Tuple
from datetime import datetime
from enum import Enum
import statistics

logger = logging.getLogger(__name__)


class AnalysisType(Enum):
    CORRELATION = "correlation"
    REGRESSION = "regression"
    COVARIANCE = "covariance"
    FACTOR_ANALYSIS = "factor_analysis"
    CLUSTER_ANALYSIS = "cluster_analysis"
    TIME_SERIES = "time_series"
    CROSS_TABULATION = "cross_tabulation"
    ANOVA = "anova"
    CHI_SQUARE = "chi_square"
    T_TEST = "t_test"


@dataclass
class Variable:
    name: str
    values: List[float]
    variable_type: str = "continuous"
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    @property
    def mean(self) -> float:
        return statistics.mean(self.values) if self.values else 0.0
    
    @property
    def std(self) -> float:
        return statistics.stdev(self.values) if len(self.values) > 1 else 0.0
    
    @property
    def variance(self) -> float:
        return statistics.variance(self.values) if len(self.values) > 1 else 0.0
    
    @property
    def median(self) -> float:
        return statistics.median(self.values) if self.values else 0.0
    
    @property
    def min_val(self) -> float:
        return min(self.values) if self.values else 0.0
    
    @property
    def max_val(self) -> float:
        return max(self.values) if self.values else 0.0
    
    @property
    def range_val(self) -> float:
        return self.max_val - self.min_val
    
    @property
    def n(self) -> int:
        return len(self.values)


@dataclass
class CorrelationResult:
    var1: str
    var2: str
    coefficient: float
    p_value: float
    n: int
    method: str = "pearson"
    significant: bool = False
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "var1": self.var1,
            "var2": self.var2,
            "coefficient": round(self.coefficient, 4),
            "p_value": round(self.p_value, 4),
            "n": self.n,
            "method": self.method,
            "significant": self.significant,
            "strength": self._interpret_strength()
        }
    
    def _interpret_strength(self) -> str:
        r = abs(self.coefficient)
        if r >= 0.9:
            return "very_strong"
        elif r >= 0.7:
            return "strong"
        elif r >= 0.5:
            return "moderate"
        elif r >= 0.3:
            return "weak"
        else:
            return "negligible"


@dataclass
class RegressionResult:
    dependent_var: str
    independent_vars: List[str]
    coefficients: Dict[str, float]
    intercept: float
    r_squared: float
    adjusted_r_squared: float
    f_statistic: float
    p_value: float
    std_errors: Dict[str, float]
    residuals: List[float]
    n: int
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "dependent_var": self.dependent_var,
            "independent_vars": self.independent_vars,
            "coefficients": {k: round(v, 4) for k, v in self.coefficients.items()},
            "intercept": round(self.intercept, 4),
            "r_squared": round(self.r_squared, 4),
            "adjusted_r_squared": round(self.adjusted_r_squared, 4),
            "f_statistic": round(self.f_statistic, 4),
            "p_value": round(self.p_value, 4),
            "n": self.n,
            "model_quality": self._interpret_r_squared()
        }
    
    def _interpret_r_squared(self) -> str:
        if self.r_squared >= 0.9:
            return "excellent"
        elif self.r_squared >= 0.7:
            return "good"
        elif self.r_squared >= 0.5:
            return "moderate"
        elif self.r_squared >= 0.3:
            return "weak"
        else:
            return "poor"


@dataclass
class CovarianceMatrix:
    variables: List[str]
    matrix: List[List[float]]
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "variables": self.variables,
            "matrix": [[round(v, 4) for v in row] for row in self.matrix]
        }
    
    def get_covariance(self, var1: str, var2: str) -> float:
        i = self.variables.index(var1)
        j = self.variables.index(var2)
        return self.matrix[i][j]


@dataclass
class FactorAnalysisResult:
    variables: List[str]
    n_factors: int
    loadings: Dict[str, List[float]]
    eigenvalues: List[float]
    variance_explained: List[float]
    cumulative_variance: List[float]
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "variables": self.variables,
            "n_factors": self.n_factors,
            "loadings": {k: [round(v, 4) for v in vals] for k, vals in self.loadings.items()},
            "eigenvalues": [round(v, 4) for v in self.eigenvalues],
            "variance_explained": [round(v, 4) for v in self.variance_explained],
            "cumulative_variance": [round(v, 4) for v in self.cumulative_variance]
        }


@dataclass
class SynthesisResult:
    variables: List[str]
    correlations: List[CorrelationResult]
    covariance_matrix: CovarianceMatrix
    regression: Optional[RegressionResult]
    factor_analysis: Optional[FactorAnalysisResult]
    descriptive_stats: Dict[str, Dict[str, float]]
    interactions: List[Dict[str, Any]]
    insights: List[str]
    timestamp: datetime = field(default_factory=datetime.now)
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "variables": self.variables,
            "correlations": [c.to_dict() for c in self.correlations],
            "covariance_matrix": self.covariance_matrix.to_dict(),
            "regression": self.regression.to_dict() if self.regression else None,
            "factor_analysis": self.factor_analysis.to_dict() if self.factor_analysis else None,
            "descriptive_stats": self.descriptive_stats,
            "interactions": self.interactions,
            "insights": self.insights,
            "timestamp": self.timestamp.isoformat()
        }


class StatisticalEngine:
    def __init__(self):
        self.variables: Dict[str, Variable] = {}
        self.results_cache: Dict[str, Any] = {}
    
    def add_variable(self, name: str, values: List[float], variable_type: str = "continuous", metadata: Dict[str, Any] = None):
        self.variables[name] = Variable(
            name=name,
            values=values,
            variable_type=variable_type,
            metadata=metadata or {}
        )
        logger.info(f"Added variable: {name} with {len(values)} observations")
    
    def add_variables_from_dict(self, data: Dict[str, List[float]]):
        for name, values in data.items():
            self.add_variable(name, values)
    
    def get_descriptive_stats(self, var_name: str) -> Dict[str, float]:
        if var_name not in self.variables:
            raise ValueError(f"Variable {var_name} not found")
        
        var = self.variables[var_name]
        return {
            "n": var.n,
            "mean": round(var.mean, 4),
            "std": round(var.std, 4),
            "variance": round(var.variance, 4),
            "median": round(var.median, 4),
            "min": round(var.min_val, 4),
            "max": round(var.max_val, 4),
            "range": round(var.range_val, 4)
        }
    
    def pearson_correlation(self, var1_name: str, var2_name: str) -> CorrelationResult:
        if var1_name not in self.variables or var2_name not in self.variables:
            raise ValueError("One or both variables not found")
        
        var1 = self.variables[var1_name]
        var2 = self.variables[var2_name]
        
        n = min(var1.n, var2.n)
        x = var1.values[:n]
        y = var2.values[:n]
        
        mean_x = statistics.mean(x)
        mean_y = statistics.mean(y)
        
        numerator = sum((x[i] - mean_x) * (y[i] - mean_y) for i in range(n))
        
        sum_sq_x = sum((xi - mean_x) ** 2 for xi in x)
        sum_sq_y = sum((yi - mean_y) ** 2 for yi in y)
        
        denominator = math.sqrt(sum_sq_x * sum_sq_y)
        
        if denominator == 0:
            r = 0.0
        else:
            r = numerator / denominator
        
        t_stat = r * math.sqrt((n - 2) / (1 - r**2 + 1e-10))
        p_value = self._t_to_p(t_stat, n - 2)
        
        return CorrelationResult(
            var1=var1_name,
            var2=var2_name,
            coefficient=r,
            p_value=p_value,
            n=n,
            method="pearson",
            significant=p_value < 0.05
        )
    
    def spearman_correlation(self, var1_name: str, var2_name: str) -> CorrelationResult:
        if var1_name not in self.variables or var2_name not in self.variables:
            raise ValueError("One or both variables not found")
        
        var1 = self.variables[var1_name]
        var2 = self.variables[var2_name]
        
        n = min(var1.n, var2.n)
        x = var1.values[:n]
        y = var2.values[:n]
        
        rank_x = self._rank(x)
        rank_y = self._rank(y)
        
        d_squared = sum((rank_x[i] - rank_y[i]) ** 2 for i in range(n))
        
        rho = 1 - (6 * d_squared) / (n * (n**2 - 1))
        
        t_stat = rho * math.sqrt((n - 2) / (1 - rho**2 + 1e-10))
        p_value = self._t_to_p(t_stat, n - 2)
        
        return CorrelationResult(
            var1=var1_name,
            var2=var2_name,
            coefficient=rho,
            p_value=p_value,
            n=n,
            method="spearman",
            significant=p_value < 0.05
        )
    
    def correlation_matrix(self, var_names: List[str] = None, method: str = "pearson") -> List[CorrelationResult]:
        if var_names is None:
            var_names = list(self.variables.keys())
        
        results = []
        for i, var1 in enumerate(var_names):
            for var2 in var_names[i+1:]:
                if method == "pearson":
                    result = self.pearson_correlation(var1, var2)
                else:
                    result = self.spearman_correlation(var1, var2)
                results.append(result)
        
        return results
    
    def covariance_matrix(self, var_names: List[str] = None) -> CovarianceMatrix:
        if var_names is None:
            var_names = list(self.variables.keys())
        
        n_vars = len(var_names)
        matrix = [[0.0] * n_vars for _ in range(n_vars)]
        
        for i, var1_name in enumerate(var_names):
            for j, var2_name in enumerate(var_names):
                var1 = self.variables[var1_name]
                var2 = self.variables[var2_name]
                
                n = min(var1.n, var2.n)
                x = var1.values[:n]
                y = var2.values[:n]
                
                mean_x = statistics.mean(x)
                mean_y = statistics.mean(y)
                
                cov = sum((x[k] - mean_x) * (y[k] - mean_y) for k in range(n)) / (n - 1)
                matrix[i][j] = cov
        
        return CovarianceMatrix(variables=var_names, matrix=matrix)
    
    def simple_linear_regression(self, dependent: str, independent: str) -> RegressionResult:
        if dependent not in self.variables or independent not in self.variables:
            raise ValueError("One or both variables not found")
        
        y_var = self.variables[dependent]
        x_var = self.variables[independent]
        
        n = min(y_var.n, x_var.n)
        y = y_var.values[:n]
        x = x_var.values[:n]
        
        mean_x = statistics.mean(x)
        mean_y = statistics.mean(y)
        
        numerator = sum((x[i] - mean_x) * (y[i] - mean_y) for i in range(n))
        denominator = sum((xi - mean_x) ** 2 for xi in x)
        
        if denominator == 0:
            slope = 0.0
        else:
            slope = numerator / denominator
        
        intercept = mean_y - slope * mean_x
        
        y_pred = [intercept + slope * xi for xi in x]
        residuals = [y[i] - y_pred[i] for i in range(n)]
        
        ss_res = sum(r ** 2 for r in residuals)
        ss_tot = sum((yi - mean_y) ** 2 for yi in y)
        
        r_squared = 1 - (ss_res / ss_tot) if ss_tot != 0 else 0.0
        adjusted_r_squared = 1 - (1 - r_squared) * (n - 1) / (n - 2)
        
        mse = ss_res / (n - 2) if n > 2 else 0
        se_slope = math.sqrt(mse / denominator) if denominator > 0 and mse > 0 else 0
        
        f_stat = (ss_tot - ss_res) / mse if mse > 0 else 0
        p_value = self._f_to_p(f_stat, 1, n - 2)
        
        return RegressionResult(
            dependent_var=dependent,
            independent_vars=[independent],
            coefficients={independent: slope},
            intercept=intercept,
            r_squared=r_squared,
            adjusted_r_squared=adjusted_r_squared,
            f_statistic=f_stat,
            p_value=p_value,
            std_errors={independent: se_slope},
            residuals=residuals,
            n=n
        )
    
    def multiple_regression(self, dependent: str, independents: List[str]) -> RegressionResult:
        if dependent not in self.variables:
            raise ValueError(f"Dependent variable {dependent} not found")
        
        for ind in independents:
            if ind not in self.variables:
                raise ValueError(f"Independent variable {ind} not found")
        
        y_var = self.variables[dependent]
        n = y_var.n
        
        for ind in independents:
            n = min(n, self.variables[ind].n)
        
        y = y_var.values[:n]
        X = [[1.0] + [self.variables[ind].values[i] for ind in independents] for i in range(n)]
        
        coeffs = self._ols_coefficients(X, y)
        
        intercept = coeffs[0]
        slopes = {independents[i]: coeffs[i+1] for i in range(len(independents))}
        
        y_pred = [sum(X[i][j] * coeffs[j] for j in range(len(coeffs))) for i in range(n)]
        residuals = [y[i] - y_pred[i] for i in range(n)]
        
        mean_y = statistics.mean(y)
        ss_res = sum(r ** 2 for r in residuals)
        ss_tot = sum((yi - mean_y) ** 2 for yi in y)
        
        r_squared = 1 - (ss_res / ss_tot) if ss_tot != 0 else 0.0
        
        k = len(independents)
        adjusted_r_squared = 1 - (1 - r_squared) * (n - 1) / (n - k - 1) if n > k + 1 else 0
        
        mse = ss_res / (n - k - 1) if n > k + 1 else 0
        msr = (ss_tot - ss_res) / k if k > 0 else 0
        f_stat = msr / mse if mse > 0 else 0
        p_value = self._f_to_p(f_stat, k, n - k - 1)
        
        std_errors = {ind: 0.0 for ind in independents}
        
        return RegressionResult(
            dependent_var=dependent,
            independent_vars=independents,
            coefficients=slopes,
            intercept=intercept,
            r_squared=r_squared,
            adjusted_r_squared=adjusted_r_squared,
            f_statistic=f_stat,
            p_value=p_value,
            std_errors=std_errors,
            residuals=residuals,
            n=n
        )
    
    def factor_analysis(self, var_names: List[str] = None, n_factors: int = None) -> FactorAnalysisResult:
        if var_names is None:
            var_names = list(self.variables.keys())
        
        corr_matrix = []
        for var1 in var_names:
            row = []
            for var2 in var_names:
                if var1 == var2:
                    row.append(1.0)
                else:
                    result = self.pearson_correlation(var1, var2)
                    row.append(result.coefficient)
            corr_matrix.append(row)
        
        eigenvalues = self._compute_eigenvalues(corr_matrix)
        
        if n_factors is None:
            n_factors = sum(1 for ev in eigenvalues if ev > 1.0)
            n_factors = max(1, min(n_factors, len(var_names) // 2))
        
        total_variance = sum(eigenvalues)
        variance_explained = [ev / total_variance for ev in eigenvalues[:n_factors]]
        cumulative_variance = []
        cumsum = 0
        for ve in variance_explained:
            cumsum += ve
            cumulative_variance.append(cumsum)
        
        loadings = {}
        for i, var in enumerate(var_names):
            loadings[var] = [corr_matrix[i][j] * math.sqrt(eigenvalues[j]) 
                           for j in range(n_factors)]
        
        return FactorAnalysisResult(
            variables=var_names,
            n_factors=n_factors,
            loadings=loadings,
            eigenvalues=eigenvalues[:n_factors],
            variance_explained=variance_explained,
            cumulative_variance=cumulative_variance
        )
    
    def synthesize_variables(self, var_names: List[str] = None, dependent: str = None) -> SynthesisResult:
        if var_names is None:
            var_names = list(self.variables.keys())
        
        if len(var_names) < 2:
            raise ValueError("Need at least 2 variables for synthesis")
        
        descriptive_stats = {var: self.get_descriptive_stats(var) for var in var_names}
        
        correlations = self.correlation_matrix(var_names)
        
        cov_matrix = self.covariance_matrix(var_names)
        
        regression = None
        if dependent and dependent in var_names:
            independents = [v for v in var_names if v != dependent]
            if independents:
                regression = self.multiple_regression(dependent, independents)
        
        factor_result = None
        if len(var_names) >= 3:
            try:
                factor_result = self.factor_analysis(var_names)
            except Exception as e:
                logger.warning(f"Factor analysis failed: {e}")
        
        interactions = self._detect_interactions(correlations)
        
        insights = self._generate_insights(correlations, regression, factor_result, descriptive_stats)
        
        return SynthesisResult(
            variables=var_names,
            correlations=correlations,
            covariance_matrix=cov_matrix,
            regression=regression,
            factor_analysis=factor_result,
            descriptive_stats=descriptive_stats,
            interactions=interactions,
            insights=insights
        )
    
    def _detect_interactions(self, correlations: List[CorrelationResult]) -> List[Dict[str, Any]]:
        interactions = []
        
        strong_correlations = [c for c in correlations if abs(c.coefficient) >= 0.5]
        
        for corr in strong_correlations:
            interaction = {
                "variables": [corr.var1, corr.var2],
                "type": "positive" if corr.coefficient > 0 else "negative",
                "strength": corr._interpret_strength(),
                "coefficient": corr.coefficient,
                "significant": corr.significant
            }
            interactions.append(interaction)
        
        return interactions
    
    def _generate_insights(self, correlations: List[CorrelationResult], 
                          regression: Optional[RegressionResult],
                          factor_analysis: Optional[FactorAnalysisResult],
                          descriptive_stats: Dict[str, Dict[str, float]]) -> List[str]:
        insights = []
        
        strong_pos = [c for c in correlations if c.coefficient >= 0.7 and c.significant]
        strong_neg = [c for c in correlations if c.coefficient <= -0.7 and c.significant]
        
        if strong_pos:
            for c in strong_pos[:3]:
                insights.append(f"Strong positive correlation ({c.coefficient:.2f}) between {c.var1} and {c.var2}")
        
        if strong_neg:
            for c in strong_neg[:3]:
                insights.append(f"Strong negative correlation ({c.coefficient:.2f}) between {c.var1} and {c.var2}")
        
        if regression:
            if regression.r_squared >= 0.7:
                insights.append(f"Regression model explains {regression.r_squared*100:.1f}% of variance in {regression.dependent_var}")
            
            for var, coef in regression.coefficients.items():
                if abs(coef) > 0.5:
                    direction = "increases" if coef > 0 else "decreases"
                    insights.append(f"{var} significantly {direction} {regression.dependent_var} (coef={coef:.2f})")
        
        if factor_analysis and factor_analysis.cumulative_variance:
            total_explained = factor_analysis.cumulative_variance[-1]
            insights.append(f"{factor_analysis.n_factors} factors explain {total_explained*100:.1f}% of total variance")
        
        high_variance = [(var, stats["std"]) for var, stats in descriptive_stats.items() 
                        if stats["std"] > stats["mean"] * 0.5]
        if high_variance:
            for var, std in high_variance[:2]:
                insights.append(f"{var} shows high variability (std={std:.2f})")
        
        return insights
    
    def _rank(self, values: List[float]) -> List[float]:
        sorted_indices = sorted(range(len(values)), key=lambda i: values[i])
        ranks = [0.0] * len(values)
        for rank, idx in enumerate(sorted_indices, 1):
            ranks[idx] = float(rank)
        return ranks
    
    def _t_to_p(self, t: float, df: int) -> float:
        if df <= 0:
            return 1.0
        x = df / (df + t * t)
        return self._incomplete_beta(df / 2, 0.5, x)
    
    def _f_to_p(self, f: float, df1: int, df2: int) -> float:
        if df1 <= 0 or df2 <= 0 or f <= 0:
            return 1.0
        x = df2 / (df2 + df1 * f)
        return self._incomplete_beta(df2 / 2, df1 / 2, x)
    
    def _incomplete_beta(self, a: float, b: float, x: float) -> float:
        if x <= 0:
            return 0.0
        if x >= 1:
            return 1.0
        
        result = 0.0
        term = 1.0
        for n in range(100):
            term *= (a + n) * x / (a + b + n)
            result += term / (a + n + 1)
            if abs(term) < 1e-10:
                break
        
        return min(1.0, max(0.0, result * (x ** a) * ((1 - x) ** b) / a))
    
    def _ols_coefficients(self, X: List[List[float]], y: List[float]) -> List[float]:
        n = len(y)
        k = len(X[0])
        
        XtX = [[sum(X[i][j] * X[i][l] for i in range(n)) for l in range(k)] for j in range(k)]
        Xty = [sum(X[i][j] * y[i] for i in range(n)) for j in range(k)]
        
        try:
            XtX_inv = self._matrix_inverse(XtX)
            coeffs = [sum(XtX_inv[j][l] * Xty[l] for l in range(k)) for j in range(k)]
            return coeffs
        except Exception:
            return [0.0] * k
    
    def _matrix_inverse(self, matrix: List[List[float]]) -> List[List[float]]:
        n = len(matrix)
        augmented = [row[:] + [1.0 if i == j else 0.0 for j in range(n)] for i, row in enumerate(matrix)]
        
        for i in range(n):
            max_row = max(range(i, n), key=lambda r: abs(augmented[r][i]))
            augmented[i], augmented[max_row] = augmented[max_row], augmented[i]
            
            pivot = augmented[i][i]
            if abs(pivot) < 1e-10:
                raise ValueError("Matrix is singular")
            
            augmented[i] = [x / pivot for x in augmented[i]]
            
            for j in range(n):
                if i != j:
                    factor = augmented[j][i]
                    augmented[j] = [augmented[j][k] - factor * augmented[i][k] for k in range(2 * n)]
        
        return [row[n:] for row in augmented]
    
    def _compute_eigenvalues(self, matrix: List[List[float]]) -> List[float]:
        n = len(matrix)
        eigenvalues = []
        
        for i in range(n):
            eigenvalues.append(matrix[i][i])
        
        eigenvalues.sort(reverse=True)
        return eigenvalues


class MentalModelStatistics:
    def __init__(self):
        self.engine = StatisticalEngine()
        self.model_scores: Dict[str, List[float]] = {}
        self.document_analyses: List[Dict[str, Any]] = []
    
    def add_model_scores(self, model_name: str, scores: List[float]):
        self.model_scores[model_name] = scores
        self.engine.add_variable(model_name, scores)
    
    def add_document_analysis(self, doc_id: str, model_scores: Dict[str, float]):
        self.document_analyses.append({
            "doc_id": doc_id,
            "scores": model_scores,
            "timestamp": datetime.now().isoformat()
        })
        
        for model, score in model_scores.items():
            if model not in self.model_scores:
                self.model_scores[model] = []
            self.model_scores[model].append(score)
    
    def analyze_model_correlations(self) -> List[CorrelationResult]:
        for model, scores in self.model_scores.items():
            if model not in self.engine.variables:
                self.engine.add_variable(model, scores)
        
        return self.engine.correlation_matrix()
    
    def find_model_clusters(self) -> Dict[str, List[str]]:
        correlations = self.analyze_model_correlations()
        
        clusters: Dict[str, List[str]] = {}
        cluster_id = 0
        assigned = set()
        
        for corr in sorted(correlations, key=lambda c: -abs(c.coefficient)):
            if corr.coefficient >= 0.7:
                if corr.var1 not in assigned and corr.var2 not in assigned:
                    cluster_name = f"cluster_{cluster_id}"
                    clusters[cluster_name] = [corr.var1, corr.var2]
                    assigned.add(corr.var1)
                    assigned.add(corr.var2)
                    cluster_id += 1
                elif corr.var1 in assigned and corr.var2 not in assigned:
                    for name, members in clusters.items():
                        if corr.var1 in members:
                            members.append(corr.var2)
                            assigned.add(corr.var2)
                            break
                elif corr.var2 in assigned and corr.var1 not in assigned:
                    for name, members in clusters.items():
                        if corr.var2 in members:
                            members.append(corr.var1)
                            assigned.add(corr.var1)
                            break
        
        return clusters
    
    def synthesize_all(self, dependent_model: str = None) -> SynthesisResult:
        for model, scores in self.model_scores.items():
            if model not in self.engine.variables:
                self.engine.add_variable(model, scores)
        
        return self.engine.synthesize_variables(
            var_names=list(self.model_scores.keys()),
            dependent=dependent_model
        )
    
    def get_covariate_effects(self, target_model: str, covariates: List[str]) -> Dict[str, Any]:
        for model, scores in self.model_scores.items():
            if model not in self.engine.variables:
                self.engine.add_variable(model, scores)
        
        regression = self.engine.multiple_regression(target_model, covariates)
        
        return {
            "target": target_model,
            "covariates": covariates,
            "effects": regression.coefficients,
            "r_squared": regression.r_squared,
            "significant_covariates": [
                cov for cov, coef in regression.coefficients.items()
                if abs(coef) > 0.1
            ]
        }


def create_statistical_engine() -> StatisticalEngine:
    return StatisticalEngine()


def create_mental_model_statistics() -> MentalModelStatistics:
    return MentalModelStatistics()
