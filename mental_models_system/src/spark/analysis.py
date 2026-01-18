#!/usr/bin/env python3
"""
PySpark Analysis Engine
High-performance analysis of mental models and case studies.
"""

from pyspark.sql import SparkSession
from pyspark.sql.functions import col, avg, count, sum as spark_sum, stddev, corr, when, lit
from pyspark.sql.window import Window
import numpy as np

def create_spark_session():
    """Create SparkSession with PostgreSQL support."""
    return SparkSession.builder \
        .appName("MentalModelsAnalysis") \
        .config("spark.jars", "/usr/share/java/postgresql-jdbc4.jar") \
        .config("spark.driver.memory", "4g") \
        .config("spark.executor.memory", "4g") \
        .getOrCreate()

def load_from_postgres(spark, table_name):
    """Load table from PostgreSQL."""
    jdbc_url = "jdbc:postgresql://localhost:5432/mental_models"
    properties = {
        "user": "postgres",
        "driver": "org.postgresql.Driver"
    }
    return spark.read.jdbc(jdbc_url, table_name, properties=properties)

class MentalModelsAnalyzer:
    """Analyzer for mental models and case studies."""
    
    def __init__(self, spark):
        self.spark = spark
        self.case_studies = None
        self.mental_models = None
        self.planck_matrix = None
    
    def load_data(self):
        """Load all data from PostgreSQL."""
        print("Loading data from PostgreSQL...")
        self.case_studies = load_from_postgres(self.spark, "case_studies")
        self.mental_models = load_from_postgres(self.spark, "mental_models")
        self.planck_matrix = load_from_postgres(self.spark, "planck_matrix")
        print(f"Loaded {self.case_studies.count()} case studies")
        print(f"Loaded {self.mental_models.count()} mental models")
        print(f"Loaded {self.planck_matrix.count()} planck matrix entries")
    
    def analyze_model_frequency(self):
        """Analyze frequency of each model across cases."""
        return self.planck_matrix \
            .groupBy("model_name") \
            .agg(
                count("*").alias("occurrence_count"),
                avg("effect_size").alias("avg_effect_size"),
                stddev("effect_size").alias("std_effect_size")
            ) \
            .orderBy(col("occurrence_count").desc())
    
    def analyze_severity_by_model(self):
        """Analyze case severity by model involvement."""
        return self.planck_matrix \
            .join(self.case_studies, self.planck_matrix.case_id == self.case_studies.id) \
            .groupBy("model_name") \
            .agg(
                avg("severity").alias("avg_severity"),
                stddev("severity").alias("std_severity"),
                count("*").alias("case_count")
            ) \
            .orderBy(col("avg_severity").desc())
    
    def find_lollapalooza_patterns(self, min_models=3):
        """Find cases with multiple interacting models (lollapalooza)."""
        model_counts = self.planck_matrix \
            .groupBy("case_id", "case_name") \
            .agg(count("*").alias("model_count"))
        
        return model_counts \
            .filter(col("model_count") >= min_models) \
            .join(self.case_studies, model_counts.case_id == self.case_studies.id) \
            .select("case_name", "model_count", "severity", "financial_impact") \
            .orderBy(col("severity").desc())
    
    def calculate_model_correlations(self):
        """Calculate correlations between model co-occurrences."""
        # Pivot to create model presence matrix
        pivot_df = self.planck_matrix \
            .groupBy("case_id") \
            .pivot("model_name") \
            .agg(count("*"))
        
        return pivot_df
    
    def monte_carlo_simulation(self, model_effects, n_simulations=10000):
        """Run Monte Carlo simulation on model effects."""
        np.random.seed(42)
        
        # Collect effect sizes
        effects = self.planck_matrix \
            .select("effect_size") \
            .toPandas()["effect_size"].values
        
        # Run simulations
        simulations = np.random.choice(effects, size=(n_simulations, len(effects)), replace=True)
        combined_effects = simulations.sum(axis=1)
        
        return {
            "mean": np.mean(combined_effects),
            "std": np.std(combined_effects),
            "p5": np.percentile(combined_effects, 5),
            "p95": np.percentile(combined_effects, 95),
            "p50": np.percentile(combined_effects, 50)
        }
    
    def decade_analysis(self):
        """Analyze patterns by decade."""
        from pyspark.sql.functions import year, floor
        
        return self.case_studies \
            .withColumn("decade", (floor(year("date") / 10) * 10).cast("int")) \
            .groupBy("decade") \
            .agg(
                count("*").alias("case_count"),
                avg("severity").alias("avg_severity"),
                spark_sum("financial_impact").alias("total_impact")
            ) \
            .orderBy("decade")
    
    def region_analysis(self):
        """Analyze patterns by region."""
        return self.case_studies \
            .groupBy("region") \
            .agg(
                count("*").alias("case_count"),
                avg("severity").alias("avg_severity"),
                spark_sum("financial_impact").alias("total_impact")
            ) \
            .orderBy(col("case_count").desc())

def run_full_analysis():
    """Run complete analysis pipeline."""
    spark = create_spark_session()
    analyzer = MentalModelsAnalyzer(spark)
    
    try:
        analyzer.load_data()
        
        print("\n=== MODEL FREQUENCY ANALYSIS ===")
        model_freq = analyzer.analyze_model_frequency()
        model_freq.show(20)
        
        print("\n=== SEVERITY BY MODEL ===")
        severity = analyzer.analyze_severity_by_model()
        severity.show(20)
        
        print("\n=== LOLLAPALOOZA PATTERNS ===")
        lollapalooza = analyzer.find_lollapalooza_patterns()
        lollapalooza.show(20)
        
        print("\n=== DECADE ANALYSIS ===")
        decades = analyzer.decade_analysis()
        decades.show()
        
        print("\n=== REGION ANALYSIS ===")
        regions = analyzer.region_analysis()
        regions.show()
        
        print("\n=== MONTE CARLO SIMULATION ===")
        mc_results = analyzer.monte_carlo_simulation(None)
        print(f"Mean: {mc_results['mean']:.2f}")
        print(f"Std: {mc_results['std']:.2f}")
        print(f"5th Percentile: {mc_results['p5']:.2f}")
        print(f"95th Percentile: {mc_results['p95']:.2f}")
        
    finally:
        spark.stop()

if __name__ == "__main__":
    run_full_analysis()
