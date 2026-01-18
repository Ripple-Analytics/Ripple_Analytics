#!/usr/bin/env python3
"""
Configuration Settings
Centralized configuration for the Mental Models System.
"""

import os
from dataclasses import dataclass
from typing import Optional
from dotenv import load_dotenv

load_dotenv()


@dataclass
class DatabaseConfig:
    """PostgreSQL database configuration."""
    host: str = os.getenv("DB_HOST", "localhost")
    port: int = int(os.getenv("DB_PORT", "5432"))
    name: str = os.getenv("DB_NAME", "mental_models")
    user: str = os.getenv("DB_USER", "postgres")
    password: str = os.getenv("DB_PASSWORD", "")
    
    @property
    def connection_string(self) -> str:
        """Get PostgreSQL connection string."""
        if self.password:
            return f"postgresql://{self.user}:{self.password}@{self.host}:{self.port}/{self.name}"
        return f"postgresql://{self.user}@{self.host}:{self.port}/{self.name}"
    
    @property
    def jdbc_url(self) -> str:
        """Get JDBC URL for Spark."""
        return f"jdbc:postgresql://{self.host}:{self.port}/{self.name}"


@dataclass
class SparkConfig:
    """PySpark configuration."""
    app_name: str = "MentalModelsAnalysis"
    driver_memory: str = os.getenv("SPARK_DRIVER_MEMORY", "4g")
    executor_memory: str = os.getenv("SPARK_EXECUTOR_MEMORY", "4g")
    jdbc_jar_path: str = os.getenv("SPARK_JDBC_JAR", "/usr/share/java/postgresql-jdbc4.jar")


@dataclass
class ExportConfig:
    """Export configuration."""
    output_dir: str = os.getenv("EXPORT_OUTPUT_DIR", "data/processed")
    excel_template: Optional[str] = os.getenv("EXCEL_TEMPLATE", None)
    max_rows_per_sheet: int = int(os.getenv("MAX_ROWS_PER_SHEET", "1000000"))


@dataclass
class APIConfig:
    """API configuration."""
    host: str = os.getenv("API_HOST", "0.0.0.0")
    port: int = int(os.getenv("API_PORT", "8000"))
    debug: bool = os.getenv("API_DEBUG", "false").lower() == "true"
    cors_origins: list = None
    
    def __post_init__(self):
        origins = os.getenv("CORS_ORIGINS", "*")
        self.cors_origins = origins.split(",") if origins else ["*"]


@dataclass
class AnalysisConfig:
    """Analysis configuration."""
    monte_carlo_simulations: int = int(os.getenv("MC_SIMULATIONS", "10000"))
    random_seed: int = int(os.getenv("RANDOM_SEED", "42"))
    confidence_level: float = float(os.getenv("CONFIDENCE_LEVEL", "0.95"))
    min_lollapalooza_models: int = int(os.getenv("MIN_LOLLAPALOOZA_MODELS", "3"))


@dataclass
class DashboardConfig:
    """Dashboard configuration."""
    host: str = os.getenv("DASHBOARD_HOST", "0.0.0.0")
    port: int = int(os.getenv("DASHBOARD_PORT", "8050"))
    debug: bool = os.getenv("DASHBOARD_DEBUG", "false").lower() == "true"


class Settings:
    """Main settings container."""
    
    def __init__(self):
        self.database = DatabaseConfig()
        self.spark = SparkConfig()
        self.export = ExportConfig()
        self.api = APIConfig()
        self.analysis = AnalysisConfig()
        self.dashboard = DashboardConfig()
    
    @classmethod
    def from_env(cls, env_file: str = ".env") -> "Settings":
        """Load settings from environment file."""
        load_dotenv(env_file)
        return cls()


settings = Settings()
