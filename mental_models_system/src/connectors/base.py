"""
Base Connector Framework

Abstract base classes and registry for all connectors.
"""

import os
import json
import logging
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime
from typing import Dict, List, Optional, Any, Type
from enum import Enum
from pathlib import Path

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class ConnectorType(Enum):
    """Types of connectors."""
    CHAT = "chat"
    VERSION_CONTROL = "version_control"
    PROJECT_MANAGEMENT = "project_management"
    WEB_SCRAPING = "web_scraping"
    DATA_SOURCE = "data_source"
    STORAGE = "storage"
    LLM = "llm"
    NOTIFICATION = "notification"


class ConnectorStatus(Enum):
    """Connector status."""
    CONNECTED = "connected"
    DISCONNECTED = "disconnected"
    ERROR = "error"
    RATE_LIMITED = "rate_limited"


@dataclass
class ConnectorConfig:
    """Configuration for a connector."""
    name: str
    connector_type: ConnectorType
    enabled: bool = True
    credentials: Dict[str, str] = field(default_factory=dict)
    settings: Dict[str, Any] = field(default_factory=dict)
    rate_limit: int = 0  # requests per minute, 0 = unlimited
    retry_count: int = 3
    timeout: int = 30
    
    @classmethod
    def from_env(cls, name: str, connector_type: ConnectorType, 
                 env_prefix: str, required_vars: List[str] = None) -> "ConnectorConfig":
        """Create config from environment variables."""
        credentials = {}
        for var in (required_vars or []):
            env_key = f"{env_prefix}_{var}".upper()
            value = os.environ.get(env_key)
            if value:
                credentials[var] = value
        
        return cls(
            name=name,
            connector_type=connector_type,
            credentials=credentials,
            enabled=bool(credentials)
        )
    
    def to_dict(self) -> Dict:
        return {
            "name": self.name,
            "connector_type": self.connector_type.value,
            "enabled": self.enabled,
            "settings": self.settings,
            "rate_limit": self.rate_limit
        }


class BaseConnector(ABC):
    """Abstract base class for all connectors."""
    
    # Class attributes to be overridden
    NAME: str = "base"
    TYPE: ConnectorType = ConnectorType.DATA_SOURCE
    DESCRIPTION: str = "Base connector"
    REQUIRED_CREDENTIALS: List[str] = []
    OPTIONAL_CREDENTIALS: List[str] = []
    OPEN_SOURCE: bool = True  # Whether this is fully open source
    
    def __init__(self, config: ConnectorConfig = None):
        self.config = config or ConnectorConfig(
            name=self.NAME,
            connector_type=self.TYPE
        )
        self.status = ConnectorStatus.DISCONNECTED
        self._client = None
        self._last_request = None
        self._request_count = 0
    
    @abstractmethod
    async def connect(self) -> bool:
        """Establish connection to the service."""
        pass
    
    @abstractmethod
    async def disconnect(self) -> bool:
        """Disconnect from the service."""
        pass
    
    @abstractmethod
    async def health_check(self) -> bool:
        """Check if the connection is healthy."""
        pass
    
    def is_configured(self) -> bool:
        """Check if all required credentials are present."""
        for cred in self.REQUIRED_CREDENTIALS:
            if cred not in self.config.credentials:
                return False
        return True
    
    def get_missing_credentials(self) -> List[str]:
        """Get list of missing required credentials."""
        return [
            cred for cred in self.REQUIRED_CREDENTIALS
            if cred not in self.config.credentials
        ]
    
    async def _rate_limit_check(self):
        """Check and enforce rate limiting."""
        if self.config.rate_limit <= 0:
            return
        
        now = datetime.now()
        if self._last_request:
            elapsed = (now - self._last_request).total_seconds()
            min_interval = 60.0 / self.config.rate_limit
            
            if elapsed < min_interval:
                import asyncio
                await asyncio.sleep(min_interval - elapsed)
        
        self._last_request = datetime.now()
        self._request_count += 1
    
    def get_info(self) -> Dict:
        """Get connector information."""
        return {
            "name": self.NAME,
            "type": self.TYPE.value,
            "description": self.DESCRIPTION,
            "status": self.status.value,
            "configured": self.is_configured(),
            "open_source": self.OPEN_SOURCE,
            "required_credentials": self.REQUIRED_CREDENTIALS,
            "optional_credentials": self.OPTIONAL_CREDENTIALS,
            "missing_credentials": self.get_missing_credentials()
        }


class ConnectorRegistry:
    """Registry for managing all connectors."""
    
    _instance = None
    
    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
            cls._instance._connectors: Dict[str, BaseConnector] = {}
            cls._instance._connector_classes: Dict[str, Type[BaseConnector]] = {}
        return cls._instance
    
    def register_class(self, connector_class: Type[BaseConnector]):
        """Register a connector class."""
        self._connector_classes[connector_class.NAME] = connector_class
        logger.info(f"Registered connector class: {connector_class.NAME}")
    
    def create(self, name: str, config: ConnectorConfig = None) -> Optional[BaseConnector]:
        """Create and register a connector instance."""
        if name not in self._connector_classes:
            logger.error(f"Unknown connector: {name}")
            return None
        
        connector = self._connector_classes[name](config)
        self._connectors[name] = connector
        return connector
    
    def get(self, name: str) -> Optional[BaseConnector]:
        """Get a connector by name."""
        return self._connectors.get(name)
    
    def get_by_type(self, connector_type: ConnectorType) -> List[BaseConnector]:
        """Get all connectors of a specific type."""
        return [
            c for c in self._connectors.values()
            if c.TYPE == connector_type
        ]
    
    def list_all(self) -> List[Dict]:
        """List all registered connectors."""
        return [c.get_info() for c in self._connectors.values()]
    
    def list_available(self) -> List[Dict]:
        """List all available connector classes."""
        return [
            {
                "name": cls.NAME,
                "type": cls.TYPE.value,
                "description": cls.DESCRIPTION,
                "open_source": cls.OPEN_SOURCE,
                "required_credentials": cls.REQUIRED_CREDENTIALS
            }
            for cls in self._connector_classes.values()
        ]
    
    async def connect_all(self) -> Dict[str, bool]:
        """Connect all configured connectors."""
        results = {}
        for name, connector in self._connectors.items():
            if connector.is_configured():
                try:
                    results[name] = await connector.connect()
                except Exception as e:
                    logger.error(f"Failed to connect {name}: {e}")
                    results[name] = False
            else:
                results[name] = False
        return results
    
    async def disconnect_all(self) -> Dict[str, bool]:
        """Disconnect all connectors."""
        results = {}
        for name, connector in self._connectors.items():
            try:
                results[name] = await connector.disconnect()
            except Exception as e:
                logger.error(f"Failed to disconnect {name}: {e}")
                results[name] = False
        return results
    
    async def health_check_all(self) -> Dict[str, bool]:
        """Health check all connectors."""
        results = {}
        for name, connector in self._connectors.items():
            try:
                results[name] = await connector.health_check()
            except Exception as e:
                logger.error(f"Health check failed for {name}: {e}")
                results[name] = False
        return results
    
    def export_config(self, path: str):
        """Export connector configurations."""
        configs = {
            name: connector.config.to_dict()
            for name, connector in self._connectors.items()
        }
        
        with open(path, 'w') as f:
            json.dump(configs, f, indent=2)
    
    def import_config(self, path: str):
        """Import connector configurations."""
        with open(path, 'r') as f:
            configs = json.load(f)
        
        for name, config_dict in configs.items():
            if name in self._connector_classes:
                config = ConnectorConfig(
                    name=config_dict["name"],
                    connector_type=ConnectorType(config_dict["connector_type"]),
                    enabled=config_dict.get("enabled", True),
                    settings=config_dict.get("settings", {}),
                    rate_limit=config_dict.get("rate_limit", 0)
                )
                self.create(name, config)


# Global registry instance
registry = ConnectorRegistry()
