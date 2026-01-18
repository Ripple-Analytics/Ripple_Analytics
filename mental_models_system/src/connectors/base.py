"""
Base connector class and configuration for all connectors.
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional
from datetime import datetime
import logging

logger = logging.getLogger(__name__)


@dataclass
class ConnectorConfig:
    """Configuration for a connector."""
    name: str
    connector_type: str
    credentials: Dict[str, str] = field(default_factory=dict)
    settings: Dict[str, Any] = field(default_factory=dict)
    enabled: bool = True
    rate_limit: Optional[int] = None  # requests per minute
    retry_count: int = 3
    timeout: int = 30  # seconds


@dataclass
class ConnectorResult:
    """Result from a connector operation."""
    success: bool
    data: Any = None
    error: Optional[str] = None
    metadata: Dict[str, Any] = field(default_factory=dict)
    timestamp: datetime = field(default_factory=datetime.utcnow)


class BaseConnector(ABC):
    """Base class for all connectors."""
    
    def __init__(self, config: ConnectorConfig):
        self.config = config
        self.logger = logging.getLogger(f"{__name__}.{config.name}")
        self._connected = False
    
    @abstractmethod
    async def connect(self) -> bool:
        """Establish connection to the data source."""
        pass
    
    @abstractmethod
    async def disconnect(self) -> bool:
        """Close connection to the data source."""
        pass
    
    @abstractmethod
    async def fetch(self, query: Dict[str, Any]) -> ConnectorResult:
        """Fetch data from the source."""
        pass
    
    @abstractmethod
    async def push(self, data: Any) -> ConnectorResult:
        """Push data to the source (if supported)."""
        pass
    
    @property
    def is_connected(self) -> bool:
        return self._connected
    
    async def health_check(self) -> bool:
        """Check if the connector is healthy."""
        try:
            if not self._connected:
                await self.connect()
            return self._connected
        except Exception as e:
            self.logger.error(f"Health check failed: {e}")
            return False
    
    def __repr__(self) -> str:
        return f"{self.__class__.__name__}(name={self.config.name}, connected={self._connected})"


class ConnectorRegistry:
    """Registry for managing multiple connectors."""
    
    _instance = None
    
    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
            cls._instance._connectors: Dict[str, BaseConnector] = {}
        return cls._instance
    
    def register(self, connector: BaseConnector) -> None:
        """Register a connector."""
        self._connectors[connector.config.name] = connector
    
    def get(self, name: str) -> Optional[BaseConnector]:
        """Get a connector by name."""
        return self._connectors.get(name)
    
    def list_connectors(self) -> List[str]:
        """List all registered connector names."""
        return list(self._connectors.keys())
    
    async def connect_all(self) -> Dict[str, bool]:
        """Connect all registered connectors."""
        results = {}
        for name, connector in self._connectors.items():
            try:
                results[name] = await connector.connect()
            except Exception as e:
                logger.error(f"Failed to connect {name}: {e}")
                results[name] = False
        return results
    
    async def disconnect_all(self) -> Dict[str, bool]:
        """Disconnect all registered connectors."""
        results = {}
        for name, connector in self._connectors.items():
            try:
                results[name] = await connector.disconnect()
            except Exception as e:
                logger.error(f"Failed to disconnect {name}: {e}")
                results[name] = False
        return results
