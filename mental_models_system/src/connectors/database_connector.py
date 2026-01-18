"""
Database connector for PostgreSQL, MySQL, and SQLite.
"""

from typing import Any, Dict, List, Optional, Union
import logging
from dataclasses import dataclass

from .base import BaseConnector, ConnectorConfig, ConnectorResult

logger = logging.getLogger(__name__)


@dataclass
class QueryResult:
    """Result from a database query."""
    rows: List[Dict[str, Any]]
    row_count: int
    columns: List[str]


class DatabaseConnector(BaseConnector):
    """Connector for database integration (PostgreSQL, MySQL, SQLite)."""
    
    def __init__(self, config: ConnectorConfig):
        super().__init__(config)
        self._connection = None
        self._db_type = config.settings.get("db_type", "postgresql")
        self._host = config.credentials.get("host", "localhost")
        self._port = config.credentials.get("port", 5432)
        self._database = config.credentials.get("database", "")
        self._user = config.credentials.get("user", "")
        self._password = config.credentials.get("password", "")
    
    async def connect(self) -> bool:
        """Establish connection to database."""
        try:
            if self._db_type == "postgresql":
                return await self._connect_postgresql()
            elif self._db_type == "mysql":
                return await self._connect_mysql()
            elif self._db_type == "sqlite":
                return await self._connect_sqlite()
            else:
                logger.error(f"Unsupported database type: {self._db_type}")
                return False
        except Exception as e:
            logger.error(f"Failed to connect to database: {e}")
            return False
    
    async def _connect_postgresql(self) -> bool:
        """Connect to PostgreSQL database."""
        try:
            import asyncpg
            self._connection = await asyncpg.connect(
                host=self._host,
                port=self._port,
                database=self._database,
                user=self._user,
                password=self._password
            )
            self._connected = True
            logger.info(f"Connected to PostgreSQL database: {self._database}")
            return True
        except ImportError:
            logger.error("asyncpg not installed. Install with: pip install asyncpg")
            return False
        except Exception as e:
            logger.error(f"PostgreSQL connection error: {e}")
            return False
    
    async def _connect_mysql(self) -> bool:
        """Connect to MySQL database."""
        try:
            import aiomysql
            self._connection = await aiomysql.connect(
                host=self._host,
                port=self._port,
                db=self._database,
                user=self._user,
                password=self._password
            )
            self._connected = True
            logger.info(f"Connected to MySQL database: {self._database}")
            return True
        except ImportError:
            logger.error("aiomysql not installed. Install with: pip install aiomysql")
            return False
        except Exception as e:
            logger.error(f"MySQL connection error: {e}")
            return False
    
    async def _connect_sqlite(self) -> bool:
        """Connect to SQLite database."""
        try:
            import aiosqlite
            db_path = self.config.credentials.get("path", ":memory:")
            self._connection = await aiosqlite.connect(db_path)
            self._connected = True
            logger.info(f"Connected to SQLite database: {db_path}")
            return True
        except ImportError:
            logger.error("aiosqlite not installed. Install with: pip install aiosqlite")
            return False
        except Exception as e:
            logger.error(f"SQLite connection error: {e}")
            return False
    
    async def disconnect(self) -> bool:
        """Close database connection."""
        try:
            if self._connection:
                await self._connection.close()
                self._connection = None
            self._connected = False
            return True
        except Exception as e:
            logger.error(f"Error disconnecting from database: {e}")
            return False
    
    async def fetch(self, query: Dict[str, Any]) -> ConnectorResult:
        """
        Fetch data from database.
        
        Query parameters:
        - sql: SQL query string
        - params: Query parameters (list or dict)
        - fetch_one: Return single row (default False)
        """
        if not self._connection:
            return ConnectorResult(success=False, error="Not connected")
        
        try:
            sql = query.get("sql", "")
            params = query.get("params", [])
            fetch_one = query.get("fetch_one", False)
            
            if self._db_type == "postgresql":
                if fetch_one:
                    row = await self._connection.fetchrow(sql, *params)
                    data = dict(row) if row else None
                else:
                    rows = await self._connection.fetch(sql, *params)
                    data = [dict(row) for row in rows]
            elif self._db_type == "sqlite":
                async with self._connection.execute(sql, params) as cursor:
                    if fetch_one:
                        row = await cursor.fetchone()
                        if row:
                            columns = [desc[0] for desc in cursor.description]
                            data = dict(zip(columns, row))
                        else:
                            data = None
                    else:
                        rows = await cursor.fetchall()
                        columns = [desc[0] for desc in cursor.description]
                        data = [dict(zip(columns, row)) for row in rows]
            else:
                return ConnectorResult(success=False, error=f"Fetch not implemented for {self._db_type}")
            
            return ConnectorResult(
                success=True,
                data=data,
                metadata={"sql": sql, "row_count": len(data) if isinstance(data, list) else 1}
            )
        except Exception as e:
            logger.error(f"Database fetch error: {e}")
            return ConnectorResult(success=False, error=str(e))
    
    async def push(self, data: Any) -> ConnectorResult:
        """
        Push data to database (execute INSERT/UPDATE/DELETE).
        
        Data parameters:
        - sql: SQL statement
        - params: Statement parameters
        - many: Execute many (for batch inserts)
        - values: List of value tuples (for executemany)
        """
        if not self._connection:
            return ConnectorResult(success=False, error="Not connected")
        
        try:
            sql = data.get("sql", "")
            params = data.get("params", [])
            many = data.get("many", False)
            values = data.get("values", [])
            
            if self._db_type == "postgresql":
                if many:
                    await self._connection.executemany(sql, values)
                else:
                    await self._connection.execute(sql, *params)
            elif self._db_type == "sqlite":
                if many:
                    await self._connection.executemany(sql, values)
                else:
                    await self._connection.execute(sql, params)
                await self._connection.commit()
            else:
                return ConnectorResult(success=False, error=f"Push not implemented for {self._db_type}")
            
            return ConnectorResult(
                success=True,
                metadata={"sql": sql, "rows_affected": len(values) if many else 1}
            )
        except Exception as e:
            logger.error(f"Database push error: {e}")
            return ConnectorResult(success=False, error=str(e))
    
    async def execute_query(self, sql: str, params: Optional[List] = None) -> ConnectorResult:
        """Execute a SELECT query."""
        return await self.fetch({"sql": sql, "params": params or []})
    
    async def execute_statement(self, sql: str, params: Optional[List] = None) -> ConnectorResult:
        """Execute an INSERT/UPDATE/DELETE statement."""
        return await self.push({"sql": sql, "params": params or []})
    
    async def insert_many(self, sql: str, values: List[tuple]) -> ConnectorResult:
        """Insert multiple rows."""
        return await self.push({"sql": sql, "many": True, "values": values})
