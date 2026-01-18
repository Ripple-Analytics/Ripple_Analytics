"""
Redis Caching Layer.

Provides caching for:
- LLM responses (expensive API calls)
- Analysis results
- Search queries
- Computed metrics

Features:
- TTL-based expiration
- LRU eviction
- Compression for large values
- Fallback to in-memory cache if Redis unavailable
"""

import json
import hashlib
import zlib
import time
from typing import Any, Optional, Dict, Callable, TypeVar
from functools import wraps
from dataclasses import dataclass
from datetime import datetime, timedelta
import os

T = TypeVar('T')


@dataclass
class CacheConfig:
    """Cache configuration."""
    host: str = "localhost"
    port: int = 6379
    db: int = 0
    password: Optional[str] = None
    default_ttl: int = 3600  # 1 hour
    max_memory: str = "256mb"
    compression_threshold: int = 1024  # Compress values > 1KB
    prefix: str = "mms:"  # Mental Models System prefix


class CacheStats:
    """Track cache statistics."""
    
    def __init__(self):
        self.hits = 0
        self.misses = 0
        self.sets = 0
        self.deletes = 0
        self.errors = 0
        self.compression_saves = 0
        self.start_time = datetime.now()
    
    @property
    def hit_rate(self) -> float:
        total = self.hits + self.misses
        return self.hits / total if total > 0 else 0.0
    
    def to_dict(self) -> Dict:
        return {
            "hits": self.hits,
            "misses": self.misses,
            "sets": self.sets,
            "deletes": self.deletes,
            "errors": self.errors,
            "hit_rate": f"{self.hit_rate:.2%}",
            "compression_saves": self.compression_saves,
            "uptime_seconds": (datetime.now() - self.start_time).total_seconds()
        }


class InMemoryCache:
    """Fallback in-memory cache when Redis is unavailable."""
    
    def __init__(self, max_size: int = 1000):
        self.cache: Dict[str, tuple] = {}  # key -> (value, expiry_time)
        self.max_size = max_size
        self.access_order: list = []  # For LRU eviction
    
    def get(self, key: str) -> Optional[Any]:
        if key in self.cache:
            value, expiry = self.cache[key]
            if expiry is None or time.time() < expiry:
                # Update access order for LRU
                if key in self.access_order:
                    self.access_order.remove(key)
                self.access_order.append(key)
                return value
            else:
                # Expired
                del self.cache[key]
                if key in self.access_order:
                    self.access_order.remove(key)
        return None
    
    def set(self, key: str, value: Any, ttl: Optional[int] = None):
        # Evict if at capacity
        while len(self.cache) >= self.max_size:
            if self.access_order:
                oldest = self.access_order.pop(0)
                if oldest in self.cache:
                    del self.cache[oldest]
        
        expiry = time.time() + ttl if ttl else None
        self.cache[key] = (value, expiry)
        if key in self.access_order:
            self.access_order.remove(key)
        self.access_order.append(key)
    
    def delete(self, key: str) -> bool:
        if key in self.cache:
            del self.cache[key]
            if key in self.access_order:
                self.access_order.remove(key)
            return True
        return False
    
    def clear(self):
        self.cache.clear()
        self.access_order.clear()
    
    def keys(self, pattern: str = "*") -> list:
        import fnmatch
        return [k for k in self.cache.keys() if fnmatch.fnmatch(k, pattern)]


class RedisCache:
    """
    Redis-based caching layer with fallback to in-memory cache.
    
    Usage:
        cache = RedisCache()
        
        # Direct usage
        cache.set("key", {"data": "value"}, ttl=3600)
        value = cache.get("key")
        
        # Decorator usage
        @cache.cached(ttl=3600)
        def expensive_function(arg):
            return compute_something(arg)
    """
    
    def __init__(self, config: CacheConfig = None):
        self.config = config or CacheConfig()
        self.stats = CacheStats()
        self.redis_client = None
        self.fallback_cache = InMemoryCache()
        
        # Try to connect to Redis
        self._connect()
    
    def _connect(self):
        """Attempt to connect to Redis."""
        try:
            import redis
            self.redis_client = redis.Redis(
                host=os.getenv("REDIS_HOST", self.config.host),
                port=int(os.getenv("REDIS_PORT", self.config.port)),
                db=self.config.db,
                password=os.getenv("REDIS_PASSWORD", self.config.password),
                decode_responses=False,  # We handle encoding ourselves
                socket_timeout=5,
                socket_connect_timeout=5
            )
            # Test connection
            self.redis_client.ping()
            self._using_redis = True
        except Exception as e:
            print(f"⚠️ Redis unavailable ({e}), using in-memory cache")
            self._using_redis = False
            self.redis_client = None
    
    def _make_key(self, key: str) -> str:
        """Create a prefixed key."""
        return f"{self.config.prefix}{key}"
    
    def _hash_key(self, *args, **kwargs) -> str:
        """Create a hash key from arguments."""
        key_data = json.dumps({"args": args, "kwargs": kwargs}, sort_keys=True)
        return hashlib.sha256(key_data.encode()).hexdigest()[:16]
    
    def _serialize(self, value: Any) -> bytes:
        """Serialize and optionally compress a value."""
        data = json.dumps(value).encode('utf-8')
        
        if len(data) > self.config.compression_threshold:
            compressed = zlib.compress(data)
            if len(compressed) < len(data):
                self.stats.compression_saves += 1
                return b'Z' + compressed  # Prefix with 'Z' to indicate compression
        
        return b'R' + data  # Prefix with 'R' for raw
    
    def _deserialize(self, data: bytes) -> Any:
        """Deserialize and optionally decompress a value."""
        if data[0:1] == b'Z':
            data = zlib.decompress(data[1:])
        else:
            data = data[1:]
        
        return json.loads(data.decode('utf-8'))
    
    def get(self, key: str) -> Optional[Any]:
        """Get a value from cache."""
        full_key = self._make_key(key)
        
        try:
            if self._using_redis:
                data = self.redis_client.get(full_key)
                if data:
                    self.stats.hits += 1
                    return self._deserialize(data)
            else:
                value = self.fallback_cache.get(full_key)
                if value is not None:
                    self.stats.hits += 1
                    return value
            
            self.stats.misses += 1
            return None
            
        except Exception as e:
            self.stats.errors += 1
            self.stats.misses += 1
            return None
    
    def set(self, key: str, value: Any, ttl: int = None) -> bool:
        """Set a value in cache."""
        full_key = self._make_key(key)
        ttl = ttl or self.config.default_ttl
        
        try:
            if self._using_redis:
                data = self._serialize(value)
                self.redis_client.setex(full_key, ttl, data)
            else:
                self.fallback_cache.set(full_key, value, ttl)
            
            self.stats.sets += 1
            return True
            
        except Exception as e:
            self.stats.errors += 1
            return False
    
    def delete(self, key: str) -> bool:
        """Delete a value from cache."""
        full_key = self._make_key(key)
        
        try:
            if self._using_redis:
                result = self.redis_client.delete(full_key)
            else:
                result = self.fallback_cache.delete(full_key)
            
            if result:
                self.stats.deletes += 1
            return bool(result)
            
        except Exception as e:
            self.stats.errors += 1
            return False
    
    def clear(self, pattern: str = "*") -> int:
        """Clear cache entries matching pattern."""
        full_pattern = self._make_key(pattern)
        count = 0
        
        try:
            if self._using_redis:
                keys = self.redis_client.keys(full_pattern)
                if keys:
                    count = self.redis_client.delete(*keys)
            else:
                keys = self.fallback_cache.keys(full_pattern)
                for key in keys:
                    if self.fallback_cache.delete(key):
                        count += 1
            
            self.stats.deletes += count
            return count
            
        except Exception as e:
            self.stats.errors += 1
            return 0
    
    def cached(
        self, 
        ttl: int = None, 
        key_prefix: str = "",
        key_builder: Callable = None
    ):
        """
        Decorator to cache function results.
        
        Args:
            ttl: Time to live in seconds
            key_prefix: Prefix for cache key
            key_builder: Custom function to build cache key
        """
        def decorator(func: Callable[..., T]) -> Callable[..., T]:
            @wraps(func)
            def wrapper(*args, **kwargs) -> T:
                # Build cache key
                if key_builder:
                    cache_key = key_builder(*args, **kwargs)
                else:
                    cache_key = f"{key_prefix}{func.__name__}:{self._hash_key(*args, **kwargs)}"
                
                # Try to get from cache
                cached_value = self.get(cache_key)
                if cached_value is not None:
                    return cached_value
                
                # Compute and cache
                result = func(*args, **kwargs)
                self.set(cache_key, result, ttl)
                
                return result
            
            return wrapper
        return decorator
    
    def get_stats(self) -> Dict:
        """Get cache statistics."""
        stats = self.stats.to_dict()
        stats["using_redis"] = self._using_redis
        
        if self._using_redis:
            try:
                info = self.redis_client.info("memory")
                stats["redis_memory_used"] = info.get("used_memory_human", "unknown")
            except:
                pass
        else:
            stats["in_memory_entries"] = len(self.fallback_cache.cache)
        
        return stats


# =============================================================================
# SPECIALIZED CACHES
# =============================================================================

class LLMCache(RedisCache):
    """
    Specialized cache for LLM responses.
    
    Features:
    - Longer TTL for expensive LLM calls
    - Semantic key building
    - Response validation
    """
    
    def __init__(self, config: CacheConfig = None):
        config = config or CacheConfig()
        config.default_ttl = 86400  # 24 hours for LLM responses
        config.prefix = "mms:llm:"
        super().__init__(config)
    
    def _build_llm_key(self, model: str, prompt: str, **kwargs) -> str:
        """Build a cache key for LLM requests."""
        key_data = {
            "model": model,
            "prompt": prompt[:500],  # Truncate long prompts
            "params": {k: v for k, v in kwargs.items() if k in ["temperature", "max_tokens"]}
        }
        return hashlib.sha256(json.dumps(key_data, sort_keys=True).encode()).hexdigest()[:24]
    
    def get_llm_response(self, model: str, prompt: str, **kwargs) -> Optional[str]:
        """Get cached LLM response."""
        key = self._build_llm_key(model, prompt, **kwargs)
        return self.get(key)
    
    def set_llm_response(self, model: str, prompt: str, response: str, **kwargs) -> bool:
        """Cache an LLM response."""
        key = self._build_llm_key(model, prompt, **kwargs)
        return self.set(key, response)


class AnalysisCache(RedisCache):
    """
    Specialized cache for analysis results.
    
    Features:
    - Medium TTL for analysis results
    - Document-based keys
    - Incremental caching
    """
    
    def __init__(self, config: CacheConfig = None):
        config = config or CacheConfig()
        config.default_ttl = 3600  # 1 hour for analysis
        config.prefix = "mms:analysis:"
        super().__init__(config)
    
    def _build_analysis_key(self, doc_hash: str, analysis_type: str) -> str:
        """Build a cache key for analysis results."""
        return f"{analysis_type}:{doc_hash}"
    
    def get_analysis(self, doc_hash: str, analysis_type: str) -> Optional[Dict]:
        """Get cached analysis result."""
        key = self._build_analysis_key(doc_hash, analysis_type)
        return self.get(key)
    
    def set_analysis(self, doc_hash: str, analysis_type: str, result: Dict) -> bool:
        """Cache an analysis result."""
        key = self._build_analysis_key(doc_hash, analysis_type)
        return self.set(key, result)


class SearchCache(RedisCache):
    """
    Specialized cache for search results.
    
    Features:
    - Short TTL for search results
    - Query normalization
    - Result pagination caching
    """
    
    def __init__(self, config: CacheConfig = None):
        config = config or CacheConfig()
        config.default_ttl = 300  # 5 minutes for search
        config.prefix = "mms:search:"
        super().__init__(config)
    
    def _normalize_query(self, query: str) -> str:
        """Normalize a search query."""
        return " ".join(query.lower().split())
    
    def _build_search_key(self, query: str, page: int = 0, limit: int = 10) -> str:
        """Build a cache key for search results."""
        normalized = self._normalize_query(query)
        return f"{hashlib.md5(normalized.encode()).hexdigest()[:12]}:{page}:{limit}"
    
    def get_search_results(self, query: str, page: int = 0, limit: int = 10) -> Optional[Dict]:
        """Get cached search results."""
        key = self._build_search_key(query, page, limit)
        return self.get(key)
    
    def set_search_results(self, query: str, results: Dict, page: int = 0, limit: int = 10) -> bool:
        """Cache search results."""
        key = self._build_search_key(query, page, limit)
        return self.set(key, results)


# =============================================================================
# GLOBAL CACHE INSTANCES
# =============================================================================

_cache_instance: Optional[RedisCache] = None
_llm_cache_instance: Optional[LLMCache] = None
_analysis_cache_instance: Optional[AnalysisCache] = None
_search_cache_instance: Optional[SearchCache] = None


def get_cache() -> RedisCache:
    """Get the global cache instance."""
    global _cache_instance
    if _cache_instance is None:
        _cache_instance = RedisCache()
    return _cache_instance


def get_llm_cache() -> LLMCache:
    """Get the global LLM cache instance."""
    global _llm_cache_instance
    if _llm_cache_instance is None:
        _llm_cache_instance = LLMCache()
    return _llm_cache_instance


def get_analysis_cache() -> AnalysisCache:
    """Get the global analysis cache instance."""
    global _analysis_cache_instance
    if _analysis_cache_instance is None:
        _analysis_cache_instance = AnalysisCache()
    return _analysis_cache_instance


def get_search_cache() -> SearchCache:
    """Get the global search cache instance."""
    global _search_cache_instance
    if _search_cache_instance is None:
        _search_cache_instance = SearchCache()
    return _search_cache_instance


# =============================================================================
# CONVENIENCE DECORATORS
# =============================================================================

def cached_llm(model: str = "default"):
    """Decorator for caching LLM calls."""
    def decorator(func):
        @wraps(func)
        def wrapper(prompt: str, *args, **kwargs):
            cache = get_llm_cache()
            
            # Check cache
            cached = cache.get_llm_response(model, prompt, **kwargs)
            if cached is not None:
                return cached
            
            # Call function
            result = func(prompt, *args, **kwargs)
            
            # Cache result
            if result:
                cache.set_llm_response(model, prompt, result, **kwargs)
            
            return result
        return wrapper
    return decorator


def cached_analysis(analysis_type: str):
    """Decorator for caching analysis results."""
    def decorator(func):
        @wraps(func)
        def wrapper(doc_hash: str, *args, **kwargs):
            cache = get_analysis_cache()
            
            # Check cache
            cached = cache.get_analysis(doc_hash, analysis_type)
            if cached is not None:
                return cached
            
            # Call function
            result = func(doc_hash, *args, **kwargs)
            
            # Cache result
            if result:
                cache.set_analysis(doc_hash, analysis_type, result)
            
            return result
        return wrapper
    return decorator


if __name__ == "__main__":
    # Test the cache
    cache = RedisCache()
    
    print("Testing cache...")
    
    # Test basic operations
    cache.set("test_key", {"data": "test_value", "number": 42})
    result = cache.get("test_key")
    print(f"Get result: {result}")
    
    # Test decorator
    @cache.cached(ttl=60, key_prefix="test:")
    def expensive_function(x):
        print(f"Computing for {x}...")
        return x * 2
    
    print(f"First call: {expensive_function(21)}")
    print(f"Second call (cached): {expensive_function(21)}")
    
    # Print stats
    print(f"\nCache stats: {cache.get_stats()}")
