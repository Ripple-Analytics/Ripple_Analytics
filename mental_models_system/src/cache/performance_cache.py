"""
Performance Cache and Optimization Layer

Multi-level caching system for the Mental Models System.
Supports in-memory, disk, and Redis caching with automatic
invalidation and performance metrics.
"""

import os
import json
import time
import hashlib
import pickle
import asyncio
import functools
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, List, Optional, Any, Callable, TypeVar, Union
from dataclasses import dataclass, field, asdict
from collections import OrderedDict
from threading import Lock
import logging

logger = logging.getLogger(__name__)

T = TypeVar('T')


@dataclass
class CacheEntry:
    """A single cache entry."""
    key: str
    value: Any
    created_at: float
    expires_at: Optional[float] = None
    hits: int = 0
    last_accessed: float = field(default_factory=time.time)
    size_bytes: int = 0
    
    @property
    def is_expired(self) -> bool:
        if self.expires_at is None:
            return False
        return time.time() > self.expires_at
    
    def access(self):
        self.hits += 1
        self.last_accessed = time.time()


@dataclass
class CacheStats:
    """Cache statistics."""
    hits: int = 0
    misses: int = 0
    evictions: int = 0
    size_bytes: int = 0
    entry_count: int = 0
    oldest_entry: Optional[float] = None
    newest_entry: Optional[float] = None
    
    @property
    def hit_rate(self) -> float:
        total = self.hits + self.misses
        return self.hits / total if total > 0 else 0.0
    
    def to_dict(self) -> Dict:
        return {
            **asdict(self),
            'hit_rate': self.hit_rate
        }


class LRUCache:
    """
    Thread-safe LRU (Least Recently Used) cache.
    
    Features:
    - Configurable max size (entries or bytes)
    - TTL support
    - Thread-safe operations
    - Automatic eviction
    """
    
    def __init__(
        self,
        max_entries: int = 1000,
        max_size_bytes: int = 100 * 1024 * 1024,  # 100MB
        default_ttl: int = 3600  # 1 hour
    ):
        self.max_entries = max_entries
        self.max_size_bytes = max_size_bytes
        self.default_ttl = default_ttl
        
        self._cache: OrderedDict[str, CacheEntry] = OrderedDict()
        self._lock = Lock()
        self._stats = CacheStats()
    
    def _estimate_size(self, value: Any) -> int:
        """Estimate the size of a value in bytes."""
        try:
            return len(pickle.dumps(value))
        except:
            return 1024  # Default estimate
    
    def _evict_if_needed(self):
        """Evict entries if cache is over capacity."""
        # Evict by entry count
        while len(self._cache) > self.max_entries:
            oldest_key = next(iter(self._cache))
            entry = self._cache.pop(oldest_key)
            self._stats.size_bytes -= entry.size_bytes
            self._stats.evictions += 1
        
        # Evict by size
        while self._stats.size_bytes > self.max_size_bytes and self._cache:
            oldest_key = next(iter(self._cache))
            entry = self._cache.pop(oldest_key)
            self._stats.size_bytes -= entry.size_bytes
            self._stats.evictions += 1
    
    def get(self, key: str) -> Optional[Any]:
        """Get a value from the cache."""
        with self._lock:
            entry = self._cache.get(key)
            
            if entry is None:
                self._stats.misses += 1
                return None
            
            if entry.is_expired:
                del self._cache[key]
                self._stats.size_bytes -= entry.size_bytes
                self._stats.misses += 1
                return None
            
            # Move to end (most recently used)
            self._cache.move_to_end(key)
            entry.access()
            self._stats.hits += 1
            
            return entry.value
    
    def set(
        self,
        key: str,
        value: Any,
        ttl: int = None
    ):
        """Set a value in the cache."""
        with self._lock:
            # Remove existing entry if present
            if key in self._cache:
                old_entry = self._cache.pop(key)
                self._stats.size_bytes -= old_entry.size_bytes
            
            # Create new entry
            size = self._estimate_size(value)
            now = time.time()
            expires_at = now + (ttl or self.default_ttl) if (ttl or self.default_ttl) else None
            
            entry = CacheEntry(
                key=key,
                value=value,
                created_at=now,
                expires_at=expires_at,
                size_bytes=size
            )
            
            self._cache[key] = entry
            self._stats.size_bytes += size
            
            # Update stats
            if self._stats.oldest_entry is None or now < self._stats.oldest_entry:
                self._stats.oldest_entry = now
            self._stats.newest_entry = now
            
            # Evict if needed
            self._evict_if_needed()
    
    def delete(self, key: str) -> bool:
        """Delete a key from the cache."""
        with self._lock:
            if key in self._cache:
                entry = self._cache.pop(key)
                self._stats.size_bytes -= entry.size_bytes
                return True
            return False
    
    def clear(self):
        """Clear all entries from the cache."""
        with self._lock:
            self._cache.clear()
            self._stats = CacheStats()
    
    def get_stats(self) -> CacheStats:
        """Get cache statistics."""
        with self._lock:
            self._stats.entry_count = len(self._cache)
            return CacheStats(**asdict(self._stats))
    
    def cleanup_expired(self) -> int:
        """Remove all expired entries. Returns count of removed entries."""
        with self._lock:
            expired_keys = [
                key for key, entry in self._cache.items()
                if entry.is_expired
            ]
            for key in expired_keys:
                entry = self._cache.pop(key)
                self._stats.size_bytes -= entry.size_bytes
            return len(expired_keys)


class DiskCache:
    """
    Disk-based cache for larger or persistent data.
    
    Features:
    - Persistent storage
    - Automatic cleanup
    - Size limits
    """
    
    def __init__(
        self,
        cache_dir: str = None,
        max_size_bytes: int = 1024 * 1024 * 1024,  # 1GB
        default_ttl: int = 86400  # 24 hours
    ):
        self.cache_dir = Path(cache_dir or '/tmp/mental_models_cache')
        self.cache_dir.mkdir(parents=True, exist_ok=True)
        self.max_size_bytes = max_size_bytes
        self.default_ttl = default_ttl
        
        self._index_file = self.cache_dir / 'index.json'
        self._index: Dict[str, Dict] = self._load_index()
        self._stats = CacheStats()
    
    def _load_index(self) -> Dict[str, Dict]:
        """Load the cache index from disk."""
        if self._index_file.exists():
            try:
                with open(self._index_file, 'r') as f:
                    return json.load(f)
            except:
                pass
        return {}
    
    def _save_index(self):
        """Save the cache index to disk."""
        try:
            with open(self._index_file, 'w') as f:
                json.dump(self._index, f)
        except Exception as e:
            logger.error(f"Failed to save cache index: {e}")
    
    def _get_cache_path(self, key: str) -> Path:
        """Get the file path for a cache key."""
        key_hash = hashlib.sha256(key.encode()).hexdigest()
        return self.cache_dir / f"{key_hash}.cache"
    
    def get(self, key: str) -> Optional[Any]:
        """Get a value from the disk cache."""
        if key not in self._index:
            self._stats.misses += 1
            return None
        
        meta = self._index[key]
        
        # Check expiration
        if meta.get('expires_at') and time.time() > meta['expires_at']:
            self.delete(key)
            self._stats.misses += 1
            return None
        
        cache_path = self._get_cache_path(key)
        if not cache_path.exists():
            del self._index[key]
            self._save_index()
            self._stats.misses += 1
            return None
        
        try:
            with open(cache_path, 'rb') as f:
                value = pickle.load(f)
            
            # Update access time
            self._index[key]['last_accessed'] = time.time()
            self._index[key]['hits'] = self._index[key].get('hits', 0) + 1
            self._save_index()
            
            self._stats.hits += 1
            return value
        except Exception as e:
            logger.error(f"Failed to read cache file: {e}")
            self._stats.misses += 1
            return None
    
    def set(self, key: str, value: Any, ttl: int = None):
        """Set a value in the disk cache."""
        cache_path = self._get_cache_path(key)
        
        try:
            with open(cache_path, 'wb') as f:
                pickle.dump(value, f)
            
            size = cache_path.stat().st_size
            now = time.time()
            
            self._index[key] = {
                'created_at': now,
                'expires_at': now + (ttl or self.default_ttl) if (ttl or self.default_ttl) else None,
                'size_bytes': size,
                'hits': 0,
                'last_accessed': now
            }
            self._save_index()
            
            # Cleanup if over size limit
            self._cleanup_if_needed()
            
        except Exception as e:
            logger.error(f"Failed to write cache file: {e}")
    
    def delete(self, key: str) -> bool:
        """Delete a key from the disk cache."""
        if key not in self._index:
            return False
        
        cache_path = self._get_cache_path(key)
        if cache_path.exists():
            cache_path.unlink()
        
        del self._index[key]
        self._save_index()
        return True
    
    def _cleanup_if_needed(self):
        """Remove old entries if cache is over size limit."""
        total_size = sum(meta.get('size_bytes', 0) for meta in self._index.values())
        
        if total_size <= self.max_size_bytes:
            return
        
        # Sort by last accessed time
        sorted_keys = sorted(
            self._index.keys(),
            key=lambda k: self._index[k].get('last_accessed', 0)
        )
        
        # Remove oldest until under limit
        for key in sorted_keys:
            if total_size <= self.max_size_bytes:
                break
            
            size = self._index[key].get('size_bytes', 0)
            self.delete(key)
            total_size -= size
            self._stats.evictions += 1
    
    def cleanup_expired(self) -> int:
        """Remove all expired entries."""
        now = time.time()
        expired_keys = [
            key for key, meta in self._index.items()
            if meta.get('expires_at') and now > meta['expires_at']
        ]
        
        for key in expired_keys:
            self.delete(key)
        
        return len(expired_keys)
    
    def get_stats(self) -> CacheStats:
        """Get cache statistics."""
        total_size = sum(meta.get('size_bytes', 0) for meta in self._index.values())
        
        created_times = [meta.get('created_at', 0) for meta in self._index.values()]
        
        return CacheStats(
            hits=self._stats.hits,
            misses=self._stats.misses,
            evictions=self._stats.evictions,
            size_bytes=total_size,
            entry_count=len(self._index),
            oldest_entry=min(created_times) if created_times else None,
            newest_entry=max(created_times) if created_times else None
        )
    
    def clear(self):
        """Clear all entries from the cache."""
        for key in list(self._index.keys()):
            self.delete(key)
        self._stats = CacheStats()


class MultiLevelCache:
    """
    Multi-level cache combining memory and disk caching.
    
    L1: In-memory LRU cache (fast, limited size)
    L2: Disk cache (slower, larger capacity)
    L3: Optional Redis (distributed, persistent)
    """
    
    def __init__(
        self,
        l1_max_entries: int = 1000,
        l1_max_size_bytes: int = 100 * 1024 * 1024,
        l2_max_size_bytes: int = 1024 * 1024 * 1024,
        l2_cache_dir: str = None,
        default_ttl: int = 3600,
        redis_client: Any = None
    ):
        self.l1 = LRUCache(
            max_entries=l1_max_entries,
            max_size_bytes=l1_max_size_bytes,
            default_ttl=default_ttl
        )
        
        self.l2 = DiskCache(
            cache_dir=l2_cache_dir,
            max_size_bytes=l2_max_size_bytes,
            default_ttl=default_ttl * 24  # Disk cache lives longer
        )
        
        self.redis = redis_client
        self.default_ttl = default_ttl
    
    def get(self, key: str) -> Optional[Any]:
        """Get a value, checking each cache level."""
        # Try L1 first
        value = self.l1.get(key)
        if value is not None:
            return value
        
        # Try L2
        value = self.l2.get(key)
        if value is not None:
            # Promote to L1
            self.l1.set(key, value)
            return value
        
        # Try Redis if available
        if self.redis:
            try:
                cached = self.redis.get(f"mms:{key}")
                if cached:
                    value = pickle.loads(cached)
                    # Promote to L1 and L2
                    self.l1.set(key, value)
                    self.l2.set(key, value)
                    return value
            except Exception as e:
                logger.warning(f"Redis get failed: {e}")
        
        return None
    
    def set(self, key: str, value: Any, ttl: int = None, levels: List[int] = None):
        """
        Set a value in the cache.
        
        Args:
            key: Cache key
            value: Value to cache
            ttl: Time to live in seconds
            levels: Which cache levels to use (default: all)
        """
        levels = levels or [1, 2, 3]
        ttl = ttl or self.default_ttl
        
        if 1 in levels:
            self.l1.set(key, value, ttl)
        
        if 2 in levels:
            self.l2.set(key, value, ttl * 24)
        
        if 3 in levels and self.redis:
            try:
                self.redis.setex(
                    f"mms:{key}",
                    ttl * 24 * 7,  # Redis lives longest
                    pickle.dumps(value)
                )
            except Exception as e:
                logger.warning(f"Redis set failed: {e}")
    
    def delete(self, key: str):
        """Delete a key from all cache levels."""
        self.l1.delete(key)
        self.l2.delete(key)
        
        if self.redis:
            try:
                self.redis.delete(f"mms:{key}")
            except Exception as e:
                logger.warning(f"Redis delete failed: {e}")
    
    def clear(self):
        """Clear all cache levels."""
        self.l1.clear()
        self.l2.clear()
        
        if self.redis:
            try:
                # Delete all keys with our prefix
                keys = self.redis.keys("mms:*")
                if keys:
                    self.redis.delete(*keys)
            except Exception as e:
                logger.warning(f"Redis clear failed: {e}")
    
    def get_stats(self) -> Dict[str, Any]:
        """Get statistics for all cache levels."""
        return {
            'l1': self.l1.get_stats().to_dict(),
            'l2': self.l2.get_stats().to_dict(),
            'redis_available': self.redis is not None
        }
    
    def cleanup_expired(self) -> Dict[str, int]:
        """Remove expired entries from all levels."""
        return {
            'l1': self.l1.cleanup_expired(),
            'l2': self.l2.cleanup_expired()
        }
    
    def cached(self, ttl: int = None, key_prefix: str = ""):
        """Return a decorator to cache function results."""
        return cached(cache=self, ttl=ttl or self.default_ttl, key_prefix=key_prefix)


# =============================================================================
# DECORATORS
# =============================================================================

def cached(
    cache: Union[LRUCache, DiskCache, MultiLevelCache] = None,
    ttl: int = 3600,
    key_prefix: str = ""
):
    """
    Decorator to cache function results.
    
    Args:
        cache: Cache instance to use (creates default if None)
        ttl: Time to live in seconds
        key_prefix: Prefix for cache keys
    """
    _cache = cache or LRUCache()
    
    def decorator(func: Callable[..., T]) -> Callable[..., T]:
        @functools.wraps(func)
        def wrapper(*args, **kwargs) -> T:
            # Generate cache key
            key_parts = [key_prefix, func.__name__]
            key_parts.extend(str(arg) for arg in args)
            key_parts.extend(f"{k}={v}" for k, v in sorted(kwargs.items()))
            cache_key = hashlib.sha256(":".join(key_parts).encode()).hexdigest()
            
            # Try to get from cache
            result = _cache.get(cache_key)
            if result is not None:
                return result
            
            # Call function and cache result
            result = func(*args, **kwargs)
            _cache.set(cache_key, result, ttl)
            
            return result
        
        return wrapper
    return decorator


def async_cached(
    cache: Union[LRUCache, DiskCache, MultiLevelCache] = None,
    ttl: int = 3600,
    key_prefix: str = ""
):
    """
    Decorator to cache async function results.
    """
    _cache = cache or LRUCache()
    
    def decorator(func: Callable[..., T]) -> Callable[..., T]:
        @functools.wraps(func)
        async def wrapper(*args, **kwargs) -> T:
            # Generate cache key
            key_parts = [key_prefix, func.__name__]
            key_parts.extend(str(arg) for arg in args)
            key_parts.extend(f"{k}={v}" for k, v in sorted(kwargs.items()))
            cache_key = hashlib.sha256(":".join(key_parts).encode()).hexdigest()
            
            # Try to get from cache
            result = _cache.get(cache_key)
            if result is not None:
                return result
            
            # Call function and cache result
            result = await func(*args, **kwargs)
            _cache.set(cache_key, result, ttl)
            
            return result
        
        return wrapper
    return decorator


# =============================================================================
# GLOBAL CACHE INSTANCE
# =============================================================================

_global_cache: Optional[MultiLevelCache] = None


def get_cache() -> MultiLevelCache:
    """Get the global cache instance."""
    global _global_cache
    if _global_cache is None:
        _global_cache = MultiLevelCache()
    return _global_cache


def init_cache(
    l1_max_entries: int = 1000,
    l1_max_size_bytes: int = 100 * 1024 * 1024,
    l2_max_size_bytes: int = 1024 * 1024 * 1024,
    l2_cache_dir: str = None,
    redis_url: str = None
) -> MultiLevelCache:
    """Initialize the global cache with custom settings."""
    global _global_cache
    
    redis_client = None
    if redis_url:
        try:
            import redis
            redis_client = redis.from_url(redis_url)
        except Exception as e:
            logger.warning(f"Failed to connect to Redis: {e}")
    
    _global_cache = MultiLevelCache(
        l1_max_entries=l1_max_entries,
        l1_max_size_bytes=l1_max_size_bytes,
        l2_max_size_bytes=l2_max_size_bytes,
        l2_cache_dir=l2_cache_dir,
        redis_client=redis_client
    )
    
    return _global_cache
