"""
Rate Limiting Middleware

Production-grade rate limiting for the Mental Models System API:
- Token bucket algorithm
- Sliding window rate limiting
- Per-user and per-IP limits
- Redis backend for distributed systems
- In-memory fallback for single instance
"""

import asyncio
import time
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Callable, Dict, List, Optional, Tuple
import hashlib
import json


class RateLimitStrategy(Enum):
    """Rate limiting strategies."""
    TOKEN_BUCKET = "token_bucket"
    SLIDING_WINDOW = "sliding_window"
    FIXED_WINDOW = "fixed_window"
    LEAKY_BUCKET = "leaky_bucket"


@dataclass
class RateLimitConfig:
    """Configuration for rate limiting."""
    requests_per_minute: int = 60
    requests_per_hour: int = 1000
    requests_per_day: int = 10000
    burst_limit: int = 10
    strategy: RateLimitStrategy = RateLimitStrategy.TOKEN_BUCKET
    
    # Per-endpoint overrides
    endpoint_limits: Dict[str, int] = field(default_factory=dict)
    
    # Exempt paths
    exempt_paths: List[str] = field(default_factory=lambda: ["/health", "/metrics"])
    
    # Exempt API keys
    exempt_keys: List[str] = field(default_factory=list)


@dataclass
class RateLimitResult:
    """Result of a rate limit check."""
    allowed: bool
    remaining: int
    reset_at: datetime
    retry_after: Optional[int] = None
    limit: int = 0
    
    def to_headers(self) -> Dict[str, str]:
        """Convert to HTTP headers."""
        headers = {
            "X-RateLimit-Limit": str(self.limit),
            "X-RateLimit-Remaining": str(max(0, self.remaining)),
            "X-RateLimit-Reset": str(int(self.reset_at.timestamp()))
        }
        if self.retry_after:
            headers["Retry-After"] = str(self.retry_after)
        return headers


class RateLimitBackend(ABC):
    """Abstract backend for rate limit storage."""
    
    @abstractmethod
    async def get(self, key: str) -> Optional[Dict[str, Any]]:
        """Get rate limit data for a key."""
        pass
    
    @abstractmethod
    async def set(self, key: str, data: Dict[str, Any], ttl: int) -> None:
        """Set rate limit data for a key."""
        pass
    
    @abstractmethod
    async def increment(self, key: str, amount: int = 1) -> int:
        """Increment counter for a key."""
        pass
    
    @abstractmethod
    async def expire(self, key: str, ttl: int) -> None:
        """Set expiration for a key."""
        pass


class InMemoryBackend(RateLimitBackend):
    """In-memory rate limit backend for single instance deployments."""
    
    def __init__(self):
        self._data: Dict[str, Tuple[Dict[str, Any], float]] = {}
        self._lock = asyncio.Lock()
    
    async def get(self, key: str) -> Optional[Dict[str, Any]]:
        async with self._lock:
            if key in self._data:
                data, expires_at = self._data[key]
                if time.time() < expires_at:
                    return data
                else:
                    del self._data[key]
            return None
    
    async def set(self, key: str, data: Dict[str, Any], ttl: int) -> None:
        async with self._lock:
            self._data[key] = (data, time.time() + ttl)
    
    async def increment(self, key: str, amount: int = 1) -> int:
        async with self._lock:
            if key in self._data:
                data, expires_at = self._data[key]
                if time.time() < expires_at:
                    data["count"] = data.get("count", 0) + amount
                    self._data[key] = (data, expires_at)
                    return data["count"]
            # Key doesn't exist or expired
            self._data[key] = ({"count": amount}, time.time() + 60)
            return amount
    
    async def expire(self, key: str, ttl: int) -> None:
        async with self._lock:
            if key in self._data:
                data, _ = self._data[key]
                self._data[key] = (data, time.time() + ttl)
    
    def cleanup(self):
        """Remove expired entries."""
        now = time.time()
        expired = [k for k, (_, exp) in self._data.items() if now >= exp]
        for k in expired:
            del self._data[k]


class RedisBackend(RateLimitBackend):
    """Redis backend for distributed rate limiting."""
    
    def __init__(self, redis_url: str = "redis://localhost:6379"):
        self.redis_url = redis_url
        self._client = None
    
    async def _get_client(self):
        if self._client is None:
            try:
                import redis.asyncio as redis
                self._client = redis.from_url(self.redis_url)
            except ImportError:
                raise RuntimeError("redis package not installed. Install with: pip install redis")
        return self._client
    
    async def get(self, key: str) -> Optional[Dict[str, Any]]:
        client = await self._get_client()
        data = await client.get(f"ratelimit:{key}")
        if data:
            return json.loads(data)
        return None
    
    async def set(self, key: str, data: Dict[str, Any], ttl: int) -> None:
        client = await self._get_client()
        await client.setex(f"ratelimit:{key}", ttl, json.dumps(data))
    
    async def increment(self, key: str, amount: int = 1) -> int:
        client = await self._get_client()
        return await client.incrby(f"ratelimit:{key}", amount)
    
    async def expire(self, key: str, ttl: int) -> None:
        client = await self._get_client()
        await client.expire(f"ratelimit:{key}", ttl)


class TokenBucketLimiter:
    """Token bucket rate limiter implementation."""
    
    def __init__(self, backend: RateLimitBackend, config: RateLimitConfig):
        self.backend = backend
        self.config = config
        self.refill_rate = config.requests_per_minute / 60.0  # tokens per second
    
    async def check(self, identifier: str) -> RateLimitResult:
        """Check if request is allowed."""
        key = f"bucket:{identifier}"
        now = time.time()
        
        data = await self.backend.get(key)
        
        if data is None:
            # Initialize bucket
            data = {
                "tokens": self.config.burst_limit - 1,
                "last_update": now
            }
            await self.backend.set(key, data, 3600)
            
            return RateLimitResult(
                allowed=True,
                remaining=data["tokens"],
                reset_at=datetime.fromtimestamp(now + 60),
                limit=self.config.requests_per_minute
            )
        
        # Calculate token refill
        time_passed = now - data["last_update"]
        tokens_to_add = time_passed * self.refill_rate
        current_tokens = min(self.config.burst_limit, data["tokens"] + tokens_to_add)
        
        if current_tokens >= 1:
            # Allow request
            data["tokens"] = current_tokens - 1
            data["last_update"] = now
            await self.backend.set(key, data, 3600)
            
            return RateLimitResult(
                allowed=True,
                remaining=int(data["tokens"]),
                reset_at=datetime.fromtimestamp(now + 60),
                limit=self.config.requests_per_minute
            )
        else:
            # Rate limited
            wait_time = (1 - current_tokens) / self.refill_rate
            
            return RateLimitResult(
                allowed=False,
                remaining=0,
                reset_at=datetime.fromtimestamp(now + wait_time),
                retry_after=int(wait_time) + 1,
                limit=self.config.requests_per_minute
            )


class SlidingWindowLimiter:
    """Sliding window rate limiter implementation."""
    
    def __init__(self, backend: RateLimitBackend, config: RateLimitConfig):
        self.backend = backend
        self.config = config
    
    async def check(self, identifier: str, window_seconds: int = 60) -> RateLimitResult:
        """Check if request is allowed."""
        key = f"window:{identifier}:{int(time.time() // window_seconds)}"
        prev_key = f"window:{identifier}:{int(time.time() // window_seconds) - 1}"
        
        now = time.time()
        window_start = int(now // window_seconds) * window_seconds
        position_in_window = (now - window_start) / window_seconds
        
        # Get current and previous window counts
        current_data = await self.backend.get(key)
        prev_data = await self.backend.get(prev_key)
        
        current_count = current_data.get("count", 0) if current_data else 0
        prev_count = prev_data.get("count", 0) if prev_data else 0
        
        # Calculate weighted count
        weighted_count = prev_count * (1 - position_in_window) + current_count
        
        limit = self.config.requests_per_minute
        
        if weighted_count < limit:
            # Allow request
            await self.backend.increment(key)
            await self.backend.expire(key, window_seconds * 2)
            
            return RateLimitResult(
                allowed=True,
                remaining=int(limit - weighted_count - 1),
                reset_at=datetime.fromtimestamp(window_start + window_seconds),
                limit=limit
            )
        else:
            # Rate limited
            return RateLimitResult(
                allowed=False,
                remaining=0,
                reset_at=datetime.fromtimestamp(window_start + window_seconds),
                retry_after=int(window_start + window_seconds - now) + 1,
                limit=limit
            )


class RateLimiter:
    """
    Main rate limiter class.
    
    Provides configurable rate limiting for the Mental Models System API.
    """
    
    def __init__(
        self,
        config: Optional[RateLimitConfig] = None,
        backend: Optional[RateLimitBackend] = None
    ):
        self.config = config or RateLimitConfig()
        self.backend = backend or InMemoryBackend()
        
        # Initialize strategy-specific limiter
        if self.config.strategy == RateLimitStrategy.TOKEN_BUCKET:
            self._limiter = TokenBucketLimiter(self.backend, self.config)
        else:
            self._limiter = SlidingWindowLimiter(self.backend, self.config)
        
        # Statistics
        self._stats = {
            "total_requests": 0,
            "allowed_requests": 0,
            "blocked_requests": 0,
            "by_identifier": {}
        }
    
    def _get_identifier(
        self,
        ip: Optional[str] = None,
        api_key: Optional[str] = None,
        user_id: Optional[str] = None
    ) -> str:
        """Generate unique identifier for rate limiting."""
        if api_key:
            return f"key:{hashlib.sha256(api_key.encode()).hexdigest()[:16]}"
        elif user_id:
            return f"user:{user_id}"
        elif ip:
            return f"ip:{ip}"
        else:
            return "anonymous"
    
    async def check(
        self,
        ip: Optional[str] = None,
        api_key: Optional[str] = None,
        user_id: Optional[str] = None,
        endpoint: Optional[str] = None
    ) -> RateLimitResult:
        """
        Check if a request should be allowed.
        
        Args:
            ip: Client IP address
            api_key: API key if provided
            user_id: User ID if authenticated
            endpoint: Request endpoint path
        
        Returns:
            RateLimitResult indicating if request is allowed
        """
        # Check exemptions
        if endpoint and endpoint in self.config.exempt_paths:
            return RateLimitResult(
                allowed=True,
                remaining=999999,
                reset_at=datetime.now(),
                limit=999999
            )
        
        if api_key and api_key in self.config.exempt_keys:
            return RateLimitResult(
                allowed=True,
                remaining=999999,
                reset_at=datetime.now(),
                limit=999999
            )
        
        identifier = self._get_identifier(ip, api_key, user_id)
        
        # Check endpoint-specific limits
        if endpoint and endpoint in self.config.endpoint_limits:
            # Create temporary config for endpoint
            endpoint_config = RateLimitConfig(
                requests_per_minute=self.config.endpoint_limits[endpoint],
                burst_limit=min(self.config.burst_limit, self.config.endpoint_limits[endpoint])
            )
            limiter = TokenBucketLimiter(self.backend, endpoint_config)
            result = await limiter.check(f"{identifier}:{endpoint}")
        else:
            result = await self._limiter.check(identifier)
        
        # Update statistics
        self._stats["total_requests"] += 1
        if result.allowed:
            self._stats["allowed_requests"] += 1
        else:
            self._stats["blocked_requests"] += 1
        
        if identifier not in self._stats["by_identifier"]:
            self._stats["by_identifier"][identifier] = {"allowed": 0, "blocked": 0}
        
        if result.allowed:
            self._stats["by_identifier"][identifier]["allowed"] += 1
        else:
            self._stats["by_identifier"][identifier]["blocked"] += 1
        
        return result
    
    def get_stats(self) -> Dict[str, Any]:
        """Get rate limiting statistics."""
        return {
            **self._stats,
            "block_rate": (
                self._stats["blocked_requests"] / self._stats["total_requests"]
                if self._stats["total_requests"] > 0 else 0
            )
        }
    
    def reset_stats(self):
        """Reset statistics."""
        self._stats = {
            "total_requests": 0,
            "allowed_requests": 0,
            "blocked_requests": 0,
            "by_identifier": {}
        }


# FastAPI middleware
def create_rate_limit_middleware(limiter: RateLimiter):
    """Create FastAPI middleware for rate limiting."""
    
    async def rate_limit_middleware(request, call_next):
        # Extract identifiers
        ip = request.client.host if request.client else None
        api_key = request.headers.get("X-API-Key")
        user_id = getattr(request.state, "user_id", None)
        endpoint = request.url.path
        
        # Check rate limit
        result = await limiter.check(ip=ip, api_key=api_key, user_id=user_id, endpoint=endpoint)
        
        if not result.allowed:
            from fastapi.responses import JSONResponse
            return JSONResponse(
                status_code=429,
                content={
                    "error": "Rate limit exceeded",
                    "retry_after": result.retry_after,
                    "message": f"Too many requests. Please retry after {result.retry_after} seconds."
                },
                headers=result.to_headers()
            )
        
        # Process request
        response = await call_next(request)
        
        # Add rate limit headers
        for header, value in result.to_headers().items():
            response.headers[header] = value
        
        return response
    
    return rate_limit_middleware


if __name__ == "__main__":
    import asyncio
    
    async def test():
        # Test rate limiter
        config = RateLimitConfig(
            requests_per_minute=10,
            burst_limit=5
        )
        
        limiter = RateLimiter(config=config)
        
        # Simulate requests
        for i in range(15):
            result = await limiter.check(ip="192.168.1.1")
            print(f"Request {i+1}: allowed={result.allowed}, remaining={result.remaining}")
            
            if not result.allowed:
                print(f"  Retry after: {result.retry_after}s")
        
        print(f"\nStats: {limiter.get_stats()}")
    
    asyncio.run(test())
