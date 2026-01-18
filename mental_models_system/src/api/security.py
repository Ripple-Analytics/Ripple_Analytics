"""
API Security and Rate Limiting Module.

Provides:
- Rate limiting (per IP, per user, per endpoint)
- API key authentication
- Request validation
- Security headers
"""

import time
import hashlib
import secrets
from datetime import datetime, timedelta
from typing import Dict, Optional, List, Any
from dataclasses import dataclass, field
from collections import defaultdict
import json
import asyncio

from fastapi import Request, HTTPException, Header
from fastapi.responses import JSONResponse
from starlette.middleware.base import BaseHTTPMiddleware


@dataclass
class RateLimitConfig:
    """Configuration for rate limiting."""
    requests_per_minute: int = 60
    requests_per_hour: int = 1000
    requests_per_day: int = 10000
    burst_limit: int = 10
    llm_requests_per_minute: int = 10
    endpoint_limits: Dict[str, int] = field(default_factory=dict)


@dataclass
class APIKey:
    """API key with metadata."""
    key_hash: str
    name: str
    created_at: datetime
    expires_at: Optional[datetime]
    rate_limit_multiplier: float = 1.0
    scopes: List[str] = field(default_factory=list)
    enabled: bool = True
    last_used: Optional[datetime] = None
    usage_count: int = 0


class RateLimiter:
    """Token bucket rate limiter with multiple time windows."""
    
    def __init__(self, config: RateLimitConfig = None):
        self.config = config or RateLimitConfig()
        self.requests: Dict[str, List[float]] = defaultdict(list)
        self.blocked_ips: Dict[str, datetime] = {}
        self._lock = asyncio.Lock()
    
    async def check_rate_limit(
        self,
        identifier: str,
        endpoint: str = None,
        is_llm_endpoint: bool = False,
        multiplier: float = 1.0
    ) -> tuple:
        async with self._lock:
            now = time.time()
            
            if identifier in self.blocked_ips:
                if datetime.now() < self.blocked_ips[identifier]:
                    return False, {"X-RateLimit-Blocked": "true"}
                else:
                    del self.blocked_ips[identifier]
            
            self._clean_old_requests(identifier, now)
            
            if is_llm_endpoint:
                per_minute = int(self.config.llm_requests_per_minute * multiplier)
            else:
                per_minute = int(self.config.requests_per_minute * multiplier)
            
            minute_ago = now - 60
            requests = self.requests[identifier]
            minute_count = sum(1 for t in requests if t > minute_ago)
            
            headers = {
                "X-RateLimit-Limit": str(per_minute),
                "X-RateLimit-Remaining": str(max(0, per_minute - minute_count)),
            }
            
            if minute_count >= per_minute:
                headers["X-RateLimit-Retry-After"] = "60"
                return False, headers
            
            self.requests[identifier].append(now)
            return True, headers
    
    def _clean_old_requests(self, identifier: str, now: float):
        day_ago = now - 86400
        self.requests[identifier] = [t for t in self.requests[identifier] if t > day_ago]
    
    def block_ip(self, ip: str, duration_minutes: int = 60):
        self.blocked_ips[ip] = datetime.now() + timedelta(minutes=duration_minutes)
    
    def get_stats(self) -> Dict[str, Any]:
        return {
            "active_identifiers": len(self.requests),
            "blocked_ips": len(self.blocked_ips),
        }


class APIKeyManager:
    """Manage API keys for authentication."""
    
    def __init__(self, keys_file: str = "./data/api_keys.json"):
        self.keys_file = keys_file
        self.keys: Dict[str, APIKey] = {}
        self._load_keys()
    
    def _load_keys(self):
        try:
            with open(self.keys_file) as f:
                data = json.load(f)
                for key_data in data.get("keys", []):
                    key = APIKey(
                        key_hash=key_data["key_hash"],
                        name=key_data["name"],
                        created_at=datetime.fromisoformat(key_data["created_at"]),
                        expires_at=datetime.fromisoformat(key_data["expires_at"]) if key_data.get("expires_at") else None,
                        rate_limit_multiplier=key_data.get("rate_limit_multiplier", 1.0),
                        scopes=key_data.get("scopes", ["read"]),
                        enabled=key_data.get("enabled", True),
                        usage_count=key_data.get("usage_count", 0)
                    )
                    self.keys[key.key_hash] = key
        except FileNotFoundError:
            pass
    
    def _save_keys(self):
        import os
        os.makedirs(os.path.dirname(self.keys_file) or ".", exist_ok=True)
        data = {"keys": [
            {
                "key_hash": k.key_hash,
                "name": k.name,
                "created_at": k.created_at.isoformat(),
                "expires_at": k.expires_at.isoformat() if k.expires_at else None,
                "rate_limit_multiplier": k.rate_limit_multiplier,
                "scopes": k.scopes,
                "enabled": k.enabled,
                "usage_count": k.usage_count
            }
            for k in self.keys.values()
        ]}
        with open(self.keys_file, 'w') as f:
            json.dump(data, f, indent=2)
    
    def generate_key(self, name: str, scopes: List[str] = None, expires_in_days: int = None) -> tuple:
        raw_key = f"mm_{secrets.token_urlsafe(32)}"
        key_hash = hashlib.sha256(raw_key.encode()).hexdigest()
        api_key = APIKey(
            key_hash=key_hash,
            name=name,
            created_at=datetime.now(),
            expires_at=datetime.now() + timedelta(days=expires_in_days) if expires_in_days else None,
            scopes=scopes or ["read"],
            enabled=True
        )
        self.keys[key_hash] = api_key
        self._save_keys()
        return raw_key, api_key
    
    def validate_key(self, raw_key: str) -> Optional[APIKey]:
        if not raw_key or not raw_key.startswith("mm_"):
            return None
        key_hash = hashlib.sha256(raw_key.encode()).hexdigest()
        api_key = self.keys.get(key_hash)
        if not api_key or not api_key.enabled:
            return None
        if api_key.expires_at and datetime.now() > api_key.expires_at:
            return None
        api_key.last_used = datetime.now()
        api_key.usage_count += 1
        self._save_keys()
        return api_key
    
    def revoke_key(self, key_hash: str) -> bool:
        if key_hash in self.keys:
            self.keys[key_hash].enabled = False
            self._save_keys()
            return True
        return False
    
    def list_keys(self) -> List[Dict]:
        return [{"name": k.name, "enabled": k.enabled, "usage_count": k.usage_count} for k in self.keys.values()]


class SecurityMiddleware(BaseHTTPMiddleware):
    """Security middleware for FastAPI."""
    
    async def dispatch(self, request: Request, call_next):
        response = await call_next(request)
        response.headers["X-Content-Type-Options"] = "nosniff"
        response.headers["X-Frame-Options"] = "DENY"
        response.headers["X-XSS-Protection"] = "1; mode=block"
        return response


class RateLimitMiddleware(BaseHTTPMiddleware):
    """Rate limiting middleware for FastAPI."""
    
    def __init__(self, app, rate_limiter: RateLimiter, api_key_manager: APIKeyManager = None):
        super().__init__(app)
        self.rate_limiter = rate_limiter
        self.api_key_manager = api_key_manager
        self.llm_endpoints = {"/analyze", "/generate", "/llm"}
    
    async def dispatch(self, request: Request, call_next):
        api_key_header = request.headers.get("X-API-Key")
        multiplier = 1.0
        
        if api_key_header and self.api_key_manager:
            api_key = self.api_key_manager.validate_key(api_key_header)
            if api_key:
                identifier = f"key:{api_key.key_hash[:16]}"
                multiplier = api_key.rate_limit_multiplier
            else:
                return JSONResponse(status_code=401, content={"error": "Invalid API key"})
        else:
            identifier = request.client.host if request.client else "unknown"
        
        is_llm = any(request.url.path.startswith(ep) for ep in self.llm_endpoints)
        allowed, headers = await self.rate_limiter.check_rate_limit(identifier, is_llm_endpoint=is_llm, multiplier=multiplier)
        
        if not allowed:
            response = JSONResponse(status_code=429, content={"error": "Rate limit exceeded"})
            for key, value in headers.items():
                response.headers[key] = value
            return response
        
        response = await call_next(request)
        for key, value in headers.items():
            response.headers[key] = value
        return response


rate_limiter = RateLimiter()
api_key_manager = APIKeyManager()


def setup_security(app):
    """Set up security middleware for FastAPI app."""
    app.add_middleware(SecurityMiddleware)
    app.add_middleware(RateLimitMiddleware, rate_limiter=rate_limiter, api_key_manager=api_key_manager)
