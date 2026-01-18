"""
Autonomous Signal Harvester

Continuously monitors data streams, extracts mental model signals, and triggers alerts.
Integrates with Firecrawl for web scraping and your local LLM for analysis.

Architecture:
┌─────────────────────────────────────────────────────────────────────────┐
│                    AUTONOMOUS SIGNAL HARVESTER                          │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  DATA SOURCES                    PROCESSING                 OUTPUT      │
│  ┌──────────────┐               ┌──────────────┐          ┌──────────┐ │
│  │ Firecrawl    │               │ Local LLM    │          │ Alerts   │ │
│  │ - News       │               │ Analysis     │          │ - Slack  │ │
│  │ - SEC        │               │              │          │ - Email  │ │
│  │ - Earnings   │               │ 129 Mental   │          │ - Webhook│ │
│  │ - Custom     │               │ Models       │          └──────────┘ │
│  └──────────────┘               │              │          ┌──────────┐ │
│                                 │ Lollapalooza │          │ Database │ │
│                                 │ Detection    │          │ Storage  │ │
│                                 └──────────────┘          └──────────┘ │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
"""

import os
import json
import asyncio
import hashlib
import subprocess
from dataclasses import dataclass, field, asdict
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any, Callable
from enum import Enum
from pathlib import Path
import logging
from collections import deque
import threading
import time

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# =============================================================================
# DATA STRUCTURES
# =============================================================================

class SignalSource(Enum):
    """Sources of signals."""
    NEWS = "news"
    SEC_FILING = "sec_filing"
    EARNINGS = "earnings"
    SOCIAL = "social"
    CUSTOM_URL = "custom_url"
    RSS = "rss"
    API = "api"


class SignalPriority(Enum):
    """Priority levels for signals."""
    LOW = 1
    MEDIUM = 2
    HIGH = 3
    CRITICAL = 4


@dataclass
class RawSignal:
    """A raw signal from a data source."""
    id: str
    source: SignalSource
    url: str
    title: str
    content: str
    timestamp: datetime
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    def content_hash(self) -> str:
        """Generate hash for deduplication."""
        return hashlib.md5(self.content.encode()).hexdigest()


@dataclass
class ProcessedSignal:
    """A signal after mental model analysis."""
    raw_signal: RawSignal
    models_detected: List[Dict[str, Any]]  # [{model_id, model_name, relevance, evidence}]
    lollapalooza_score: float  # 0-1, based on model convergence
    priority: SignalPriority
    insights: List[str]
    processed_at: datetime
    # Failure mode integration
    failure_modes_detected: List[Dict[str, Any]] = field(default_factory=list)  # [{mode_name, model_name, risk_score, warning_signs}]
    risk_assessment: Dict[str, Any] = field(default_factory=dict)  # {overall_risk, safeguards, warnings}
    
    def to_dict(self) -> Dict:
        return {
            "id": self.raw_signal.id,
            "source": self.raw_signal.source.value,
            "url": self.raw_signal.url,
            "title": self.raw_signal.title,
            "models_detected": self.models_detected,
            "lollapalooza_score": self.lollapalooza_score,
            "priority": self.priority.value,
            "insights": self.insights,
            "processed_at": self.processed_at.isoformat(),
            "timestamp": self.raw_signal.timestamp.isoformat(),
            "failure_modes_detected": self.failure_modes_detected,
            "risk_assessment": self.risk_assessment
        }


@dataclass
class HarvesterConfig:
    """Configuration for the signal harvester."""
    # Data sources
    news_queries: List[str] = field(default_factory=lambda: [
        "Warren Buffett investment",
        "market crash warning",
        "company moat competitive advantage",
        "network effects business",
        "regulatory antitrust"
    ])
    sec_companies: List[str] = field(default_factory=lambda: [
        "AAPL", "MSFT", "GOOGL", "AMZN", "META", "BRK.A", "NVDA"
    ])
    custom_urls: List[str] = field(default_factory=list)
    
    # Processing
    llm_backend: str = "ollama"
    llm_model: str = "llama3:70b"
    llm_url: str = "http://localhost:11434"
    batch_size: int = 10
    
    # Alerts
    lollapalooza_threshold: float = 0.7
    slack_channel: Optional[str] = None
    webhook_url: Optional[str] = None
    
    # Scheduling
    news_interval_minutes: int = 30
    sec_interval_hours: int = 6
    
    # Storage
    storage_path: str = "./data/signals"


# =============================================================================
# FIRECRAWL INTEGRATION
# =============================================================================

class FirecrawlClient:
    """Client for Firecrawl MCP server."""
    
    def __init__(self):
        self.server = "firecrawl"
    
    def _call_mcp(self, tool: str, args: Dict) -> Dict:
        """Call an MCP tool and return the result."""
        cmd = [
            "manus-mcp-cli", "tool", "call", tool,
            "--server", self.server,
            "--input", json.dumps(args)
        ]
        
        try:
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=120
            )
            
            if result.returncode != 0:
                logger.error(f"MCP call failed: {result.stderr}")
                return {"error": result.stderr}
            
            # Parse the result file path from output
            output = result.stdout
            if "Tool execution result:" in output:
                # Extract JSON from output
                json_start = output.find("{")
                if json_start != -1:
                    return json.loads(output[json_start:])
            
            # Try to find result file
            for line in output.split("\n"):
                if "result" in line.lower() and ".json" in line:
                    path = line.split(":")[-1].strip()
                    if os.path.exists(path):
                        with open(path, 'r') as f:
                            return json.load(f)
            
            return {"raw_output": output}
            
        except subprocess.TimeoutExpired:
            logger.error("MCP call timed out")
            return {"error": "timeout"}
        except Exception as e:
            logger.error(f"MCP call error: {e}")
            return {"error": str(e)}
    
    def search(self, query: str, limit: int = 10, source: str = "news") -> List[Dict]:
        """Search the web using Firecrawl."""
        # Sources must be array of objects with 'type' property
        sources = [{"type": "news"}] if source == "news" else [{"type": "web"}]
        
        result = self._call_mcp("firecrawl_search", {
            "query": query,
            "limit": limit,
            "sources": sources
        })
        
        if "error" in result:
            logger.error(f"Search failed: {result['error']}")
            return []
        
        # Handle different response formats
        if "web" in result:
            return result["web"]
        elif "news" in result:
            return result["news"]
        elif "results" in result:
            return result["results"]
        elif "data" in result:
            return result["data"]
        
        return []
    
    def scrape(self, url: str, formats: List[str] = None) -> Dict:
        """Scrape a single URL."""
        formats = formats or ["markdown"]
        
        result = self._call_mcp("firecrawl_scrape", {
            "url": url,
            "formats": formats,
            "onlyMainContent": True
        })
        
        return result
    
    def extract(self, urls: List[str], prompt: str, schema: Dict = None) -> List[Dict]:
        """Extract structured data from URLs."""
        args = {
            "urls": urls,
            "prompt": prompt
        }
        if schema:
            args["schema"] = schema
        
        result = self._call_mcp("firecrawl_extract", args)
        
        if "error" in result:
            logger.error(f"Extract failed: {result['error']}")
            return []
        
        return result.get("results", result.get("data", []))


# =============================================================================
# DATA COLLECTORS
# =============================================================================

class NewsCollector:
    """Collects news signals using Firecrawl."""
    
    def __init__(self, firecrawl: FirecrawlClient, queries: List[str]):
        self.firecrawl = firecrawl
        self.queries = queries
        self.seen_hashes: set = set()
    
    def collect(self) -> List[RawSignal]:
        """Collect news signals."""
        signals = []
        
        for query in self.queries:
            logger.info(f"Searching news: {query}")
            results = self.firecrawl.search(query, limit=5, source="news")
            
            for item in results:
                # Create signal
                signal = RawSignal(
                    id=hashlib.md5(f"{item.get('url', '')}{item.get('title', '')}".encode()).hexdigest()[:12],
                    source=SignalSource.NEWS,
                    url=item.get("url", ""),
                    title=item.get("title", ""),
                    content=item.get("description", item.get("content", "")),
                    timestamp=datetime.now(),
                    metadata={
                        "query": query,
                        "source_name": item.get("source", "")
                    }
                )
                
                # Deduplicate
                content_hash = signal.content_hash()
                if content_hash not in self.seen_hashes:
                    self.seen_hashes.add(content_hash)
                    signals.append(signal)
        
        logger.info(f"Collected {len(signals)} news signals")
        return signals


class SECCollector:
    """Collects SEC filing signals."""
    
    def __init__(self, firecrawl: FirecrawlClient, companies: List[str]):
        self.firecrawl = firecrawl
        self.companies = companies
        self.seen_hashes: set = set()
    
    def collect(self) -> List[RawSignal]:
        """Collect SEC filing signals."""
        signals = []
        
        for ticker in self.companies:
            query = f"{ticker} SEC filing 10-K 10-Q 8-K"
            logger.info(f"Searching SEC filings: {ticker}")
            
            results = self.firecrawl.search(query, limit=3, source="web")
            
            for item in results:
                signal = RawSignal(
                    id=hashlib.md5(f"{item.get('url', '')}{ticker}".encode()).hexdigest()[:12],
                    source=SignalSource.SEC_FILING,
                    url=item.get("url", ""),
                    title=item.get("title", ""),
                    content=item.get("description", item.get("content", "")),
                    timestamp=datetime.now(),
                    metadata={
                        "ticker": ticker,
                        "filing_type": self._detect_filing_type(item.get("title", ""))
                    }
                )
                
                content_hash = signal.content_hash()
                if content_hash not in self.seen_hashes:
                    self.seen_hashes.add(content_hash)
                    signals.append(signal)
        
        logger.info(f"Collected {len(signals)} SEC signals")
        return signals
    
    def _detect_filing_type(self, title: str) -> str:
        """Detect SEC filing type from title."""
        title_lower = title.lower()
        if "10-k" in title_lower:
            return "10-K"
        elif "10-q" in title_lower:
            return "10-Q"
        elif "8-k" in title_lower:
            return "8-K"
        return "unknown"


class CustomURLCollector:
    """Collects signals from custom URLs."""
    
    def __init__(self, firecrawl: FirecrawlClient, urls: List[str]):
        self.firecrawl = firecrawl
        self.urls = urls
    
    def collect(self) -> List[RawSignal]:
        """Collect signals from custom URLs."""
        signals = []
        
        for url in self.urls:
            logger.info(f"Scraping custom URL: {url}")
            result = self.firecrawl.scrape(url)
            
            if "error" not in result:
                content = result.get("markdown", result.get("content", ""))
                signal = RawSignal(
                    id=hashlib.md5(url.encode()).hexdigest()[:12],
                    source=SignalSource.CUSTOM_URL,
                    url=url,
                    title=result.get("title", url),
                    content=content[:10000],  # Limit content size
                    timestamp=datetime.now(),
                    metadata={"scraped": True}
                )
                signals.append(signal)
        
        logger.info(f"Collected {len(signals)} custom URL signals")
        return signals


# =============================================================================
# SIGNAL PROCESSOR
# =============================================================================

class SignalProcessor:
    """Processes signals through mental model analysis."""
    
    def __init__(self, config: HarvesterConfig):
        self.config = config
        self.models_data = self._load_models()
    
    def _load_models(self) -> Dict:
        """Load mental models data."""
        models_path = Path(__file__).parent.parent.parent / "data" / "raw" / "mental_models_complete.json"
        
        if models_path.exists():
            with open(models_path, 'r') as f:
                data = json.load(f)
            
            # Build lookup
            models = {}
            categories = {c["id"]: c["name"] for c in data.get("categories", [])}
            
            for m in data.get("mental_models", []):
                m["category_name"] = categories.get(m.get("category_id"), "")
                models[m["id"]] = m
            
            return models
        
        return {}
    
    def _get_model_summary(self) -> str:
        """Get summary of models for LLM prompt."""
        summary = []
        for model_id, model in self.models_data.items():
            summary.append(f"- {model['name']}: {model.get('description', '')[:100]}")
        return "\n".join(summary[:50])  # Limit to 50 models for prompt
    
    def _call_local_llm(self, prompt: str) -> str:
        """Call local LLM for analysis."""
        # Try Ollama first
        try:
            import requests
            response = requests.post(
                f"{self.config.llm_url}/api/generate",
                json={
                    "model": self.config.llm_model,
                    "prompt": prompt,
                    "stream": False
                },
                timeout=60
            )
            if response.status_code == 200:
                return response.json().get("response", "")
        except Exception as e:
            logger.warning(f"Local LLM call failed: {e}")
        
        # Fallback to OpenAI-compatible API
        try:
            from openai import OpenAI
            client = OpenAI()
            response = client.chat.completions.create(
                model="gpt-4.1-mini",
                messages=[{"role": "user", "content": prompt}],
                max_tokens=2000
            )
            return response.choices[0].message.content
        except Exception as e:
            logger.error(f"OpenAI fallback failed: {e}")
            return ""
    
    def process(self, signal: RawSignal) -> ProcessedSignal:
        """Process a signal through mental model analysis."""
        
        # Build analysis prompt
        prompt = f"""Analyze this content through the lens of Charlie Munger's mental models.

CONTENT:
Title: {signal.title}
Source: {signal.source.value}
Text: {signal.content[:3000]}

MENTAL MODELS TO CONSIDER:
{self._get_model_summary()}

TASK:
1. Identify which mental models apply to this content
2. For each model, provide:
   - Model name (exact match from list)
   - Relevance score (0.0-1.0)
   - Brief evidence from the text
3. If 3+ models converge strongly, this is a Lollapalooza effect
4. Provide key insights

Respond in JSON format:
{{
  "models": [
    {{"name": "Model Name", "relevance": 0.85, "evidence": "Quote or explanation"}}
  ],
  "lollapalooza_score": 0.0-1.0,
  "insights": ["insight1", "insight2"]
}}
"""
        
        # Get LLM analysis
        response = self._call_local_llm(prompt)
        
        # Parse response
        try:
            # Find JSON in response
            json_start = response.find("{")
            json_end = response.rfind("}") + 1
            if json_start != -1 and json_end > json_start:
                analysis = json.loads(response[json_start:json_end])
            else:
                analysis = {"models": [], "lollapalooza_score": 0, "insights": []}
        except json.JSONDecodeError:
            logger.warning(f"Failed to parse LLM response for signal {signal.id}")
            analysis = {"models": [], "lollapalooza_score": 0, "insights": []}
        
        # Calculate priority
        lollapalooza_score = analysis.get("lollapalooza_score", 0)
        num_models = len(analysis.get("models", []))
        
        if lollapalooza_score >= 0.8 or num_models >= 5:
            priority = SignalPriority.CRITICAL
        elif lollapalooza_score >= 0.6 or num_models >= 3:
            priority = SignalPriority.HIGH
        elif num_models >= 2:
            priority = SignalPriority.MEDIUM
        else:
            priority = SignalPriority.LOW
        
        # Get failure mode analysis for detected models
        failure_modes = []
        risk_assessment = {}
        
        try:
            from ..safeguards.failure_search import FailureModeSearchEngine
            fm_engine = FailureModeSearchEngine()
            
            # Get failure modes for detected models
            detected_model_names = [m.get('name', '') for m in analysis.get('models', [])]
            
            # Assess risk based on content and models
            assessment = fm_engine.assess_risk(signal.content[:2000], detected_model_names)
            
            failure_modes = [
                {
                    'mode_name': fm.mode_name,
                    'model_name': fm.model_name,
                    'risk_score': fm.relevance_score,
                    'warning_signs': fm.warning_signs[:3],
                    'safeguards': fm.safeguards[:3]
                }
                for fm in assessment.top_failure_modes[:5]
            ]
            
            risk_assessment = {
                'overall_risk': assessment.overall_risk_score,
                'risk_by_category': assessment.risk_by_category,
                'top_safeguards': assessment.recommended_safeguards[:5],
                'warning_signs': assessment.warning_signs_to_watch[:5]
            }
            
            # Adjust priority based on risk
            if assessment.overall_risk_score >= 0.7:
                priority = SignalPriority.CRITICAL
            elif assessment.overall_risk_score >= 0.5 and priority.value < SignalPriority.HIGH.value:
                priority = SignalPriority.HIGH
                
        except Exception as e:
            logger.warning(f"Failure mode analysis failed: {e}")
        
        return ProcessedSignal(
            raw_signal=signal,
            models_detected=analysis.get("models", []),
            lollapalooza_score=lollapalooza_score,
            priority=priority,
            insights=analysis.get("insights", []),
            processed_at=datetime.now(),
            failure_modes_detected=failure_modes,
            risk_assessment=risk_assessment
        )


# =============================================================================
# ALERT SYSTEM
# =============================================================================

class AlertSystem:
    """Sends alerts for high-priority signals."""
    
    def __init__(self, config: HarvesterConfig):
        self.config = config
    
    def send_alert(self, signal: ProcessedSignal):
        """Send alert for a processed signal."""
        if signal.priority.value < SignalPriority.HIGH.value:
            return
        
        message = self._format_alert(signal)
        
        # Send to Slack if configured
        if self.config.slack_channel:
            self._send_slack(message)
        
        # Send to webhook if configured
        if self.config.webhook_url:
            self._send_webhook(signal)
        
        logger.info(f"Alert sent for signal {signal.raw_signal.id}: {signal.raw_signal.title[:50]}")
    
    def _format_alert(self, signal: ProcessedSignal) -> str:
        """Format alert message."""
        models_str = ", ".join([m["name"] for m in signal.models_detected[:5]])
        
        return f"""🚨 *{signal.priority.name} SIGNAL DETECTED*

*Title:* {signal.raw_signal.title}
*Source:* {signal.raw_signal.source.value}
*URL:* {signal.raw_signal.url}

*Mental Models Detected ({len(signal.models_detected)}):*
{models_str}

*Lollapalooza Score:* {signal.lollapalooza_score:.2f}

*Key Insights:*
{chr(10).join(['• ' + i for i in signal.insights[:3]])}

_Processed at {signal.processed_at.strftime('%Y-%m-%d %H:%M:%S')}_
"""
    
    def _send_slack(self, message: str):
        """Send alert to Slack."""
        try:
            cmd = [
                "manus-mcp-cli", "tool", "call", "slack_send_message",
                "--server", "slack",
                "--input", json.dumps({
                    "channel_id": self.config.slack_channel,
                    "message": message
                })
            ]
            subprocess.run(cmd, capture_output=True, timeout=30)
        except Exception as e:
            logger.error(f"Failed to send Slack alert: {e}")
    
    def _send_webhook(self, signal: ProcessedSignal):
        """Send alert to webhook."""
        try:
            import requests
            requests.post(
                self.config.webhook_url,
                json=signal.to_dict(),
                timeout=10
            )
        except Exception as e:
            logger.error(f"Failed to send webhook alert: {e}")


# =============================================================================
# STORAGE
# =============================================================================

class SignalStorage:
    """Stores processed signals."""
    
    def __init__(self, storage_path: str):
        self.storage_path = Path(storage_path)
        self.storage_path.mkdir(parents=True, exist_ok=True)
    
    def save(self, signal: ProcessedSignal):
        """Save a processed signal."""
        date_str = signal.processed_at.strftime("%Y-%m-%d")
        file_path = self.storage_path / f"signals_{date_str}.jsonl"
        
        with open(file_path, 'a') as f:
            f.write(json.dumps(signal.to_dict()) + "\n")
    
    def load_recent(self, days: int = 7) -> List[Dict]:
        """Load recent signals."""
        signals = []
        cutoff = datetime.now() - timedelta(days=days)
        
        for file_path in sorted(self.storage_path.glob("signals_*.jsonl"), reverse=True):
            # Check date from filename
            date_str = file_path.stem.replace("signals_", "")
            try:
                file_date = datetime.strptime(date_str, "%Y-%m-%d")
                if file_date < cutoff:
                    break
            except ValueError:
                continue
            
            with open(file_path, 'r') as f:
                for line in f:
                    try:
                        signals.append(json.loads(line))
                    except json.JSONDecodeError:
                        continue
        
        return signals


# =============================================================================
# MAIN HARVESTER
# =============================================================================

class AutonomousSignalHarvester:
    """
    Main harvester that orchestrates data collection, processing, and alerting.
    """
    
    def __init__(self, config: HarvesterConfig = None):
        self.config = config or HarvesterConfig()
        
        # Initialize components
        self.firecrawl = FirecrawlClient()
        self.processor = SignalProcessor(self.config)
        self.alerts = AlertSystem(self.config)
        self.storage = SignalStorage(self.config.storage_path)
        
        # Initialize collectors
        self.news_collector = NewsCollector(
            self.firecrawl, 
            self.config.news_queries
        )
        self.sec_collector = SECCollector(
            self.firecrawl,
            self.config.sec_companies
        )
        self.custom_collector = CustomURLCollector(
            self.firecrawl,
            self.config.custom_urls
        )
        
        # State
        self.running = False
        self.stats = {
            "signals_collected": 0,
            "signals_processed": 0,
            "alerts_sent": 0,
            "lollapalooza_detected": 0
        }
    
    def harvest_once(self) -> List[ProcessedSignal]:
        """Run one harvest cycle."""
        logger.info("Starting harvest cycle...")
        processed_signals = []
        
        # Collect from all sources
        raw_signals = []
        raw_signals.extend(self.news_collector.collect())
        raw_signals.extend(self.sec_collector.collect())
        raw_signals.extend(self.custom_collector.collect())
        
        self.stats["signals_collected"] += len(raw_signals)
        logger.info(f"Collected {len(raw_signals)} raw signals")
        
        # Process signals
        for signal in raw_signals:
            try:
                processed = self.processor.process(signal)
                processed_signals.append(processed)
                
                # Store
                self.storage.save(processed)
                self.stats["signals_processed"] += 1
                
                # Check for alerts
                if processed.lollapalooza_score >= self.config.lollapalooza_threshold:
                    self.stats["lollapalooza_detected"] += 1
                    self.alerts.send_alert(processed)
                    self.stats["alerts_sent"] += 1
                elif processed.priority.value >= SignalPriority.HIGH.value:
                    self.alerts.send_alert(processed)
                    self.stats["alerts_sent"] += 1
                
            except Exception as e:
                logger.error(f"Error processing signal {signal.id}: {e}")
        
        logger.info(f"Processed {len(processed_signals)} signals")
        return processed_signals
    
    def start(self, run_once: bool = False):
        """Start the harvester."""
        self.running = True
        logger.info("Signal Harvester started")
        
        if run_once:
            return self.harvest_once()
        
        # Continuous mode
        while self.running:
            try:
                self.harvest_once()
                
                # Wait for next cycle
                logger.info(f"Waiting {self.config.news_interval_minutes} minutes until next cycle...")
                time.sleep(self.config.news_interval_minutes * 60)
                
            except KeyboardInterrupt:
                logger.info("Harvester stopped by user")
                break
            except Exception as e:
                logger.error(f"Harvester error: {e}")
                time.sleep(60)  # Wait 1 minute on error
    
    def stop(self):
        """Stop the harvester."""
        self.running = False
        logger.info("Signal Harvester stopped")
    
    def get_stats(self) -> Dict:
        """Get harvester statistics."""
        return {
            **self.stats,
            "running": self.running,
            "config": {
                "news_queries": len(self.config.news_queries),
                "sec_companies": len(self.config.sec_companies),
                "custom_urls": len(self.config.custom_urls)
            }
        }


# =============================================================================
# CLI INTERFACE
# =============================================================================

def main():
    """CLI entry point."""
    import argparse
    
    parser = argparse.ArgumentParser(description="Autonomous Signal Harvester")
    parser.add_argument("--once", action="store_true", help="Run once and exit")
    parser.add_argument("--slack-channel", help="Slack channel ID for alerts")
    parser.add_argument("--queries", nargs="+", help="News search queries")
    parser.add_argument("--companies", nargs="+", help="SEC filing companies (tickers)")
    parser.add_argument("--urls", nargs="+", help="Custom URLs to monitor")
    parser.add_argument("--interval", type=int, default=30, help="Minutes between cycles")
    
    args = parser.parse_args()
    
    # Build config
    config = HarvesterConfig(
        news_interval_minutes=args.interval
    )
    
    if args.slack_channel:
        config.slack_channel = args.slack_channel
    if args.queries:
        config.news_queries = args.queries
    if args.companies:
        config.sec_companies = args.companies
    if args.urls:
        config.custom_urls = args.urls
    
    # Run harvester
    harvester = AutonomousSignalHarvester(config)
    
    try:
        harvester.start(run_once=args.once)
    finally:
        print(f"\nStats: {json.dumps(harvester.get_stats(), indent=2)}")


if __name__ == "__main__":
    main()
