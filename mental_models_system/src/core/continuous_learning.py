import logging
import asyncio
import json
import hashlib
import sqlite3
import re
from dataclasses import dataclass, field
from typing import Dict, Any, List, Optional, Callable
from datetime import datetime, timedelta
from pathlib import Path
from enum import Enum
from urllib.parse import urlparse, urljoin
import threading
import time

logger = logging.getLogger(__name__)


class DataSourceType(Enum):
    WEB_SCRAPER = "web_scraper"
    FILE_WATCHER = "file_watcher"
    API_POLLER = "api_poller"
    SENSOR_STREAM = "sensor_stream"
    DATABASE_MONITOR = "database_monitor"
    RSS_FEED = "rss_feed"
    SOCIAL_MEDIA = "social_media"
    EMAIL_MONITOR = "email_monitor"


class LearningMode(Enum):
    CONTINUOUS = "continuous"
    BATCH = "batch"
    INCREMENTAL = "incremental"
    REAL_TIME = "real_time"


@dataclass
class DataPoint:
    source: str
    source_type: DataSourceType
    content: str
    metadata: Dict[str, Any] = field(default_factory=dict)
    timestamp: datetime = field(default_factory=datetime.now)
    content_hash: str = ""
    
    def __post_init__(self):
        if not self.content_hash:
            self.content_hash = hashlib.sha256(self.content.encode()).hexdigest()[:16]


@dataclass
class LearningResult:
    patterns_detected: int = 0
    correlations_found: int = 0
    anomalies_identified: int = 0
    models_updated: int = 0
    insights_generated: List[str] = field(default_factory=list)
    processing_time: float = 0.0


@dataclass
class ScraperConfig:
    name: str
    urls: List[str]
    interval_seconds: int = 3600
    selectors: Dict[str, str] = field(default_factory=dict)
    headers: Dict[str, str] = field(default_factory=dict)
    follow_links: bool = False
    max_depth: int = 2
    respect_robots: bool = True
    rate_limit: float = 1.0


class WebScraper:
    def __init__(self, config: ScraperConfig):
        self.config = config
        self.visited_urls: set = set()
        self.session = None
        self.last_scrape: Dict[str, datetime] = {}
        
    async def initialize(self):
        try:
            import aiohttp
            self.session = aiohttp.ClientSession(headers=self.config.headers)
        except ImportError:
            logger.warning("aiohttp not available, using requests fallback")
            self.session = None
    
    async def close(self):
        if self.session:
            await self.session.close()
    
    async def scrape_url(self, url: str, depth: int = 0) -> List[DataPoint]:
        if url in self.visited_urls:
            return []
        
        if depth > self.config.max_depth:
            return []
        
        self.visited_urls.add(url)
        data_points = []
        
        try:
            content = await self._fetch_content(url)
            if not content:
                return []
            
            text_content = self._extract_text(content)
            
            data_point = DataPoint(
                source=url,
                source_type=DataSourceType.WEB_SCRAPER,
                content=text_content,
                metadata={
                    "scraper_name": self.config.name,
                    "depth": depth,
                    "url": url
                }
            )
            data_points.append(data_point)
            
            if self.config.follow_links and depth < self.config.max_depth:
                links = self._extract_links(content, url)
                for link in links[:10]:
                    await asyncio.sleep(self.config.rate_limit)
                    child_points = await self.scrape_url(link, depth + 1)
                    data_points.extend(child_points)
            
            self.last_scrape[url] = datetime.now()
            
        except Exception as e:
            logger.error(f"Error scraping {url}: {e}")
        
        return data_points
    
    async def _fetch_content(self, url: str) -> Optional[str]:
        try:
            if self.session:
                async with self.session.get(url, timeout=30) as response:
                    if response.status == 200:
                        return await response.text()
            else:
                import requests
                response = requests.get(url, headers=self.config.headers, timeout=30)
                if response.status_code == 200:
                    return response.text
        except Exception as e:
            logger.error(f"Failed to fetch {url}: {e}")
        return None
    
    def _extract_text(self, html: str) -> str:
        try:
            from bs4 import BeautifulSoup
            soup = BeautifulSoup(html, 'html.parser')
            
            for script in soup(["script", "style", "nav", "footer", "header"]):
                script.decompose()
            
            text = soup.get_text(separator=' ', strip=True)
            text = re.sub(r'\s+', ' ', text)
            return text[:50000]
        except ImportError:
            text = re.sub(r'<[^>]+>', ' ', html)
            text = re.sub(r'\s+', ' ', text)
            return text[:50000]
    
    def _extract_links(self, html: str, base_url: str) -> List[str]:
        links = []
        try:
            from bs4 import BeautifulSoup
            soup = BeautifulSoup(html, 'html.parser')
            
            for a in soup.find_all('a', href=True):
                href = a['href']
                full_url = urljoin(base_url, href)
                
                parsed = urlparse(full_url)
                base_parsed = urlparse(base_url)
                
                if parsed.netloc == base_parsed.netloc:
                    links.append(full_url)
        except ImportError:
            pattern = r'href=["\']([^"\']+)["\']'
            matches = re.findall(pattern, html)
            for href in matches:
                full_url = urljoin(base_url, href)
                links.append(full_url)
        
        return list(set(links))
    
    async def run_scrape_cycle(self) -> List[DataPoint]:
        all_points = []
        
        for url in self.config.urls:
            last = self.last_scrape.get(url)
            if last and (datetime.now() - last).total_seconds() < self.config.interval_seconds:
                continue
            
            points = await self.scrape_url(url)
            all_points.extend(points)
            
            await asyncio.sleep(self.config.rate_limit)
        
        return all_points


class PatternDetector:
    def __init__(self):
        self.patterns: Dict[str, int] = {}
        self.correlations: List[Dict[str, Any]] = []
        self.anomalies: List[Dict[str, Any]] = []
    
    def detect_patterns(self, data_points: List[DataPoint]) -> int:
        patterns_found = 0
        
        word_freq: Dict[str, int] = {}
        for point in data_points:
            words = point.content.lower().split()
            for word in words:
                if len(word) > 4:
                    word_freq[word] = word_freq.get(word, 0) + 1
        
        for word, count in word_freq.items():
            if count > len(data_points) * 0.3:
                if word not in self.patterns:
                    self.patterns[word] = 0
                    patterns_found += 1
                self.patterns[word] += count
        
        return patterns_found
    
    def find_correlations(self, data_points: List[DataPoint]) -> int:
        correlations_found = 0
        
        source_content: Dict[str, List[str]] = {}
        for point in data_points:
            source = point.source_type.value
            if source not in source_content:
                source_content[source] = []
            source_content[source].append(point.content)
        
        sources = list(source_content.keys())
        for i, source1 in enumerate(sources):
            for source2 in sources[i+1:]:
                words1 = set(' '.join(source_content[source1]).lower().split())
                words2 = set(' '.join(source_content[source2]).lower().split())
                
                overlap = len(words1 & words2)
                total = len(words1 | words2)
                
                if total > 0 and overlap / total > 0.3:
                    self.correlations.append({
                        "source1": source1,
                        "source2": source2,
                        "overlap_ratio": overlap / total,
                        "timestamp": datetime.now().isoformat()
                    })
                    correlations_found += 1
        
        return correlations_found
    
    def identify_anomalies(self, data_points: List[DataPoint]) -> int:
        anomalies_found = 0
        
        lengths = [len(p.content) for p in data_points]
        if len(lengths) > 2:
            mean_len = sum(lengths) / len(lengths)
            variance = sum((l - mean_len) ** 2 for l in lengths) / len(lengths)
            std_dev = variance ** 0.5
            
            for i, point in enumerate(data_points):
                if abs(len(point.content) - mean_len) > 2 * std_dev:
                    self.anomalies.append({
                        "source": point.source,
                        "reason": "unusual_length",
                        "value": len(point.content),
                        "expected": mean_len,
                        "timestamp": datetime.now().isoformat()
                    })
                    anomalies_found += 1
        
        return anomalies_found


class InsightGenerator:
    def __init__(self):
        self.insights: List[str] = []
        self.mental_model_keywords = {
            "confirmation_bias": ["confirm", "belief", "evidence", "ignore", "contrary"],
            "incentive_effects": ["incentive", "reward", "motivation", "behavior", "align"],
            "compound_interest": ["compound", "exponential", "growth", "accumulate", "time"],
            "network_effects": ["network", "users", "value", "platform", "scale"],
            "margin_of_safety": ["margin", "safety", "buffer", "risk", "conservative"],
            "inversion": ["invert", "opposite", "avoid", "failure", "reverse"],
            "first_principles": ["fundamental", "basic", "assumption", "reason", "ground"],
            "circle_of_competence": ["competence", "expertise", "know", "limit", "boundary"]
        }
    
    def generate_insights(self, data_points: List[DataPoint], patterns: Dict[str, int]) -> List[str]:
        new_insights = []
        
        all_content = ' '.join(p.content.lower() for p in data_points)
        
        for model, keywords in self.mental_model_keywords.items():
            keyword_count = sum(all_content.count(kw) for kw in keywords)
            if keyword_count > 5:
                insight = f"Content shows strong relevance to {model.replace('_', ' ')} ({keyword_count} keyword matches)"
                new_insights.append(insight)
        
        top_patterns = sorted(patterns.items(), key=lambda x: -x[1])[:5]
        if top_patterns:
            pattern_str = ', '.join(f"'{p[0]}'" for p in top_patterns)
            insight = f"Recurring themes detected: {pattern_str}"
            new_insights.append(insight)
        
        sources = set(p.source_type.value for p in data_points)
        if len(sources) > 1:
            insight = f"Cross-source analysis from {len(sources)} data sources: {', '.join(sources)}"
            new_insights.append(insight)
        
        self.insights.extend(new_insights)
        return new_insights


class ContinuousLearningPipeline:
    def __init__(self, db_path: str = "learning_data.db"):
        self.db_path = db_path
        self.scrapers: Dict[str, WebScraper] = {}
        self.pattern_detector = PatternDetector()
        self.insight_generator = InsightGenerator()
        self.data_buffer: List[DataPoint] = []
        self.is_running = False
        self.learning_thread: Optional[threading.Thread] = None
        self.callbacks: List[Callable[[LearningResult], None]] = []
        
        self._init_database()
    
    def _init_database(self):
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS data_points (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                source TEXT,
                source_type TEXT,
                content_hash TEXT UNIQUE,
                content TEXT,
                metadata TEXT,
                timestamp TEXT,
                processed INTEGER DEFAULT 0
            )
        ''')
        
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS learning_results (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                patterns_detected INTEGER,
                correlations_found INTEGER,
                anomalies_identified INTEGER,
                models_updated INTEGER,
                insights TEXT,
                processing_time REAL,
                timestamp TEXT
            )
        ''')
        
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS patterns (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                pattern TEXT UNIQUE,
                count INTEGER,
                first_seen TEXT,
                last_seen TEXT
            )
        ''')
        
        conn.commit()
        conn.close()
    
    def add_scraper(self, config: ScraperConfig):
        scraper = WebScraper(config)
        self.scrapers[config.name] = scraper
        logger.info(f"Added scraper: {config.name} with {len(config.urls)} URLs")
    
    def add_callback(self, callback: Callable[[LearningResult], None]):
        self.callbacks.append(callback)
    
    def store_data_point(self, point: DataPoint):
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        try:
            cursor.execute('''
                INSERT OR IGNORE INTO data_points 
                (source, source_type, content_hash, content, metadata, timestamp)
                VALUES (?, ?, ?, ?, ?, ?)
            ''', (
                point.source,
                point.source_type.value,
                point.content_hash,
                point.content,
                json.dumps(point.metadata),
                point.timestamp.isoformat()
            ))
            conn.commit()
        except Exception as e:
            logger.error(f"Error storing data point: {e}")
        finally:
            conn.close()
    
    def store_learning_result(self, result: LearningResult):
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        try:
            cursor.execute('''
                INSERT INTO learning_results 
                (patterns_detected, correlations_found, anomalies_identified, 
                 models_updated, insights, processing_time, timestamp)
                VALUES (?, ?, ?, ?, ?, ?, ?)
            ''', (
                result.patterns_detected,
                result.correlations_found,
                result.anomalies_identified,
                result.models_updated,
                json.dumps(result.insights_generated),
                result.processing_time,
                datetime.now().isoformat()
            ))
            conn.commit()
        except Exception as e:
            logger.error(f"Error storing learning result: {e}")
        finally:
            conn.close()
    
    async def run_scrape_cycle(self) -> List[DataPoint]:
        all_points = []
        
        for name, scraper in self.scrapers.items():
            try:
                await scraper.initialize()
                points = await scraper.run_scrape_cycle()
                all_points.extend(points)
                await scraper.close()
                
                logger.info(f"Scraper {name} collected {len(points)} data points")
            except Exception as e:
                logger.error(f"Error in scraper {name}: {e}")
        
        return all_points
    
    def process_learning_cycle(self, data_points: List[DataPoint]) -> LearningResult:
        start_time = time.time()
        
        for point in data_points:
            self.store_data_point(point)
            self.data_buffer.append(point)
        
        if len(self.data_buffer) < 10:
            return LearningResult()
        
        patterns = self.pattern_detector.detect_patterns(self.data_buffer)
        correlations = self.pattern_detector.find_correlations(self.data_buffer)
        anomalies = self.pattern_detector.identify_anomalies(self.data_buffer)
        
        insights = self.insight_generator.generate_insights(
            self.data_buffer, 
            self.pattern_detector.patterns
        )
        
        if len(self.data_buffer) > 1000:
            self.data_buffer = self.data_buffer[-500:]
        
        processing_time = time.time() - start_time
        
        result = LearningResult(
            patterns_detected=patterns,
            correlations_found=correlations,
            anomalies_identified=anomalies,
            models_updated=1 if patterns > 0 else 0,
            insights_generated=insights,
            processing_time=processing_time
        )
        
        self.store_learning_result(result)
        
        for callback in self.callbacks:
            try:
                callback(result)
            except Exception as e:
                logger.error(f"Callback error: {e}")
        
        return result
    
    def _learning_loop(self):
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        
        while self.is_running:
            try:
                data_points = loop.run_until_complete(self.run_scrape_cycle())
                
                if data_points:
                    result = self.process_learning_cycle(data_points)
                    logger.info(
                        f"Learning cycle complete: {result.patterns_detected} patterns, "
                        f"{result.correlations_found} correlations, "
                        f"{len(result.insights_generated)} insights"
                    )
                
                time.sleep(60)
                
            except Exception as e:
                logger.error(f"Error in learning loop: {e}")
                time.sleep(10)
        
        loop.close()
    
    def start(self):
        if self.is_running:
            return
        
        self.is_running = True
        self.learning_thread = threading.Thread(target=self._learning_loop, daemon=True)
        self.learning_thread.start()
        logger.info("Continuous learning pipeline started")
    
    def stop(self):
        self.is_running = False
        if self.learning_thread:
            self.learning_thread.join(timeout=5)
        logger.info("Continuous learning pipeline stopped")
    
    def get_stats(self) -> Dict[str, Any]:
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        try:
            cursor.execute("SELECT COUNT(*) FROM data_points")
            total_data_points = cursor.fetchone()[0]
            
            cursor.execute("SELECT COUNT(*) FROM learning_results")
            total_cycles = cursor.fetchone()[0]
            
            cursor.execute("""
                SELECT SUM(patterns_detected), SUM(correlations_found), 
                       SUM(anomalies_identified), SUM(models_updated)
                FROM learning_results
            """)
            row = cursor.fetchone()
            
            cursor.execute("SELECT COUNT(*) FROM patterns")
            unique_patterns = cursor.fetchone()[0]
            
            return {
                "total_data_points": total_data_points,
                "total_learning_cycles": total_cycles,
                "total_patterns_detected": row[0] or 0,
                "total_correlations_found": row[1] or 0,
                "total_anomalies_identified": row[2] or 0,
                "total_models_updated": row[3] or 0,
                "unique_patterns": unique_patterns,
                "buffer_size": len(self.data_buffer),
                "active_scrapers": len(self.scrapers),
                "is_running": self.is_running
            }
        finally:
            conn.close()


def create_default_scrapers() -> List[ScraperConfig]:
    return [
        ScraperConfig(
            name="mental_models_news",
            urls=[
                "https://fs.blog/mental-models/",
                "https://www.farnamstreetblog.com/",
            ],
            interval_seconds=3600,
            follow_links=True,
            max_depth=1
        ),
        ScraperConfig(
            name="investing_wisdom",
            urls=[
                "https://www.berkshirehathaway.com/letters/letters.html",
            ],
            interval_seconds=86400,
            follow_links=True,
            max_depth=2
        ),
        ScraperConfig(
            name="decision_making",
            urls=[
                "https://www.lesswrong.com/",
            ],
            interval_seconds=7200,
            follow_links=False
        )
    ]


def create_continuous_learning_pipeline(db_path: str = "learning_data.db") -> ContinuousLearningPipeline:
    pipeline = ContinuousLearningPipeline(db_path)
    
    for config in create_default_scrapers():
        pipeline.add_scraper(config)
    
    return pipeline
