"""
Open Source Web Scraping Connectors

Fully open source alternatives to Firecrawl:
- Scrapy: Industrial-strength scraping framework
- Playwright: Browser automation for JS-heavy sites
- BeautifulSoup: Simple HTML parsing
- Selenium: Browser automation (legacy)
"""

import os
import json
import asyncio
import logging
import hashlib
from dataclasses import dataclass, field
from datetime import datetime
from typing import Dict, List, Optional, Any
from pathlib import Path
from urllib.parse import urlparse

from .base import BaseConnector, ConnectorConfig, ConnectorType, ConnectorStatus

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


@dataclass
class ScrapedPage:
    """A scraped web page."""
    url: str
    title: str
    content: str
    html: str
    links: List[str]
    images: List[str]
    metadata: Dict[str, Any]
    scraped_at: datetime
    
    def to_dict(self) -> Dict:
        return {
            "url": self.url,
            "title": self.title,
            "content": self.content,
            "links": self.links,
            "images": self.images,
            "metadata": self.metadata,
            "scraped_at": self.scraped_at.isoformat()
        }


# =============================================================================
# BEAUTIFULSOUP CONNECTOR (Simplest)
# =============================================================================

class BeautifulSoupConnector(BaseConnector):
    """
    Simple web scraping using BeautifulSoup + requests.
    
    Best for:
    - Static HTML pages
    - Simple scraping tasks
    - Quick one-off scrapes
    """
    
    NAME = "beautifulsoup"
    TYPE = ConnectorType.WEB_SCRAPING
    DESCRIPTION = "Simple HTML scraping with BeautifulSoup"
    REQUIRED_CREDENTIALS = []
    OPEN_SOURCE = True
    
    def __init__(self, config: ConnectorConfig = None):
        super().__init__(config)
        self._session = None
    
    async def connect(self) -> bool:
        """Initialize requests session."""
        try:
            import requests
            self._session = requests.Session()
            self._session.headers.update({
                "User-Agent": "Mozilla/5.0 (compatible; MentalModelsBot/1.0)"
            })
            self.status = ConnectorStatus.CONNECTED
            return True
        except ImportError:
            logger.error("requests not installed. Run: pip install requests beautifulsoup4")
            self.status = ConnectorStatus.ERROR
            return False
    
    async def disconnect(self) -> bool:
        """Close session."""
        if self._session:
            self._session.close()
        self.status = ConnectorStatus.DISCONNECTED
        return True
    
    async def health_check(self) -> bool:
        """Check if session is active."""
        return self._session is not None
    
    async def scrape(self, url: str, extract_text: bool = True) -> Optional[ScrapedPage]:
        """Scrape a single URL."""
        await self._rate_limit_check()
        
        try:
            from bs4 import BeautifulSoup
            
            response = self._session.get(url, timeout=self.config.timeout)
            response.raise_for_status()
            
            soup = BeautifulSoup(response.text, 'html.parser')
            
            # Extract title
            title = soup.title.string if soup.title else ""
            
            # Extract text content
            content = ""
            if extract_text:
                # Remove script and style elements
                for script in soup(["script", "style", "nav", "footer", "header"]):
                    script.decompose()
                content = soup.get_text(separator="\n", strip=True)
            
            # Extract links
            links = [
                a.get("href") for a in soup.find_all("a", href=True)
                if a.get("href").startswith(("http", "/"))
            ]
            
            # Extract images
            images = [
                img.get("src") for img in soup.find_all("img", src=True)
            ]
            
            # Extract metadata
            metadata = {}
            for meta in soup.find_all("meta"):
                name = meta.get("name") or meta.get("property")
                content_val = meta.get("content")
                if name and content_val:
                    metadata[name] = content_val
            
            return ScrapedPage(
                url=url,
                title=title,
                content=content,
                html=response.text,
                links=links[:100],  # Limit
                images=images[:50],
                metadata=metadata,
                scraped_at=datetime.now()
            )
            
        except Exception as e:
            logger.error(f"Failed to scrape {url}: {e}")
            return None
    
    async def scrape_multiple(self, urls: List[str], delay: float = 1.0) -> List[ScrapedPage]:
        """Scrape multiple URLs with delay."""
        results = []
        for url in urls:
            page = await self.scrape(url)
            if page:
                results.append(page)
            await asyncio.sleep(delay)
        return results
    
    async def search_and_scrape(self, query: str, num_results: int = 10) -> List[ScrapedPage]:
        """Search using DuckDuckGo and scrape results."""
        try:
            # Use DuckDuckGo HTML search (no API key needed)
            search_url = f"https://html.duckduckgo.com/html/?q={query}"
            
            from bs4 import BeautifulSoup
            response = self._session.get(search_url, timeout=30)
            soup = BeautifulSoup(response.text, 'html.parser')
            
            # Extract result URLs
            urls = []
            for result in soup.find_all("a", class_="result__a"):
                href = result.get("href")
                if href and href.startswith("http"):
                    urls.append(href)
                if len(urls) >= num_results:
                    break
            
            # Scrape each result
            return await self.scrape_multiple(urls)
            
        except Exception as e:
            logger.error(f"Search failed: {e}")
            return []


# =============================================================================
# PLAYWRIGHT CONNECTOR (JavaScript Support)
# =============================================================================

class PlaywrightConnector(BaseConnector):
    """
    Browser automation with Playwright.
    
    Best for:
    - JavaScript-heavy sites
    - Single-page applications
    - Sites requiring interaction
    - Screenshot capture
    """
    
    NAME = "playwright"
    TYPE = ConnectorType.WEB_SCRAPING
    DESCRIPTION = "Browser automation for JS-heavy sites"
    REQUIRED_CREDENTIALS = []
    OPEN_SOURCE = True
    
    def __init__(self, config: ConnectorConfig = None):
        super().__init__(config)
        self._browser = None
        self._context = None
        self._playwright = None
    
    async def connect(self) -> bool:
        """Launch browser."""
        try:
            from playwright.async_api import async_playwright
            
            self._playwright = await async_playwright().start()
            self._browser = await self._playwright.chromium.launch(
                headless=True,
                args=["--no-sandbox", "--disable-dev-shm-usage"]
            )
            self._context = await self._browser.new_context(
                user_agent="Mozilla/5.0 (compatible; MentalModelsBot/1.0)"
            )
            
            self.status = ConnectorStatus.CONNECTED
            return True
            
        except ImportError:
            logger.error("playwright not installed. Run: pip install playwright && playwright install")
            self.status = ConnectorStatus.ERROR
            return False
        except Exception as e:
            logger.error(f"Failed to launch browser: {e}")
            self.status = ConnectorStatus.ERROR
            return False
    
    async def disconnect(self) -> bool:
        """Close browser."""
        try:
            if self._context:
                await self._context.close()
            if self._browser:
                await self._browser.close()
            if self._playwright:
                await self._playwright.stop()
            
            self.status = ConnectorStatus.DISCONNECTED
            return True
        except Exception as e:
            logger.error(f"Error closing browser: {e}")
            return False
    
    async def health_check(self) -> bool:
        """Check if browser is running."""
        return self._browser is not None and self._browser.is_connected()
    
    async def scrape(self, url: str, wait_for: str = None,
                    screenshot: bool = False) -> Optional[ScrapedPage]:
        """Scrape a URL with full JavaScript execution."""
        await self._rate_limit_check()
        
        try:
            page = await self._context.new_page()
            
            try:
                await page.goto(url, wait_until="networkidle", timeout=60000)
                
                if wait_for:
                    await page.wait_for_selector(wait_for, timeout=10000)
                
                # Get title
                title = await page.title()
                
                # Get text content
                content = await page.evaluate("""
                    () => {
                        const elements = document.querySelectorAll('script, style, nav, footer, header');
                        elements.forEach(el => el.remove());
                        return document.body.innerText;
                    }
                """)
                
                # Get HTML
                html = await page.content()
                
                # Get links
                links = await page.evaluate("""
                    () => Array.from(document.querySelectorAll('a[href]'))
                        .map(a => a.href)
                        .filter(href => href.startsWith('http'))
                        .slice(0, 100)
                """)
                
                # Get images
                images = await page.evaluate("""
                    () => Array.from(document.querySelectorAll('img[src]'))
                        .map(img => img.src)
                        .slice(0, 50)
                """)
                
                # Get metadata
                metadata = await page.evaluate("""
                    () => {
                        const meta = {};
                        document.querySelectorAll('meta').forEach(m => {
                            const name = m.getAttribute('name') || m.getAttribute('property');
                            const content = m.getAttribute('content');
                            if (name && content) meta[name] = content;
                        });
                        return meta;
                    }
                """)
                
                # Screenshot if requested
                if screenshot:
                    screenshot_path = f"/tmp/screenshot_{hashlib.md5(url.encode()).hexdigest()[:8]}.png"
                    await page.screenshot(path=screenshot_path, full_page=True)
                    metadata["screenshot"] = screenshot_path
                
                return ScrapedPage(
                    url=url,
                    title=title,
                    content=content,
                    html=html,
                    links=links,
                    images=images,
                    metadata=metadata,
                    scraped_at=datetime.now()
                )
                
            finally:
                await page.close()
                
        except Exception as e:
            logger.error(f"Failed to scrape {url}: {e}")
            return None
    
    async def scrape_with_interaction(self, url: str,
                                      actions: List[Dict]) -> Optional[ScrapedPage]:
        """Scrape with custom interactions (clicks, scrolls, etc.)."""
        try:
            page = await self._context.new_page()
            
            try:
                await page.goto(url, wait_until="networkidle")
                
                # Execute actions
                for action in actions:
                    action_type = action.get("type")
                    
                    if action_type == "click":
                        await page.click(action["selector"])
                    elif action_type == "fill":
                        await page.fill(action["selector"], action["value"])
                    elif action_type == "scroll":
                        await page.evaluate("window.scrollBy(0, window.innerHeight)")
                    elif action_type == "wait":
                        await asyncio.sleep(action.get("seconds", 1))
                    elif action_type == "wait_for":
                        await page.wait_for_selector(action["selector"])
                
                # Now scrape
                return await self.scrape(url)
                
            finally:
                await page.close()
                
        except Exception as e:
            logger.error(f"Interaction scrape failed: {e}")
            return None


# =============================================================================
# SCRAPY CONNECTOR (Industrial Strength)
# =============================================================================

class ScrapyConnector(BaseConnector):
    """
    Industrial-strength web scraping with Scrapy.
    
    Best for:
    - Large-scale crawling
    - Complex scraping pipelines
    - Respect for robots.txt
    - Built-in rate limiting
    """
    
    NAME = "scrapy"
    TYPE = ConnectorType.WEB_SCRAPING
    DESCRIPTION = "Industrial-strength web crawling framework"
    REQUIRED_CREDENTIALS = []
    OPEN_SOURCE = True
    
    def __init__(self, config: ConnectorConfig = None):
        super().__init__(config)
        self._output_dir = Path("/tmp/scrapy_output")
    
    async def connect(self) -> bool:
        """Verify Scrapy is available."""
        try:
            import scrapy
            self._output_dir.mkdir(parents=True, exist_ok=True)
            self.status = ConnectorStatus.CONNECTED
            return True
        except ImportError:
            logger.error("scrapy not installed. Run: pip install scrapy")
            self.status = ConnectorStatus.ERROR
            return False
    
    async def disconnect(self) -> bool:
        """Cleanup."""
        self.status = ConnectorStatus.DISCONNECTED
        return True
    
    async def health_check(self) -> bool:
        """Check Scrapy availability."""
        try:
            import scrapy
            return True
        except ImportError:
            return False
    
    def _create_spider_script(self, urls: List[str], output_file: str) -> str:
        """Create a temporary Scrapy spider script."""
        script = f'''
import scrapy
import json
from scrapy.crawler import CrawlerProcess

class MentalModelsSpider(scrapy.Spider):
    name = "mental_models"
    start_urls = {json.dumps(urls)}
    
    custom_settings = {{
        "ROBOTSTXT_OBEY": True,
        "CONCURRENT_REQUESTS": 4,
        "DOWNLOAD_DELAY": 1,
        "FEEDS": {{
            "{output_file}": {{"format": "json"}},
        }},
    }}
    
    def parse(self, response):
        yield {{
            "url": response.url,
            "title": response.css("title::text").get(),
            "content": " ".join(response.css("body *::text").getall()),
            "links": response.css("a::attr(href)").getall()[:100],
            "images": response.css("img::attr(src)").getall()[:50],
        }}

if __name__ == "__main__":
    process = CrawlerProcess()
    process.crawl(MentalModelsSpider)
    process.start()
'''
        return script
    
    async def crawl(self, urls: List[str]) -> List[ScrapedPage]:
        """Crawl multiple URLs using Scrapy."""
        import subprocess
        import tempfile
        
        output_file = self._output_dir / f"output_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        
        # Create spider script
        script = self._create_spider_script(urls, str(output_file))
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as f:
            f.write(script)
            script_path = f.name
        
        try:
            # Run spider
            result = subprocess.run(
                ["python", script_path],
                capture_output=True,
                text=True,
                timeout=300
            )
            
            if result.returncode != 0:
                logger.error(f"Scrapy failed: {result.stderr}")
                return []
            
            # Read results
            if output_file.exists():
                with open(output_file, 'r') as f:
                    data = json.load(f)
                
                return [
                    ScrapedPage(
                        url=item["url"],
                        title=item.get("title", ""),
                        content=item.get("content", ""),
                        html="",
                        links=item.get("links", []),
                        images=item.get("images", []),
                        metadata={},
                        scraped_at=datetime.now()
                    )
                    for item in data
                ]
            
            return []
            
        finally:
            # Cleanup
            Path(script_path).unlink(missing_ok=True)
    
    async def scrape(self, url: str) -> Optional[ScrapedPage]:
        """Scrape a single URL."""
        results = await self.crawl([url])
        return results[0] if results else None


# =============================================================================
# RSS FEED CONNECTOR
# =============================================================================

class RSSConnector(BaseConnector):
    """
    RSS feed reader for news and blog monitoring.
    
    Best for:
    - News monitoring
    - Blog aggregation
    - Podcast feeds
    """
    
    NAME = "rss"
    TYPE = ConnectorType.DATA_SOURCE
    DESCRIPTION = "RSS/Atom feed reader"
    REQUIRED_CREDENTIALS = []
    OPEN_SOURCE = True
    
    def __init__(self, config: ConnectorConfig = None):
        super().__init__(config)
        self._feeds: Dict[str, str] = {}  # name -> url
    
    async def connect(self) -> bool:
        """Verify feedparser is available."""
        try:
            import feedparser
            self.status = ConnectorStatus.CONNECTED
            return True
        except ImportError:
            logger.error("feedparser not installed. Run: pip install feedparser")
            self.status = ConnectorStatus.ERROR
            return False
    
    async def disconnect(self) -> bool:
        self.status = ConnectorStatus.DISCONNECTED
        return True
    
    async def health_check(self) -> bool:
        try:
            import feedparser
            return True
        except ImportError:
            return False
    
    def add_feed(self, name: str, url: str):
        """Add a feed to monitor."""
        self._feeds[name] = url
    
    def add_default_feeds(self):
        """Add default financial/business feeds."""
        default_feeds = {
            "hacker_news": "https://news.ycombinator.com/rss",
            "reuters_business": "https://feeds.reuters.com/reuters/businessNews",
            "wsj_markets": "https://feeds.a.dj.com/rss/RSSMarketsMain.xml",
            "ft_markets": "https://www.ft.com/markets?format=rss",
            "sec_filings": "https://www.sec.gov/cgi-bin/browse-edgar?action=getcurrent&type=10-K&company=&dateb=&owner=include&count=40&output=atom",
        }
        self._feeds.update(default_feeds)
    
    async def fetch_feed(self, url: str) -> List[Dict]:
        """Fetch and parse a feed."""
        import feedparser
        
        feed = feedparser.parse(url)
        
        entries = []
        for entry in feed.entries:
            entries.append({
                "title": entry.get("title", ""),
                "link": entry.get("link", ""),
                "summary": entry.get("summary", ""),
                "published": entry.get("published", ""),
                "author": entry.get("author", ""),
                "tags": [t.get("term") for t in entry.get("tags", [])]
            })
        
        return entries
    
    async def fetch_all_feeds(self) -> Dict[str, List[Dict]]:
        """Fetch all registered feeds."""
        results = {}
        for name, url in self._feeds.items():
            try:
                results[name] = await self.fetch_feed(url)
            except Exception as e:
                logger.error(f"Failed to fetch {name}: {e}")
                results[name] = []
        return results
    
    async def search_feeds(self, query: str) -> List[Dict]:
        """Search across all feeds."""
        all_entries = await self.fetch_all_feeds()
        
        query_lower = query.lower()
        matches = []
        
        for feed_name, entries in all_entries.items():
            for entry in entries:
                if (query_lower in entry.get("title", "").lower() or
                    query_lower in entry.get("summary", "").lower()):
                    entry["feed"] = feed_name
                    matches.append(entry)
        
        return matches


# Register connectors
from .base import registry
registry.register_class(BeautifulSoupConnector)
registry.register_class(PlaywrightConnector)
registry.register_class(ScrapyConnector)
registry.register_class(RSSConnector)
