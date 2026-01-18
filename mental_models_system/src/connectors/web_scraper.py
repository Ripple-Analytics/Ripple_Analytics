"""
Open source web scraping alternatives to Firecrawl.

Supports:
- BeautifulSoup for HTML parsing
- Scrapy for structured crawling
- Selenium (headless) for JavaScript-rendered pages

Note: All scrapers run in headless mode only.
"""

from typing import Any, Dict, List, Optional
import aiohttp
import logging
from dataclasses import dataclass, field
from abc import ABC, abstractmethod

from .base import BaseConnector, ConnectorConfig, ConnectorResult

logger = logging.getLogger(__name__)


@dataclass
class ScrapedPage:
    """Result from scraping a single page."""
    url: str
    title: str = ""
    text: str = ""
    html: str = ""
    links: List[str] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)


class WebScraper(BaseConnector):
    """
    Open source web scraper using BeautifulSoup.
    
    This is a simple, lightweight scraper for basic HTML pages.
    For JavaScript-rendered pages, use SeleniumScraper.
    """
    
    def __init__(self, config: ConnectorConfig):
        super().__init__(config)
        self._session: Optional[aiohttp.ClientSession] = None
        self._user_agent = config.settings.get(
            "user_agent",
            "Mozilla/5.0 (compatible; RippleAnalytics/1.0; +https://github.com/Ripple-Analytics)"
        )
    
    async def connect(self) -> bool:
        """Initialize HTTP session."""
        try:
            headers = {
                "User-Agent": self._user_agent,
                "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
                "Accept-Language": "en-US,en;q=0.5"
            }
            
            timeout = aiohttp.ClientTimeout(total=self.config.timeout)
            self._session = aiohttp.ClientSession(headers=headers, timeout=timeout)
            self._connected = True
            return True
        except Exception as e:
            logger.error(f"Failed to initialize scraper: {e}")
            return False
    
    async def disconnect(self) -> bool:
        """Close HTTP session."""
        if self._session:
            await self._session.close()
            self._session = None
        self._connected = False
        return True
    
    async def fetch(self, query: Dict[str, Any]) -> ConnectorResult:
        """
        Fetch and parse a web page.
        
        Query parameters:
        - url: URL to scrape
        - extract: What to extract ("text", "links", "all")
        - selector: CSS selector for specific elements
        """
        if not self._session:
            return ConnectorResult(success=False, error="Not connected")
        
        try:
            url = query.get("url")
            extract = query.get("extract", "all")
            selector = query.get("selector")
            
            async with self._session.get(url) as resp:
                if resp.status != 200:
                    return ConnectorResult(
                        success=False,
                        error=f"HTTP {resp.status}: {resp.reason}"
                    )
                
                html = await resp.text()
                
                # Parse with BeautifulSoup
                from bs4 import BeautifulSoup
                soup = BeautifulSoup(html, "html.parser")
                
                # Extract title
                title_tag = soup.find("title")
                title = title_tag.get_text(strip=True) if title_tag else ""
                
                # Extract based on selector or full page
                if selector:
                    elements = soup.select(selector)
                    text = "\n".join(el.get_text(strip=True) for el in elements)
                else:
                    # Remove script and style elements
                    for script in soup(["script", "style", "nav", "footer", "header"]):
                        script.decompose()
                    text = soup.get_text(separator="\n", strip=True)
                
                # Extract links
                links = []
                if extract in ["links", "all"]:
                    for a in soup.find_all("a", href=True):
                        href = a["href"]
                        if href.startswith("http"):
                            links.append(href)
                
                page = ScrapedPage(
                    url=url,
                    title=title,
                    text=text,
                    html=html if extract == "all" else "",
                    links=links,
                    metadata={"status": resp.status, "content_type": resp.content_type}
                )
                
                return ConnectorResult(
                    success=True,
                    data=page,
                    metadata={"url": url, "text_length": len(text)}
                )
                
        except ImportError:
            return ConnectorResult(
                success=False,
                error="BeautifulSoup not installed. Install with: pip install beautifulsoup4"
            )
        except Exception as e:
            logger.error(f"Scraping error: {e}")
            return ConnectorResult(success=False, error=str(e))
    
    async def push(self, data: Any) -> ConnectorResult:
        """Web scraper doesn't support push operations."""
        return ConnectorResult(success=False, error="Push not supported for web scraper")
    
    async def scrape_url(self, url: str, selector: Optional[str] = None) -> ConnectorResult:
        """Scrape a single URL."""
        return await self.fetch({"url": url, "selector": selector, "extract": "all"})
    
    async def scrape_multiple(self, urls: List[str]) -> List[ConnectorResult]:
        """Scrape multiple URLs."""
        results = []
        for url in urls:
            result = await self.scrape_url(url)
            results.append(result)
        return results


class BeautifulSoupParser:
    """
    Standalone BeautifulSoup parser for HTML content.
    
    Use this when you already have HTML content and just need to parse it.
    """
    
    def __init__(self, parser: str = "html.parser"):
        self.parser = parser
    
    def parse(self, html: str) -> "BeautifulSoup":
        """Parse HTML content."""
        from bs4 import BeautifulSoup
        return BeautifulSoup(html, self.parser)
    
    def extract_text(self, html: str, selector: Optional[str] = None) -> str:
        """Extract text from HTML."""
        soup = self.parse(html)
        
        if selector:
            elements = soup.select(selector)
            return "\n".join(el.get_text(strip=True) for el in elements)
        
        # Remove non-content elements
        for tag in soup(["script", "style", "nav", "footer", "header"]):
            tag.decompose()
        
        return soup.get_text(separator="\n", strip=True)
    
    def extract_links(self, html: str, base_url: str = "") -> List[str]:
        """Extract all links from HTML."""
        from urllib.parse import urljoin
        
        soup = self.parse(html)
        links = []
        
        for a in soup.find_all("a", href=True):
            href = a["href"]
            if base_url and not href.startswith("http"):
                href = urljoin(base_url, href)
            if href.startswith("http"):
                links.append(href)
        
        return links
    
    def extract_metadata(self, html: str) -> Dict[str, str]:
        """Extract metadata from HTML head."""
        soup = self.parse(html)
        metadata = {}
        
        # Title
        title = soup.find("title")
        if title:
            metadata["title"] = title.get_text(strip=True)
        
        # Meta tags
        for meta in soup.find_all("meta"):
            name = meta.get("name") or meta.get("property")
            content = meta.get("content")
            if name and content:
                metadata[name] = content
        
        return metadata


class ScrapySpider:
    """
    Scrapy-based spider for structured web crawling.
    
    This is a wrapper around Scrapy for more complex crawling needs.
    Note: Scrapy runs synchronously, so this is for batch processing.
    """
    
    def __init__(self, name: str = "ripple_spider", allowed_domains: Optional[List[str]] = None):
        self.name = name
        self.allowed_domains = allowed_domains or []
        self.start_urls: List[str] = []
        self.results: List[ScrapedPage] = []
    
    def configure(
        self,
        start_urls: List[str],
        allowed_domains: Optional[List[str]] = None,
        max_depth: int = 2,
        max_pages: int = 100
    ) -> None:
        """Configure the spider."""
        self.start_urls = start_urls
        if allowed_domains:
            self.allowed_domains = allowed_domains
        self.max_depth = max_depth
        self.max_pages = max_pages
    
    def run(self) -> List[ScrapedPage]:
        """
        Run the spider and return results.
        
        Note: This requires Scrapy to be installed.
        """
        try:
            from scrapy.crawler import CrawlerProcess
            from scrapy import Spider, Request
            from scrapy.http import Response
            
            results = []
            
            class RippleSpider(Spider):
                name = self.name
                allowed_domains = self.allowed_domains
                start_urls = self.start_urls
                custom_settings = {
                    "DEPTH_LIMIT": self.max_depth,
                    "CLOSESPIDER_PAGECOUNT": self.max_pages,
                    "LOG_LEVEL": "WARNING",
                    "ROBOTSTXT_OBEY": True,
                }
                
                def parse(inner_self, response: Response):
                    from bs4 import BeautifulSoup
                    
                    soup = BeautifulSoup(response.text, "html.parser")
                    
                    # Remove non-content
                    for tag in soup(["script", "style"]):
                        tag.decompose()
                    
                    title = soup.find("title")
                    
                    page = ScrapedPage(
                        url=response.url,
                        title=title.get_text(strip=True) if title else "",
                        text=soup.get_text(separator="\n", strip=True),
                        html=response.text,
                        links=[link.url for link in response.css("a::attr(href)").getall()]
                    )
                    results.append(page)
                    
                    # Follow links
                    for href in response.css("a::attr(href)").getall():
                        yield response.follow(href, inner_self.parse)
            
            process = CrawlerProcess()
            process.crawl(RippleSpider)
            process.start()
            
            self.results = results
            return results
            
        except ImportError:
            logger.error("Scrapy not installed. Install with: pip install scrapy")
            return []
        except Exception as e:
            logger.error(f"Scrapy error: {e}")
            return []


class SeleniumScraper:
    """
    Selenium-based scraper for JavaScript-rendered pages.
    
    IMPORTANT: This scraper runs in HEADLESS mode only.
    """
    
    def __init__(self, headless: bool = True):
        if not headless:
            raise ValueError("SeleniumScraper only supports headless mode")
        self.headless = True
        self._driver = None
    
    def start(self) -> bool:
        """Start the Selenium driver."""
        try:
            from selenium import webdriver
            from selenium.webdriver.chrome.options import Options
            from selenium.webdriver.chrome.service import Service
            
            options = Options()
            options.add_argument("--headless")
            options.add_argument("--no-sandbox")
            options.add_argument("--disable-dev-shm-usage")
            options.add_argument("--disable-gpu")
            options.add_argument("--window-size=1920,1080")
            
            self._driver = webdriver.Chrome(options=options)
            return True
        except ImportError:
            logger.error("Selenium not installed. Install with: pip install selenium")
            return False
        except Exception as e:
            logger.error(f"Failed to start Selenium: {e}")
            return False
    
    def stop(self) -> None:
        """Stop the Selenium driver."""
        if self._driver:
            self._driver.quit()
            self._driver = None
    
    def scrape(self, url: str, wait_time: int = 5) -> Optional[ScrapedPage]:
        """
        Scrape a JavaScript-rendered page.
        
        Args:
            url: URL to scrape
            wait_time: Seconds to wait for JavaScript to render
        """
        if not self._driver:
            if not self.start():
                return None
        
        try:
            import time
            from bs4 import BeautifulSoup
            
            self._driver.get(url)
            time.sleep(wait_time)  # Wait for JS to render
            
            html = self._driver.page_source
            soup = BeautifulSoup(html, "html.parser")
            
            # Remove non-content
            for tag in soup(["script", "style"]):
                tag.decompose()
            
            title = soup.find("title")
            
            return ScrapedPage(
                url=url,
                title=title.get_text(strip=True) if title else "",
                text=soup.get_text(separator="\n", strip=True),
                html=html,
                links=[a["href"] for a in soup.find_all("a", href=True) if a["href"].startswith("http")]
            )
        except Exception as e:
            logger.error(f"Selenium scraping error: {e}")
            return None
