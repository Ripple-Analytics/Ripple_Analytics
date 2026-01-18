"""
Data Source Connectors

Financial and news data sources:
- SEC EDGAR (filings)
- Yahoo Finance (market data)
- Alpha Vantage (market data)
- GDELT (global events)
- RSS Feeds (news)
"""

import os
import json
import asyncio
import logging
import aiohttp
from dataclasses import dataclass
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any
from pathlib import Path

from .base import BaseConnector, ConnectorConfig, ConnectorType, ConnectorStatus
from .scraping import RSSConnector  # Re-export

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# =============================================================================
# SEC EDGAR CONNECTOR
# =============================================================================

@dataclass
class SECFiling:
    """An SEC filing."""
    accession_number: str
    form_type: str
    company_name: str
    cik: str
    filed_date: str
    document_url: str
    
    def to_dict(self) -> Dict:
        return {
            "accession_number": self.accession_number,
            "form_type": self.form_type,
            "company_name": self.company_name,
            "cik": self.cik,
            "filed_date": self.filed_date,
            "document_url": self.document_url
        }


class SECConnector(BaseConnector):
    """
    SEC EDGAR connector for company filings.
    
    Free access to:
    - 10-K (annual reports)
    - 10-Q (quarterly reports)
    - 8-K (current reports)
    - 13-F (institutional holdings)
    - DEF 14A (proxy statements)
    """
    
    NAME = "sec"
    TYPE = ConnectorType.DATA_SOURCE
    DESCRIPTION = "SEC EDGAR filings database"
    REQUIRED_CREDENTIALS = []
    OPTIONAL_CREDENTIALS = ["user_agent"]
    OPEN_SOURCE = True
    
    BASE_URL = "https://data.sec.gov"
    SEARCH_URL = "https://efts.sec.gov/LATEST/search-index"
    
    def __init__(self, config: ConnectorConfig = None):
        super().__init__(config)
        self._user_agent = os.environ.get(
            "SEC_USER_AGENT",
            "MentalModelsSystem contact@example.com"
        )
    
    async def connect(self) -> bool:
        self.status = ConnectorStatus.CONNECTED
        return True
    
    async def disconnect(self) -> bool:
        self.status = ConnectorStatus.DISCONNECTED
        return True
    
    async def health_check(self) -> bool:
        return True
    
    async def get_company_filings(self, cik: str, form_type: str = None,
                                  limit: int = 10) -> List[SECFiling]:
        """Get filings for a company by CIK."""
        await self._rate_limit_check()
        
        # Pad CIK to 10 digits
        cik_padded = cik.zfill(10)
        
        url = f"{self.BASE_URL}/submissions/CIK{cik_padded}.json"
        
        async with aiohttp.ClientSession() as session:
            headers = {"User-Agent": self._user_agent}
            
            async with session.get(url, headers=headers) as resp:
                if resp.status != 200:
                    logger.error(f"SEC API error: {resp.status}")
                    return []
                
                data = await resp.json()
        
        filings = []
        recent = data.get("filings", {}).get("recent", {})
        
        forms = recent.get("form", [])
        accessions = recent.get("accessionNumber", [])
        dates = recent.get("filingDate", [])
        docs = recent.get("primaryDocument", [])
        
        company_name = data.get("name", "")
        
        for i in range(min(len(forms), limit)):
            if form_type and forms[i] != form_type:
                continue
            
            accession = accessions[i].replace("-", "")
            doc_url = f"{self.BASE_URL}/Archives/edgar/data/{cik}/{accession}/{docs[i]}"
            
            filings.append(SECFiling(
                accession_number=accessions[i],
                form_type=forms[i],
                company_name=company_name,
                cik=cik,
                filed_date=dates[i],
                document_url=doc_url
            ))
            
            if len(filings) >= limit:
                break
        
        return filings
    
    async def search_filings(self, query: str, form_type: str = None,
                            date_from: str = None, date_to: str = None,
                            limit: int = 20) -> List[SECFiling]:
        """Search SEC filings by keyword."""
        await self._rate_limit_check()
        
        # Use full-text search API
        url = "https://efts.sec.gov/LATEST/search-index"
        
        params = {
            "q": query,
            "dateRange": "custom",
            "startdt": date_from or (datetime.now() - timedelta(days=365)).strftime("%Y-%m-%d"),
            "enddt": date_to or datetime.now().strftime("%Y-%m-%d"),
        }
        
        if form_type:
            params["forms"] = form_type
        
        async with aiohttp.ClientSession() as session:
            headers = {"User-Agent": self._user_agent}
            
            async with session.get(url, params=params, headers=headers) as resp:
                if resp.status != 200:
                    logger.error(f"SEC search error: {resp.status}")
                    return []
                
                data = await resp.json()
        
        filings = []
        for hit in data.get("hits", {}).get("hits", [])[:limit]:
            source = hit.get("_source", {})
            
            filings.append(SECFiling(
                accession_number=source.get("adsh", ""),
                form_type=source.get("form", ""),
                company_name=source.get("display_names", [""])[0],
                cik=source.get("ciks", [""])[0],
                filed_date=source.get("file_date", ""),
                document_url=source.get("file_url", "")
            ))
        
        return filings
    
    async def get_filing_content(self, filing: SECFiling) -> Optional[str]:
        """Download and extract filing content."""
        await self._rate_limit_check()
        
        async with aiohttp.ClientSession() as session:
            headers = {"User-Agent": self._user_agent}
            
            async with session.get(filing.document_url, headers=headers) as resp:
                if resp.status != 200:
                    return None
                
                return await resp.text()


# =============================================================================
# YAHOO FINANCE CONNECTOR
# =============================================================================

@dataclass
class StockQuote:
    """A stock quote."""
    symbol: str
    price: float
    change: float
    change_percent: float
    volume: int
    market_cap: float
    pe_ratio: float
    timestamp: datetime
    
    def to_dict(self) -> Dict:
        return {
            "symbol": self.symbol,
            "price": self.price,
            "change": self.change,
            "change_percent": self.change_percent,
            "volume": self.volume,
            "market_cap": self.market_cap,
            "pe_ratio": self.pe_ratio,
            "timestamp": self.timestamp.isoformat()
        }


class YahooFinanceConnector(BaseConnector):
    """
    Yahoo Finance connector using yfinance library.
    
    Free access to:
    - Real-time quotes
    - Historical data
    - Company info
    - Financial statements
    - Options data
    """
    
    NAME = "yahoo_finance"
    TYPE = ConnectorType.DATA_SOURCE
    DESCRIPTION = "Yahoo Finance market data"
    REQUIRED_CREDENTIALS = []
    OPEN_SOURCE = True
    
    def __init__(self, config: ConnectorConfig = None):
        super().__init__(config)
        self._yf = None
    
    async def connect(self) -> bool:
        try:
            import yfinance as yf
            self._yf = yf
            self.status = ConnectorStatus.CONNECTED
            return True
        except ImportError:
            logger.error("yfinance not installed. Run: pip install yfinance")
            self.status = ConnectorStatus.ERROR
            return False
    
    async def disconnect(self) -> bool:
        self.status = ConnectorStatus.DISCONNECTED
        return True
    
    async def health_check(self) -> bool:
        return self._yf is not None
    
    async def get_quote(self, symbol: str) -> Optional[StockQuote]:
        """Get current quote for a symbol."""
        if not self._yf:
            return None
        
        try:
            ticker = self._yf.Ticker(symbol)
            info = ticker.info
            
            return StockQuote(
                symbol=symbol,
                price=info.get("currentPrice", info.get("regularMarketPrice", 0)),
                change=info.get("regularMarketChange", 0),
                change_percent=info.get("regularMarketChangePercent", 0),
                volume=info.get("regularMarketVolume", 0),
                market_cap=info.get("marketCap", 0),
                pe_ratio=info.get("trailingPE", 0),
                timestamp=datetime.now()
            )
        except Exception as e:
            logger.error(f"Failed to get quote for {symbol}: {e}")
            return None
    
    async def get_quotes(self, symbols: List[str]) -> List[StockQuote]:
        """Get quotes for multiple symbols."""
        quotes = []
        for symbol in symbols:
            quote = await self.get_quote(symbol)
            if quote:
                quotes.append(quote)
        return quotes
    
    async def get_historical(self, symbol: str, period: str = "1y",
                            interval: str = "1d") -> List[Dict]:
        """Get historical price data."""
        if not self._yf:
            return []
        
        try:
            ticker = self._yf.Ticker(symbol)
            hist = ticker.history(period=period, interval=interval)
            
            return [
                {
                    "date": idx.isoformat(),
                    "open": row["Open"],
                    "high": row["High"],
                    "low": row["Low"],
                    "close": row["Close"],
                    "volume": row["Volume"]
                }
                for idx, row in hist.iterrows()
            ]
        except Exception as e:
            logger.error(f"Failed to get history for {symbol}: {e}")
            return []
    
    async def get_financials(self, symbol: str) -> Dict:
        """Get financial statements."""
        if not self._yf:
            return {}
        
        try:
            ticker = self._yf.Ticker(symbol)
            
            return {
                "income_statement": ticker.income_stmt.to_dict() if hasattr(ticker, 'income_stmt') else {},
                "balance_sheet": ticker.balance_sheet.to_dict() if hasattr(ticker, 'balance_sheet') else {},
                "cash_flow": ticker.cashflow.to_dict() if hasattr(ticker, 'cashflow') else {}
            }
        except Exception as e:
            logger.error(f"Failed to get financials for {symbol}: {e}")
            return {}
    
    async def get_company_info(self, symbol: str) -> Dict:
        """Get company information."""
        if not self._yf:
            return {}
        
        try:
            ticker = self._yf.Ticker(symbol)
            return ticker.info
        except Exception as e:
            logger.error(f"Failed to get info for {symbol}: {e}")
            return {}
    
    async def screen_stocks(self, criteria: Dict) -> List[str]:
        """Screen stocks based on criteria."""
        # This would require a stock screener API or database
        # For now, return popular stocks
        logger.warning("Stock screening requires additional data source")
        return ["AAPL", "MSFT", "GOOGL", "AMZN", "META"]


# =============================================================================
# GDELT CONNECTOR (Global Events)
# =============================================================================

class GDELTConnector(BaseConnector):
    """
    GDELT Project connector for global events data.
    
    Free access to:
    - Global news events
    - Sentiment analysis
    - Geographic data
    - Entity extraction
    """
    
    NAME = "gdelt"
    TYPE = ConnectorType.DATA_SOURCE
    DESCRIPTION = "GDELT global events database"
    REQUIRED_CREDENTIALS = []
    OPEN_SOURCE = True
    
    BASE_URL = "https://api.gdeltproject.org/api/v2"
    
    def __init__(self, config: ConnectorConfig = None):
        super().__init__(config)
    
    async def connect(self) -> bool:
        self.status = ConnectorStatus.CONNECTED
        return True
    
    async def disconnect(self) -> bool:
        self.status = ConnectorStatus.DISCONNECTED
        return True
    
    async def health_check(self) -> bool:
        return True
    
    async def search_news(self, query: str, mode: str = "artlist",
                         max_records: int = 75, timespan: str = "1d") -> List[Dict]:
        """Search GDELT news database."""
        await self._rate_limit_check()
        
        url = f"{self.BASE_URL}/doc/doc"
        
        params = {
            "query": query,
            "mode": mode,
            "maxrecords": max_records,
            "timespan": timespan,
            "format": "json"
        }
        
        async with aiohttp.ClientSession() as session:
            async with session.get(url, params=params) as resp:
                if resp.status != 200:
                    logger.error(f"GDELT API error: {resp.status}")
                    return []
                
                data = await resp.json()
        
        return data.get("articles", [])
    
    async def get_timeline(self, query: str, mode: str = "timelinevol",
                          timespan: str = "1m") -> List[Dict]:
        """Get event timeline."""
        await self._rate_limit_check()
        
        url = f"{self.BASE_URL}/doc/doc"
        
        params = {
            "query": query,
            "mode": mode,
            "timespan": timespan,
            "format": "json"
        }
        
        async with aiohttp.ClientSession() as session:
            async with session.get(url, params=params) as resp:
                if resp.status != 200:
                    return []
                
                data = await resp.json()
        
        return data.get("timeline", [])
    
    async def get_tone(self, query: str, timespan: str = "1m") -> Dict:
        """Get sentiment/tone analysis."""
        await self._rate_limit_check()
        
        url = f"{self.BASE_URL}/doc/doc"
        
        params = {
            "query": query,
            "mode": "timelinetone",
            "timespan": timespan,
            "format": "json"
        }
        
        async with aiohttp.ClientSession() as session:
            async with session.get(url, params=params) as resp:
                if resp.status != 200:
                    return {}
                
                return await resp.json()


# =============================================================================
# ALPHA VANTAGE CONNECTOR
# =============================================================================

class AlphaVantageConnector(BaseConnector):
    """
    Alpha Vantage connector for market data.
    
    Free tier available:
    - Real-time quotes
    - Historical data
    - Technical indicators
    - Fundamental data
    """
    
    NAME = "alpha_vantage"
    TYPE = ConnectorType.DATA_SOURCE
    DESCRIPTION = "Alpha Vantage market data API"
    REQUIRED_CREDENTIALS = ["api_key"]
    OPEN_SOURCE = True
    
    BASE_URL = "https://www.alphavantage.co/query"
    
    def __init__(self, config: ConnectorConfig = None):
        super().__init__(config)
        self._api_key = os.environ.get("ALPHA_VANTAGE_API_KEY")
    
    async def connect(self) -> bool:
        api_key = self.config.credentials.get("api_key") or self._api_key
        if api_key:
            self._api_key = api_key
            self.status = ConnectorStatus.CONNECTED
            return True
        
        logger.warning("Alpha Vantage API key not set")
        self.status = ConnectorStatus.ERROR
        return False
    
    async def disconnect(self) -> bool:
        self.status = ConnectorStatus.DISCONNECTED
        return True
    
    async def health_check(self) -> bool:
        return self._api_key is not None
    
    async def get_quote(self, symbol: str) -> Optional[Dict]:
        """Get real-time quote."""
        await self._rate_limit_check()
        
        params = {
            "function": "GLOBAL_QUOTE",
            "symbol": symbol,
            "apikey": self._api_key
        }
        
        async with aiohttp.ClientSession() as session:
            async with session.get(self.BASE_URL, params=params) as resp:
                if resp.status != 200:
                    return None
                
                data = await resp.json()
                return data.get("Global Quote", {})
    
    async def get_daily(self, symbol: str, outputsize: str = "compact") -> List[Dict]:
        """Get daily historical data."""
        await self._rate_limit_check()
        
        params = {
            "function": "TIME_SERIES_DAILY",
            "symbol": symbol,
            "outputsize": outputsize,
            "apikey": self._api_key
        }
        
        async with aiohttp.ClientSession() as session:
            async with session.get(self.BASE_URL, params=params) as resp:
                if resp.status != 200:
                    return []
                
                data = await resp.json()
                time_series = data.get("Time Series (Daily)", {})
                
                return [
                    {"date": date, **values}
                    for date, values in time_series.items()
                ]


# Register connectors
from .base import registry
registry.register_class(SECConnector)
registry.register_class(YahooFinanceConnector)
registry.register_class(GDELTConnector)
registry.register_class(AlphaVantageConnector)
