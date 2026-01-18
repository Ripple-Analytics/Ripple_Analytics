"""
Connectors for integrating with external data sources and services.

Available Connectors:
- GitHub: Repository data, issues, PRs, commits
- Google Drive: Documents, spreadsheets, files
- Email: Gmail, Outlook integration
- Slack: Messages, channels, threads
- Database: PostgreSQL, MySQL, SQLite
- Web Scraping: Scrapy, BeautifulSoup, Selenium (headless)
- API: Generic REST/GraphQL client
- File System: Local and remote file access
"""

from .base import BaseConnector, ConnectorConfig
from .github_connector import GitHubConnector
from .google_drive_connector import GoogleDriveConnector
from .slack_connector import SlackConnector
from .database_connector import DatabaseConnector
from .web_scraper import WebScraper, ScrapySpider, BeautifulSoupParser
from .api_connector import APIConnector
from .file_connector import FileConnector

__all__ = [
    "BaseConnector",
    "ConnectorConfig",
    "GitHubConnector",
    "GoogleDriveConnector",
    "SlackConnector",
    "DatabaseConnector",
    "WebScraper",
    "ScrapySpider",
    "BeautifulSoupParser",
    "APIConnector",
    "FileConnector",
]
