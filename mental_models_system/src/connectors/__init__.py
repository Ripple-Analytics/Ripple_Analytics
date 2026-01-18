"""
Comprehensive Connector Framework

All integrations for the Mental Models System - fully open source alternatives available.

CONNECTOR CATEGORIES:
├── Chat & Communication
│   ├── Slack (slack_connector)
│   ├── Discord (discord_connector)
│   ├── Matrix (matrix_connector)
│   ├── Mattermost (mattermost_connector)
│   └── Telegram (telegram_connector)
│
├── Version Control & Project Management
│   ├── GitHub (github_connector)
│   ├── GitLab (gitlab_connector)
│   ├── Linear (linear_connector)
│   ├── Jira (jira_connector)
│   └── Notion (notion_connector)
│
├── Web Scraping (Open Source)
│   ├── Scrapy (scrapy_connector)
│   ├── Playwright (playwright_connector)
│   ├── BeautifulSoup (bs4_connector)
│   └── Selenium (selenium_connector)
│
├── Data Sources
│   ├── RSS Feeds (rss_connector)
│   ├── SEC EDGAR (sec_connector)
│   ├── GDELT (gdelt_connector)
│   ├── Yahoo Finance (yahoo_connector)
│   └── Alpha Vantage (alphavantage_connector)
│
├── Storage
│   ├── Google Drive (gdrive_connector)
│   ├── S3/MinIO (s3_connector)
│   ├── Local Filesystem (local_connector)
│   └── Dropbox (dropbox_connector)
│
└── LLM Providers
    ├── Ollama (ollama_connector)
    ├── llama.cpp (llamacpp_connector)
    ├── vLLM (vllm_connector)
    ├── OpenAI (openai_connector)
    └── Anthropic (anthropic_connector)
"""

from .base import (
    BaseConnector,
    ConnectorConfig,
    ConnectorType,
    ConnectorStatus,
    ConnectorRegistry
)

from .github_connector import GitHubConnector
from .scraping import ScrapyConnector, PlaywrightConnector, BeautifulSoupConnector
from .chat import SlackConnector, DiscordConnector, MatrixConnector
from .data import RSSConnector, SECConnector, YahooFinanceConnector
from .storage import LocalConnector, S3Connector, GDriveConnector
from .llm import OllamaConnector, OpenAIConnector, LlamaCppConnector
from .zapier_connector import ZapierConnector, create_zapier_connector
from .huggingface_connector import HuggingfaceConnector, create_huggingface_connector
from .lm_studio_connector import (
    LMStudioConnector,
    create_lm_studio_connector,
    MENTAL_MODEL_PROMPTS,
    ChatMessage,
    CompletionResponse,
    LMStudioModel,
    LLAMAModelType,
)

__all__ = [
    # Base
    "BaseConnector",
    "ConnectorConfig", 
    "ConnectorType",
    "ConnectorStatus",
    "ConnectorRegistry",
    
    # GitHub
    "GitHubConnector",
    
    # Scraping
    "ScrapyConnector",
    "PlaywrightConnector", 
    "BeautifulSoupConnector",
    
    # Chat
    "SlackConnector",
    "DiscordConnector",
    "MatrixConnector",
    
    # Data
    "RSSConnector",
    "SECConnector",
    "YahooFinanceConnector",
    
    # Storage
    "LocalConnector",
    "S3Connector",
    "GDriveConnector",
    
    # LLM
    "OllamaConnector",
    "OpenAIConnector",
    "LlamaCppConnector",
    
    # Automation
    "ZapierConnector",
    "create_zapier_connector",
    
    # Huggingface
    "HuggingfaceConnector",
    "create_huggingface_connector",
    
    # LM Studio
    "LMStudioConnector",
    "create_lm_studio_connector",
    "MENTAL_MODEL_PROMPTS",
    "ChatMessage",
    "CompletionResponse",
    "LMStudioModel",
    "LLAMAModelType",
]
