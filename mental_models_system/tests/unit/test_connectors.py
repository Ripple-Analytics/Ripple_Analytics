"""
Unit tests for connector framework.
"""

import pytest
from unittest.mock import AsyncMock, MagicMock, patch

# Import connectors
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from connectors.base import (
    BaseConnector, ConnectorConfig, ConnectorRegistry,
    ConnectorType, ConnectorStatus
)


class TestConnectorConfig:
    """Tests for ConnectorConfig."""
    
    def test_default_config(self):
        """Test default configuration."""
        config = ConnectorConfig()
        assert config.credentials == {}
        assert config.settings == {}
        assert config.enabled is True
    
    def test_custom_config(self):
        """Test custom configuration."""
        config = ConnectorConfig(
            credentials={"api_key": "test"},
            settings={"timeout": 30},
            enabled=False
        )
        assert config.credentials["api_key"] == "test"
        assert config.settings["timeout"] == 30
        assert config.enabled is False


class TestConnectorRegistry:
    """Tests for ConnectorRegistry."""
    
    def test_registry_singleton(self):
        """Test registry is singleton."""
        reg1 = ConnectorRegistry()
        reg2 = ConnectorRegistry()
        # Both should access the same registered connectors
        assert reg1.list_available() == reg2.list_available()
    
    def test_list_available(self):
        """Test listing available connectors."""
        registry = ConnectorRegistry()
        available = registry.list_available()
        
        # Should have connectors registered
        assert len(available) > 0
        
        # Each should have required fields
        for connector in available:
            assert "name" in connector
            assert "type" in connector
            assert "description" in connector
            assert "open_source" in connector
    
    def test_list_by_type(self):
        """Test listing connectors by type."""
        registry = ConnectorRegistry()
        
        # List LLM connectors
        llm_connectors = registry.list_by_type(ConnectorType.LLM)
        for conn in llm_connectors:
            assert conn["type"] == ConnectorType.LLM.value
    
    def test_list_open_source(self):
        """Test listing only open source connectors."""
        registry = ConnectorRegistry()
        oss_connectors = registry.list_open_source()
        
        for conn in oss_connectors:
            assert conn["open_source"] is True


class TestConnectorStatus:
    """Tests for ConnectorStatus enum."""
    
    def test_status_values(self):
        """Test status enum values."""
        assert ConnectorStatus.DISCONNECTED.value == "disconnected"
        assert ConnectorStatus.CONNECTING.value == "connecting"
        assert ConnectorStatus.CONNECTED.value == "connected"
        assert ConnectorStatus.ERROR.value == "error"


class TestConnectorType:
    """Tests for ConnectorType enum."""
    
    def test_type_values(self):
        """Test type enum values."""
        assert ConnectorType.CHAT.value == "chat"
        assert ConnectorType.SCRAPING.value == "scraping"
        assert ConnectorType.DATA.value == "data"
        assert ConnectorType.STORAGE.value == "storage"
        assert ConnectorType.LLM.value == "llm"
        assert ConnectorType.VERSION_CONTROL.value == "version_control"


@pytest.mark.asyncio
class TestBaseConnector:
    """Tests for BaseConnector abstract class."""
    
    async def test_connector_info(self, mock_connector):
        """Test getting connector info."""
        # Create a concrete mock
        mock_connector.NAME = "test"
        mock_connector.TYPE = ConnectorType.DATA
        mock_connector.DESCRIPTION = "Test connector"
        mock_connector.OPEN_SOURCE = True
        mock_connector.status = ConnectorStatus.DISCONNECTED
        
        assert mock_connector.status == ConnectorStatus.DISCONNECTED


@pytest.mark.asyncio
class TestOllamaConnector:
    """Tests for Ollama connector."""
    
    async def test_connect_success(self):
        """Test successful connection."""
        with patch('aiohttp.ClientSession') as mock_session:
            mock_response = AsyncMock()
            mock_response.status = 200
            mock_response.__aenter__ = AsyncMock(return_value=mock_response)
            mock_response.__aexit__ = AsyncMock()
            
            mock_session_instance = MagicMock()
            mock_session_instance.get = MagicMock(return_value=mock_response)
            mock_session_instance.__aenter__ = AsyncMock(return_value=mock_session_instance)
            mock_session_instance.__aexit__ = AsyncMock()
            mock_session.return_value = mock_session_instance
            
            from connectors.llm import OllamaConnector
            connector = OllamaConnector()
            # Test would require actual async context
    
    async def test_generate_prompt(self, mock_llm_client):
        """Test generating text from prompt."""
        response = await mock_llm_client.generate("Test prompt")
        assert response.text is not None
        assert response.tokens_used > 0
    
    async def test_embed_text(self, mock_llm_client):
        """Test generating embeddings."""
        embedding = await mock_llm_client.embed("Test text")
        assert len(embedding) > 0
        assert all(isinstance(x, float) for x in embedding)


@pytest.mark.asyncio
class TestGitHubConnector:
    """Tests for GitHub connector."""
    
    async def test_create_issue(self, mock_github_connector):
        """Test creating an issue."""
        result = await mock_github_connector.create_issue(
            title="Test Issue",
            body="Test body",
            repo="test/repo"
        )
        assert "number" in result
    
    async def test_list_issues(self, mock_github_connector):
        """Test listing issues."""
        issues = await mock_github_connector.list_issues()
        assert isinstance(issues, list)
    
    async def test_create_pr(self, mock_github_connector):
        """Test creating a PR."""
        result = await mock_github_connector.create_pr(
            title="Test PR",
            body="Test body",
            head="feature-branch"
        )
        assert "number" in result


@pytest.mark.asyncio
class TestSlackConnector:
    """Tests for Slack connector."""
    
    async def test_send_message(self, mock_slack_connector):
        """Test sending a message."""
        result = await mock_slack_connector.send_message(
            channel="#test",
            message="Test message"
        )
        assert "ts" in result
    
    async def test_read_channel(self, mock_slack_connector):
        """Test reading channel messages."""
        messages = await mock_slack_connector.read_channel()
        assert isinstance(messages, list)


@pytest.mark.asyncio
class TestLocalStorageConnector:
    """Tests for local storage connector."""
    
    async def test_write_and_read(self, temp_data_dir):
        """Test writing and reading files."""
        from connectors.storage import LocalConnector
        
        connector = LocalConnector(ConnectorConfig(
            settings={"base_path": str(temp_data_dir)}
        ))
        await connector.connect()
        
        # Write file
        await connector.write_text("test.txt", "Hello World")
        
        # Read file
        content = await connector.read_text("test.txt")
        assert content == "Hello World"
        
        await connector.disconnect()
    
    async def test_list_files(self, temp_data_dir):
        """Test listing files."""
        from connectors.storage import LocalConnector
        
        connector = LocalConnector(ConnectorConfig(
            settings={"base_path": str(temp_data_dir)}
        ))
        await connector.connect()
        
        # Create some files
        await connector.write_text("file1.txt", "Content 1")
        await connector.write_text("file2.txt", "Content 2")
        
        # List files
        files = await connector.list_files()
        assert len(files) >= 2
        
        await connector.disconnect()
    
    async def test_delete_file(self, temp_data_dir):
        """Test deleting files."""
        from connectors.storage import LocalConnector
        
        connector = LocalConnector(ConnectorConfig(
            settings={"base_path": str(temp_data_dir)}
        ))
        await connector.connect()
        
        # Create and delete file
        await connector.write_text("to_delete.txt", "Delete me")
        result = await connector.delete_file("to_delete.txt")
        assert result is True
        
        # Verify deleted
        content = await connector.read_text("to_delete.txt")
        assert content is None
        
        await connector.disconnect()


@pytest.mark.asyncio
class TestZapierConnector:
    """Tests for Zapier connector."""
    
    async def test_connect(self):
        """Test connecting the Zapier connector."""
        from connectors.zapier_connector import ZapierConnector
        
        connector = ZapierConnector()
        result = await connector.connect()
        assert result is True
        assert connector.status == ConnectorStatus.CONNECTED
        await connector.disconnect()
    
    async def test_disconnect(self):
        """Test disconnecting the Zapier connector."""
        from connectors.zapier_connector import ZapierConnector
        
        connector = ZapierConnector()
        await connector.connect()
        result = await connector.disconnect()
        assert result is True
        assert connector.status == ConnectorStatus.DISCONNECTED
    
    async def test_health_check(self):
        """Test health check."""
        from connectors.zapier_connector import ZapierConnector
        
        connector = ZapierConnector()
        await connector.connect()
        assert await connector.health_check() is True
        await connector.disconnect()
        assert await connector.health_check() is False
    
    async def test_register_webhook(self):
        """Test registering webhooks."""
        from connectors.zapier_connector import ZapierConnector
        
        connector = ZapierConnector()
        connector.register_webhook("test_hook", "https://hooks.zapier.com/test")
        
        webhooks = connector.get_webhooks()
        assert "test_hook" in webhooks
        assert webhooks["test_hook"] == "https://hooks.zapier.com/test"
    
    async def test_trigger_unknown_webhook(self):
        """Test triggering unknown webhook raises error."""
        from connectors.zapier_connector import ZapierConnector
        
        connector = ZapierConnector()
        await connector.connect()
        
        with pytest.raises(ValueError, match="Unknown webhook"):
            await connector.trigger("nonexistent", {"data": "test"})
        
        await connector.disconnect()
    
    async def test_trigger_with_mock(self):
        """Test triggering webhook with mocked response."""
        from connectors.zapier_connector import ZapierConnector
        
        with patch('aiohttp.ClientSession') as mock_session:
            mock_response = AsyncMock()
            mock_response.status = 200
            mock_response.json = AsyncMock(return_value={"status": "success"})
            mock_response.__aenter__ = AsyncMock(return_value=mock_response)
            mock_response.__aexit__ = AsyncMock()
            
            mock_session_instance = MagicMock()
            mock_session_instance.post = MagicMock(return_value=mock_response)
            mock_session_instance.close = AsyncMock()
            mock_session.return_value = mock_session_instance
            
            connector = ZapierConnector()
            await connector.connect()
            connector.register_webhook("test", "https://hooks.zapier.com/test")
            
            result = await connector.trigger("test", {"key": "value"})
            assert result["success"] is True
    
    def test_factory_function(self):
        """Test create_zapier_connector factory function."""
        from connectors.zapier_connector import create_zapier_connector
        
        webhooks = {
            "default": "https://hooks.zapier.com/default",
            "analysis": "https://hooks.zapier.com/analysis"
        }
        
        connector = create_zapier_connector(webhooks)
        assert connector is not None
        assert "default" in connector.get_webhooks()
        assert "analysis" in connector.get_webhooks()
    
    def test_connector_info(self):
        """Test getting connector info."""
        from connectors.zapier_connector import ZapierConnector
        
        connector = ZapierConnector()
        info = connector.get_info()
        
        assert info["name"] == "zapier"
        assert info["type"] == "notification"
        assert info["open_source"] is True


@pytest.mark.asyncio
class TestHuggingfaceConnector:
    """Tests for Huggingface connector."""
    
    async def test_connect(self):
        """Test connecting the Huggingface connector."""
        from connectors.huggingface_connector import HuggingfaceConnector
        
        connector = HuggingfaceConnector()
        result = await connector.connect()
        assert result is True
        assert connector.status == ConnectorStatus.CONNECTED
        await connector.disconnect()
    
    async def test_disconnect(self):
        """Test disconnecting the Huggingface connector."""
        from connectors.huggingface_connector import HuggingfaceConnector
        
        connector = HuggingfaceConnector()
        await connector.connect()
        result = await connector.disconnect()
        assert result is True
        assert connector.status == ConnectorStatus.DISCONNECTED
    
    async def test_get_embeddings_with_mock(self):
        """Test getting embeddings with mocked response."""
        from connectors.huggingface_connector import HuggingfaceConnector
        
        with patch('aiohttp.ClientSession') as mock_session:
            mock_response = AsyncMock()
            mock_response.status = 200
            mock_response.json = AsyncMock(return_value=[[0.1, 0.2, 0.3]])
            mock_response.__aenter__ = AsyncMock(return_value=mock_response)
            mock_response.__aexit__ = AsyncMock()
            
            mock_session_instance = MagicMock()
            mock_session_instance.post = MagicMock(return_value=mock_response)
            mock_session_instance.close = AsyncMock()
            mock_session.return_value = mock_session_instance
            
            connector = HuggingfaceConnector()
            await connector.connect()
            
            result = await connector.get_embeddings(["test text"])
            assert result["success"] is True
            assert "embeddings" in result
    
    async def test_classify_text_with_mock(self):
        """Test text classification with mocked response."""
        from connectors.huggingface_connector import HuggingfaceConnector
        
        with patch('aiohttp.ClientSession') as mock_session:
            mock_response = AsyncMock()
            mock_response.status = 200
            mock_response.json = AsyncMock(return_value={
                "labels": ["positive", "negative"],
                "scores": [0.9, 0.1]
            })
            mock_response.__aenter__ = AsyncMock(return_value=mock_response)
            mock_response.__aexit__ = AsyncMock()
            
            mock_session_instance = MagicMock()
            mock_session_instance.post = MagicMock(return_value=mock_response)
            mock_session_instance.close = AsyncMock()
            mock_session.return_value = mock_session_instance
            
            connector = HuggingfaceConnector()
            await connector.connect()
            
            result = await connector.classify_text("test text", ["positive", "negative"])
            assert result["success"] is True
            assert "labels" in result
            assert "scores" in result
    
    async def test_summarize_text_with_mock(self):
        """Test text summarization with mocked response."""
        from connectors.huggingface_connector import HuggingfaceConnector
        
        with patch('aiohttp.ClientSession') as mock_session:
            mock_response = AsyncMock()
            mock_response.status = 200
            mock_response.json = AsyncMock(return_value=[{"summary_text": "This is a summary."}])
            mock_response.__aenter__ = AsyncMock(return_value=mock_response)
            mock_response.__aexit__ = AsyncMock()
            
            mock_session_instance = MagicMock()
            mock_session_instance.post = MagicMock(return_value=mock_response)
            mock_session_instance.close = AsyncMock()
            mock_session.return_value = mock_session_instance
            
            connector = HuggingfaceConnector()
            await connector.connect()
            
            result = await connector.summarize_text("Long text to summarize...")
            assert result["success"] is True
            assert "summary" in result
    
    async def test_search_models_with_mock(self):
        """Test model search with mocked response."""
        from connectors.huggingface_connector import HuggingfaceConnector
        
        with patch('aiohttp.ClientSession') as mock_session:
            mock_response = AsyncMock()
            mock_response.status = 200
            mock_response.json = AsyncMock(return_value=[
                {"id": "model1", "downloads": 1000, "likes": 50, "pipeline_tag": "text-classification"}
            ])
            mock_response.__aenter__ = AsyncMock(return_value=mock_response)
            mock_response.__aexit__ = AsyncMock()
            
            mock_session_instance = MagicMock()
            mock_session_instance.get = MagicMock(return_value=mock_response)
            mock_session_instance.close = AsyncMock()
            mock_session.return_value = mock_session_instance
            
            connector = HuggingfaceConnector()
            await connector.connect()
            
            result = await connector.search_models(query="sentiment")
            assert result["success"] is True
            assert "models" in result
    
    def test_factory_function(self):
        """Test create_huggingface_connector factory function."""
        from connectors.huggingface_connector import create_huggingface_connector
        
        connector = create_huggingface_connector(api_token="test_token")
        assert connector is not None
        assert connector.config.credentials.get("api_token") == "test_token"
    
    def test_factory_function_no_token(self):
        """Test create_huggingface_connector without token."""
        from connectors.huggingface_connector import create_huggingface_connector
        
        connector = create_huggingface_connector()
        assert connector is not None
    
    def test_connector_info(self):
        """Test getting connector info."""
        from connectors.huggingface_connector import HuggingfaceConnector
        
        connector = HuggingfaceConnector()
        info = connector.get_info()
        
        assert info["name"] == "huggingface"
        assert info["type"] == "llm"
        assert info["open_source"] is True
    
    async def test_classify_by_mental_models(self):
        """Test mental model classification."""
        from connectors.huggingface_connector import HuggingfaceConnector
        
        with patch('aiohttp.ClientSession') as mock_session:
            mock_response = AsyncMock()
            mock_response.status = 200
            mock_response.json = AsyncMock(return_value={
                "labels": ["Psychology & Human Behavior", "Economics & Markets"],
                "scores": [0.8, 0.6]
            })
            mock_response.__aenter__ = AsyncMock(return_value=mock_response)
            mock_response.__aexit__ = AsyncMock()
            
            mock_session_instance = MagicMock()
            mock_session_instance.post = MagicMock(return_value=mock_response)
            mock_session_instance.close = AsyncMock()
            mock_session.return_value = mock_session_instance
            
            connector = HuggingfaceConnector()
            await connector.connect()
            
            result = await connector.classify_by_mental_models("Test document about human behavior")
            assert result["success"] is True
    
    async def test_detect_cognitive_biases(self):
        """Test cognitive bias detection."""
        from connectors.huggingface_connector import HuggingfaceConnector
        
        with patch('aiohttp.ClientSession') as mock_session:
            mock_response = AsyncMock()
            mock_response.status = 200
            mock_response.json = AsyncMock(return_value={
                "labels": ["Confirmation Bias", "Anchoring Bias"],
                "scores": [0.7, 0.5]
            })
            mock_response.__aenter__ = AsyncMock(return_value=mock_response)
            mock_response.__aexit__ = AsyncMock()
            
            mock_session_instance = MagicMock()
            mock_session_instance.post = MagicMock(return_value=mock_response)
            mock_session_instance.close = AsyncMock()
            mock_session.return_value = mock_session_instance
            
            connector = HuggingfaceConnector()
            await connector.connect()
            
            result = await connector.detect_cognitive_biases("Test document with potential biases")
            assert result["success"] is True


class TestConnectorFactory:
    """Tests for connector factory pattern."""
    
    def test_create_connector_by_name(self):
        """Test creating connector by name."""
        registry = ConnectorRegistry()
        
        # Should be able to get connector class
        available = registry.list_available()
        assert len(available) > 0
        
        # Verify we have expected connectors
        names = [c["name"] for c in available]
        assert "ollama" in names
        assert "github" in names
        assert "local" in names
    
    def test_zapier_in_registry(self):
        """Test that Zapier connector is registered."""
        registry = ConnectorRegistry()
        available = registry.list_available()
        names = [c["name"] for c in available]
        assert "zapier" in names
    
    def test_huggingface_in_registry(self):
        """Test that Huggingface connector is registered."""
        registry = ConnectorRegistry()
        available = registry.list_available()
        names = [c["name"] for c in available]
        assert "huggingface" in names
