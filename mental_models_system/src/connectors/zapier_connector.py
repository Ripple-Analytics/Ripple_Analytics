"""
Zapier Connector

Integrates with Zapier webhooks for automation workflows.
Supports both incoming webhooks (triggers) and outgoing webhooks (actions).
"""

import aiohttp
import logging
from typing import Dict, List, Optional, Any
from datetime import datetime

from .base import BaseConnector, ConnectorConfig, ConnectorType, ConnectorStatus

logger = logging.getLogger(__name__)


class ZapierConnector(BaseConnector):
    """Connector for Zapier automation workflows."""
    
    NAME = "zapier"
    TYPE = ConnectorType.NOTIFICATION
    DESCRIPTION = "Integrate with Zapier for automation workflows (triggers and actions)"
    REQUIRED_CREDENTIALS = []
    OPTIONAL_CREDENTIALS = ["webhook_url", "api_key"]
    OPEN_SOURCE = True
    
    def __init__(self, config: ConnectorConfig = None):
        super().__init__(config)
        self._webhooks: Dict[str, str] = {}
        self._session: Optional[aiohttp.ClientSession] = None
    
    async def connect(self) -> bool:
        """Initialize the Zapier connector."""
        try:
            self._session = aiohttp.ClientSession()
            
            if "webhook_url" in self.config.credentials:
                self._webhooks["default"] = self.config.credentials["webhook_url"]
            
            self.status = ConnectorStatus.CONNECTED
            logger.info("Zapier connector initialized")
            return True
        except Exception as e:
            logger.error(f"Failed to initialize Zapier connector: {e}")
            self.status = ConnectorStatus.ERROR
            return False
    
    async def disconnect(self) -> bool:
        """Close the Zapier connector."""
        try:
            if self._session:
                await self._session.close()
                self._session = None
            self.status = ConnectorStatus.DISCONNECTED
            return True
        except Exception as e:
            logger.error(f"Failed to disconnect Zapier connector: {e}")
            return False
    
    async def health_check(self) -> bool:
        """Check if the connector is healthy."""
        return self.status == ConnectorStatus.CONNECTED and self._session is not None
    
    def register_webhook(self, name: str, url: str):
        """Register a webhook URL for a specific trigger/action."""
        self._webhooks[name] = url
        logger.info(f"Registered Zapier webhook: {name}")
    
    def get_webhooks(self) -> Dict[str, str]:
        """Get all registered webhooks."""
        return self._webhooks.copy()
    
    async def trigger(self, webhook_name: str, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Send data to a Zapier webhook to trigger a Zap.
        
        Args:
            webhook_name: Name of the registered webhook
            data: Data to send to the webhook
            
        Returns:
            Response from Zapier
        """
        if webhook_name not in self._webhooks:
            raise ValueError(f"Unknown webhook: {webhook_name}")
        
        if not self._session:
            await self.connect()
        
        url = self._webhooks[webhook_name]
        
        payload = {
            "timestamp": datetime.utcnow().isoformat(),
            "source": "mental_models_system",
            **data
        }
        
        try:
            await self._rate_limit_check()
            
            async with self._session.post(url, json=payload) as response:
                if response.status == 200:
                    result = await response.json()
                    logger.info(f"Zapier webhook triggered: {webhook_name}")
                    return {"success": True, "response": result}
                else:
                    text = await response.text()
                    logger.error(f"Zapier webhook failed: {response.status} - {text}")
                    return {"success": False, "error": text, "status": response.status}
        except Exception as e:
            logger.error(f"Failed to trigger Zapier webhook: {e}")
            return {"success": False, "error": str(e)}
    
    async def trigger_analysis_complete(self, document_id: str, analysis_result: Dict[str, Any]) -> Dict[str, Any]:
        """Trigger when a document analysis is complete."""
        return await self.trigger("analysis_complete", {
            "event": "analysis_complete",
            "document_id": document_id,
            "models_applied": analysis_result.get("models", []),
            "biases_detected": analysis_result.get("biases", []),
            "lollapalooza_effects": analysis_result.get("lollapalooza", []),
            "summary": analysis_result.get("summary", "")
        })
    
    async def trigger_failure_mode_detected(self, model_name: str, failure_mode: Dict[str, Any]) -> Dict[str, Any]:
        """Trigger when a failure mode is detected."""
        return await self.trigger("failure_mode_detected", {
            "event": "failure_mode_detected",
            "model_name": model_name,
            "failure_mode_id": failure_mode.get("id", ""),
            "failure_mode_name": failure_mode.get("name", ""),
            "severity": failure_mode.get("severity", "medium"),
            "description": failure_mode.get("description", ""),
            "detection_signals": failure_mode.get("detection_signals", [])
        })
    
    async def trigger_new_document(self, document_path: str, document_type: str, metadata: Dict[str, Any] = None) -> Dict[str, Any]:
        """Trigger when a new document is added."""
        return await self.trigger("new_document", {
            "event": "new_document",
            "document_path": document_path,
            "document_type": document_type,
            "metadata": metadata or {}
        })
    
    async def trigger_improvement_suggested(self, improvement: Dict[str, Any]) -> Dict[str, Any]:
        """Trigger when an improvement is suggested."""
        return await self.trigger("improvement_suggested", {
            "event": "improvement_suggested",
            "improvement_id": improvement.get("id", ""),
            "title": improvement.get("title", ""),
            "description": improvement.get("description", ""),
            "priority": improvement.get("priority", "medium"),
            "source": improvement.get("source", "")
        })
    
    async def create_github_issue(self, title: str, body: str, labels: List[str] = None) -> Dict[str, Any]:
        """
        Trigger a Zap to create a GitHub issue.
        Requires a Zap configured with GitHub integration.
        """
        return await self.trigger("create_github_issue", {
            "event": "create_github_issue",
            "title": title,
            "body": body,
            "labels": labels or []
        })
    
    async def send_slack_notification(self, channel: str, message: str, attachments: List[Dict] = None) -> Dict[str, Any]:
        """
        Trigger a Zap to send a Slack notification.
        Requires a Zap configured with Slack integration.
        """
        return await self.trigger("slack_notification", {
            "event": "slack_notification",
            "channel": channel,
            "message": message,
            "attachments": attachments or []
        })
    
    async def batch_trigger(self, webhook_name: str, items: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """
        Send multiple items to a webhook in batch.
        
        Args:
            webhook_name: Name of the registered webhook
            items: List of data items to send
            
        Returns:
            List of responses from Zapier
        """
        results = []
        for item in items:
            result = await self.trigger(webhook_name, item)
            results.append(result)
        return results


# Register the connector
from .base import registry
registry.register_class(ZapierConnector)


def create_zapier_connector(webhook_urls: Dict[str, str] = None) -> ZapierConnector:
    """
    Factory function to create a configured Zapier connector.
    
    Args:
        webhook_urls: Dictionary mapping webhook names to URLs
        
    Returns:
        Configured ZapierConnector instance
    """
    config = ConnectorConfig(
        name="zapier",
        connector_type=ConnectorType.NOTIFICATION,
        credentials={"webhook_url": webhook_urls.get("default", "")} if webhook_urls else {},
        settings={"webhooks": webhook_urls or {}}
    )
    
    connector = ZapierConnector(config)
    
    if webhook_urls:
        for name, url in webhook_urls.items():
            connector.register_webhook(name, url)
    
    return connector
