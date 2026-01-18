"""
Pytest configuration and fixtures for Mental Models System tests.
"""

import asyncio
import json
import os
import sys
from pathlib import Path
from typing import Dict, List, Any
from unittest.mock import AsyncMock, MagicMock

import pytest

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))


# =============================================================================
# ASYNC FIXTURES
# =============================================================================

@pytest.fixture(scope="session")
def event_loop():
    """Create event loop for async tests."""
    loop = asyncio.get_event_loop_policy().new_event_loop()
    yield loop
    loop.close()


# =============================================================================
# DATA FIXTURES
# =============================================================================

@pytest.fixture
def sample_mental_models() -> List[Dict]:
    """Sample mental models for testing."""
    return [
        {
            "id": 1,
            "name": "Incentive-Caused Bias",
            "category": "Psychology",
            "description": "People tend to act in their own self-interest",
            "thinker": "Charlie Munger",
            "complexity": 3,
            "applicability": ["investment", "business", "personal"]
        },
        {
            "id": 2,
            "name": "Social Proof",
            "category": "Psychology",
            "description": "People follow the actions of others",
            "thinker": "Robert Cialdini",
            "complexity": 2,
            "applicability": ["marketing", "investment"]
        },
        {
            "id": 3,
            "name": "Compounding",
            "category": "Mathematics",
            "description": "Exponential growth over time",
            "thinker": "Albert Einstein",
            "complexity": 2,
            "applicability": ["investment", "learning"]
        }
    ]


@pytest.fixture
def sample_case_studies() -> List[Dict]:
    """Sample case studies for testing."""
    return [
        {
            "id": 1,
            "title": "Enron Collapse",
            "year": 2001,
            "category": "Corporate Fraud",
            "description": "Accounting fraud and corporate governance failure",
            "models_applied": [1, 2],
            "lollapalooza_score": 0.9
        },
        {
            "id": 2,
            "title": "Berkshire Hathaway Success",
            "year": 1965,
            "category": "Investment",
            "description": "Long-term value investing success",
            "models_applied": [1, 3],
            "lollapalooza_score": 0.85
        }
    ]


@pytest.fixture
def sample_failure_modes() -> Dict:
    """Sample failure modes for testing."""
    return {
        "1": [
            {
                "id": "1_F1",
                "name": "Misaligned Incentives",
                "description": "Incentives not aligned with desired outcomes",
                "warning_signals": ["Short-term focus", "Gaming metrics"],
                "safeguards": ["Regular incentive audits", "Multiple metrics"]
            }
        ]
    }


@pytest.fixture
def sample_document_text() -> str:
    """Sample document text for analysis."""
    return """
    The company's management team was heavily incentivized through stock options,
    leading to short-term decision making that prioritized quarterly earnings over
    long-term value creation. This created a classic case of incentive-caused bias,
    where the rational self-interest of executives diverged from shareholder interests.
    
    The social proof effect was also evident, as other companies in the industry
    adopted similar compensation structures, normalizing the behavior.
    """


# =============================================================================
# MOCK FIXTURES
# =============================================================================

@pytest.fixture
def mock_llm_client():
    """Mock LLM client for testing without actual LLM calls."""
    client = AsyncMock()
    client.generate = AsyncMock(return_value=MagicMock(
        text='{"models": [1, 2], "confidence": 0.85}',
        tokens_used=100,
        latency_ms=500
    ))
    client.embed = AsyncMock(return_value=[0.1] * 384)
    return client


@pytest.fixture
def mock_connector():
    """Mock connector for testing."""
    connector = AsyncMock()
    connector.connect = AsyncMock(return_value=True)
    connector.disconnect = AsyncMock(return_value=True)
    connector.health_check = AsyncMock(return_value=True)
    return connector


@pytest.fixture
def mock_github_connector(mock_connector):
    """Mock GitHub connector."""
    mock_connector.create_issue = AsyncMock(return_value={"number": 123})
    mock_connector.list_issues = AsyncMock(return_value=[])
    mock_connector.create_pr = AsyncMock(return_value={"number": 456})
    return mock_connector


@pytest.fixture
def mock_slack_connector(mock_connector):
    """Mock Slack connector."""
    mock_connector.send_message = AsyncMock(return_value={"ts": "123.456"})
    mock_connector.read_channel = AsyncMock(return_value=[])
    return mock_connector


# =============================================================================
# FILE FIXTURES
# =============================================================================

@pytest.fixture
def temp_data_dir(tmp_path) -> Path:
    """Create temporary data directory."""
    data_dir = tmp_path / "data"
    data_dir.mkdir()
    return data_dir


@pytest.fixture
def temp_models_file(temp_data_dir, sample_mental_models) -> Path:
    """Create temporary mental models JSON file."""
    # Create raw subdirectory to match expected path structure
    raw_dir = temp_data_dir / "raw"
    raw_dir.mkdir(exist_ok=True)
    
    file_path = raw_dir / "mental_models_complete.json"
    with open(file_path, "w") as f:
        json.dump({"mental_models": sample_mental_models}, f)
    return file_path


@pytest.fixture
def temp_cases_file(temp_data_dir, sample_case_studies) -> Path:
    """Create temporary case studies JSON file."""
    file_path = temp_data_dir / "case_studies.json"
    with open(file_path, "w") as f:
        json.dump({"case_studies": sample_case_studies}, f)
    return file_path


# =============================================================================
# ENVIRONMENT FIXTURES
# =============================================================================

@pytest.fixture
def clean_env():
    """Provide clean environment variables."""
    original_env = os.environ.copy()
    yield
    os.environ.clear()
    os.environ.update(original_env)


@pytest.fixture
def test_env(clean_env):
    """Set up test environment variables."""
    os.environ["OLLAMA_HOST"] = "http://localhost:11434"
    os.environ["DATABASE_URL"] = "postgresql://test:test@localhost:5432/test"
    os.environ["ENVIRONMENT"] = "test"
    yield


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def assert_valid_analysis_result(result: Dict):
    """Assert that an analysis result has expected structure."""
    assert "models" in result or "model_ids" in result
    assert "confidence" in result or "score" in result


def assert_valid_connector_response(response: Dict):
    """Assert that a connector response has expected structure."""
    assert response is not None
    # Add more specific assertions based on connector type
