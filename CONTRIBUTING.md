# Contributing to Mental Models System

Thank you for your interest in contributing! This document provides guidelines and instructions for contributing.

## Development Principle

**Improvement = Iteration Speed × Iteration Magnitude**

Both variables must be maximized:

- **Speed**: Push frequently, don't batch, communicate blockers immediately
- **Magnitude**: Each commit adds real value, solve hard problems first, think 10x not 10%

## Getting Started

### Prerequisites

- Python 3.11+
- Docker and Docker Compose
- Git
- (Optional) Ollama for local LLM testing

### Setup

```bash
# Clone the repository
git clone https://github.com/Ripple-Analytics/Ripple_Analytics.git
cd Ripple_Analytics

# Copy environment template
cp .env.example .env

# Install dependencies
cd mental_models_system
pip install -r requirements.txt
pip install -r requirements-dev.txt  # Development dependencies

# Start services
docker-compose up -d

# Run tests
pytest tests/ -v
```

## Code Standards

### Style Guide

- Follow PEP 8 for Python code
- Use type hints for all function signatures
- Maximum line length: 100 characters
- Use meaningful variable and function names

### Documentation

- All public functions must have docstrings
- Use Google-style docstrings
- Include examples in docstrings where helpful

```python
def analyze_text(text: str, models: List[int] = None) -> AnalysisResult:
    """
    Analyze text using mental models.
    
    Args:
        text: The text to analyze
        models: Optional list of model IDs to use. If None, uses all models.
    
    Returns:
        AnalysisResult containing identified models and confidence scores.
    
    Example:
        >>> result = await analyzer.analyze_text("Company had misaligned incentives")
        >>> print(result.models)
        [{'id': 1, 'name': 'Incentive-Caused Bias', 'relevance': 0.9}]
    """
```

### Testing

- Write tests for all new features
- Maintain minimum 80% code coverage
- Use pytest for testing
- Mock external services in unit tests

```bash
# Run all tests
pytest tests/ -v

# Run with coverage
pytest tests/ --cov=src --cov-report=html

# Run specific test file
pytest tests/unit/test_connectors.py -v

# Run tests matching pattern
pytest tests/ -k "test_analyze" -v
```

### Commit Messages

Use conventional commits format:

```
type(scope): description

[optional body]

[optional footer]
```

Types:
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation only
- `style`: Code style (formatting, etc.)
- `refactor`: Code refactoring
- `test`: Adding tests
- `chore`: Maintenance tasks

Examples:
```
feat(connectors): add Matrix chat connector
fix(analyzer): handle empty text input gracefully
docs(readme): add connector usage examples
test(connectors): add unit tests for GitHub connector
```

## Pull Request Process

1. **Create a branch** from `master`:
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. **Make your changes** following the code standards

3. **Write/update tests** for your changes

4. **Run the test suite**:
   ```bash
   pytest tests/ -v
   ```

5. **Update documentation** if needed

6. **Commit your changes** with a clear message

7. **Push to your branch**:
   ```bash
   git push origin feature/your-feature-name
   ```

8. **Create a Pull Request** with:
   - Clear description of changes
   - Link to related issues
   - Screenshots if UI changes

9. **Address review feedback** promptly

## Adding New Connectors

When adding a new connector:

1. Create connector file in `src/connectors/`
2. Inherit from `BaseConnector`
3. Implement required methods:
   - `connect()`
   - `disconnect()`
   - `health_check()`
4. Register in `__init__.py`
5. Add tests in `tests/unit/test_connectors.py`
6. Update README with usage examples

Example:
```python
from .base import BaseConnector, ConnectorConfig, ConnectorType, ConnectorStatus

class MyConnector(BaseConnector):
    NAME = "my_connector"
    TYPE = ConnectorType.DATA
    DESCRIPTION = "My custom connector"
    REQUIRED_CREDENTIALS = ["api_key"]
    OPTIONAL_CREDENTIALS = ["timeout"]
    OPEN_SOURCE = True
    
    async def connect(self) -> bool:
        # Implementation
        self.status = ConnectorStatus.CONNECTED
        return True
    
    async def disconnect(self) -> bool:
        self.status = ConnectorStatus.DISCONNECTED
        return True
    
    async def health_check(self) -> bool:
        return self.status == ConnectorStatus.CONNECTED

# Register
from .base import registry
registry.register_class(MyConnector)
```

## Adding Mental Models

When adding new mental models:

1. Add to `data/raw/mental_models_complete.json`
2. Include all required fields:
   - `id`: Unique integer
   - `name`: Model name
   - `category`: One of the 8 categories
   - `description`: Clear description
   - `thinker`: Original thinker
   - `complexity`: 1-5 scale
   - `applicability`: List of domains
3. Add 5 failure modes in `src/safeguards/failure_modes.py`
4. Add at least one case study reference

## Reporting Issues

When reporting issues, include:

1. **Description**: Clear description of the issue
2. **Steps to reproduce**: Detailed steps
3. **Expected behavior**: What should happen
4. **Actual behavior**: What actually happens
5. **Environment**: OS, Python version, etc.
6. **Logs**: Relevant error messages

## Feature Requests

For feature requests:

1. Check existing issues first
2. Describe the use case
3. Explain why it's valuable
4. Suggest implementation approach if possible

## Code of Conduct

- Be respectful and inclusive
- Focus on constructive feedback
- Help others learn and grow
- Maintain professional communication

## Questions?

- Open an issue for questions
- Join our Slack channel (if available)
- Check existing documentation

## License

By contributing, you agree that your contributions will be licensed under the MIT License.
