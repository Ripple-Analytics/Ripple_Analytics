# Contributing to Ripple Analytics Mental Models System

Thank you for your interest in contributing to the Mental Models System! This document provides guidelines and instructions for contributing.

## Getting Started

1. Fork the repository
2. Clone your fork: `git clone https://github.com/YOUR_USERNAME/Ripple_Analytics.git`
3. Create a feature branch: `git checkout -b feature/your-feature-name`
4. Make your changes
5. Run tests: `pytest tests/`
6. Commit your changes: `git commit -m "Add your feature"`
7. Push to your fork: `git push origin feature/your-feature-name`
8. Create a Pull Request

## Development Setup

### Prerequisites

- Python 3.9+
- Docker and Docker Compose
- PostgreSQL with pgvector extension
- LM Studio (for local LLM inference)

### Installation

```bash
# Clone the repository
git clone https://github.com/Ripple-Analytics/Ripple_Analytics.git
cd Ripple_Analytics

# Create virtual environment
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install dependencies
pip install -r mental_models_system/requirements.txt

# Start services with Docker
docker-compose -f mental_models_system/docker-compose.yml up -d
```

## Code Style

- Follow PEP 8 for Python code
- Use type hints where possible
- Write docstrings for all public functions and classes
- Keep functions focused and small
- Prefer composition over inheritance

## Testing

- Write tests for all new features
- Ensure all tests pass before submitting PR
- Aim for high test coverage on critical paths

```bash
# Run all tests
pytest tests/

# Run with coverage
pytest --cov=mental_models_system tests/
```

## Pull Request Guidelines

1. Keep PRs focused on a single feature or fix
2. Write clear, descriptive commit messages
3. Update documentation as needed
4. Add tests for new functionality
5. Ensure CI passes before requesting review

## Areas for Contribution

### High Priority

- Additional mental models and failure modes
- Improved detection algorithms for failure modes
- Better safeguard implementations
- Performance optimizations
- Documentation improvements

### Connectors and Integrations

- New data source connectors (see `mental_models_system/src/connectors/`)
- Slack bot improvements
- GitHub integration enhancements
- Additional LLM provider support

### Web Scraping (Open Source Alternatives)

We use open source web scraping tools. Contributions welcome for:
- Scrapy spiders for specific data sources
- BeautifulSoup parsers
- Selenium-based scrapers (headless only)

## Reporting Issues

When reporting issues, please include:
- Clear description of the problem
- Steps to reproduce
- Expected vs actual behavior
- Environment details (OS, Python version, etc.)

## Code of Conduct

- Be respectful and inclusive
- Focus on constructive feedback
- Help others learn and grow
- Assume good intentions

## Questions?

Open an issue or reach out to the maintainers.

Thank you for contributing!
