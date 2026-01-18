# Mental Models System - Desktop Application

Windows/Mac/Linux desktop application for continuous data harvesting and analysis using the Mental Models System.

## Features

- **Continuous Data Harvesting**: Monitor directories and scrape URLs 24/7
- **GitHub Auto-Updates**: Hot loading updates from GitHub releases
- **System Tray Integration**: Runs in background, minimizes to tray
- **API Integration**: Pipes all data to the Mental Models web API
- **Cross-Platform**: Windows, macOS, and Linux support

## Quick Start

### Prerequisites

- Node.js 18+ 
- npm or yarn
- Mental Models API running (default: http://localhost:8000)

### Installation

```bash
cd mental_models_system/desktop-app
npm install
npm start
```

### Building for Distribution

```bash
# Windows
npm run build:win

# macOS
npm run build:mac

# Linux
npm run build:linux

# All platforms
npm run build
```

## Configuration

The app stores configuration in:
- Windows: `%APPDATA%/mental-models-system/config.json`
- macOS: `~/Library/Application Support/mental-models-system/config.json`
- Linux: `~/.config/mental-models-system/config.json`

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `MENTAL_MODELS_API_URL` | API server URL | `http://localhost:8000` |

## Data Harvesting

The desktop app continuously harvests data from:

1. **Watch Directories**: Monitor folders for new/changed files
   - Supported formats: `.txt`, `.md`, `.json`, `.csv`, `.html`, `.xml`, `.pdf`
   - Recursive scanning
   - Automatic deduplication via content hashing

2. **Web Scraping**: Scrape URLs on a schedule
   - Headless HTTP scraping (no browser required)
   - Configurable scrape interval (default: 1 hour)
   - Link extraction and text extraction

3. **API Sync**: Batch sync to Mental Models API
   - Configurable batch size (default: 100 items)
   - Configurable sync interval (default: 30 seconds)
   - Automatic retry on failure

## Auto-Updates

The app automatically checks for updates from GitHub releases:

1. On startup (configurable)
2. Downloads updates in background
3. Prompts to restart when ready
4. Supports rollback on failure

### Publishing Updates

1. Update version in `package.json`
2. Commit and push changes
3. Create a GitHub release with the new version tag
4. The app will automatically detect and download the update

## Architecture

```
desktop-app/
├── main.js           # Electron main process
├── preload.js        # Secure bridge to renderer
├── harvester.js      # Data harvesting engine
├── index.html        # UI
├── package.json      # Dependencies and build config
└── build/            # Build resources (icons)
```

## API Endpoints Used

The desktop app communicates with these API endpoints:

- `POST /api/distributed/submit` - Submit harvested data for analysis
- `GET /api/distributed/status` - Get cluster status
- `POST /api/continuous/start` - Start server-side continuous processing
- `POST /api/continuous/stop` - Stop server-side continuous processing

## Development

```bash
# Run in development mode
npm run dev

# Run with DevTools open
npm start -- --dev
```

## Troubleshooting

### App won't start
- Check Node.js version (18+ required)
- Run `npm install` to ensure dependencies are installed
- Check logs in `%APPDATA%/mental-models-system/logs/`

### Can't connect to API
- Verify API URL in Settings
- Ensure Mental Models API is running
- Check firewall settings

### Updates not working
- Check internet connection
- Verify GitHub release exists
- Check logs for update errors

## License

MIT
