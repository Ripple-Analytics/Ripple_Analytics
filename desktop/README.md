# Mental Models Desktop Analyzer

A command-line tool that automatically discovers mental models in your documents using local AI (LM Studio).

## Quick Start

### Prerequisites

1. **Java 17+** - [Download OpenJDK](https://adoptium.net/)
2. **Clojure CLI** - [Installation Guide](https://clojure.org/guides/install_clojure)
3. **LM Studio** - [Download](https://lmstudio.ai/)

### Setup LM Studio

1. Open LM Studio
2. Download a model (recommended: Mistral 7B, Llama 3 8B, or similar)
3. Go to the **Server** tab
4. Click **Start Server** (runs on `localhost:1234`)

### Run the Analyzer

**Windows:**
```batch
run.bat scan "C:\Users\YourName\Documents\Research"
```

**Mac/Linux:**
```bash
./run.sh scan ~/Documents/Research
```

## Commands

| Command | Description |
|---------|-------------|
| `scan <folder>` | Scan all documents in a folder for mental models |
| `watch <folder>` | Watch a folder and analyze new files automatically |
| `status` | Show analysis statistics |
| `config` | Show current configuration |
| `help` | Show help message |

## Supported File Types

- `.txt` - Plain text
- `.md` - Markdown
- `.pdf` - PDF documents
- `.docx` - Word documents
- `.html` - HTML files

## What It Does

1. **Discovers files** - Recursively scans the folder for supported documents
2. **Extracts text** - Uses Apache PDFBox and POI for PDF/DOCX extraction
3. **Analyzes with AI** - Sends text to LM Studio for mental model identification
4. **Detects Lollapaloozas** - Alerts when 3+ mental models converge
5. **Stores results** - Saves to local SQLite database (~/.mental-models/data.db)
6. **Syncs to web** - Optionally syncs discoveries to the web app

## Configuration

The app stores configuration in `~/.mental-models/data.db`. You can modify settings:

```clojure
;; In a REPL
(require '[mental-models.desktop.db :as db])
(db/set-config! :lm-studio-url "http://localhost:1234")
(db/set-config! :web-app-url "https://your-app.manus.space")
```

## Example Output

```
╔════════════════════════════════════════════════════════════════╗
║         🧠 MENTAL MODELS DESKTOP ANALYZER 🧠                   ║
╚════════════════════════════════════════════════════════════════╝

Checking LM Studio connection... ✅ Connected
============================================================
Starting folder scan: /Users/you/Documents/Research
============================================================
Found 23 files to process

[1/23] Processing: investment-thesis.pdf
Analyzing: investment-thesis.pdf (15432 chars)
Completed: investment-thesis.pdf

[2/23] Processing: quarterly-report.docx
Analyzing: quarterly-report.docx (8921 chars)
🎯 LOLLAPALOOZA DETECTED in quarterly-report.docx - Models: Confirmation Bias, Sunk Cost Fallacy, Availability Heuristic
Completed: quarterly-report.docx
...
```

## Development

Start a REPL for development:

```bash
clojure -M:repl
```

Run tests:

```bash
clojure -M:test
```

## Architecture

```
desktop/
├── deps.edn                 # Dependencies
├── run.bat                  # Windows launcher
├── run.sh                   # Mac/Linux launcher
└── src/
    └── mental_models/
        └── desktop/
            ├── cli.clj      # Command-line interface
            ├── db.clj       # SQLite storage
            ├── extractor.clj # PDF/DOCX text extraction
            └── watcher.clj  # File system watcher
```

## Troubleshooting

### "LM Studio not available"
- Ensure LM Studio is running
- Check the server is started (Server tab → Start Server)
- Verify it's on port 1234: `curl http://localhost:1234/v1/models`

### "Failed to extract PDF"
- Ensure the PDF isn't password-protected
- Try a different PDF to rule out corruption

### "Out of memory"
- Large documents may need more heap space
- Run with: `JAVA_OPTS="-Xmx4g" ./run.sh scan ~/folder`
