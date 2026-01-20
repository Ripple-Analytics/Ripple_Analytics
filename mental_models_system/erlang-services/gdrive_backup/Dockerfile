FROM erlang:26-alpine

WORKDIR /app

# Install build dependencies
RUN apk add --no-cache git curl tar gzip

# Copy source files
COPY rebar.config .
COPY src/ src/

# Build the application
RUN rebar3 compile && rebar3 release

# Expose port
EXPOSE 8006

# Create data directory for backups
RUN mkdir -p /data/backups

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:8006/health || exit 1

# Start the application
CMD ["sh", "-c", "erl -pa _build/default/lib/*/ebin -eval 'application:ensure_all_started(gdrive_backup)' -noshell"]
