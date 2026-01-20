FROM clojure:temurin-21-tools-deps-alpine

WORKDIR /app

# Create data directory
RUN mkdir -p /data

# Copy dependency files first for better caching
COPY deps.edn .

# Download dependencies
RUN clojure -P

# Copy source code
COPY src src/

# Expose port
EXPOSE 8003

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=30s --retries=3 \
    CMD wget -q --spider http://localhost:8003/health || exit 1

# Run the application
CMD ["clojure", "-M:run"]
