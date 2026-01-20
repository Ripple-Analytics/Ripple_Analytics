FROM erlang:26-alpine

# Build arguments for versioning
ARG COMMIT_HASH=unknown
ARG BUILD_TIME=unknown

# Install build dependencies
RUN apk add --no-cache git curl

# Set working directory
WORKDIR /app

# Copy rebar config first for dependency caching
COPY rebar.config .

# Get dependencies
RUN wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
RUN ./rebar3 get-deps

# Copy source code
COPY src/ src/
COPY config/ config/

# Compile
RUN ./rebar3 compile

# Create data directory for knowledge base
RUN mkdir -p /data

# Set environment variables
ENV LM_STUDIO_SERVICE_PORT=8007
ENV LM_STUDIO_URL=http://host.docker.internal:1234
ENV COMMIT_HASH=${COMMIT_HASH}
ENV BUILD_TIME=${BUILD_TIME}

# Expose port
EXPOSE 8007

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=30s --retries=3 \
    CMD curl -f http://localhost:8007/health || exit 1

# Run the application
CMD ["./rebar3", "shell", "--apps", "lm_studio_service"]
