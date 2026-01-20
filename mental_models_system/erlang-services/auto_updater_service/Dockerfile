FROM erlang:26-alpine AS builder

# Install build dependencies
RUN apk add --no-cache git curl

# Install rebar3
RUN curl -o /usr/local/bin/rebar3 https://s3.amazonaws.com/rebar3/rebar3 && \
    chmod +x /usr/local/bin/rebar3

WORKDIR /app

# Copy source files
COPY rebar.config .
COPY src/ src/

# Fetch dependencies and compile
RUN rebar3 compile
RUN rebar3 as prod release

# Production image
FROM erlang:26-alpine

# Install runtime dependencies
RUN apk add --no-cache \
    bash \
    curl \
    git \
    docker-cli \
    docker-cli-compose \
    openssh-client \
    unzip

# Create directories
RUN mkdir -p /app /repo /data /root/.ssh && chmod 700 /root/.ssh

WORKDIR /app

# Copy the release from builder
COPY --from=builder /app/_build/prod/rel/auto_updater_service ./

# Expose API port
EXPOSE 8006

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=30s --retries=3 \
    CMD curl -f http://localhost:8006/health || exit 1

# Run the release
CMD ["bin/auto_updater_service", "foreground"]
