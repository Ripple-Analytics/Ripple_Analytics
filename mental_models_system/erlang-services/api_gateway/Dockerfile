FROM erlang:26-alpine

WORKDIR /app

# Install build dependencies
RUN apk add --no-cache git

# Copy rebar config
COPY rebar.config .

# Fetch dependencies
RUN rebar3 get-deps

# Copy source code
COPY src src/

# Compile
RUN rebar3 compile

# Build release
RUN rebar3 release

# Expose port
EXPOSE 8000

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=30s --retries=3 \
    CMD wget -q --spider http://localhost:8000/health || exit 1

# Run the release
CMD ["_build/default/rel/api_gateway/bin/api_gateway", "foreground"]
