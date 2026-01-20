FROM erlang:26-alpine

WORKDIR /app

# Install build dependencies
RUN apk add --no-cache git

# Copy rebar config
COPY rebar.config .

# Fetch dependencies
RUN rebar3 get-deps

# CACHE BUSTER: This ARG changes with each commit, forcing rebuild of src layer
ARG COMMIT_HASH=unknown
ARG BUILD_TIME=unknown
RUN echo "Building commit: ${COMMIT_HASH} at ${BUILD_TIME}"

# Copy source code
COPY src src/

# Compile
RUN rebar3 compile

# Expose port
EXPOSE 8001

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=30s --retries=3 \
    CMD wget -q --spider http://localhost:8001/health || exit 1

# Run the application
CMD ["sh", "-c", "erl -pa _build/default/lib/*/ebin -eval 'application:ensure_all_started(analysis_service)' -noshell -noinput"]
