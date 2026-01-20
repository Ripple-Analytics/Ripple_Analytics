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

# Expose port
EXPOSE 8000

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=30s --retries=3 \
    CMD wget -q --spider http://localhost:8000/health || exit 1

# Run the application
CMD ["sh", "-c", "erl -pa _build/default/lib/*/ebin -eval 'application:ensure_all_started(api_gateway)' -noshell -noinput"]
