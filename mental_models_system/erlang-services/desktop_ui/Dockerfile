FROM erlang:26-alpine

WORKDIR /app
RUN apk add --no-cache git curl

# Download rebar3
RUN curl -o rebar3 https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3

COPY rebar.config .
RUN ./rebar3 get-deps

# CACHE BUSTER: This ARG changes with each commit, forcing rebuild of src layer
# Pass --build-arg COMMIT_HASH=$(git rev-parse HEAD) during docker build
ARG COMMIT_HASH=unknown
ARG BUILD_TIME=unknown
RUN echo "Building commit: ${COMMIT_HASH} at ${BUILD_TIME}"

# Copy source files (this layer will rebuild when COMMIT_HASH changes)
COPY src src/

# Compile the application
RUN ./rebar3 compile

EXPOSE 3000

HEALTHCHECK --interval=30s --timeout=10s --start-period=30s --retries=3 \
    CMD wget -q --spider http://localhost:3000/health || exit 1

CMD ["sh", "-c", "erl -pa _build/default/lib/*/ebin -eval 'application:ensure_all_started(desktop_ui)' -noshell -noinput"]
