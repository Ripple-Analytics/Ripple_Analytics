FROM erlang:26-alpine

WORKDIR /app
RUN apk add --no-cache git

COPY rebar.config .
RUN rebar3 get-deps

# CACHE BUSTER: This ARG changes with each commit, forcing rebuild of src layer
ARG COMMIT_HASH=unknown
ARG BUILD_TIME=unknown
RUN echo "Building commit: ${COMMIT_HASH} at ${BUILD_TIME}"

COPY src src/
RUN rebar3 compile

EXPOSE 8003
VOLUME /data

HEALTHCHECK --interval=30s --timeout=10s --start-period=30s --retries=3 \
    CMD wget -q --spider http://localhost:8003/health || exit 1

CMD ["sh", "-c", "erl -pa _build/default/lib/*/ebin -eval 'application:ensure_all_started(storage_service)' -noshell -noinput"]
