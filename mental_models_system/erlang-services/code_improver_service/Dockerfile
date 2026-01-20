FROM erlang:26-alpine AS builder

WORKDIR /app

RUN apk add --no-cache git

COPY rebar.config rebar.lock* ./
RUN rebar3 get-deps

# CACHE BUSTER: This ARG changes with each commit, forcing rebuild of src layer
ARG COMMIT_HASH=unknown
ARG BUILD_TIME=unknown
RUN echo "Building commit: ${COMMIT_HASH} at ${BUILD_TIME}"

COPY config ./config
COPY src ./src

RUN rebar3 as prod release

FROM erlang:26-alpine

WORKDIR /app

RUN apk add --no-cache \
    bash \
    curl \
    git \
    openssl

COPY --from=builder /app/_build/prod/rel/code_improver_service ./

ARG COMMIT_HASH=unknown
ARG BUILD_TIME=unknown
ENV COMMIT_HASH=${COMMIT_HASH}
ENV BUILD_TIME=${BUILD_TIME}

HEALTHCHECK --interval=30s --timeout=10s --start-period=30s --retries=3 \
    CMD curl -f http://localhost:8010/health || exit 1

EXPOSE 8010

CMD ["bin/code_improver_service", "foreground"]
