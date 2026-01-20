FROM erlang:26-alpine

# Install build dependencies (gcc, make needed for jiffy native compilation)
RUN apk add --no-cache git docker-cli docker-compose build-base

WORKDIR /app

COPY rebar.config .

RUN wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
RUN ./rebar3 get-deps

# CACHE BUSTER: This ARG changes with each commit, forcing rebuild of src layer
ARG COMMIT_HASH=unknown
ARG BUILD_TIME=unknown
ENV COMMIT_HASH=${COMMIT_HASH}
ENV BUILD_TIME=${BUILD_TIME}
RUN echo "Building commit: ${COMMIT_HASH} at ${BUILD_TIME}"

COPY src src/
RUN ./rebar3 compile

HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD wget -q --spider http://localhost:8007/health || exit 1

ENV PORT=8007
EXPOSE 8007

CMD ["./rebar3", "shell", "--apps", "deployment_controller"]
