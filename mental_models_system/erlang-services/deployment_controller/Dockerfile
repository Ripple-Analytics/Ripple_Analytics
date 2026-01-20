FROM erlang:26-alpine

RUN apk add --no-cache git docker-cli docker-compose

WORKDIR /app

# Copy rebar config and source files
COPY rebar.config .
COPY src src/

# Download rebar3 and fetch dependencies
RUN wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
RUN ./rebar3 get-deps

# Cache buster args - these change on each build to ensure fresh source is compiled
ARG COMMIT_HASH=unknown
ARG BUILD_TIME=unknown
ENV COMMIT_HASH=${COMMIT_HASH}
ENV BUILD_TIME=${BUILD_TIME}

# Compile the application
RUN ./rebar3 compile

HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD wget -q --spider http://localhost:8007/health || exit 1

ENV PORT=8007
EXPOSE 8007

CMD ["./rebar3", "shell", "--apps", "deployment_controller"]
