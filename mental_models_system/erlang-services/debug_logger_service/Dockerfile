FROM erlang:26-alpine AS builder

RUN apk add --no-cache git

WORKDIR /build
COPY rebar.config .
COPY src/ src/
COPY config/ config/

RUN rebar3 as prod release

# Runtime image
FROM erlang:26-alpine

RUN apk add --no-cache git docker-cli bash curl

WORKDIR /app

COPY --from=builder /build/_build/prod/rel/debug_logger_service ./

# Create repo mount point
RUN mkdir -p /repo

ENV RELX_REPLACE_OS_VARS=true

CMD ["/app/bin/debug_logger_service", "foreground"]
