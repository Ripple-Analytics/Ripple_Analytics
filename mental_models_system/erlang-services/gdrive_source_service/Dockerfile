FROM erlang:26-alpine AS builder
RUN apk add --no-cache git curl
RUN curl -o /usr/local/bin/rebar3 https://s3.amazonaws.com/rebar3/rebar3 && chmod +x /usr/local/bin/rebar3
WORKDIR /build
COPY rebar.config .
COPY src/ src/
COPY config/ config/
RUN rebar3 compile && rebar3 as prod release

FROM erlang:26-alpine
RUN apk add --no-cache bash curl unzip rclone
RUN mkdir -p /app /repo/gdrive
WORKDIR /app
COPY --from=builder /build/_build/prod/rel/gdrive_source_service ./
EXPOSE 8011
HEALTHCHECK --interval=30s --timeout=10s --start-period=30s --retries=3 CMD curl -f http://localhost:8011/health || exit 1
CMD ["bin/gdrive_source_service", "foreground"]
