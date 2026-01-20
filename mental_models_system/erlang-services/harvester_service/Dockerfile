FROM erlang:26-alpine

WORKDIR /app
RUN apk add --no-cache git

COPY rebar.config .
RUN rebar3 get-deps

COPY src src/
RUN rebar3 compile && rebar3 release

EXPOSE 8002

HEALTHCHECK --interval=30s --timeout=10s --start-period=30s --retries=3 \
    CMD wget -q --spider http://localhost:8002/health || exit 1

CMD ["_build/default/rel/harvester_service/bin/harvester_service", "foreground"]
