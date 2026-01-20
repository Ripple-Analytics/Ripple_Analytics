FROM erlang:26-alpine

WORKDIR /app
RUN apk add --no-cache git

COPY rebar.config .
RUN rebar3 get-deps

COPY src src/
RUN rebar3 compile && rebar3 release

EXPOSE 3000

HEALTHCHECK --interval=30s --timeout=10s --start-period=30s --retries=3 \
    CMD wget -q --spider http://localhost:3000/health || exit 1

CMD ["_build/default/rel/desktop_ui/bin/desktop_ui", "foreground"]
