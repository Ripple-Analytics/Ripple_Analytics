FROM erlang:26-alpine

WORKDIR /app
RUN apk add --no-cache git

COPY rebar.config .
RUN rebar3 get-deps

COPY src src/
RUN rebar3 compile

EXPOSE 8005

HEALTHCHECK --interval=30s --timeout=10s --start-period=30s --retries=3 \
    CMD wget -q --spider http://localhost:8005/health || exit 1

CMD ["erl", "-pa", "_build/default/lib/*/ebin", "-s", "chaos_engineering_app", "-noshell"]
