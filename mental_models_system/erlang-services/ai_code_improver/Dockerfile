FROM erlang:26-alpine

WORKDIR /app

RUN apk add --no-cache git curl

COPY rebar.config .
RUN rebar3 get-deps

COPY src/ src/

RUN rebar3 compile

EXPOSE 8025

CMD ["rebar3", "shell", "--apps", "ai_code_improver"]
