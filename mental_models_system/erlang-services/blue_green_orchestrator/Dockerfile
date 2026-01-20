FROM erlang:26-alpine

RUN apk add --no-cache curl wget

WORKDIR /app

COPY rebar.config .
COPY src/ src/

RUN ./rebar3 compile
RUN ./rebar3 release

EXPOSE 8020

CMD ["_build/default/rel/blue_green_orchestrator/bin/blue_green_orchestrator", "foreground"]
