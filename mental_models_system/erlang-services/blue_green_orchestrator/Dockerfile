FROM erlang:26-alpine

RUN apk add --no-cache curl wget git

WORKDIR /app

# Download rebar3
RUN curl -o rebar3 https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3

COPY rebar.config .
RUN ./rebar3 get-deps

COPY src/ src/

RUN ./rebar3 compile
RUN ./rebar3 release

EXPOSE 8020

CMD ["_build/default/rel/blue_green_orchestrator/bin/blue_green_orchestrator", "foreground"]
