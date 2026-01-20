FROM erlang:26-alpine

WORKDIR /app

RUN apk add --no-cache curl wget git bash

COPY rebar.config .
COPY src/ src/

RUN wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
RUN ./rebar3 compile
RUN ./rebar3 release

EXPOSE 8010

CMD ["_build/default/rel/lan_backup/bin/lan_backup", "foreground"]
