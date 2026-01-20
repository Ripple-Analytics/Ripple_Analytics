FROM erlang:26-alpine

WORKDIR /app

RUN apk add --no-cache curl wget git bash

COPY rebar.config .
COPY src/ src/

RUN wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
RUN ./rebar3 compile
RUN ./rebar3 release

EXPOSE 8008

CMD ["_build/default/rel/s3_backup/bin/s3_backup", "foreground"]
