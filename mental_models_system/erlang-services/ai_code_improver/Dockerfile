FROM erlang:26-alpine

WORKDIR /app

RUN apk add --no-cache git curl
# Download rebar3
RUN curl -o rebar3 https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3

COPY rebar.config .
RUN ./rebar3 get-deps

COPY src/ src/

RUN ./rebar3 compile

EXPOSE 8025

CMD ["rebar3", "shell", "--apps", "ai_code_improver"]
