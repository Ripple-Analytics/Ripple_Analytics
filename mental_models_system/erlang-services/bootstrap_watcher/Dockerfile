FROM alpine:3.18

RUN apk add --no-cache bash git curl docker-cli

WORKDIR /watcher

COPY watch.sh /watcher/watch.sh
RUN chmod +x /watcher/watch.sh

CMD ["/watcher/watch.sh"]
