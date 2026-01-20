FROM alpine:3.18

# Install minimal dependencies - curl for API, git for pulling, docker-compose for rebuilding
RUN apk add --no-cache \
    bash \
    curl \
    git \
    docker-cli \
    docker-compose

# Create data directory
RUN mkdir -p /data

WORKDIR /watcher

# Copy watcher script
COPY watch.sh /watcher/watch.sh
RUN chmod +x /watcher/watch.sh

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD [ -f /data/bootstrap_status.json ] && cat /data/bootstrap_status.json | grep -q '"status"' || exit 1

CMD ["/watcher/watch.sh"]
