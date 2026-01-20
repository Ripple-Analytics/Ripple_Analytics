FROM alpine:3.19

# Install required tools
RUN apk add --no-cache \
    bash \
    curl \
    git \
    docker-cli \
    docker-cli-compose \
    unzip \
    jq

# Create app directory
WORKDIR /updater

# Copy update script
COPY auto_update.sh /updater/auto_update.sh
RUN chmod +x /updater/auto_update.sh

# Run the auto-updater
CMD ["/updater/auto_update.sh"]
