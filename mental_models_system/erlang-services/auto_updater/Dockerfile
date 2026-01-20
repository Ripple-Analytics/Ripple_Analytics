FROM alpine:3.19

# Install required tools including openssh for SSH authentication
RUN apk add --no-cache \
    bash \
    curl \
    git \
    docker-cli \
    docker-cli-compose \
    unzip \
    jq \
    openssh-client \
    dos2unix

# Create app directory
WORKDIR /updater

# Copy update script and fix line endings (handles Windows CRLF)
COPY auto_update.sh /updater/auto_update.sh
RUN dos2unix /updater/auto_update.sh && chmod +x /updater/auto_update.sh

# Run the auto-updater
CMD ["/updater/auto_update.sh"]
