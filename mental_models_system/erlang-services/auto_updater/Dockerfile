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
    openssh-client

# Create app directory
WORKDIR /updater

# Create SSH directory
RUN mkdir -p /root/.ssh && chmod 700 /root/.ssh

# Copy update script
COPY auto_update.sh /updater/auto_update.sh
RUN chmod +x /updater/auto_update.sh

# Run the auto-updater
CMD ["/updater/auto_update.sh"]
