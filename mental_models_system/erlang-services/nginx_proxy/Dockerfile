FROM nginx:alpine

# Copy nginx configuration
COPY nginx.conf /etc/nginx/nginx.conf
COPY active.conf /etc/nginx/conf.d/active.conf
COPY active_env /etc/nginx/active_env

# Create directory for dynamic config
RUN mkdir -p /etc/nginx/conf.d

# Health check
HEALTHCHECK --interval=10s --timeout=5s --start-period=5s --retries=3 \
    CMD wget -q --spider http://localhost:8080/health || exit 1

EXPOSE 3000 8080

CMD ["nginx", "-g", "daemon off;"]
