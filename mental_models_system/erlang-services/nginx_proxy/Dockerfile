FROM nginx:alpine

# Copy nginx configuration
COPY nginx.conf /etc/nginx/nginx.conf
COPY active.conf /etc/nginx/conf.d/active.conf
COPY active_api.conf /etc/nginx/conf.d/active_api.conf
COPY active_analysis.conf /etc/nginx/conf.d/active_analysis.conf
COPY active_harvester.conf /etc/nginx/conf.d/active_harvester.conf
COPY active_storage.conf /etc/nginx/conf.d/active_storage.conf
COPY active_env /etc/nginx/active_env

# Create directory for dynamic config
RUN mkdir -p /etc/nginx/conf.d

# Copy all active environment configuration files
COPY active.conf /etc/nginx/conf.d/active.conf
COPY active-api-gateway.conf /etc/nginx/conf.d/active-api-gateway.conf
COPY active-analysis.conf /etc/nginx/conf.d/active-analysis.conf
COPY active-harvester.conf /etc/nginx/conf.d/active-harvester.conf
COPY active-storage.conf /etc/nginx/conf.d/active-storage.conf
COPY active-chaos.conf /etc/nginx/conf.d/active-chaos.conf
COPY active_env /etc/nginx/active_env

# Health check
HEALTHCHECK --interval=10s --timeout=5s --start-period=5s --retries=3 \
    CMD wget -q --spider http://localhost:8080/health || exit 1

# Expose all service ports
# 3000 - Desktop UI
# 8000 - API Gateway
# 8001 - Analysis Service
# 8002 - Harvester Service
# 8003 - Storage Service
# 8080 - Health check endpoint
EXPOSE 3000 8000 8001 8002 8003 8080

CMD ["nginx", "-g", "daemon off;"]
