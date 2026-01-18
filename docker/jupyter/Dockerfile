# Mental Models System - Jupyter Notebook Service
# Interactive analysis environment for mental models research
FROM jupyter/pyspark-notebook:latest

USER root

# Install additional system packages
RUN apt-get update && apt-get install -y \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

USER ${NB_UID}

# Install Python packages for mental models analysis
RUN pip install --no-cache-dir \
    psycopg2-binary \
    sqlalchemy \
    pandas \
    numpy \
    scipy \
    scikit-learn \
    statsmodels \
    hmmlearn \
    matplotlib \
    seaborn \
    plotly \
    openpyxl \
    python-dotenv \
    tqdm \
    nashpy \
    pymc \
    arviz

# Set working directory
WORKDIR /home/jovyan

# Expose Jupyter port
EXPOSE 8888

# Start Jupyter
CMD ["start-notebook.sh", "--NotebookApp.token=${JUPYTER_TOKEN}"]
