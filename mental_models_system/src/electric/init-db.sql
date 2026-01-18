-- Mental Models System Database Initialization
-- This script runs automatically when the PostgreSQL container starts

-- Enable UUID extension
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- Enable pgvector for embeddings (if available)
-- CREATE EXTENSION IF NOT EXISTS vector;

-- Analyses table - stores analysis results
CREATE TABLE IF NOT EXISTS analyses (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    situation TEXT NOT NULL,
    models_used JSONB DEFAULT '[]'::jsonb,
    analysis_result JSONB DEFAULT '{}'::jsonb,
    biases_detected JSONB DEFAULT '[]'::jsonb,
    recommendations JSONB DEFAULT '[]'::jsonb,
    lollapalooza_effects JSONB DEFAULT '[]'::jsonb,
    llm_powered BOOLEAN DEFAULT false,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Sessions table - tracks user sessions
CREATE TABLE IF NOT EXISTS sessions (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    user_id VARCHAR(255),
    device_type VARCHAR(50),
    started_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    ended_at TIMESTAMP WITH TIME ZONE,
    metadata JSONB DEFAULT '{}'::jsonb
);

-- Model usage table - tracks which models are used
CREATE TABLE IF NOT EXISTS model_usage (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    model_id VARCHAR(255) NOT NULL,
    session_id UUID REFERENCES sessions(id),
    analysis_id UUID REFERENCES analyses(id),
    used_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    context JSONB DEFAULT '{}'::jsonb
);

-- Learning history table - stores processed content
CREATE TABLE IF NOT EXISTS learning_history (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    content_hash VARCHAR(64) NOT NULL UNIQUE,
    content_type VARCHAR(50),
    source VARCHAR(255),
    processed_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    insights JSONB DEFAULT '[]'::jsonb,
    patterns JSONB DEFAULT '[]'::jsonb,
    metadata JSONB DEFAULT '{}'::jsonb
);

-- Feedback table - stores user feedback
CREATE TABLE IF NOT EXISTS feedback (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    analysis_id UUID REFERENCES analyses(id),
    rating INTEGER CHECK (rating >= 1 AND rating <= 5),
    comment TEXT,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Tech debt analyses table
CREATE TABLE IF NOT EXISTS tech_debt_analyses (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    codebase_path TEXT NOT NULL,
    total_nodes INTEGER,
    total_edges INTEGER,
    tangles JSONB DEFAULT '[]'::jsonb,
    high_coupling_nodes JSONB DEFAULT '[]'::jsonb,
    refactoring_plan JSONB DEFAULT '[]'::jsonb,
    analyzed_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Distributed tasks table - for task queue persistence
CREATE TABLE IF NOT EXISTS distributed_tasks (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    task_type VARCHAR(100) NOT NULL,
    priority VARCHAR(20) DEFAULT 'normal',
    status VARCHAR(20) DEFAULT 'pending',
    data JSONB DEFAULT '{}'::jsonb,
    result JSONB,
    worker_id VARCHAR(255),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    started_at TIMESTAMP WITH TIME ZONE,
    completed_at TIMESTAMP WITH TIME ZONE,
    error TEXT
);

-- Cluster nodes table - for node registry
CREATE TABLE IF NOT EXISTS cluster_nodes (
    id VARCHAR(255) PRIMARY KEY,
    node_type VARCHAR(50),
    capabilities JSONB DEFAULT '[]'::jsonb,
    capacity INTEGER DEFAULT 1,
    current_load INTEGER DEFAULT 0,
    last_heartbeat TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    metadata JSONB DEFAULT '{}'::jsonb
);

-- Scraped content table - for web scraper results
CREATE TABLE IF NOT EXISTS scraped_content (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    url TEXT NOT NULL,
    content_hash VARCHAR(64),
    title TEXT,
    content TEXT,
    links JSONB DEFAULT '[]'::jsonb,
    scraped_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    metadata JSONB DEFAULT '{}'::jsonb
);

-- Sensor data table - for continuous sensor collection
CREATE TABLE IF NOT EXISTS sensor_data (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    device_id VARCHAR(255) NOT NULL,
    sensor_type VARCHAR(50) NOT NULL,
    value JSONB NOT NULL,
    unit VARCHAR(20),
    recorded_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Create indexes for performance
CREATE INDEX IF NOT EXISTS idx_analyses_created_at ON analyses(created_at DESC);
CREATE INDEX IF NOT EXISTS idx_model_usage_model_id ON model_usage(model_id);
CREATE INDEX IF NOT EXISTS idx_learning_history_content_hash ON learning_history(content_hash);
CREATE INDEX IF NOT EXISTS idx_distributed_tasks_status ON distributed_tasks(status);
CREATE INDEX IF NOT EXISTS idx_distributed_tasks_priority ON distributed_tasks(priority);
CREATE INDEX IF NOT EXISTS idx_cluster_nodes_last_heartbeat ON cluster_nodes(last_heartbeat);
CREATE INDEX IF NOT EXISTS idx_sensor_data_device_id ON sensor_data(device_id);
CREATE INDEX IF NOT EXISTS idx_sensor_data_recorded_at ON sensor_data(recorded_at DESC);

-- Create updated_at trigger function
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$ language 'plpgsql';

-- Apply trigger to analyses table
DROP TRIGGER IF EXISTS update_analyses_updated_at ON analyses;
CREATE TRIGGER update_analyses_updated_at
    BEFORE UPDATE ON analyses
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at_column();

-- Grant permissions (for development)
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO mental_models;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO mental_models;
