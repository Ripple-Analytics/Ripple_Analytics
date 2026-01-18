-- Analysis Results Tables
-- Stores all text analyses and mental model scores

-- Main analysis table
CREATE TABLE IF NOT EXISTS analyses (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  user_id TEXT NOT NULL,
  device_id TEXT,
  text TEXT NOT NULL,
  text_length INTEGER,
  source VARCHAR(50) DEFAULT 'manual', -- file, news, folder, manual
  source_url TEXT,
  metadata JSONB DEFAULT '{}',
  
  -- Analysis results
  models_analyzed INTEGER DEFAULT 129,
  successful_analyses INTEGER,
  average_score DECIMAL(3,2),
  
  -- Lollapalooza detection
  lollapalooza_detected BOOLEAN DEFAULT FALSE,
  convergence_score DECIMAL(3,2),
  convergence_count INTEGER DEFAULT 0,
  
  -- Top 10 models (stored as JSON for quick access)
  top_10_models JSONB,
  
  -- All scores (full results)
  all_scores JSONB,
  
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  
  INDEX idx_user_id (user_id),
  INDEX idx_device_id (device_id),
  INDEX idx_created_at (created_at),
  INDEX idx_lollapalooza (lollapalooza_detected)
);

-- Batch analysis table
CREATE TABLE IF NOT EXISTS batch_analyses (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  batch_id UUID NOT NULL UNIQUE,
  user_id TEXT NOT NULL,
  device_id TEXT,
  analysis_count INTEGER,
  analyses JSONB, -- Array of analysis results
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  
  INDEX idx_user_id (user_id),
  INDEX idx_batch_id (batch_id),
  INDEX idx_created_at (created_at)
);

-- Model detection frequency table (for analytics)
CREATE TABLE IF NOT EXISTS model_detections (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  user_id TEXT NOT NULL,
  model_slug VARCHAR(255) NOT NULL,
  model_name VARCHAR(255),
  detection_count INTEGER DEFAULT 1,
  avg_score DECIMAL(3,2),
  last_detected TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  
  UNIQUE (user_id, model_slug),
  INDEX idx_user_id (user_id),
  INDEX idx_model_slug (model_slug),
  INDEX idx_avg_score (avg_score)
);

-- Lollapalooza events table
CREATE TABLE IF NOT EXISTS lollapalooza_events (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  analysis_id UUID NOT NULL REFERENCES analyses(id) ON DELETE CASCADE,
  user_id TEXT NOT NULL,
  convergence_score DECIMAL(3,2),
  convergence_count INTEGER,
  converging_models JSONB, -- Array of converging model objects
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  
  INDEX idx_user_id (user_id),
  INDEX idx_analysis_id (analysis_id),
  INDEX idx_created_at (created_at),
  INDEX idx_convergence_score (convergence_score)
);

-- Failure mode detections table
CREATE TABLE IF NOT EXISTS failure_mode_detections (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  analysis_id UUID NOT NULL REFERENCES analyses(id) ON DELETE CASCADE,
  user_id TEXT NOT NULL,
  model_slug VARCHAR(255),
  model_name VARCHAR(255),
  failure_mode_name VARCHAR(255),
  risk_level VARCHAR(50), -- critical, high, medium, low
  detected_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  
  INDEX idx_user_id (user_id),
  INDEX idx_analysis_id (analysis_id),
  INDEX idx_model_slug (model_slug),
  INDEX idx_risk_level (risk_level)
);

-- Analysis statistics table (for dashboard)
CREATE TABLE IF NOT EXISTS analysis_statistics (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  user_id TEXT NOT NULL UNIQUE,
  total_analyses INTEGER DEFAULT 0,
  total_texts_analyzed INTEGER DEFAULT 0,
  total_text_length BIGINT DEFAULT 0,
  avg_analysis_score DECIMAL(3,2),
  lollapalooza_count INTEGER DEFAULT 0,
  most_common_model VARCHAR(255),
  last_analysis TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  
  INDEX idx_user_id (user_id)
);

-- Create indexes for common queries
CREATE INDEX IF NOT EXISTS idx_analyses_user_created ON analyses(user_id, created_at DESC);
CREATE INDEX IF NOT EXISTS idx_analyses_lollapalooza ON analyses(user_id, lollapalooza_detected, created_at DESC);
CREATE INDEX IF NOT EXISTS idx_model_detections_user_score ON model_detections(user_id, avg_score DESC);
