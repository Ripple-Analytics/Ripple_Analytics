-- Mental Models System Database Schema
-- Electric Clojure Edition

-- Categories
CREATE TABLE IF NOT EXISTS categories (
  id INT AUTO_INCREMENT PRIMARY KEY,
  name VARCHAR(100) NOT NULL,
  slug VARCHAR(100) NOT NULL UNIQUE,
  description TEXT,
  color VARCHAR(50) DEFAULT 'amber',
  icon VARCHAR(50),
  display_order INT DEFAULT 0,
  created_at BIGINT DEFAULT (UNIX_TIMESTAMP() * 1000),
  updated_at BIGINT DEFAULT (UNIX_TIMESTAMP() * 1000)
);

-- Mental Models
CREATE TABLE IF NOT EXISTS mental_models (
  id INT AUTO_INCREMENT PRIMARY KEY,
  name VARCHAR(200) NOT NULL,
  slug VARCHAR(200) NOT NULL UNIQUE,
  description TEXT,
  detailed_explanation TEXT,
  example TEXT,
  counter_example TEXT,
  thinker VARCHAR(200),
  source VARCHAR(500),
  category_id INT,
  complexity INT DEFAULT 2,
  is_core BOOLEAN DEFAULT FALSE,
  tags JSON,
  related_models JSON,
  created_at BIGINT DEFAULT (UNIX_TIMESTAMP() * 1000),
  updated_at BIGINT DEFAULT (UNIX_TIMESTAMP() * 1000),
  FOREIGN KEY (category_id) REFERENCES categories(id)
);

-- Failure Modes
CREATE TABLE IF NOT EXISTS failure_modes (
  id INT AUTO_INCREMENT PRIMARY KEY,
  model_id INT NOT NULL,
  name VARCHAR(200) NOT NULL,
  description TEXT,
  triggers TEXT,
  safeguard TEXT,
  risk_level ENUM('low', 'medium', 'high') DEFAULT 'medium',
  examples JSON,
  created_at BIGINT DEFAULT (UNIX_TIMESTAMP() * 1000),
  FOREIGN KEY (model_id) REFERENCES mental_models(id)
);

-- Users
CREATE TABLE IF NOT EXISTS users (
  id INT AUTO_INCREMENT PRIMARY KEY,
  open_id VARCHAR(100) UNIQUE,
  name VARCHAR(200),
  email VARCHAR(200),
  avatar_url VARCHAR(500),
  role ENUM('user', 'admin') DEFAULT 'user',
  preferences JSON,
  created_at BIGINT DEFAULT (UNIX_TIMESTAMP() * 1000),
  last_login_at BIGINT
);

-- Decisions
CREATE TABLE IF NOT EXISTS decisions (
  id INT AUTO_INCREMENT PRIMARY KEY,
  user_id INT,
  title VARCHAR(500) NOT NULL,
  context TEXT,
  options TEXT,
  chosen_option TEXT,
  models_applied TEXT,
  reasoning TEXT,
  outcome TEXT,
  outcome_rating INT,
  status ENUM('pending', 'completed', 'abandoned') DEFAULT 'pending',
  decision_date BIGINT,
  review_date BIGINT,
  created_at BIGINT DEFAULT (UNIX_TIMESTAMP() * 1000),
  updated_at BIGINT DEFAULT (UNIX_TIMESTAMP() * 1000),
  FOREIGN KEY (user_id) REFERENCES users(id)
);

-- Decision Models (junction table)
CREATE TABLE IF NOT EXISTS decision_models (
  id INT AUTO_INCREMENT PRIMARY KEY,
  decision_id INT NOT NULL,
  model_id INT NOT NULL,
  relevance DECIMAL(3,2),
  notes TEXT,
  created_at BIGINT DEFAULT (UNIX_TIMESTAMP() * 1000),
  FOREIGN KEY (decision_id) REFERENCES decisions(id),
  FOREIGN KEY (model_id) REFERENCES mental_models(id)
);

-- Model Usage Tracking
CREATE TABLE IF NOT EXISTS model_usage (
  id INT AUTO_INCREMENT PRIMARY KEY,
  user_id INT,
  model_id INT NOT NULL,
  decision_id INT,
  context VARCHAR(500),
  outcome_rating INT,
  notes TEXT,
  created_at BIGINT DEFAULT (UNIX_TIMESTAMP() * 1000),
  FOREIGN KEY (user_id) REFERENCES users(id),
  FOREIGN KEY (model_id) REFERENCES mental_models(id),
  FOREIGN KEY (decision_id) REFERENCES decisions(id)
);

-- Analyses
CREATE TABLE IF NOT EXISTS analyses (
  id INT AUTO_INCREMENT PRIMARY KEY,
  user_id INT,
  input_text TEXT,
  analysis_type ENUM('models', 'lollapalooza', 'decision', 'document') DEFAULT 'models',
  result JSON,
  models_found JSON,
  created_at BIGINT DEFAULT (UNIX_TIMESTAMP() * 1000),
  FOREIGN KEY (user_id) REFERENCES users(id)
);

-- Signals (for Signal Harvester)
CREATE TABLE IF NOT EXISTS signals (
  id INT AUTO_INCREMENT PRIMARY KEY,
  user_id INT,
  source VARCHAR(200),
  source_url VARCHAR(500),
  title VARCHAR(500),
  content TEXT,
  signal_type VARCHAR(100),
  models_relevant JSON,
  strength DECIMAL(3,2),
  processed BOOLEAN DEFAULT FALSE,
  created_at BIGINT DEFAULT (UNIX_TIMESTAMP() * 1000),
  FOREIGN KEY (user_id) REFERENCES users(id)
);

-- Model Updates (from academic sources)
CREATE TABLE IF NOT EXISTS model_updates (
  id INT AUTO_INCREMENT PRIMARY KEY,
  model_name VARCHAR(200),
  paper_title VARCHAR(500),
  paper_id VARCHAR(200),
  insights TEXT,
  evidence_type VARCHAR(100),
  relevance DECIMAL(3,2),
  source VARCHAR(100),
  reviewed BOOLEAN DEFAULT FALSE,
  created_at BIGINT DEFAULT (UNIX_TIMESTAMP() * 1000)
);

-- Model Connections (for Knowledge Graph)
CREATE TABLE IF NOT EXISTS model_connections (
  id INT AUTO_INCREMENT PRIMARY KEY,
  source_model_id INT NOT NULL,
  target_model_id INT NOT NULL,
  connection_type ENUM('related', 'opposite', 'builds_on', 'conflicts', 'synergy') DEFAULT 'related',
  strength DECIMAL(3,2) DEFAULT 0.5,
  description TEXT,
  created_at BIGINT DEFAULT (UNIX_TIMESTAMP() * 1000),
  FOREIGN KEY (source_model_id) REFERENCES mental_models(id),
  FOREIGN KEY (target_model_id) REFERENCES mental_models(id)
);

-- Indexes for performance
CREATE INDEX idx_models_category ON mental_models(category_id);
CREATE INDEX idx_models_slug ON mental_models(slug);
CREATE INDEX idx_decisions_user ON decisions(user_id);
CREATE INDEX idx_decisions_status ON decisions(status);
CREATE INDEX idx_model_usage_user ON model_usage(user_id);
CREATE INDEX idx_model_usage_model ON model_usage(model_id);
CREATE INDEX idx_analyses_user ON analyses(user_id);
CREATE INDEX idx_signals_user ON signals(user_id);
CREATE INDEX idx_connections_source ON model_connections(source_model_id);
CREATE INDEX idx_connections_target ON model_connections(target_model_id);
