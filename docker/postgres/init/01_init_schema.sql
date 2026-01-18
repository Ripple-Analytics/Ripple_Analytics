-- Mental Models System - Database Schema
-- The Oligarch's Operating System - 100-Year Infrastructure
-- Designed for 1.2M+ case studies and 113 mental models

-- Enable TimescaleDB extension for time-series optimization
CREATE EXTENSION IF NOT EXISTS timescaledb;

-- =============================================================================
-- CORE TABLES
-- =============================================================================

-- Thinkers: The 16+ legendary minds whose wisdom we codify
CREATE TABLE thinkers (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL UNIQUE,
    full_name VARCHAR(200),
    birth_year INTEGER,
    death_year INTEGER,
    primary_domain VARCHAR(100),
    description TEXT,
    key_works TEXT[],
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Categories: 17 categories of mental models
CREATE TABLE categories (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL UNIQUE,
    description TEXT,
    parent_category_id INTEGER REFERENCES categories(id),
    sort_order INTEGER DEFAULT 0,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Mental Models: 113 models from multiple thinkers
CREATE TABLE mental_models (
    id SERIAL PRIMARY KEY,
    name VARCHAR(200) NOT NULL,
    slug VARCHAR(200) NOT NULL UNIQUE,
    description TEXT,
    detailed_explanation TEXT,
    category_id INTEGER REFERENCES categories(id),
    primary_thinker_id INTEGER REFERENCES thinkers(id),
    complexity_level INTEGER CHECK (complexity_level BETWEEN 1 AND 5),
    applicability_score DECIMAL(3,2) CHECK (applicability_score BETWEEN 0 AND 1),
    keywords TEXT[],
    related_models INTEGER[],
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Principles: 264+ principles from various thinkers
CREATE TABLE principles (
    id SERIAL PRIMARY KEY,
    thinker_id INTEGER REFERENCES thinkers(id),
    title VARCHAR(300) NOT NULL,
    description TEXT,
    category VARCHAR(100),
    source VARCHAR(200),
    page_reference VARCHAR(50),
    importance_level INTEGER CHECK (importance_level BETWEEN 1 AND 5),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Case Studies: 1.2M+ historical examples (1955-2024)
CREATE TABLE case_studies (
    id SERIAL PRIMARY KEY,
    title VARCHAR(500) NOT NULL,
    description TEXT,
    event_date DATE,
    event_year INTEGER,
    decade VARCHAR(10),
    region VARCHAR(100),
    country VARCHAR(100),
    industry VARCHAR(100),
    outcome VARCHAR(50), -- success, failure, mixed, neutral
    outcome_magnitude DECIMAL(5,2), -- -100 to +100 scale
    key_actors TEXT[],
    data_sources TEXT[],
    confidence_score DECIMAL(3,2) CHECK (confidence_score BETWEEN 0 AND 1),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Convert case_studies to hypertable for time-series optimization
SELECT create_hypertable('case_studies', 'event_date', 
    chunk_time_interval => INTERVAL '1 year',
    if_not_exists => TRUE);

-- Model-Case Mappings: The Planck Matrix (6.7M+ mappings)
CREATE TABLE model_case_mappings (
    id SERIAL PRIMARY KEY,
    model_id INTEGER REFERENCES mental_models(id),
    case_study_id INTEGER REFERENCES case_studies(id),
    relevance_score DECIMAL(3,2) CHECK (relevance_score BETWEEN 0 AND 1),
    application_type VARCHAR(50), -- primary, secondary, tertiary
    notes TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(model_id, case_study_id)
);

-- Lollapalooza Events: Multi-model convergence patterns
CREATE TABLE lollapalooza_events (
    id SERIAL PRIMARY KEY,
    case_study_id INTEGER REFERENCES case_studies(id),
    model_ids INTEGER[] NOT NULL,
    model_count INTEGER NOT NULL,
    synergy_score DECIMAL(5,2),
    description TEXT,
    outcome_multiplier DECIMAL(5,2),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- =============================================================================
-- ANALYSIS TABLES
-- =============================================================================

-- Model Frequency Analysis
CREATE TABLE model_frequency_analysis (
    id SERIAL PRIMARY KEY,
    model_id INTEGER REFERENCES mental_models(id),
    decade VARCHAR(10),
    region VARCHAR(100),
    frequency_count INTEGER,
    success_rate DECIMAL(5,4),
    avg_outcome_magnitude DECIMAL(5,2),
    analysis_date DATE DEFAULT CURRENT_DATE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Regime Detection (HMM results)
CREATE TABLE regime_detection (
    id SERIAL PRIMARY KEY,
    detection_date DATE NOT NULL,
    regime_type VARCHAR(50), -- bull, bear, sideways, crisis, recovery
    confidence DECIMAL(3,2),
    duration_days INTEGER,
    associated_models INTEGER[],
    market_indicators JSONB,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Monte Carlo Simulations
CREATE TABLE monte_carlo_results (
    id SERIAL PRIMARY KEY,
    simulation_name VARCHAR(200),
    model_ids INTEGER[],
    num_simulations INTEGER,
    mean_outcome DECIMAL(10,4),
    std_outcome DECIMAL(10,4),
    percentile_5 DECIMAL(10,4),
    percentile_25 DECIMAL(10,4),
    percentile_50 DECIMAL(10,4),
    percentile_75 DECIMAL(10,4),
    percentile_95 DECIMAL(10,4),
    parameters JSONB,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- =============================================================================
-- INDEXES FOR PERFORMANCE
-- =============================================================================

CREATE INDEX idx_mental_models_category ON mental_models(category_id);
CREATE INDEX idx_mental_models_thinker ON mental_models(primary_thinker_id);
CREATE INDEX idx_mental_models_slug ON mental_models(slug);

CREATE INDEX idx_case_studies_year ON case_studies(event_year);
CREATE INDEX idx_case_studies_decade ON case_studies(decade);
CREATE INDEX idx_case_studies_region ON case_studies(region);
CREATE INDEX idx_case_studies_industry ON case_studies(industry);
CREATE INDEX idx_case_studies_outcome ON case_studies(outcome);

CREATE INDEX idx_model_case_mappings_model ON model_case_mappings(model_id);
CREATE INDEX idx_model_case_mappings_case ON model_case_mappings(case_study_id);
CREATE INDEX idx_model_case_mappings_relevance ON model_case_mappings(relevance_score);

CREATE INDEX idx_lollapalooza_model_count ON lollapalooza_events(model_count);
CREATE INDEX idx_lollapalooza_synergy ON lollapalooza_events(synergy_score);

CREATE INDEX idx_principles_thinker ON principles(thinker_id);
CREATE INDEX idx_principles_category ON principles(category);

-- =============================================================================
-- VIEWS FOR COMMON QUERIES
-- =============================================================================

-- Top performing models by success rate
CREATE VIEW v_top_models AS
SELECT 
    mm.id,
    mm.name,
    mm.slug,
    c.name as category,
    t.name as thinker,
    COUNT(DISTINCT mcm.case_study_id) as case_count,
    AVG(CASE WHEN cs.outcome = 'success' THEN 1.0 ELSE 0.0 END) as success_rate,
    AVG(cs.outcome_magnitude) as avg_magnitude
FROM mental_models mm
LEFT JOIN categories c ON mm.category_id = c.id
LEFT JOIN thinkers t ON mm.primary_thinker_id = t.id
LEFT JOIN model_case_mappings mcm ON mm.id = mcm.model_id
LEFT JOIN case_studies cs ON mcm.case_study_id = cs.id
GROUP BY mm.id, mm.name, mm.slug, c.name, t.name;

-- Lollapalooza patterns summary
CREATE VIEW v_lollapalooza_summary AS
SELECT 
    le.model_count,
    COUNT(*) as event_count,
    AVG(le.synergy_score) as avg_synergy,
    AVG(le.outcome_multiplier) as avg_multiplier
FROM lollapalooza_events le
GROUP BY le.model_count
ORDER BY le.model_count;

-- Decade analysis
CREATE VIEW v_decade_analysis AS
SELECT 
    cs.decade,
    COUNT(*) as case_count,
    AVG(CASE WHEN cs.outcome = 'success' THEN 1.0 ELSE 0.0 END) as success_rate,
    AVG(cs.outcome_magnitude) as avg_magnitude,
    COUNT(DISTINCT mcm.model_id) as models_applied
FROM case_studies cs
LEFT JOIN model_case_mappings mcm ON cs.id = mcm.case_study_id
GROUP BY cs.decade
ORDER BY cs.decade;

-- =============================================================================
-- FUNCTIONS
-- =============================================================================

-- Function to find related models
CREATE OR REPLACE FUNCTION find_related_models(model_slug VARCHAR)
RETURNS TABLE (
    related_model_name VARCHAR,
    co_occurrence_count INTEGER,
    correlation_score DECIMAL
) AS $$
BEGIN
    RETURN QUERY
    SELECT 
        mm2.name::VARCHAR,
        COUNT(*)::INTEGER as co_occurrence,
        (COUNT(*)::DECIMAL / (SELECT COUNT(*) FROM model_case_mappings WHERE model_id = mm1.id))::DECIMAL as correlation
    FROM mental_models mm1
    JOIN model_case_mappings mcm1 ON mm1.id = mcm1.model_id
    JOIN model_case_mappings mcm2 ON mcm1.case_study_id = mcm2.case_study_id AND mcm1.model_id != mcm2.model_id
    JOIN mental_models mm2 ON mcm2.model_id = mm2.id
    WHERE mm1.slug = model_slug
    GROUP BY mm2.name
    ORDER BY co_occurrence DESC
    LIMIT 10;
END;
$$ LANGUAGE plpgsql;

-- Function to detect Lollapalooza patterns
CREATE OR REPLACE FUNCTION detect_lollapalooza(min_models INTEGER DEFAULT 3)
RETURNS TABLE (
    case_id INTEGER,
    case_title VARCHAR,
    model_count INTEGER,
    models TEXT[],
    outcome VARCHAR,
    magnitude DECIMAL
) AS $$
BEGIN
    RETURN QUERY
    SELECT 
        cs.id::INTEGER,
        cs.title::VARCHAR,
        COUNT(DISTINCT mcm.model_id)::INTEGER as model_count,
        ARRAY_AGG(DISTINCT mm.name)::TEXT[] as models,
        cs.outcome::VARCHAR,
        cs.outcome_magnitude::DECIMAL
    FROM case_studies cs
    JOIN model_case_mappings mcm ON cs.id = mcm.case_study_id
    JOIN mental_models mm ON mcm.model_id = mm.id
    GROUP BY cs.id, cs.title, cs.outcome, cs.outcome_magnitude
    HAVING COUNT(DISTINCT mcm.model_id) >= min_models
    ORDER BY COUNT(DISTINCT mcm.model_id) DESC, cs.outcome_magnitude DESC;
END;
$$ LANGUAGE plpgsql;

-- Grant permissions
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO oligarch;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO oligarch;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA public TO oligarch;
