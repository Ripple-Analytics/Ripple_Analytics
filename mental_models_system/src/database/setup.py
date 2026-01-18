#!/usr/bin/env python3
"""
PostgreSQL Database Setup
Creates all tables for the Mental Models System.
"""

import psycopg2
from psycopg2.extensions import ISOLATION_LEVEL_AUTOCOMMIT

DB_NAME = "mental_models"

SCHEMA = """
-- Frameworks (Thinkers)
CREATE TABLE IF NOT EXISTS frameworks (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    description TEXT,
    source VARCHAR(500)
);

-- Mental Models
CREATE TABLE IF NOT EXISTS mental_models (
    id SERIAL PRIMARY KEY,
    name VARCHAR(200) NOT NULL,
    category VARCHAR(100),
    framework_id INTEGER REFERENCES frameworks(id),
    description TEXT,
    detection_signs TEXT,
    defense_strategies TEXT,
    inversion_question TEXT,
    lindy_age_years INTEGER,
    originator VARCHAR(200),
    modern_synthesizer VARCHAR(200)
);

-- Thinker Principles
CREATE TABLE IF NOT EXISTS thinker_principles (
    id SERIAL PRIMARY KEY,
    thinker VARCHAR(100) NOT NULL,
    principle_name VARCHAR(200) NOT NULL,
    principle_description TEXT,
    category VARCHAR(100),
    application_domain VARCHAR(100),
    source VARCHAR(500)
);

-- Case Studies (Partitioned by Year for Scale)
CREATE TABLE IF NOT EXISTS case_studies (
    id SERIAL,
    name VARCHAR(500) NOT NULL,
    date DATE,
    category VARCHAR(100),
    region VARCHAR(100),
    severity DECIMAL(3,2),
    financial_impact DECIMAL(20,2),
    duration_days INTEGER,
    models_involved INTEGER,
    lollapalooza_score DECIMAL(5,2),
    reflexivity_score DECIMAL(3,2),
    description TEXT,
    lessons_learned TEXT,
    PRIMARY KEY (id, date)
) PARTITION BY RANGE (date);

-- Create partitions for 70 years (1955-2025)
DO $$
DECLARE
    year_start INTEGER := 1955;
    year_end INTEGER := 2025;
    year_current INTEGER;
BEGIN
    FOR year_current IN year_start..year_end LOOP
        EXECUTE format(
            'CREATE TABLE IF NOT EXISTS case_studies_%s PARTITION OF case_studies 
             FOR VALUES FROM (%L) TO (%L)',
            year_current,
            year_current || '-01-01',
            (year_current + 1) || '-01-01'
        );
    END LOOP;
END $$;

-- Planck Case-Model Matrix (Deep Knowledge)
CREATE TABLE IF NOT EXISTS planck_matrix (
    id SERIAL PRIMARY KEY,
    case_id INTEGER NOT NULL,
    case_name VARCHAR(500),
    model_id INTEGER NOT NULL,
    model_name VARCHAR(200),
    applies BOOLEAN DEFAULT FALSE,
    direction VARCHAR(20),
    effect_size DECIMAL(5,3),
    p_value DECIMAL(8,6),
    confidence_lower DECIMAL(5,3),
    confidence_upper DECIMAL(5,3),
    variance DECIMAL(6,4),
    magnitude VARCHAR(20),
    mechanism_summary TEXT,
    mechanism_detail TEXT,
    causal_chain TEXT
);

-- Model Interactions (Pairwise)
CREATE TABLE IF NOT EXISTS model_interactions_pairwise (
    id SERIAL PRIMARY KEY,
    model_a_id INTEGER REFERENCES mental_models(id),
    model_b_id INTEGER REFERENCES mental_models(id),
    co_occurrence_count INTEGER,
    synergy_ratio DECIMAL(5,2),
    interaction_type VARCHAR(50),
    mechanism TEXT
);

-- Model Interactions (Triple - Lollapalooza)
CREATE TABLE IF NOT EXISTS model_interactions_triple (
    id SERIAL PRIMARY KEY,
    model_a_id INTEGER REFERENCES mental_models(id),
    model_b_id INTEGER REFERENCES mental_models(id),
    model_c_id INTEGER REFERENCES mental_models(id),
    co_occurrence_count INTEGER,
    lollapalooza_multiplier DECIMAL(5,2),
    mechanism TEXT
);

-- Intellectual Lineage
CREATE TABLE IF NOT EXISTS intellectual_lineage (
    id SERIAL PRIMARY KEY,
    model_id INTEGER REFERENCES mental_models(id),
    originator VARCHAR(200),
    originator_year INTEGER,
    modern_synthesizer VARCHAR(200),
    synthesis_year INTEGER,
    lineage_description TEXT
);

-- Indexes for Performance
CREATE INDEX IF NOT EXISTS idx_case_studies_severity ON case_studies(severity);
CREATE INDEX IF NOT EXISTS idx_case_studies_category ON case_studies(category);
CREATE INDEX IF NOT EXISTS idx_planck_matrix_case ON planck_matrix(case_id);
CREATE INDEX IF NOT EXISTS idx_planck_matrix_model ON planck_matrix(model_id);
CREATE INDEX IF NOT EXISTS idx_mental_models_category ON mental_models(category);
"""

def create_database():
    """Create the database if it doesn't exist."""
    conn = psycopg2.connect(dbname="postgres", user="postgres")
    conn.set_isolation_level(ISOLATION_LEVEL_AUTOCOMMIT)
    cur = conn.cursor()
    
    cur.execute(f"SELECT 1 FROM pg_database WHERE datname = '{DB_NAME}'")
    if not cur.fetchone():
        cur.execute(f"CREATE DATABASE {DB_NAME}")
        print(f"Database '{DB_NAME}' created.")
    else:
        print(f"Database '{DB_NAME}' already exists.")
    
    cur.close()
    conn.close()

def create_schema():
    """Create all tables."""
    conn = psycopg2.connect(dbname=DB_NAME, user="postgres")
    cur = conn.cursor()
    cur.execute(SCHEMA)
    conn.commit()
    cur.close()
    conn.close()
    print("Schema created successfully.")

if __name__ == "__main__":
    create_database()
    create_schema()
