#!/usr/bin/env python3
"""
Database Tests
Unit tests for database setup and population.
"""

import unittest
import os
import sys

sys.path.append(os.path.dirname(os.path.dirname(__file__)))


class TestDatabaseSchema(unittest.TestCase):
    """Test database schema definitions."""
    
    def test_frameworks_table_exists(self):
        """Test that frameworks table schema is defined."""
        from src.database.setup import SCHEMA
        self.assertIn("CREATE TABLE IF NOT EXISTS frameworks", SCHEMA)
    
    def test_mental_models_table_exists(self):
        """Test that mental_models table schema is defined."""
        from src.database.setup import SCHEMA
        self.assertIn("CREATE TABLE IF NOT EXISTS mental_models", SCHEMA)
    
    def test_case_studies_table_exists(self):
        """Test that case_studies table schema is defined."""
        from src.database.setup import SCHEMA
        self.assertIn("CREATE TABLE IF NOT EXISTS case_studies", SCHEMA)
    
    def test_planck_matrix_table_exists(self):
        """Test that planck_matrix table schema is defined."""
        from src.database.setup import SCHEMA
        self.assertIn("CREATE TABLE IF NOT EXISTS planck_matrix", SCHEMA)
    
    def test_thinker_principles_table_exists(self):
        """Test that thinker_principles table schema is defined."""
        from src.database.setup import SCHEMA
        self.assertIn("CREATE TABLE IF NOT EXISTS thinker_principles", SCHEMA)


class TestPopulateData(unittest.TestCase):
    """Test data population definitions."""
    
    def test_frameworks_defined(self):
        """Test that frameworks are defined."""
        from src.database.populate import FRAMEWORKS
        self.assertGreater(len(FRAMEWORKS), 0)
        self.assertIn("Munger", [f[0] for f in FRAMEWORKS])
        self.assertIn("Soros", [f[0] for f in FRAMEWORKS])
        self.assertIn("Dalio", [f[0] for f in FRAMEWORKS])
    
    def test_mental_models_defined(self):
        """Test that mental models are defined."""
        from src.database.populate import MENTAL_MODELS
        self.assertGreater(len(MENTAL_MODELS), 50)
        
        model_names = [m[0] for m in MENTAL_MODELS]
        self.assertIn("Compound Interest", model_names)
        self.assertIn("Reflexivity", model_names)
        self.assertIn("First Principles", model_names)
    
    def test_mental_models_have_categories(self):
        """Test that mental models have categories."""
        from src.database.populate import MENTAL_MODELS
        categories = set(m[1] for m in MENTAL_MODELS)
        self.assertIn("Mathematics", categories)
        self.assertIn("Psychology", categories)
        self.assertIn("Economics", categories)
    
    def test_thinker_principles_defined(self):
        """Test that thinker principles are defined."""
        from src.database.populate import THINKER_PRINCIPLES
        self.assertGreater(len(THINKER_PRINCIPLES), 50)
        
        thinkers = set(p[0] for p in THINKER_PRINCIPLES)
        self.assertIn("Munger", thinkers)
        self.assertIn("Musk", thinkers)
        self.assertIn("Franklin", thinkers)
    
    def test_exemplar_cases_defined(self):
        """Test that exemplar case studies are defined."""
        from src.database.populate import EXEMPLAR_CASES
        self.assertGreater(len(EXEMPLAR_CASES), 10)
        
        case_names = [c[0] for c in EXEMPLAR_CASES]
        self.assertIn("2008 Financial Crisis", case_names)
        self.assertIn("1929 Wall Street Crash", case_names)
    
    def test_case_categories_defined(self):
        """Test that case categories are defined."""
        from src.database.populate import CASE_CATEGORIES
        self.assertIn("Financial Crisis", CASE_CATEGORIES)
        self.assertIn("Market Bubble", CASE_CATEGORIES)
        self.assertIn("Fraud", CASE_CATEGORIES)
    
    def test_regions_defined(self):
        """Test that regions are defined."""
        from src.database.populate import REGIONS
        self.assertIn("North America", REGIONS)
        self.assertIn("Europe", REGIONS)
        self.assertIn("Global", REGIONS)


class TestMentalModelStructure(unittest.TestCase):
    """Test mental model data structure."""
    
    def test_model_tuple_structure(self):
        """Test that model tuples have correct structure."""
        from src.database.populate import MENTAL_MODELS
        for model in MENTAL_MODELS:
            self.assertEqual(len(model), 6, f"Model {model[0]} has wrong tuple length")
            self.assertIsInstance(model[0], str)
            self.assertIsInstance(model[1], str)
            self.assertIsInstance(model[2], str)
            self.assertIsInstance(model[3], str)
            self.assertIsInstance(model[4], str)
            self.assertIsInstance(model[5], int)
    
    def test_lindy_ages_positive(self):
        """Test that Lindy ages are positive."""
        from src.database.populate import MENTAL_MODELS
        for model in MENTAL_MODELS:
            self.assertGreater(model[5], 0, f"Model {model[0]} has non-positive Lindy age")


class TestPrincipleStructure(unittest.TestCase):
    """Test principle data structure."""
    
    def test_principle_tuple_structure(self):
        """Test that principle tuples have correct structure."""
        from src.database.populate import THINKER_PRINCIPLES
        for principle in THINKER_PRINCIPLES:
            self.assertEqual(len(principle), 5, f"Principle has wrong tuple length")
            self.assertIsInstance(principle[0], str)
            self.assertIsInstance(principle[1], str)
            self.assertIsInstance(principle[2], str)
            self.assertIsInstance(principle[3], str)
            self.assertIsInstance(principle[4], str)


if __name__ == "__main__":
    unittest.main()
