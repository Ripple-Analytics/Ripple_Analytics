"""
Complete Database Population Script for Mental Models System
Integrates Charlie Munger's 129 mental models and 50 case studies
Source: Poor Charlie's Almanack, Worldly Wisdom speeches, personal documents
"""

import json
import os
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Any

# Try to import database modules, fall back to JSON if not available
try:
    import psycopg2
    from psycopg2.extras import execute_values
    HAS_POSTGRES = True
except ImportError:
    HAS_POSTGRES = False

try:
    from sqlalchemy import create_engine, text
    from sqlalchemy.orm import sessionmaker
    HAS_SQLALCHEMY = True
except ImportError:
    HAS_SQLALCHEMY = False


class MentalModelsPopulator:
    """Populates the mental models database with comprehensive data."""
    
    def __init__(self, data_dir: str = None):
        """Initialize with data directory path."""
        if data_dir is None:
            # Default to the raw data directory
            self.data_dir = Path(__file__).parent.parent.parent / "data" / "raw"
        else:
            self.data_dir = Path(data_dir)
        
        self.models_data = None
        self.case_studies_data = None
        
    def load_data(self) -> bool:
        """Load mental models and case studies from JSON files."""
        try:
            # Load mental models
            models_file = self.data_dir / "mental_models_complete.json"
            if models_file.exists():
                with open(models_file, 'r', encoding='utf-8') as f:
                    self.models_data = json.load(f)
                print(f"Loaded {len(self.models_data.get('mental_models', []))} mental models")
            else:
                print(f"Warning: {models_file} not found")
                return False
            
            # Load case studies
            case_studies_file = self.data_dir / "case_studies_complete.json"
            if case_studies_file.exists():
                with open(case_studies_file, 'r', encoding='utf-8') as f:
                    self.case_studies_data = json.load(f)
                print(f"Loaded {len(self.case_studies_data.get('case_studies', []))} case studies")
            else:
                print(f"Warning: {case_studies_file} not found")
                
            return True
            
        except Exception as e:
            print(f"Error loading data: {e}")
            return False
    
    def get_thinkers(self) -> List[Dict]:
        """Get list of thinkers from loaded data."""
        if self.models_data:
            return self.models_data.get('thinkers', [])
        return []
    
    def get_categories(self) -> List[Dict]:
        """Get list of categories from loaded data."""
        if self.models_data:
            return self.models_data.get('categories', [])
        return []
    
    def get_mental_models(self) -> List[Dict]:
        """Get list of mental models from loaded data."""
        if self.models_data:
            return self.models_data.get('mental_models', [])
        return []
    
    def get_case_studies(self) -> List[Dict]:
        """Get list of case studies from loaded data."""
        if self.case_studies_data:
            return self.case_studies_data.get('case_studies', [])
        return []
    
    def get_lollapalooza_patterns(self) -> List[Dict]:
        """Get lollapalooza patterns from case studies data."""
        if self.case_studies_data:
            patterns = self.case_studies_data.get('lollapalooza_patterns', {})
            return patterns.get('patterns', [])
        return []
    
    def populate_postgres(self, connection_string: str) -> bool:
        """Populate PostgreSQL database with all data."""
        if not HAS_POSTGRES:
            print("psycopg2 not installed, skipping PostgreSQL population")
            return False
            
        try:
            conn = psycopg2.connect(connection_string)
            cur = conn.cursor()
            
            # Insert thinkers
            thinkers = self.get_thinkers()
            if thinkers:
                thinker_values = [
                    (t['id'], t['name'], t.get('full_name'), t.get('birth_year'), 
                     t.get('death_year'), t.get('primary_domain'), 
                     json.dumps(t.get('key_works', [])))
                    for t in thinkers
                ]
                execute_values(cur, """
                    INSERT INTO thinkers (id, name, full_name, birth_year, death_year, primary_domain, key_works)
                    VALUES %s
                    ON CONFLICT (id) DO UPDATE SET
                        name = EXCLUDED.name,
                        full_name = EXCLUDED.full_name,
                        primary_domain = EXCLUDED.primary_domain
                """, thinker_values)
                print(f"Inserted {len(thinkers)} thinkers")
            
            # Insert categories
            categories = self.get_categories()
            if categories:
                category_values = [
                    (c['id'], c['name'], c.get('description'), c.get('model_count', 0))
                    for c in categories
                ]
                execute_values(cur, """
                    INSERT INTO categories (id, name, description, model_count)
                    VALUES %s
                    ON CONFLICT (id) DO UPDATE SET
                        name = EXCLUDED.name,
                        description = EXCLUDED.description,
                        model_count = EXCLUDED.model_count
                """, category_values)
                print(f"Inserted {len(categories)} categories")
            
            # Insert mental models
            models = self.get_mental_models()
            if models:
                model_values = [
                    (m['id'], m['name'], m['category_id'], m.get('thinker_id'),
                     m.get('description'), m.get('complexity', 3), 
                     m.get('applicability', 0.8))
                    for m in models
                ]
                execute_values(cur, """
                    INSERT INTO mental_models (id, name, category_id, thinker_id, description, complexity, applicability)
                    VALUES %s
                    ON CONFLICT (id) DO UPDATE SET
                        name = EXCLUDED.name,
                        description = EXCLUDED.description,
                        complexity = EXCLUDED.complexity,
                        applicability = EXCLUDED.applicability
                """, model_values)
                print(f"Inserted {len(models)} mental models")
            
            # Insert case studies
            case_studies = self.get_case_studies()
            if case_studies:
                case_study_values = [
                    (cs['id'], cs['title'], cs.get('description'), cs.get('outcome'),
                     cs.get('lollapalooza_score', 0), cs.get('key_insight'),
                     cs.get('year'), cs.get('domain'), cs.get('thinker_id'),
                     json.dumps(cs.get('mental_model_ids', [])))
                    for cs in case_studies
                ]
                execute_values(cur, """
                    INSERT INTO case_studies (id, title, description, outcome, lollapalooza_score, 
                                             key_insight, year, domain, thinker_id, mental_model_ids)
                    VALUES %s
                    ON CONFLICT (id) DO UPDATE SET
                        title = EXCLUDED.title,
                        description = EXCLUDED.description,
                        lollapalooza_score = EXCLUDED.lollapalooza_score,
                        key_insight = EXCLUDED.key_insight
                """, case_study_values)
                print(f"Inserted {len(case_studies)} case studies")
            
            conn.commit()
            cur.close()
            conn.close()
            print("PostgreSQL population complete")
            return True
            
        except Exception as e:
            print(f"Error populating PostgreSQL: {e}")
            return False
    
    def export_to_csv(self, output_dir: str = None) -> bool:
        """Export all data to CSV files for analysis."""
        import csv
        
        if output_dir is None:
            output_dir = self.data_dir.parent / "processed"
        else:
            output_dir = Path(output_dir)
        
        output_dir.mkdir(parents=True, exist_ok=True)
        
        try:
            # Export thinkers
            thinkers = self.get_thinkers()
            if thinkers:
                with open(output_dir / "thinkers.csv", 'w', newline='', encoding='utf-8') as f:
                    writer = csv.DictWriter(f, fieldnames=['id', 'name', 'full_name', 'birth_year', 'death_year', 'primary_domain'])
                    writer.writeheader()
                    for t in thinkers:
                        writer.writerow({k: t.get(k) for k in writer.fieldnames})
                print(f"Exported thinkers to {output_dir / 'thinkers.csv'}")
            
            # Export categories
            categories = self.get_categories()
            if categories:
                with open(output_dir / "categories.csv", 'w', newline='', encoding='utf-8') as f:
                    writer = csv.DictWriter(f, fieldnames=['id', 'name', 'description', 'model_count'])
                    writer.writeheader()
                    for c in categories:
                        writer.writerow({k: c.get(k) for k in writer.fieldnames})
                print(f"Exported categories to {output_dir / 'categories.csv'}")
            
            # Export mental models
            models = self.get_mental_models()
            if models:
                with open(output_dir / "mental_models.csv", 'w', newline='', encoding='utf-8') as f:
                    writer = csv.DictWriter(f, fieldnames=['id', 'name', 'category_id', 'thinker_id', 'description', 'complexity', 'applicability'])
                    writer.writeheader()
                    for m in models:
                        writer.writerow({k: m.get(k) for k in writer.fieldnames})
                print(f"Exported mental models to {output_dir / 'mental_models.csv'}")
            
            # Export case studies
            case_studies = self.get_case_studies()
            if case_studies:
                with open(output_dir / "case_studies.csv", 'w', newline='', encoding='utf-8') as f:
                    fieldnames = ['id', 'title', 'description', 'outcome', 'lollapalooza_score', 'key_insight', 'year', 'domain', 'thinker_id']
                    writer = csv.DictWriter(f, fieldnames=fieldnames)
                    writer.writeheader()
                    for cs in case_studies:
                        writer.writerow({k: cs.get(k) for k in fieldnames})
                print(f"Exported case studies to {output_dir / 'case_studies.csv'}")
            
            # Export case study to model mappings
            if case_studies:
                with open(output_dir / "case_study_models.csv", 'w', newline='', encoding='utf-8') as f:
                    writer = csv.writer(f)
                    writer.writerow(['case_study_id', 'mental_model_id'])
                    for cs in case_studies:
                        for model_id in cs.get('mental_model_ids', []):
                            writer.writerow([cs['id'], model_id])
                print(f"Exported case study mappings to {output_dir / 'case_study_models.csv'}")
            
            return True
            
        except Exception as e:
            print(f"Error exporting to CSV: {e}")
            return False
    
    def generate_summary_stats(self) -> Dict[str, Any]:
        """Generate summary statistics for the loaded data."""
        stats = {
            'total_thinkers': len(self.get_thinkers()),
            'total_categories': len(self.get_categories()),
            'total_mental_models': len(self.get_mental_models()),
            'total_case_studies': len(self.get_case_studies()),
            'total_lollapalooza_patterns': len(self.get_lollapalooza_patterns()),
        }
        
        # Category breakdown
        models = self.get_mental_models()
        category_counts = {}
        for m in models:
            cat_id = m.get('category_id')
            category_counts[cat_id] = category_counts.get(cat_id, 0) + 1
        stats['models_by_category'] = category_counts
        
        # Case study outcomes
        case_studies = self.get_case_studies()
        outcome_counts = {'success': 0, 'failure': 0, 'mixed': 0}
        for cs in case_studies:
            outcome = cs.get('outcome', 'unknown')
            if outcome in outcome_counts:
                outcome_counts[outcome] += 1
        stats['case_study_outcomes'] = outcome_counts
        
        # Average lollapalooza score
        scores = [cs.get('lollapalooza_score', 0) for cs in case_studies if cs.get('lollapalooza_score')]
        stats['avg_lollapalooza_score'] = sum(scores) / len(scores) if scores else 0
        
        # Most referenced models in case studies
        model_refs = {}
        for cs in case_studies:
            for model_id in cs.get('mental_model_ids', []):
                model_refs[model_id] = model_refs.get(model_id, 0) + 1
        
        # Get top 10 most referenced models
        sorted_refs = sorted(model_refs.items(), key=lambda x: x[1], reverse=True)[:10]
        model_names = {m['id']: m['name'] for m in models}
        stats['top_referenced_models'] = [
            {'id': mid, 'name': model_names.get(mid, 'Unknown'), 'count': count}
            for mid, count in sorted_refs
        ]
        
        return stats


def main():
    """Main function to populate the database."""
    print("=" * 60)
    print("Mental Models System - Complete Database Population")
    print("Source: Charlie Munger's Latticework of Mental Models")
    print("=" * 60)
    
    populator = MentalModelsPopulator()
    
    # Load data
    if not populator.load_data():
        print("Failed to load data, exiting")
        return
    
    # Generate and print summary stats
    print("\n" + "=" * 60)
    print("Summary Statistics")
    print("=" * 60)
    stats = populator.generate_summary_stats()
    print(f"Total Thinkers: {stats['total_thinkers']}")
    print(f"Total Categories: {stats['total_categories']}")
    print(f"Total Mental Models: {stats['total_mental_models']}")
    print(f"Total Case Studies: {stats['total_case_studies']}")
    print(f"Total Lollapalooza Patterns: {stats['total_lollapalooza_patterns']}")
    print(f"\nCase Study Outcomes: {stats['case_study_outcomes']}")
    print(f"Average Lollapalooza Score: {stats['avg_lollapalooza_score']:.2f}")
    
    print("\nTop 10 Most Referenced Mental Models:")
    for i, model in enumerate(stats['top_referenced_models'], 1):
        print(f"  {i}. {model['name']} (referenced {model['count']} times)")
    
    # Export to CSV
    print("\n" + "=" * 60)
    print("Exporting to CSV")
    print("=" * 60)
    populator.export_to_csv()
    
    # Try PostgreSQL if available
    db_url = os.environ.get('DATABASE_URL')
    if db_url and HAS_POSTGRES:
        print("\n" + "=" * 60)
        print("Populating PostgreSQL")
        print("=" * 60)
        populator.populate_postgres(db_url)
    
    print("\n" + "=" * 60)
    print("Population Complete")
    print("=" * 60)


if __name__ == "__main__":
    main()
