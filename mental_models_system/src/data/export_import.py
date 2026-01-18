"""
Data Export/Import System for Mental Models System.

Provides full data portability with multiple formats:
- JSON (human-readable, version controlled)
- SQLite (portable database)
- CSV (spreadsheet compatible)
- Parquet (analytics, big data)

Supports:
- Full system backup/restore
- Selective export (models, cases, decisions, etc.)
- Incremental sync
- Cross-system migration
"""

import json
import csv
import sqlite3
import hashlib
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Any, Optional, Set
from dataclasses import dataclass, asdict
from enum import Enum
import gzip
import shutil


class ExportFormat(Enum):
    """Supported export formats."""
    JSON = "json"
    JSON_GZ = "json.gz"
    SQLITE = "sqlite"
    CSV = "csv"
    PARQUET = "parquet"


@dataclass
class ExportManifest:
    """Manifest for exported data."""
    version: str
    created_at: str
    format: str
    components: List[str]
    record_counts: Dict[str, int]
    checksum: str
    source_system: str
    
    def to_dict(self) -> Dict[str, Any]:
        return asdict(self)


class DataExporter:
    """
    Export system data to various formats.
    
    Supports full backup, selective export, and incremental sync.
    """
    
    VERSION = "1.0.0"
    
    def __init__(self, data_dir: str = "./data"):
        self.data_dir = Path(data_dir)
        self.export_dir = self.data_dir / "exports"
        self.export_dir.mkdir(parents=True, exist_ok=True)
    
    def export_full(
        self,
        output_path: Optional[str] = None,
        format: ExportFormat = ExportFormat.JSON_GZ,
        include_decisions: bool = True,
        include_analysis_history: bool = True
    ) -> str:
        """
        Export complete system data.
        
        Args:
            output_path: Custom output path (auto-generated if None)
            format: Export format
            include_decisions: Include decision journal
            include_analysis_history: Include analysis history
            
        Returns:
            Path to exported file
        """
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        if output_path is None:
            output_path = str(self.export_dir / f"mental_models_export_{timestamp}.{format.value}")
        
        # Collect all data
        data = {
            "mental_models": self._load_mental_models(),
            "failure_modes": self._load_failure_modes(),
            "case_studies": self._load_case_studies(),
            "thinkers": self._load_thinkers(),
        }
        
        if include_decisions:
            data["decisions"] = self._load_decisions()
        
        if include_analysis_history:
            data["analysis_history"] = self._load_analysis_history()
        
        # Calculate checksum
        checksum = self._calculate_checksum(data)
        
        # Create manifest
        manifest = ExportManifest(
            version=self.VERSION,
            created_at=datetime.now().isoformat(),
            format=format.value,
            components=list(data.keys()),
            record_counts={k: len(v) if isinstance(v, list) else 1 for k, v in data.items()},
            checksum=checksum,
            source_system="mental_models_system"
        )
        
        export_data = {
            "manifest": manifest.to_dict(),
            "data": data
        }
        
        # Export based on format
        if format == ExportFormat.JSON:
            self._export_json(export_data, output_path)
        elif format == ExportFormat.JSON_GZ:
            self._export_json_gz(export_data, output_path)
        elif format == ExportFormat.SQLITE:
            self._export_sqlite(export_data, output_path)
        elif format == ExportFormat.CSV:
            self._export_csv(export_data, output_path)
        elif format == ExportFormat.PARQUET:
            self._export_parquet(export_data, output_path)
        
        return output_path
    
    def export_selective(
        self,
        components: List[str],
        output_path: Optional[str] = None,
        format: ExportFormat = ExportFormat.JSON
    ) -> str:
        """
        Export selected components only.
        
        Args:
            components: List of components to export
            output_path: Custom output path
            format: Export format
            
        Returns:
            Path to exported file
        """
        valid_components = {
            "mental_models", "failure_modes", "case_studies",
            "thinkers", "decisions", "analysis_history"
        }
        
        invalid = set(components) - valid_components
        if invalid:
            raise ValueError(f"Invalid components: {invalid}")
        
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        if output_path is None:
            output_path = str(self.export_dir / f"selective_export_{timestamp}.{format.value}")
        
        # Load requested components
        loaders = {
            "mental_models": self._load_mental_models,
            "failure_modes": self._load_failure_modes,
            "case_studies": self._load_case_studies,
            "thinkers": self._load_thinkers,
            "decisions": self._load_decisions,
            "analysis_history": self._load_analysis_history,
        }
        
        data = {comp: loaders[comp]() for comp in components}
        
        checksum = self._calculate_checksum(data)
        
        manifest = ExportManifest(
            version=self.VERSION,
            created_at=datetime.now().isoformat(),
            format=format.value,
            components=components,
            record_counts={k: len(v) if isinstance(v, list) else 1 for k, v in data.items()},
            checksum=checksum,
            source_system="mental_models_system"
        )
        
        export_data = {
            "manifest": manifest.to_dict(),
            "data": data
        }
        
        if format == ExportFormat.JSON:
            self._export_json(export_data, output_path)
        elif format == ExportFormat.JSON_GZ:
            self._export_json_gz(export_data, output_path)
        
        return output_path
    
    def export_incremental(
        self,
        since: datetime,
        output_path: Optional[str] = None
    ) -> str:
        """
        Export only data modified since a given timestamp.
        
        Args:
            since: Export data modified after this time
            output_path: Custom output path
            
        Returns:
            Path to exported file
        """
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        if output_path is None:
            output_path = str(self.export_dir / f"incremental_export_{timestamp}.json.gz")
        
        # Load and filter data by modification time
        data = {
            "decisions": self._load_decisions_since(since),
            "analysis_history": self._load_analysis_since(since),
        }
        
        checksum = self._calculate_checksum(data)
        
        manifest = ExportManifest(
            version=self.VERSION,
            created_at=datetime.now().isoformat(),
            format="json.gz",
            components=list(data.keys()),
            record_counts={k: len(v) for k, v in data.items()},
            checksum=checksum,
            source_system="mental_models_system"
        )
        
        export_data = {
            "manifest": manifest.to_dict(),
            "incremental_since": since.isoformat(),
            "data": data
        }
        
        self._export_json_gz(export_data, output_path)
        
        return output_path
    
    def _load_mental_models(self) -> List[Dict]:
        """Load mental models from JSON files."""
        models = []
        json_path = self.data_dir / "raw" / "mental_models_complete.json"
        if json_path.exists():
            with open(json_path) as f:
                data = json.load(f)
                if "mental_models" in data:
                    models = data["mental_models"]
                elif isinstance(data, list):
                    models = data
        return models
    
    def _load_failure_modes(self) -> List[Dict]:
        """Load failure modes from JSON files."""
        modes = []
        raw_dir = self.data_dir / "raw"
        for json_file in raw_dir.glob("failure_modes*.json"):
            try:
                with open(json_file) as f:
                    data = json.load(f)
                    if isinstance(data, dict) and "failure_modes" in data:
                        modes.extend(data["failure_modes"])
                    elif isinstance(data, list):
                        modes.extend(data)
            except Exception:
                continue
        return modes
    
    def _load_case_studies(self) -> List[Dict]:
        """Load case studies from JSON files."""
        cases = []
        json_path = self.data_dir / "raw" / "case_studies_complete.json"
        if json_path.exists():
            with open(json_path) as f:
                data = json.load(f)
                if "case_studies" in data:
                    cases = data["case_studies"]
                elif isinstance(data, list):
                    cases = data
        return cases
    
    def _load_thinkers(self) -> List[Dict]:
        """Load thinkers from JSON files."""
        thinkers = []
        json_path = self.data_dir / "raw" / "mental_models_complete.json"
        if json_path.exists():
            with open(json_path) as f:
                data = json.load(f)
                if "thinkers" in data:
                    thinkers = data["thinkers"]
        return thinkers
    
    def _load_decisions(self) -> List[Dict]:
        """Load decision journal entries."""
        decisions = []
        journal_path = self.data_dir / "journal" / "decisions.json"
        if journal_path.exists():
            with open(journal_path) as f:
                decisions = json.load(f)
        return decisions
    
    def _load_analysis_history(self) -> List[Dict]:
        """Load analysis history."""
        history = []
        history_path = self.data_dir / "analysis" / "history.json"
        if history_path.exists():
            with open(history_path) as f:
                history = json.load(f)
        return history
    
    def _load_decisions_since(self, since: datetime) -> List[Dict]:
        """Load decisions modified since timestamp."""
        decisions = self._load_decisions()
        return [d for d in decisions if datetime.fromisoformat(d.get("created_at", "1970-01-01")) > since]
    
    def _load_analysis_since(self, since: datetime) -> List[Dict]:
        """Load analysis history since timestamp."""
        history = self._load_analysis_history()
        return [h for h in history if datetime.fromisoformat(h.get("timestamp", "1970-01-01")) > since]
    
    def _calculate_checksum(self, data: Dict) -> str:
        """Calculate SHA256 checksum of data."""
        json_str = json.dumps(data, sort_keys=True)
        return hashlib.sha256(json_str.encode()).hexdigest()
    
    def _export_json(self, data: Dict, path: str):
        """Export to JSON format."""
        with open(path, 'w') as f:
            json.dump(data, f, indent=2, default=str)
    
    def _export_json_gz(self, data: Dict, path: str):
        """Export to compressed JSON format."""
        json_bytes = json.dumps(data, indent=2, default=str).encode('utf-8')
        with gzip.open(path, 'wb') as f:
            f.write(json_bytes)
    
    def _export_sqlite(self, data: Dict, path: str):
        """Export to SQLite database."""
        conn = sqlite3.connect(path)
        cursor = conn.cursor()
        
        # Create tables
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS manifest (
                version TEXT,
                created_at TEXT,
                format TEXT,
                checksum TEXT,
                source_system TEXT
            )
        """)
        
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS mental_models (
                id INTEGER PRIMARY KEY,
                name TEXT,
                category TEXT,
                description TEXT,
                data JSON
            )
        """)
        
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS failure_modes (
                id INTEGER PRIMARY KEY,
                model_name TEXT,
                failure_mode TEXT,
                description TEXT,
                data JSON
            )
        """)
        
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS case_studies (
                id INTEGER PRIMARY KEY,
                title TEXT,
                models TEXT,
                data JSON
            )
        """)
        
        # Insert manifest
        manifest = data["manifest"]
        cursor.execute(
            "INSERT INTO manifest VALUES (?, ?, ?, ?, ?)",
            (manifest["version"], manifest["created_at"], manifest["format"],
             manifest["checksum"], manifest["source_system"])
        )
        
        # Insert mental models
        for i, model in enumerate(data["data"].get("mental_models", [])):
            cursor.execute(
                "INSERT INTO mental_models VALUES (?, ?, ?, ?, ?)",
                (i, model.get("name", ""), model.get("category", ""),
                 model.get("description", ""), json.dumps(model))
            )
        
        # Insert failure modes
        for i, mode in enumerate(data["data"].get("failure_modes", [])):
            cursor.execute(
                "INSERT INTO failure_modes VALUES (?, ?, ?, ?, ?)",
                (i, mode.get("model_name", ""), mode.get("failure_mode", ""),
                 mode.get("description", ""), json.dumps(mode))
            )
        
        # Insert case studies
        for i, case in enumerate(data["data"].get("case_studies", [])):
            models = ",".join(case.get("models", []))
            cursor.execute(
                "INSERT INTO case_studies VALUES (?, ?, ?, ?)",
                (i, case.get("title", ""), models, json.dumps(case))
            )
        
        conn.commit()
        conn.close()
    
    def _export_csv(self, data: Dict, path: str):
        """Export to CSV format (creates directory with multiple CSVs)."""
        export_dir = Path(path)
        export_dir.mkdir(parents=True, exist_ok=True)
        
        # Export each component to separate CSV
        for component, records in data["data"].items():
            if not records:
                continue
            
            csv_path = export_dir / f"{component}.csv"
            
            if isinstance(records, list) and len(records) > 0:
                # Get all unique keys
                keys = set()
                for record in records:
                    if isinstance(record, dict):
                        keys.update(record.keys())
                keys = sorted(keys)
                
                with open(csv_path, 'w', newline='') as f:
                    writer = csv.DictWriter(f, fieldnames=keys)
                    writer.writeheader()
                    for record in records:
                        if isinstance(record, dict):
                            # Flatten nested structures
                            flat_record = {}
                            for k, v in record.items():
                                if isinstance(v, (dict, list)):
                                    flat_record[k] = json.dumps(v)
                                else:
                                    flat_record[k] = v
                            writer.writerow(flat_record)
        
        # Write manifest
        with open(export_dir / "manifest.json", 'w') as f:
            json.dump(data["manifest"], f, indent=2)
    
    def _export_parquet(self, data: Dict, path: str):
        """Export to Parquet format (requires pyarrow)."""
        try:
            import pandas as pd
            
            export_dir = Path(path)
            export_dir.mkdir(parents=True, exist_ok=True)
            
            for component, records in data["data"].items():
                if not records or not isinstance(records, list):
                    continue
                
                df = pd.DataFrame(records)
                parquet_path = export_dir / f"{component}.parquet"
                df.to_parquet(parquet_path, index=False)
            
            # Write manifest as JSON
            with open(export_dir / "manifest.json", 'w') as f:
                json.dump(data["manifest"], f, indent=2)
                
        except ImportError:
            raise ImportError("Parquet export requires pandas and pyarrow: pip install pandas pyarrow")


class DataImporter:
    """
    Import system data from various formats.
    
    Supports full restore, merge, and validation.
    """
    
    def __init__(self, data_dir: str = "./data"):
        self.data_dir = Path(data_dir)
    
    def import_full(
        self,
        input_path: str,
        merge: bool = False,
        validate: bool = True
    ) -> Dict[str, int]:
        """
        Import complete system data.
        
        Args:
            input_path: Path to import file
            merge: If True, merge with existing data; if False, replace
            validate: Validate checksum before import
            
        Returns:
            Dictionary of imported record counts
        """
        path = Path(input_path)
        
        # Detect format
        if path.suffix == '.gz':
            data = self._load_json_gz(path)
        elif path.suffix == '.json':
            data = self._load_json(path)
        elif path.suffix == '.sqlite':
            data = self._load_sqlite(path)
        elif path.is_dir():
            # Could be CSV or Parquet export
            if (path / "manifest.json").exists():
                data = self._load_csv_dir(path)
            else:
                raise ValueError("Unknown directory format")
        else:
            raise ValueError(f"Unknown format: {path.suffix}")
        
        # Validate checksum
        if validate:
            expected_checksum = data["manifest"]["checksum"]
            actual_checksum = self._calculate_checksum(data["data"])
            if expected_checksum != actual_checksum:
                raise ValueError("Checksum mismatch - data may be corrupted")
        
        # Import data
        counts = {}
        
        if "mental_models" in data["data"]:
            counts["mental_models"] = self._import_mental_models(
                data["data"]["mental_models"], merge
            )
        
        if "failure_modes" in data["data"]:
            counts["failure_modes"] = self._import_failure_modes(
                data["data"]["failure_modes"], merge
            )
        
        if "case_studies" in data["data"]:
            counts["case_studies"] = self._import_case_studies(
                data["data"]["case_studies"], merge
            )
        
        if "decisions" in data["data"]:
            counts["decisions"] = self._import_decisions(
                data["data"]["decisions"], merge
            )
        
        return counts
    
    def validate_import(self, input_path: str) -> Dict[str, Any]:
        """
        Validate import file without importing.
        
        Returns:
            Validation results
        """
        path = Path(input_path)
        
        try:
            if path.suffix == '.gz':
                data = self._load_json_gz(path)
            elif path.suffix == '.json':
                data = self._load_json(path)
            else:
                return {"valid": False, "error": f"Unknown format: {path.suffix}"}
            
            manifest = data["manifest"]
            
            # Check version compatibility
            version_ok = manifest["version"].startswith("1.")
            
            # Verify checksum
            expected_checksum = manifest["checksum"]
            actual_checksum = self._calculate_checksum(data["data"])
            checksum_ok = expected_checksum == actual_checksum
            
            return {
                "valid": version_ok and checksum_ok,
                "version": manifest["version"],
                "version_compatible": version_ok,
                "checksum_valid": checksum_ok,
                "created_at": manifest["created_at"],
                "components": manifest["components"],
                "record_counts": manifest["record_counts"],
                "source_system": manifest["source_system"]
            }
            
        except Exception as e:
            return {"valid": False, "error": str(e)}
    
    def _load_json(self, path: Path) -> Dict:
        """Load JSON file."""
        with open(path) as f:
            return json.load(f)
    
    def _load_json_gz(self, path: Path) -> Dict:
        """Load compressed JSON file."""
        with gzip.open(path, 'rt', encoding='utf-8') as f:
            return json.load(f)
    
    def _load_sqlite(self, path: Path) -> Dict:
        """Load SQLite database."""
        conn = sqlite3.connect(path)
        cursor = conn.cursor()
        
        # Load manifest
        cursor.execute("SELECT * FROM manifest")
        row = cursor.fetchone()
        manifest = {
            "version": row[0],
            "created_at": row[1],
            "format": row[2],
            "checksum": row[3],
            "source_system": row[4],
            "components": [],
            "record_counts": {}
        }
        
        data = {}
        
        # Load mental models
        cursor.execute("SELECT data FROM mental_models")
        data["mental_models"] = [json.loads(row[0]) for row in cursor.fetchall()]
        
        # Load failure modes
        cursor.execute("SELECT data FROM failure_modes")
        data["failure_modes"] = [json.loads(row[0]) for row in cursor.fetchall()]
        
        # Load case studies
        cursor.execute("SELECT data FROM case_studies")
        data["case_studies"] = [json.loads(row[0]) for row in cursor.fetchall()]
        
        conn.close()
        
        manifest["components"] = list(data.keys())
        manifest["record_counts"] = {k: len(v) for k, v in data.items()}
        
        return {"manifest": manifest, "data": data}
    
    def _load_csv_dir(self, path: Path) -> Dict:
        """Load CSV directory export."""
        with open(path / "manifest.json") as f:
            manifest = json.load(f)
        
        data = {}
        
        for csv_file in path.glob("*.csv"):
            component = csv_file.stem
            records = []
            
            with open(csv_file, newline='') as f:
                reader = csv.DictReader(f)
                for row in reader:
                    # Try to parse JSON fields
                    parsed_row = {}
                    for k, v in row.items():
                        try:
                            parsed_row[k] = json.loads(v)
                        except (json.JSONDecodeError, TypeError):
                            parsed_row[k] = v
                    records.append(parsed_row)
            
            data[component] = records
        
        return {"manifest": manifest, "data": data}
    
    def _calculate_checksum(self, data: Dict) -> str:
        """Calculate SHA256 checksum of data."""
        json_str = json.dumps(data, sort_keys=True)
        return hashlib.sha256(json_str.encode()).hexdigest()
    
    def _import_mental_models(self, models: List[Dict], merge: bool) -> int:
        """Import mental models."""
        output_path = self.data_dir / "raw" / "mental_models_complete.json"
        output_path.parent.mkdir(parents=True, exist_ok=True)
        
        if merge and output_path.exists():
            with open(output_path) as f:
                existing = json.load(f)
            existing_models = existing.get("mental_models", [])
            existing_names = {m["name"] for m in existing_models}
            
            # Add new models
            for model in models:
                if model["name"] not in existing_names:
                    existing_models.append(model)
            
            existing["mental_models"] = existing_models
            with open(output_path, 'w') as f:
                json.dump(existing, f, indent=2)
            
            return len(existing_models)
        else:
            with open(output_path, 'w') as f:
                json.dump({"mental_models": models}, f, indent=2)
            return len(models)
    
    def _import_failure_modes(self, modes: List[Dict], merge: bool) -> int:
        """Import failure modes."""
        output_path = self.data_dir / "raw" / "failure_modes_imported.json"
        output_path.parent.mkdir(parents=True, exist_ok=True)
        
        with open(output_path, 'w') as f:
            json.dump({"failure_modes": modes}, f, indent=2)
        
        return len(modes)
    
    def _import_case_studies(self, cases: List[Dict], merge: bool) -> int:
        """Import case studies."""
        output_path = self.data_dir / "raw" / "case_studies_complete.json"
        output_path.parent.mkdir(parents=True, exist_ok=True)
        
        if merge and output_path.exists():
            with open(output_path) as f:
                existing = json.load(f)
            existing_cases = existing.get("case_studies", [])
            existing_titles = {c["title"] for c in existing_cases}
            
            for case in cases:
                if case["title"] not in existing_titles:
                    existing_cases.append(case)
            
            existing["case_studies"] = existing_cases
            with open(output_path, 'w') as f:
                json.dump(existing, f, indent=2)
            
            return len(existing_cases)
        else:
            with open(output_path, 'w') as f:
                json.dump({"case_studies": cases}, f, indent=2)
            return len(cases)
    
    def _import_decisions(self, decisions: List[Dict], merge: bool) -> int:
        """Import decision journal entries."""
        output_path = self.data_dir / "journal" / "decisions.json"
        output_path.parent.mkdir(parents=True, exist_ok=True)
        
        if merge and output_path.exists():
            with open(output_path) as f:
                existing = json.load(f)
            existing_ids = {d["id"] for d in existing}
            
            for decision in decisions:
                if decision["id"] not in existing_ids:
                    existing.append(decision)
            
            with open(output_path, 'w') as f:
                json.dump(existing, f, indent=2)
            
            return len(existing)
        else:
            with open(output_path, 'w') as f:
                json.dump(decisions, f, indent=2)
            return len(decisions)


# CLI interface
def main():
    import argparse
    
    parser = argparse.ArgumentParser(description="Mental Models System Data Export/Import")
    subparsers = parser.add_subparsers(dest="command", help="Command to run")
    
    # Export command
    export_parser = subparsers.add_parser("export", help="Export system data")
    export_parser.add_argument("--output", "-o", help="Output path")
    export_parser.add_argument("--format", "-f", choices=["json", "json.gz", "sqlite", "csv", "parquet"],
                               default="json.gz", help="Export format")
    export_parser.add_argument("--components", "-c", nargs="+", help="Components to export (selective)")
    export_parser.add_argument("--no-decisions", action="store_true", help="Exclude decision journal")
    export_parser.add_argument("--no-history", action="store_true", help="Exclude analysis history")
    
    # Import command
    import_parser = subparsers.add_parser("import", help="Import system data")
    import_parser.add_argument("input", help="Input file path")
    import_parser.add_argument("--merge", action="store_true", help="Merge with existing data")
    import_parser.add_argument("--no-validate", action="store_true", help="Skip checksum validation")
    
    # Validate command
    validate_parser = subparsers.add_parser("validate", help="Validate import file")
    validate_parser.add_argument("input", help="Input file path")
    
    args = parser.parse_args()
    
    if args.command == "export":
        exporter = DataExporter()
        format_map = {
            "json": ExportFormat.JSON,
            "json.gz": ExportFormat.JSON_GZ,
            "sqlite": ExportFormat.SQLITE,
            "csv": ExportFormat.CSV,
            "parquet": ExportFormat.PARQUET,
        }
        
        if args.components:
            path = exporter.export_selective(
                args.components,
                args.output,
                format_map[args.format]
            )
        else:
            path = exporter.export_full(
                args.output,
                format_map[args.format],
                include_decisions=not args.no_decisions,
                include_analysis_history=not args.no_history
            )
        
        print(f"Exported to: {path}")
    
    elif args.command == "import":
        importer = DataImporter()
        counts = importer.import_full(
            args.input,
            merge=args.merge,
            validate=not args.no_validate
        )
        
        print("Import complete:")
        for component, count in counts.items():
            print(f"  {component}: {count} records")
    
    elif args.command == "validate":
        importer = DataImporter()
        result = importer.validate_import(args.input)
        
        if result["valid"]:
            print("✓ File is valid")
            print(f"  Version: {result['version']}")
            print(f"  Created: {result['created_at']}")
            print(f"  Source: {result['source_system']}")
            print(f"  Components: {', '.join(result['components'])}")
            print("  Record counts:")
            for comp, count in result['record_counts'].items():
                print(f"    {comp}: {count}")
        else:
            print("✗ File is invalid")
            if "error" in result:
                print(f"  Error: {result['error']}")
    
    else:
        parser.print_help()


if __name__ == "__main__":
    main()
