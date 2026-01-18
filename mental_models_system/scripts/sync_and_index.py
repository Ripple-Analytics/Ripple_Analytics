#!/usr/bin/env python3
"""
Automated Google Drive Sync and Knowledge Mining Script

This script:
1. Syncs documents from Google Drive
2. Indexes new/changed documents
3. Generates improvement ideas
4. Outputs a daily digest

Run daily via cron or scheduled task.
"""

import os
import sys
import subprocess
import json
from datetime import datetime
from pathlib import Path

# Add parent to path
sys.path.insert(0, str(Path(__file__).parent.parent))


def sync_google_drive(remote_paths: list, local_cache: str) -> int:
    """Sync documents from Google Drive."""
    total_files = 0
    
    for remote_path in remote_paths:
        remote = f"manus_google_drive:{remote_path}"
        local = os.path.join(local_cache, remote_path.replace("/", "_") or "root")
        os.makedirs(local, exist_ok=True)
        
        cmd = [
            "rclone", "sync", remote, local,
            "--config", "/home/ubuntu/.gdrive-rclone.ini",
            "--include", "*.pdf",
            "--include", "*.txt", 
            "--include", "*.docx",
            "--include", "*.md",
            "-v"
        ]
        
        try:
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=300)
            # Count files
            for root, dirs, files in os.walk(local):
                total_files += len([f for f in files if f.endswith(('.pdf', '.txt', '.docx', '.md'))])
        except Exception as e:
            print(f"Error syncing {remote_path}: {e}")
    
    return total_files


def main():
    print("=" * 70)
    print(f"AUTOMATED KNOWLEDGE MINING - {datetime.now().strftime('%Y-%m-%d %H:%M')}")
    print("=" * 70)
    
    # Configuration
    gdrive_paths = [
        "",  # Root
        "Psychology",
        "透過 Chrome 儲存",
        "Academic Economics",
    ]
    local_cache = "/home/ubuntu/Ripple_Analytics/mental_models_system/data/gdrive_cache"
    
    # Step 1: Sync from Google Drive
    print("\n[1/3] Syncing from Google Drive...")
    num_files = sync_google_drive(gdrive_paths, local_cache)
    print(f"  Found {num_files} documents")
    
    # Step 2: Index documents
    print("\n[2/3] Indexing documents...")
    try:
        from src.research.knowledge_miner import create_knowledge_miner
        miner = create_knowledge_miner()
        indexed = miner.index_directory(local_cache)
        print(f"  Indexed {len(indexed)} documents")
    except Exception as e:
        print(f"  Indexing skipped: {e}")
    
    # Step 3: Generate digest
    print("\n[3/3] Generating improvement digest...")
    
    digest = {
        "timestamp": datetime.now().isoformat(),
        "documents_synced": num_files,
        "focus_areas": [
            "mental model failure modes",
            "decision-making biases", 
            "investment case studies",
            "cognitive safeguards"
        ],
        "status": "ready_for_idea_generation"
    }
    
    # Save digest
    digest_path = "/home/ubuntu/Ripple_Analytics/mental_models_system/data/daily_digest.json"
    os.makedirs(os.path.dirname(digest_path), exist_ok=True)
    with open(digest_path, 'w') as f:
        json.dump(digest, f, indent=2)
    
    print(f"\n  Digest saved to: {digest_path}")
    print("\n" + "=" * 70)
    print("SYNC COMPLETE")
    print("=" * 70)


if __name__ == "__main__":
    main()
