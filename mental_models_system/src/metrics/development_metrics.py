import subprocess
import re
import json
from dataclasses import dataclass, field
from typing import Dict, Any, List, Optional, Tuple
from datetime import datetime, timedelta
from pathlib import Path
from collections import defaultdict
from enum import Enum
import logging

logger = logging.getLogger(__name__)


class VelocityPeriod(Enum):
    DAILY = "daily"
    WEEKLY = "weekly"
    MONTHLY = "monthly"
    QUARTERLY = "quarterly"
    YEARLY = "yearly"


@dataclass
class CommitMetrics:
    hash: str
    author: str
    email: str
    date: datetime
    message: str
    files_changed: int
    insertions: int
    deletions: int
    net_lines: int
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "hash": self.hash,
            "author": self.author,
            "email": self.email,
            "date": self.date.isoformat(),
            "message": self.message,
            "files_changed": self.files_changed,
            "insertions": self.insertions,
            "deletions": self.deletions,
            "net_lines": self.net_lines
        }


@dataclass
class AuthorMetrics:
    name: str
    email: str
    commits: int
    insertions: int
    deletions: int
    files_touched: int
    first_commit: Optional[datetime] = None
    last_commit: Optional[datetime] = None
    avg_commit_size: float = 0
    commit_frequency: float = 0
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "email": self.email,
            "commits": self.commits,
            "insertions": self.insertions,
            "deletions": self.deletions,
            "files_touched": self.files_touched,
            "first_commit": self.first_commit.isoformat() if self.first_commit else None,
            "last_commit": self.last_commit.isoformat() if self.last_commit else None,
            "avg_commit_size": self.avg_commit_size,
            "commit_frequency": self.commit_frequency
        }


@dataclass
class FileChurnMetrics:
    file_path: str
    commits: int
    authors: int
    insertions: int
    deletions: int
    churn_rate: float
    age_days: int
    last_modified: Optional[datetime] = None
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "file_path": self.file_path,
            "commits": self.commits,
            "authors": self.authors,
            "insertions": self.insertions,
            "deletions": self.deletions,
            "churn_rate": self.churn_rate,
            "age_days": self.age_days,
            "last_modified": self.last_modified.isoformat() if self.last_modified else None
        }


class GitMetricsCollector:
    def __init__(self, repo_path: str):
        self.repo_path = Path(repo_path)
        self.commits: List[CommitMetrics] = []
        self.authors: Dict[str, AuthorMetrics] = {}
        self.file_churn: Dict[str, FileChurnMetrics] = {}
        
    def _run_git(self, args: List[str]) -> str:
        try:
            result = subprocess.run(
                ["git"] + args,
                cwd=self.repo_path,
                capture_output=True,
                text=True,
                timeout=60
            )
            return result.stdout
        except Exception as e:
            logger.warning(f"Git command failed: {e}")
            return ""
    
    def collect_all(self) -> Dict[str, Any]:
        self._collect_commits()
        self._collect_author_metrics()
        self._collect_file_churn()
        
        return self.get_full_report()
    
    def _collect_commits(self, limit: int = 1000):
        log_format = "%H|%an|%ae|%aI|%s"
        output = self._run_git([
            "log",
            f"--format={log_format}",
            f"-n{limit}",
            "--shortstat"
        ])
        
        lines = output.strip().split("\n")
        i = 0
        
        while i < len(lines):
            line = lines[i].strip()
            if not line or "|" not in line:
                i += 1
                continue
            
            parts = line.split("|")
            if len(parts) < 5:
                i += 1
                continue
            
            hash_val = parts[0]
            author = parts[1]
            email = parts[2]
            date_str = parts[3]
            message = parts[4]
            
            try:
                date = datetime.fromisoformat(date_str.replace("Z", "+00:00"))
            except Exception:
                date = datetime.now()
            
            files_changed = 0
            insertions = 0
            deletions = 0
            
            i += 1
            while i < len(lines) and lines[i].strip() and "|" not in lines[i]:
                stat_line = lines[i].strip()
                
                files_match = re.search(r"(\d+)\s+files?\s+changed", stat_line)
                if files_match:
                    files_changed = int(files_match.group(1))
                
                ins_match = re.search(r"(\d+)\s+insertions?", stat_line)
                if ins_match:
                    insertions = int(ins_match.group(1))
                
                del_match = re.search(r"(\d+)\s+deletions?", stat_line)
                if del_match:
                    deletions = int(del_match.group(1))
                
                i += 1
            
            commit = CommitMetrics(
                hash=hash_val,
                author=author,
                email=email,
                date=date,
                message=message,
                files_changed=files_changed,
                insertions=insertions,
                deletions=deletions,
                net_lines=insertions - deletions
            )
            self.commits.append(commit)
    
    def _collect_author_metrics(self):
        author_data: Dict[str, Dict] = defaultdict(lambda: {
            "commits": 0,
            "insertions": 0,
            "deletions": 0,
            "files": set(),
            "dates": []
        })
        
        for commit in self.commits:
            key = commit.email
            author_data[key]["name"] = commit.author
            author_data[key]["email"] = commit.email
            author_data[key]["commits"] += 1
            author_data[key]["insertions"] += commit.insertions
            author_data[key]["deletions"] += commit.deletions
            author_data[key]["dates"].append(commit.date)
        
        for email, data in author_data.items():
            dates = sorted(data["dates"])
            first_commit = dates[0] if dates else None
            last_commit = dates[-1] if dates else None
            
            days_active = 1
            if first_commit and last_commit:
                days_active = max(1, (last_commit - first_commit).days)
            
            self.authors[email] = AuthorMetrics(
                name=data["name"],
                email=email,
                commits=data["commits"],
                insertions=data["insertions"],
                deletions=data["deletions"],
                files_touched=len(data["files"]),
                first_commit=first_commit,
                last_commit=last_commit,
                avg_commit_size=(data["insertions"] + data["deletions"]) / max(data["commits"], 1),
                commit_frequency=data["commits"] / days_active
            )
    
    def _collect_file_churn(self):
        output = self._run_git([
            "log",
            "--format=%H",
            "--name-only",
            "-n500"
        ])
        
        file_commits: Dict[str, List[str]] = defaultdict(list)
        file_authors: Dict[str, set] = defaultdict(set)
        
        lines = output.strip().split("\n")
        current_hash = None
        current_author = None
        
        for line in lines:
            line = line.strip()
            if not line:
                continue
            
            if len(line) == 40 and all(c in "0123456789abcdef" for c in line):
                current_hash = line
                commit = next((c for c in self.commits if c.hash == current_hash), None)
                current_author = commit.email if commit else None
            elif current_hash:
                file_commits[line].append(current_hash)
                if current_author:
                    file_authors[line].add(current_author)
        
        for file_path, commits in file_commits.items():
            total_insertions = 0
            total_deletions = 0
            
            for commit_hash in commits[:10]:
                stat_output = self._run_git([
                    "show",
                    "--stat",
                    "--format=",
                    commit_hash,
                    "--",
                    file_path
                ])
                
                ins_match = re.search(r"(\d+)\s+insertions?", stat_output)
                del_match = re.search(r"(\d+)\s+deletions?", stat_output)
                
                if ins_match:
                    total_insertions += int(ins_match.group(1))
                if del_match:
                    total_deletions += int(del_match.group(1))
            
            first_commit = self._run_git([
                "log",
                "--format=%aI",
                "--follow",
                "--diff-filter=A",
                "--",
                file_path
            ]).strip().split("\n")[0]
            
            try:
                first_date = datetime.fromisoformat(first_commit.replace("Z", "+00:00"))
                age_days = (datetime.now(first_date.tzinfo) - first_date).days
            except Exception:
                age_days = 0
            
            last_modified_str = self._run_git([
                "log",
                "-1",
                "--format=%aI",
                "--",
                file_path
            ]).strip()
            
            try:
                last_modified = datetime.fromisoformat(last_modified_str.replace("Z", "+00:00"))
            except Exception:
                last_modified = None
            
            churn_rate = (total_insertions + total_deletions) / max(len(commits), 1)
            
            self.file_churn[file_path] = FileChurnMetrics(
                file_path=file_path,
                commits=len(commits),
                authors=len(file_authors[file_path]),
                insertions=total_insertions,
                deletions=total_deletions,
                churn_rate=churn_rate,
                age_days=age_days,
                last_modified=last_modified
            )
    
    def get_velocity_metrics(self, period: VelocityPeriod = VelocityPeriod.WEEKLY) -> Dict[str, Any]:
        if not self.commits:
            return {}
        
        period_days = {
            VelocityPeriod.DAILY: 1,
            VelocityPeriod.WEEKLY: 7,
            VelocityPeriod.MONTHLY: 30,
            VelocityPeriod.QUARTERLY: 90,
            VelocityPeriod.YEARLY: 365
        }
        
        days = period_days[period]
        cutoff = datetime.now() - timedelta(days=days)
        
        recent_commits = [c for c in self.commits if c.date.replace(tzinfo=None) > cutoff]
        
        return {
            "period": period.value,
            "days": days,
            "commits": len(recent_commits),
            "commits_per_day": len(recent_commits) / days,
            "insertions": sum(c.insertions for c in recent_commits),
            "deletions": sum(c.deletions for c in recent_commits),
            "net_lines": sum(c.net_lines for c in recent_commits),
            "files_changed": sum(c.files_changed for c in recent_commits),
            "unique_authors": len(set(c.email for c in recent_commits)),
            "avg_commit_size": sum(c.insertions + c.deletions for c in recent_commits) / max(len(recent_commits), 1)
        }
    
    def get_full_report(self) -> Dict[str, Any]:
        velocity_daily = self.get_velocity_metrics(VelocityPeriod.DAILY)
        velocity_weekly = self.get_velocity_metrics(VelocityPeriod.WEEKLY)
        velocity_monthly = self.get_velocity_metrics(VelocityPeriod.MONTHLY)
        
        hotspots = sorted(
            self.file_churn.values(),
            key=lambda x: x.churn_rate,
            reverse=True
        )[:20]
        
        top_contributors = sorted(
            self.authors.values(),
            key=lambda x: x.commits,
            reverse=True
        )[:10]
        
        return {
            "summary": {
                "total_commits": len(self.commits),
                "total_authors": len(self.authors),
                "total_files_tracked": len(self.file_churn),
                "total_insertions": sum(c.insertions for c in self.commits),
                "total_deletions": sum(c.deletions for c in self.commits),
                "net_lines_added": sum(c.net_lines for c in self.commits),
            },
            "velocity": {
                "daily": velocity_daily,
                "weekly": velocity_weekly,
                "monthly": velocity_monthly,
            },
            "hotspots": [h.to_dict() for h in hotspots],
            "top_contributors": [c.to_dict() for c in top_contributors],
            "all_authors": {k: v.to_dict() for k, v in self.authors.items()},
            "recent_commits": [c.to_dict() for c in self.commits[:50]],
            "timestamp": datetime.now().isoformat()
        }


class DevelopmentVelocityTracker:
    def __init__(self):
        self.metrics_history: List[Dict[str, Any]] = []
        self.baselines: Dict[str, float] = {}
        
    def record_snapshot(self, metrics: Dict[str, Any]):
        snapshot = {
            "timestamp": datetime.now().isoformat(),
            "metrics": metrics
        }
        self.metrics_history.append(snapshot)
        
        if len(self.metrics_history) == 1:
            self._set_baselines(metrics)
    
    def _set_baselines(self, metrics: Dict[str, Any]):
        summary = metrics.get("summary", {})
        for key, value in summary.items():
            if isinstance(value, (int, float)):
                self.baselines[key] = value
    
    def get_trends(self) -> Dict[str, Any]:
        if len(self.metrics_history) < 2:
            return {"status": "insufficient_data"}
        
        first = self.metrics_history[0]["metrics"].get("summary", {})
        last = self.metrics_history[-1]["metrics"].get("summary", {})
        
        trends = {}
        for key in first:
            if isinstance(first.get(key), (int, float)) and isinstance(last.get(key), (int, float)):
                change = last[key] - first[key]
                pct_change = (change / first[key] * 100) if first[key] != 0 else 0
                trends[key] = {
                    "start": first[key],
                    "end": last[key],
                    "change": change,
                    "percent_change": pct_change
                }
        
        return trends
    
    def calculate_iteration_speed(self, git_metrics: Dict[str, Any]) -> Dict[str, Any]:
        velocity = git_metrics.get("velocity", {})
        weekly = velocity.get("weekly", {})
        
        commits_per_day = weekly.get("commits_per_day", 0)
        avg_commit_size = weekly.get("avg_commit_size", 0)
        
        iteration_speed = commits_per_day * avg_commit_size
        
        return {
            "iteration_speed_score": iteration_speed,
            "commits_per_day": commits_per_day,
            "avg_commit_size": avg_commit_size,
            "interpretation": self._interpret_speed(iteration_speed)
        }
    
    def _interpret_speed(self, speed: float) -> str:
        if speed > 1000:
            return "Exceptional velocity - rapid iteration with substantial changes"
        elif speed > 500:
            return "High velocity - good balance of speed and substance"
        elif speed > 100:
            return "Moderate velocity - steady progress"
        elif speed > 10:
            return "Low velocity - consider increasing iteration frequency"
        else:
            return "Minimal velocity - development may be stalled"


class PRMetricsCollector:
    def __init__(self, repo_path: str):
        self.repo_path = Path(repo_path)
        
    def get_branch_metrics(self) -> Dict[str, Any]:
        branches_output = subprocess.run(
            ["git", "branch", "-a", "--format=%(refname:short)|%(committerdate:iso)|%(authorname)"],
            cwd=self.repo_path,
            capture_output=True,
            text=True
        ).stdout
        
        branches = []
        for line in branches_output.strip().split("\n"):
            if not line or "|" not in line:
                continue
            parts = line.split("|")
            if len(parts) >= 3:
                branches.append({
                    "name": parts[0],
                    "last_commit": parts[1],
                    "author": parts[2]
                })
        
        return {
            "total_branches": len(branches),
            "branches": branches[:50],
            "active_branches": len([b for b in branches if "remotes/origin" not in b["name"]])
        }
    
    def get_merge_metrics(self) -> Dict[str, Any]:
        merge_output = subprocess.run(
            ["git", "log", "--merges", "--format=%H|%aI|%s", "-n100"],
            cwd=self.repo_path,
            capture_output=True,
            text=True
        ).stdout
        
        merges = []
        for line in merge_output.strip().split("\n"):
            if not line or "|" not in line:
                continue
            parts = line.split("|")
            if len(parts) >= 3:
                merges.append({
                    "hash": parts[0],
                    "date": parts[1],
                    "message": parts[2]
                })
        
        return {
            "total_merges": len(merges),
            "recent_merges": merges[:20]
        }


def collect_development_metrics(repo_path: str) -> Dict[str, Any]:
    git_collector = GitMetricsCollector(repo_path)
    git_metrics = git_collector.collect_all()
    
    velocity_tracker = DevelopmentVelocityTracker()
    velocity_tracker.record_snapshot(git_metrics)
    iteration_speed = velocity_tracker.calculate_iteration_speed(git_metrics)
    
    pr_collector = PRMetricsCollector(repo_path)
    branch_metrics = pr_collector.get_branch_metrics()
    merge_metrics = pr_collector.get_merge_metrics()
    
    return {
        "git_metrics": git_metrics,
        "iteration_speed": iteration_speed,
        "branch_metrics": branch_metrics,
        "merge_metrics": merge_metrics,
        "timestamp": datetime.now().isoformat()
    }
