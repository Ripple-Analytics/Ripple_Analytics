# Mental Models Desktop - Improvements Log

## Overview
This log tracks all improvements and enhancements made to the Mental Models Desktop application.

---

## 2026-01-20: v2.1.0 - Major Update

### Bug Fixes
| Issue | Location | Description | Status |
|-------|----------|-------------|--------|
| Variable Shadowing | `swing_app.clj:2267` | `count` variable shadowed `clojure.core/count`, causing `Integer cannot be cast to IFn` | ✅ Fixed |
| Static Field Reference | `swing_app.clj:166` | Incorrect parens around `ISO_LOCAL_DATE_TIME` | ✅ Fixed |
| Regex Escape | `swing_app.clj:3135` | Invalid `\s` escape in string | ✅ Fixed |

### New Features
| Feature | Description | Files Added |
|---------|-------------|-------------|
| Hot Code Loading | Updates without restart using Clojure namespace reloading | `hotload/core.clj` |
| Delta Updates | Only downloads changed files (manifest-based) | `hotload/delta.clj` |
| State Preservation | Full state capture/restore across updates | `hotload/state.clj` |
| UI Hot-Swap | Refresh UI components without full restart | `hotload/ui_swap.clj` |
| Hotload Integration | Full integration with existing update system | `hotload/integration.clj` |

### New Mental Models Added
| ID | Name | Category | Source |
|----|------|----------|--------|
| 130 | Beauty and Elegance as Signal | Decision Making | Jim Simons |
| 131 | Data Over Intuition | Decision Making | Jim Simons |
| 132 | Hire Smart People and Let Them Work | Organizational | Jim Simons |
| 133 | Secrecy and Information Asymmetry | Strategy | Jim Simons |
| 134 | Continuous Improvement Through Iteration | Systems | Jim Simons |
| 135 | Pattern Recognition Across Domains | Learning | Jim Simons |
| 136 | Embrace Uncertainty with Probability | Decision Making | Jim Simons |
| 137 | Systematic Over Discretionary | Systems | Jim Simons |
| 138 | Compounding Small Edges | Strategy | Jim Simons |
| 139 | Scientific Method in Business | Learning | Jim Simons |
| 140 | Float Management | Finance | Charlie Munger |
| 141 | Customer Fanaticism | Business Strategy | Charlie Munger |
| 142 | Honest Assessment | Decision Making | Charlie Munger |
| 143 | Litigation Hazard | Risk Management | Charlie Munger |
| 144 | Multi-Year Performance View | Measurement | Charlie Munger |
| 145 | Circle of Competence | Decision Making | Charlie Munger |
| 146 | Margin of Safety | Risk Management | Charlie Munger |
| 147 | Inversion | Problem Solving | Charlie Munger |
| 148 | Lollapalooza Effect | Systems Thinking | Charlie Munger |
| 149 | Worldly Wisdom | Learning | Charlie Munger |

### Testing Infrastructure
| Component | Description |
|-----------|-------------|
| Pre-commit Hook | Runs clj-kondo on every commit |
| Validation Script | `scripts/validate-release.sh` for pre-release checks |
| Manifest Generator | `scripts/generate-manifest.clj` for delta updates |

### Model Count
- **Previous**: 129 models
- **Current**: 164 models
- **Added**: 35 models total
  - 10 Jim Simons models (Renaissance Technologies)
  - 10 Charlie Munger deep principles
  - 5 Nassim Taleb models (Antifragility/Risk)
  - 5 Daniel Kahneman models (Behavioral Economics)
  - 5 Additional core models (Ergodicity, Second-Order Thinking, etc.)

---

## Upcoming Improvements (To-Do)
- [ ] Steve Jobs-level attention to detail (Design Excellence)
- [ ] Information density (Value Line philosophy)
- [ ] M&S + Costco design language
- [ ] Micro-interactions and animations
- [ ] Consistent design language
- [ ] Dark mode with proper contrast

---

## Quality Metrics
| Metric | Value |
|--------|-------|
| clj-kondo Errors | 0 |
| clj-kondo Warnings | ~15 (non-critical) |
| Test Coverage | TBD |
| Code Quality | Validated |
