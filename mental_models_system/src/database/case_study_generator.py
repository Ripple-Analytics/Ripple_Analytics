#!/usr/bin/env python3
"""
Case Study Generator
Generate rich, detailed case studies with Planck knowledge (not chauffeur knowledge).

Each case study includes:
- Deep analysis of how mental models apply
- Historical context and outcomes
- Quantitative metrics
- Lessons learned

Inspired by Charlie Munger's emphasis on understanding WHY models work, not just THAT they work.
"""

import os
import sys
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass
from datetime import datetime, date
import random
import json

sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))


@dataclass
class CaseStudy:
    """Rich case study with Planck knowledge."""
    name: str
    date: date
    category: str
    region: str
    description: str
    severity: float
    financial_impact: float
    models_involved: int
    lollapalooza_score: float
    mental_models_applied: List[Dict[str, str]]
    outcome: str
    lessons_learned: List[str]
    key_decisions: List[str]
    mistakes_avoided: List[str]


# Historical case studies with deep analysis
HISTORICAL_CASES = [
    {
        "name": "2008 Financial Crisis - Subprime Mortgage Collapse",
        "date": date(2008, 9, 15),
        "category": "Financial Crisis",
        "region": "Global",
        "description": """
The 2008 financial crisis was a textbook Lollapalooza effect where multiple mental models 
failed simultaneously. The crisis demonstrated how incentive-caused bias, social proof, 
and availability bias combined to create systemic risk.
        """,
        "severity": 0.95,
        "financial_impact": 10_000_000_000_000,  # $10 trillion
        "models_involved": 8,
        "lollapalooza_score": 0.92,
        "mental_models_applied": [
            {
                "model": "Incentive-Caused Bias",
                "application": "Mortgage brokers were incentivized to originate loans regardless of quality. This created a principal-agent problem where brokers' interests diverged from borrowers and investors.",
                "effect_size": 0.85
            },
            {
                "model": "Social Proof",
                "application": "As housing prices rose, more people bought homes because 'everyone else was doing it.' This herd behavior amplified the bubble.",
                "effect_size": 0.78
            },
            {
                "model": "Availability Bias",
                "application": "Recent history of rising home prices made people overweight this trend and ignore historical data on housing cycles.",
                "effect_size": 0.72
            },
            {
                "model": "Leverage",
                "application": "Financial institutions used extreme leverage (30:1 or higher), amplifying both gains and losses. This violated Munger's principle of avoiding excessive debt.",
                "effect_size": 0.90
            },
            {
                "model": "Reciprocation Tendency",
                "application": "Rating agencies gave favorable ratings to mortgage-backed securities because issuers paid their fees, creating conflicts of interest.",
                "effect_size": 0.68
            },
            {
                "model": "Doubt-Avoidance Tendency",
                "application": "Investors and regulators avoided questioning the system because acknowledging problems would require difficult actions.",
                "effect_size": 0.65
            },
            {
                "model": "Envy/Jealousy Tendency",
                "application": "Financial institutions competed to offer the most aggressive products, fearing they would lose market share to competitors.",
                "effect_size": 0.70
            },
            {
                "model": "Deprival-Superreaction Tendency",
                "application": "When housing prices began falling, homeowners and investors experienced extreme reactions, leading to panic selling.",
                "effect_size": 0.75
            }
        ],
        "outcome": "Global financial system collapse, $10 trillion in losses, Great Recession, millions of foreclosures",
        "lessons_learned": [
            "Lollapalooza effects occur when multiple psychological tendencies align",
            "Incentive structures must be carefully designed to align interests",
            "Leverage amplifies both success and failure - use conservatively",
            "Social proof can lead entire industries astray",
            "Regulatory capture occurs when regulators become too close to regulated entities"
        ],
        "key_decisions": [
            "Lehman Brothers allowed to fail (moral hazard consideration)",
            "TARP bailout to prevent complete system collapse",
            "Federal Reserve quantitative easing to restore liquidity",
            "Dodd-Frank regulations to prevent future crises"
        ],
        "mistakes_avoided": [
            "Could have recognized housing bubble earlier using historical price-to-income ratios",
            "Could have limited leverage ratios before crisis",
            "Could have required better disclosure of mortgage-backed security contents",
            "Could have separated rating agency compensation from issuers"
        ]
    },
    {
        "name": "Long-Term Capital Management Collapse (1998)",
        "date": date(1998, 9, 23),
        "category": "Hedge Fund Failure",
        "region": "Global",
        "description": """
LTCM was run by Nobel Prize winners and used sophisticated mathematical models, yet collapsed 
spectacularly. This case demonstrates the limits of mathematical models without understanding 
of human behavior and tail risks.
        """,
        "severity": 0.88,
        "financial_impact": 4_600_000_000,
        "models_involved": 6,
        "lollapalooza_score": 0.85,
        "mental_models_applied": [
            {
                "model": "Over-Optimization Tendency",
                "application": "LTCM's models were optimized on historical data but failed to account for unprecedented events (Russian default). This is 'chauffeur knowledge' - knowing formulas without understanding principles.",
                "effect_size": 0.92
            },
            {
                "model": "Leverage",
                "application": "LTCM used 25:1 leverage, turning small losses into catastrophic ones. Munger: 'Leverage is the only way a smart person can go broke.'",
                "effect_size": 0.95
            },
            {
                "model": "Overconfidence Bias",
                "application": "Belief that Nobel Prize-winning models couldn't fail led to excessive risk-taking. Intelligence doesn't protect against psychological biases.",
                "effect_size": 0.80
            },
            {
                "model": "Gaussian Distribution Fallacy",
                "application": "Models assumed normal distribution of returns, ignoring fat tails. Nassim Taleb's 'Black Swan' problem.",
                "effect_size": 0.88
            },
            {
                "model": "Correlation Breakdown",
                "application": "Assumed correlations broke down during crisis - diversification failed when needed most. All 'uncorrelated' positions moved together.",
                "effect_size": 0.85
            },
            {
                "model": "Liquidity Illusion",
                "application": "Positions appeared liquid in normal times but became illiquid during crisis. Market depth disappeared exactly when needed.",
                "effect_size": 0.90
            }
        ],
        "outcome": "Federal Reserve-orchestrated bailout, $4.6B loss, near-systemic crisis",
        "lessons_learned": [
            "Mathematical sophistication doesn't eliminate risk - can increase it through false confidence",
            "Tail risks are more important than average returns",
            "Leverage turns mistakes into disasters",
            "Liquidity is a function of market conditions, not just asset characteristics",
            "Diversification fails when correlations converge to 1.0 during crises"
        ],
        "key_decisions": [
            "Federal Reserve coordinated private sector bailout (not taxpayer money)",
            "Orderly unwinding of positions to prevent market disruption",
            "Partners lost virtually all personal wealth"
        ],
        "mistakes_avoided": [
            "Could have stress-tested models against non-normal distributions",
            "Could have limited leverage to survivable levels",
            "Could have maintained larger cash reserves",
            "Could have recognized that 'this time is different' is usually wrong"
        ]
    },
    {
        "name": "Coca-Cola's New Coke Disaster (1985)",
        "date": date(1985, 4, 23),
        "category": "Product Launch Failure",
        "region": "United States",
        "description": """
Coca-Cola changed its century-old formula based on blind taste tests, ignoring the power of 
brand identity and loss aversion. A masterclass in how rational analysis can miss emotional factors.
        """,
        "severity": 0.65,
        "financial_impact": 30_000_000,
        "models_involved": 5,
        "lollapalooza_score": 0.70,
        "mental_models_applied": [
            {
                "model": "Deprival-Superreaction Tendency",
                "application": "Customers reacted far more strongly to losing original Coke than expected. Loss aversion is stronger than gain attraction. Kahneman: losses hurt 2-3x more than equivalent gains feel good.",
                "effect_size": 0.90
            },
            {
                "model": "Endowment Effect",
                "application": "People valued the original Coke more highly simply because they 'owned' it as part of their identity. Changing it felt like theft.",
                "effect_size": 0.85
            },
            {
                "model": "Social Proof / Identity",
                "application": "Coke was part of American identity. Changing it attacked people's sense of self and cultural belonging.",
                "effect_size": 0.80
            },
            {
                "model": "Overconfidence in Data",
                "application": "Blind taste tests showed preference for New Coke, but failed to measure emotional attachment and context. Measured the wrong thing.",
                "effect_size": 0.75
            },
            {
                "model": "Ignoring Second-Order Effects",
                "application": "Failed to consider how the change would affect brand perception, customer loyalty, and competitive dynamics beyond taste.",
                "effect_size": 0.70
            }
        ],
        "outcome": "Public outcry, return to original formula as 'Coca-Cola Classic' after 79 days, brand damage",
        "lessons_learned": [
            "Emotional factors often outweigh rational analysis in consumer decisions",
            "Brand identity is more valuable than product features",
            "Loss aversion is a powerful force - don't take away what people have",
            "Testing methodology matters - blind taste tests missed context",
            "Sometimes the best decision is to reverse a bad decision quickly"
        ],
        "key_decisions": [
            "Brought back original formula as 'Coca-Cola Classic'",
            "Kept New Coke available briefly, then phased out",
            "CEO Roberto Goizueta took responsibility publicly"
        ],
        "mistakes_avoided": [
            "Could have tested 'taking away' original Coke, not just taste preference",
            "Could have launched New Coke as separate brand (like Diet Coke)",
            "Could have recognized that 'if it ain't broke, don't fix it'",
            "Could have done smaller regional tests before national launch"
        ]
    },
    {
        "name": "Xerox PARC - Failure to Commercialize Innovation",
        "date": date(1979, 1, 1),
        "category": "Innovation Failure",
        "region": "United States",
        "description": """
Xerox PARC invented the GUI, mouse, Ethernet, and laser printing but failed to commercialize 
any of it. Apple and others captured the value. Classic case of incentive misalignment and 
organizational blindness.
        """,
        "severity": 0.75,
        "financial_impact": 100_000_000_000,  # Opportunity cost
        "models_involved": 6,
        "lollapalooza_score": 0.78,
        "mental_models_applied": [
            {
                "model": "Incentive-Caused Bias",
                "application": "Xerox management was incentivized to protect copier business, not pursue new technologies that might cannibalize it. Clayton Christensen's 'Innovator's Dilemma'.",
                "effect_size": 0.88
            },
            {
                "model": "Sunk Cost Fallacy",
                "application": "Xerox had invested heavily in copier technology and distribution, making it psychologically difficult to pivot to computers.",
                "effect_size": 0.70
            },
            {
                "model": "Not-Invented-Here Syndrome",
                "application": "Xerox headquarters didn't value PARC's innovations because they came from outside the core business. Organizational silos prevented knowledge transfer.",
                "effect_size": 0.75
            },
            {
                "model": "Availability Bias",
                "application": "Recent success in copiers made management overweight that business model and underweight potential of computers.",
                "effect_size": 0.68
            },
            {
                "model": "Doubt-Avoidance Tendency",
                "application": "Easier to stick with known copier business than confront uncertainty of computer market. Avoided difficult strategic questions.",
                "effect_size": 0.65
            },
            {
                "model": "Organizational Inertia",
                "application": "Large organizations resist change even when change is clearly needed. Structure and culture optimized for existing business.",
                "effect_size": 0.80
            }
        ],
        "outcome": "Apple, Microsoft, 3Com captured value of PARC innovations. Xerox missed computer revolution.",
        "lessons_learned": [
            "Innovation without commercialization is worthless",
            "Incentive structures must reward new opportunities, not just protect existing business",
            "Organizational structure can prevent value capture from innovation",
            "Cannibalization of existing business is often necessary for survival",
            "Geographic and organizational distance between R&D and business units is dangerous"
        ],
        "key_decisions": [
            "Steve Jobs visited PARC, saw GUI, incorporated into Macintosh",
            "Xerox took small equity stake in Apple (insufficient)",
            "Eventually spun off PARC as separate entity (too late)"
        ],
        "mistakes_avoided": [
            "Could have created separate business unit for computer innovations",
            "Could have aligned incentives to reward commercialization, not just invention",
            "Could have licensed technologies more aggressively",
            "Could have recognized that protecting copier business would ultimately fail anyway"
        ]
    },
    {
        "name": "Enron - Accounting Fraud and Collapse",
        "date": date(2001, 12, 2),
        "category": "Corporate Fraud",
        "region": "United States",
        "description": """
Enron's collapse demonstrated how incentive-caused bias, social proof among elites, and 
authority bias can combine to enable massive fraud. A Lollapalooza of bad incentives.
        """,
        "severity": 0.90,
        "financial_impact": 74_000_000_000,
        "models_involved": 7,
        "lollapalooza_score": 0.88,
        "mental_models_applied": [
            {
                "model": "Incentive-Caused Bias",
                "application": "Executives were incentivized on stock price and reported earnings, not actual cash flow. This created incentive to manipulate accounting. Munger: 'Show me the incentive and I'll show you the outcome.'",
                "effect_size": 0.95
            },
            {
                "model": "Social Proof",
                "application": "Enron was praised by media, analysts, and business schools. This social proof prevented skepticism. If everyone says it's great, it must be great.",
                "effect_size": 0.80
            },
            {
                "model": "Authority Bias",
                "application": "Arthur Andersen was prestigious auditor. Board included respected figures. Authority prevented questioning. 'Experts' must be right.",
                "effect_size": 0.75
            },
            {
                "model": "Reciprocation Tendency",
                "application": "Andersen received consulting fees from Enron, creating conflict of interest. Hard to bite hand that feeds you.",
                "effect_size": 0.85
            },
            {
                "model": "Doubt-Avoidance Tendency",
                "application": "Employees and investors avoided confronting doubts because acknowledging fraud would mean admitting they'd been fooled.",
                "effect_size": 0.70
            },
            {
                "model": "Deprival-Superreaction Tendency",
                "application": "When fraud was revealed, investors and employees lost everything, leading to extreme reactions and Congressional hearings.",
                "effect_size": 0.88
            },
            {
                "model": "Liking/Loving Tendency",
                "application": "Charismatic CEO Jeffrey Skilling inspired loyalty that prevented critical thinking. People didn't want to believe someone they liked was fraudulent.",
                "effect_size": 0.72
            }
        ],
        "outcome": "Bankruptcy, criminal convictions, Arthur Andersen destroyed, Sarbanes-Oxley Act passed",
        "lessons_learned": [
            "Incentives drive behavior - if incentives are misaligned, expect fraud",
            "Social proof and authority can enable wrongdoing by preventing skepticism",
            "Conflicts of interest (auditor also consultant) are extremely dangerous",
            "Complex financial structures are often designed to hide problems",
            "When something seems too good to be true, it usually is"
        ],
        "key_decisions": [
            "SEC investigation led to bankruptcy filing",
            "Criminal prosecutions of executives (Skilling, Fastow)",
            "Sarbanes-Oxley Act to improve corporate governance",
            "Arthur Andersen convicted (later overturned, but firm destroyed)"
        ],
        "mistakes_avoided": [
            "Could have separated auditing from consulting (conflict of interest)",
            "Could have focused on cash flow, not reported earnings",
            "Could have questioned why Enron was so much more profitable than peers",
            "Could have recognized that complexity often hides fraud"
        ]
    }
]


def generate_case_studies(output_file: str = "data/raw/case_studies.json"):
    """
    Generate comprehensive case studies with Planck knowledge.
    
    Args:
        output_file: Path to output JSON file
    """
    print("=" * 80)
    print("CASE STUDY GENERATOR - Mental Models System")
    print("=" * 80)
    print()
    print("Generating case studies with Planck knowledge (deep understanding)...")
    print("Not chauffeur knowledge (superficial facts).")
    print()
    
    # Convert historical cases to CaseStudy objects
    case_studies = []
    for case_data in HISTORICAL_CASES:
        case = CaseStudy(
            name=case_data["name"],
            date=case_data["date"],
            category=case_data["category"],
            region=case_data["region"],
            description=case_data["description"].strip(),
            severity=case_data["severity"],
            financial_impact=case_data["financial_impact"],
            models_involved=case_data["models_involved"],
            lollapalooza_score=case_data["lollapalooza_score"],
            mental_models_applied=case_data["mental_models_applied"],
            outcome=case_data["outcome"],
            lessons_learned=case_data["lessons_learned"],
            key_decisions=case_data["key_decisions"],
            mistakes_avoided=case_data["mistakes_avoided"]
        )
        case_studies.append(case)
    
    # Convert to JSON-serializable format
    cases_json = []
    for case in case_studies:
        case_dict = {
            "name": case.name,
            "date": case.date.isoformat(),
            "category": case.category,
            "region": case.region,
            "description": case.description,
            "severity": case.severity,
            "financial_impact": case.financial_impact,
            "models_involved": case.models_involved,
            "lollapalooza_score": case.lollapalooza_score,
            "mental_models_applied": case.mental_models_applied,
            "outcome": case.outcome,
            "lessons_learned": case.lessons_learned,
            "key_decisions": case.key_decisions,
            "mistakes_avoided": case.mistakes_avoided
        }
        cases_json.append(case_dict)
    
    # Ensure output directory exists
    os.makedirs(os.path.dirname(output_file), exist_ok=True)
    
    # Write to file
    with open(output_file, 'w') as f:
        json.dump(cases_json, f, indent=2)
    
    print(f"✓ Generated {len(case_studies)} case studies")
    print(f"✓ Saved to {output_file}")
    print()
    
    # Print summary
    print("Case Study Summary:")
    print("-" * 80)
    for i, case in enumerate(case_studies, 1):
        print(f"{i}. {case.name}")
        print(f"   Category: {case.category}")
        print(f"   Models Involved: {case.models_involved}")
        print(f"   Lollapalooza Score: {case.lollapalooza_score:.2f}")
        print(f"   Severity: {case.severity:.2f}")
        print()
    
    print("=" * 80)
    print("Generation complete!")
    print("=" * 80)


if __name__ == "__main__":
    generate_case_studies()
