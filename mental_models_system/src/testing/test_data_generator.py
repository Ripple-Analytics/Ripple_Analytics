"""
Test Data Generator for Mental Models System

Generates realistic test data for integration tests to ensure
comprehensive coverage and realistic scenarios.

"The best way to predict the future is to invent it."
- Alan Kay
"""

import json
from typing import List, Dict, Any
from dataclasses import dataclass, asdict


@dataclass
class TestScenario:
    """A test scenario with expected outcomes."""
    name: str
    text: str
    expected_models: List[Dict[str, Any]]
    expected_lollapalooza: bool
    description: str


class TestDataGenerator:
    """Generate comprehensive test data for mental models analysis."""
    
    def __init__(self):
        self.scenarios = self._create_scenarios()
    
    def _create_scenarios(self) -> List[TestScenario]:
        """Create comprehensive test scenarios."""
        return [
            TestScenario(
                name="amazon_moat",
                text="""
                Amazon's dominance in e-commerce demonstrates a powerful Lollapalooza effect:
                
                1. Network Effects: More sellers attract more buyers, more buyers attract more sellers.
                2. Economies of Scale: Massive volume allows lower prices and faster delivery.
                3. Switching Costs: Prime membership and ecosystem lock-in make it hard to leave.
                4. Brand: Trust and reliability create a moat.
                5. Data Advantage: Customer data enables better recommendations and inventory.
                
                These models reinforce each other, creating an almost unassailable competitive position.
                """,
                expected_models=[
                    {"model_name": "Network Effects", "relevance_score": 0.9, "confidence": 0.85, "evidence": "More sellers attract more buyers", "insights": ["Strong network effects"]},
                    {"model_name": "Economies of Scale", "relevance_score": 0.85, "confidence": 0.8, "evidence": "Massive volume allows lower prices", "insights": ["Scale advantages"]},
                    {"model_name": "Switching Costs", "relevance_score": 0.8, "confidence": 0.75, "evidence": "Prime membership lock-in", "insights": ["High switching costs"]},
                    {"model_name": "Brand", "relevance_score": 0.75, "confidence": 0.7, "evidence": "Trust and reliability", "insights": ["Strong brand moat"]},
                    {"model_name": "Data Network Effects", "relevance_score": 0.7, "confidence": 0.65, "evidence": "Customer data enables better recommendations", "insights": ["Data advantage"]},
                ],
                expected_lollapalooza=True,
                description="Multi-model competitive advantage analysis"
            ),
            TestScenario(
                name="incentive_bias",
                text="""
                The company's management team was heavily incentivized through stock options,
                leading to short-term decision making that prioritized quarterly earnings over
                long-term value creation. This created a classic case of incentive-caused bias,
                where the rational self-interest of executives diverged from shareholder interests.
                """,
                expected_models=[
                    {"model_name": "Incentive-Caused Bias", "relevance_score": 0.95, "confidence": 0.9, "evidence": "Stock options incentives", "insights": ["Misaligned incentives"]},
                    {"model_name": "Principal-Agent Problem", "relevance_score": 0.85, "confidence": 0.8, "evidence": "Diverged from shareholder interests", "insights": ["Agency problem"]},
                    {"model_name": "Short-term Thinking", "relevance_score": 0.8, "confidence": 0.75, "evidence": "Quarterly earnings focus", "insights": ["Short-termism"]},
                ],
                expected_lollapalooza=False,
                description="Single dominant model with supporting models"
            ),
        ]
    
    def get_scenario(self, name: str) -> TestScenario:
        """Get a specific test scenario."""
        for scenario in self.scenarios:
            if scenario.name == name:
                return scenario
        raise ValueError(f"Scenario '{name}' not found")
    
    def get_all_scenarios(self) -> List[TestScenario]:
        """Get all test scenarios."""
        return self.scenarios
    
    def generate_mock_llm_response(self, scenario_name: str) -> str:
        """Generate a mock LLM response for a scenario."""
        scenario = self.get_scenario(scenario_name)
        return json.dumps(scenario.expected_models)


# Singleton instance
_generator = None

def get_test_data_generator() -> TestDataGenerator:
    """Get singleton test data generator."""
    global _generator
    if _generator is None:
        _generator = TestDataGenerator()
    return _generator
