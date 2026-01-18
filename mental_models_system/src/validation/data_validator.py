"""
Data Validation Layer

Comprehensive validation for all data types in the Mental Models System:
- Mental models
- Case studies
- Failure modes
- Decisions
- Signals
- Analysis results
"""

import re
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Callable, Dict, List, Optional, Set, Tuple, Union
import json


class ValidationSeverity(Enum):
    """Severity levels for validation issues."""
    INFO = "info"
    WARNING = "warning"
    ERROR = "error"
    CRITICAL = "critical"


@dataclass
class ValidationIssue:
    """A validation issue found during validation."""
    field: str
    message: str
    severity: ValidationSeverity
    value: Any = None
    suggestion: Optional[str] = None
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "field": self.field,
            "message": self.message,
            "severity": self.severity.value,
            "value": str(self.value) if self.value is not None else None,
            "suggestion": self.suggestion
        }


@dataclass
class ValidationResult:
    """Result of a validation operation."""
    valid: bool
    issues: List[ValidationIssue] = field(default_factory=list)
    data: Dict[str, Any] = field(default_factory=dict)
    validated_at: datetime = field(default_factory=datetime.now)
    
    @property
    def errors(self) -> List[ValidationIssue]:
        return [i for i in self.issues if i.severity in (ValidationSeverity.ERROR, ValidationSeverity.CRITICAL)]
    
    @property
    def warnings(self) -> List[ValidationIssue]:
        return [i for i in self.issues if i.severity == ValidationSeverity.WARNING]
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "valid": self.valid,
            "issues": [i.to_dict() for i in self.issues],
            "error_count": len(self.errors),
            "warning_count": len(self.warnings),
            "validated_at": self.validated_at.isoformat()
        }


class FieldValidator:
    """Validator for individual fields."""
    
    @staticmethod
    def required(value: Any, field_name: str) -> Optional[ValidationIssue]:
        """Check if field is present and not empty."""
        if value is None or (isinstance(value, str) and not value.strip()):
            return ValidationIssue(
                field=field_name,
                message=f"Field '{field_name}' is required",
                severity=ValidationSeverity.ERROR,
                value=value
            )
        return None
    
    @staticmethod
    def string_length(
        value: str,
        field_name: str,
        min_length: int = 0,
        max_length: int = None
    ) -> Optional[ValidationIssue]:
        """Validate string length."""
        if not isinstance(value, str):
            return ValidationIssue(
                field=field_name,
                message=f"Field '{field_name}' must be a string",
                severity=ValidationSeverity.ERROR,
                value=value
            )
        
        if len(value) < min_length:
            return ValidationIssue(
                field=field_name,
                message=f"Field '{field_name}' must be at least {min_length} characters",
                severity=ValidationSeverity.ERROR,
                value=value,
                suggestion=f"Add more content to reach minimum length of {min_length}"
            )
        
        if max_length and len(value) > max_length:
            return ValidationIssue(
                field=field_name,
                message=f"Field '{field_name}' must be at most {max_length} characters",
                severity=ValidationSeverity.ERROR,
                value=f"{value[:50]}..." if len(value) > 50 else value,
                suggestion=f"Truncate to {max_length} characters"
            )
        
        return None
    
    @staticmethod
    def numeric_range(
        value: Union[int, float],
        field_name: str,
        min_value: float = None,
        max_value: float = None
    ) -> Optional[ValidationIssue]:
        """Validate numeric range."""
        if not isinstance(value, (int, float)):
            return ValidationIssue(
                field=field_name,
                message=f"Field '{field_name}' must be a number",
                severity=ValidationSeverity.ERROR,
                value=value
            )
        
        if min_value is not None and value < min_value:
            return ValidationIssue(
                field=field_name,
                message=f"Field '{field_name}' must be at least {min_value}",
                severity=ValidationSeverity.ERROR,
                value=value,
                suggestion=f"Increase value to at least {min_value}"
            )
        
        if max_value is not None and value > max_value:
            return ValidationIssue(
                field=field_name,
                message=f"Field '{field_name}' must be at most {max_value}",
                severity=ValidationSeverity.ERROR,
                value=value,
                suggestion=f"Decrease value to at most {max_value}"
            )
        
        return None
    
    @staticmethod
    def list_length(
        value: List,
        field_name: str,
        min_length: int = 0,
        max_length: int = None
    ) -> Optional[ValidationIssue]:
        """Validate list length."""
        if not isinstance(value, list):
            return ValidationIssue(
                field=field_name,
                message=f"Field '{field_name}' must be a list",
                severity=ValidationSeverity.ERROR,
                value=value
            )
        
        if len(value) < min_length:
            return ValidationIssue(
                field=field_name,
                message=f"Field '{field_name}' must have at least {min_length} items",
                severity=ValidationSeverity.ERROR,
                value=f"[{len(value)} items]",
                suggestion=f"Add at least {min_length - len(value)} more items"
            )
        
        if max_length and len(value) > max_length:
            return ValidationIssue(
                field=field_name,
                message=f"Field '{field_name}' must have at most {max_length} items",
                severity=ValidationSeverity.ERROR,
                value=f"[{len(value)} items]",
                suggestion=f"Remove {len(value) - max_length} items"
            )
        
        return None
    
    @staticmethod
    def enum_value(
        value: str,
        field_name: str,
        allowed_values: Set[str]
    ) -> Optional[ValidationIssue]:
        """Validate enum/allowed values."""
        if value not in allowed_values:
            return ValidationIssue(
                field=field_name,
                message=f"Field '{field_name}' must be one of: {', '.join(sorted(allowed_values))}",
                severity=ValidationSeverity.ERROR,
                value=value,
                suggestion=f"Use one of: {', '.join(sorted(allowed_values))}"
            )
        return None
    
    @staticmethod
    def regex_pattern(
        value: str,
        field_name: str,
        pattern: str,
        pattern_description: str = None
    ) -> Optional[ValidationIssue]:
        """Validate against regex pattern."""
        if not isinstance(value, str):
            return ValidationIssue(
                field=field_name,
                message=f"Field '{field_name}' must be a string",
                severity=ValidationSeverity.ERROR,
                value=value
            )
        
        if not re.match(pattern, value):
            desc = pattern_description or f"pattern {pattern}"
            return ValidationIssue(
                field=field_name,
                message=f"Field '{field_name}' does not match {desc}",
                severity=ValidationSeverity.ERROR,
                value=value
            )
        return None
    
    @staticmethod
    def date_format(
        value: str,
        field_name: str,
        format_str: str = "%Y-%m-%d"
    ) -> Optional[ValidationIssue]:
        """Validate date format."""
        try:
            datetime.strptime(value, format_str)
            return None
        except (ValueError, TypeError):
            return ValidationIssue(
                field=field_name,
                message=f"Field '{field_name}' must be a valid date in format {format_str}",
                severity=ValidationSeverity.ERROR,
                value=value,
                suggestion=f"Use format: {format_str}"
            )


class MentalModelValidator:
    """Validator for mental model data."""
    
    VALID_CATEGORIES = {
        "Psychology", "Thinking Tools", "Economics", "Moats",
        "Math", "Physics", "Biology", "Organizational"
    }
    
    def validate(self, model: Dict[str, Any]) -> ValidationResult:
        """Validate a mental model."""
        issues = []
        
        # Required fields
        required_fields = ["name", "category", "description"]
        for field in required_fields:
            issue = FieldValidator.required(model.get(field), field)
            if issue:
                issues.append(issue)
        
        # Name validation
        if model.get("name"):
            issue = FieldValidator.string_length(model["name"], "name", min_length=3, max_length=100)
            if issue:
                issues.append(issue)
        
        # Category validation
        if model.get("category"):
            issue = FieldValidator.enum_value(model["category"], "category", self.VALID_CATEGORIES)
            if issue:
                issues.append(issue)
        
        # Description validation
        if model.get("description"):
            issue = FieldValidator.string_length(model["description"], "description", min_length=50)
            if issue:
                issues.append(issue)
            elif len(model["description"]) < 100:
                issues.append(ValidationIssue(
                    field="description",
                    message="Description is short, consider adding more detail",
                    severity=ValidationSeverity.WARNING,
                    value=f"{len(model['description'])} chars",
                    suggestion="Aim for at least 100 characters for comprehensive description"
                ))
        
        # Examples validation
        if model.get("examples"):
            issue = FieldValidator.list_length(model["examples"], "examples", min_length=1)
            if issue:
                issues.append(issue)
        else:
            issues.append(ValidationIssue(
                field="examples",
                message="No examples provided",
                severity=ValidationSeverity.WARNING,
                suggestion="Add at least one real-world example"
            ))
        
        # Applicability score
        if "applicability" in model:
            issue = FieldValidator.numeric_range(model["applicability"], "applicability", 0.0, 1.0)
            if issue:
                issues.append(issue)
        
        valid = not any(i.severity in (ValidationSeverity.ERROR, ValidationSeverity.CRITICAL) for i in issues)
        
        return ValidationResult(valid=valid, issues=issues, data=model)


class FailureModeValidator:
    """Validator for failure mode data."""
    
    def validate(self, failure_mode: Dict[str, Any]) -> ValidationResult:
        """Validate a failure mode."""
        issues = []
        
        # Required fields
        required_fields = ["model_name", "failure_mode", "description"]
        for field in required_fields:
            issue = FieldValidator.required(failure_mode.get(field), field)
            if issue:
                issues.append(issue)
        
        # Description validation
        if failure_mode.get("description"):
            issue = FieldValidator.string_length(failure_mode["description"], "description", min_length=20)
            if issue:
                issues.append(issue)
        
        # Warning signs
        if failure_mode.get("warning_signs"):
            issue = FieldValidator.list_length(failure_mode["warning_signs"], "warning_signs", min_length=1)
            if issue:
                issues.append(issue)
        else:
            issues.append(ValidationIssue(
                field="warning_signs",
                message="No warning signs provided",
                severity=ValidationSeverity.WARNING,
                suggestion="Add warning signs to help detect this failure mode"
            ))
        
        # Safeguards
        if failure_mode.get("safeguards"):
            issue = FieldValidator.list_length(failure_mode["safeguards"], "safeguards", min_length=1)
            if issue:
                issues.append(issue)
        else:
            issues.append(ValidationIssue(
                field="safeguards",
                message="No safeguards provided",
                severity=ValidationSeverity.WARNING,
                suggestion="Add safeguards to prevent this failure mode"
            ))
        
        # Case study
        if not failure_mode.get("case_study"):
            issues.append(ValidationIssue(
                field="case_study",
                message="No case study provided",
                severity=ValidationSeverity.INFO,
                suggestion="Add a real-world case study for better understanding"
            ))
        
        valid = not any(i.severity in (ValidationSeverity.ERROR, ValidationSeverity.CRITICAL) for i in issues)
        
        return ValidationResult(valid=valid, issues=issues, data=failure_mode)


class DecisionValidator:
    """Validator for decision data."""
    
    def validate(self, decision: Dict[str, Any]) -> ValidationResult:
        """Validate a decision."""
        issues = []
        
        # Required fields
        required_fields = ["title", "context", "options"]
        for field in required_fields:
            issue = FieldValidator.required(decision.get(field), field)
            if issue:
                issues.append(issue)
        
        # Title validation
        if decision.get("title"):
            issue = FieldValidator.string_length(decision["title"], "title", min_length=5, max_length=200)
            if issue:
                issues.append(issue)
        
        # Context validation
        if decision.get("context"):
            issue = FieldValidator.string_length(decision["context"], "context", min_length=20)
            if issue:
                issues.append(issue)
        
        # Options validation
        if decision.get("options"):
            issue = FieldValidator.list_length(decision["options"], "options", min_length=2)
            if issue:
                issues.append(issue)
            else:
                # Validate each option
                for i, option in enumerate(decision["options"]):
                    if not option.get("name"):
                        issues.append(ValidationIssue(
                            field=f"options[{i}].name",
                            message="Option name is required",
                            severity=ValidationSeverity.ERROR
                        ))
        
        # Mental models used
        if not decision.get("models_used"):
            issues.append(ValidationIssue(
                field="models_used",
                message="No mental models specified",
                severity=ValidationSeverity.WARNING,
                suggestion="Apply relevant mental models to improve decision quality"
            ))
        
        # Risk assessment
        if "risk_score" in decision:
            issue = FieldValidator.numeric_range(decision["risk_score"], "risk_score", 0.0, 1.0)
            if issue:
                issues.append(issue)
        
        valid = not any(i.severity in (ValidationSeverity.ERROR, ValidationSeverity.CRITICAL) for i in issues)
        
        return ValidationResult(valid=valid, issues=issues, data=decision)


class SignalValidator:
    """Validator for signal data."""
    
    VALID_SOURCES = {"news", "sec", "social", "market", "research", "custom"}
    
    def validate(self, signal: Dict[str, Any]) -> ValidationResult:
        """Validate a signal."""
        issues = []
        
        # Required fields
        required_fields = ["title", "content", "source"]
        for field in required_fields:
            issue = FieldValidator.required(signal.get(field), field)
            if issue:
                issues.append(issue)
        
        # Source validation
        if signal.get("source"):
            issue = FieldValidator.enum_value(signal["source"], "source", self.VALID_SOURCES)
            if issue:
                issues.append(issue)
        
        # Content validation
        if signal.get("content"):
            issue = FieldValidator.string_length(signal["content"], "content", min_length=10)
            if issue:
                issues.append(issue)
        
        # Confidence score
        if "confidence" in signal:
            issue = FieldValidator.numeric_range(signal["confidence"], "confidence", 0.0, 1.0)
            if issue:
                issues.append(issue)
        
        # URL validation
        if signal.get("url"):
            url_pattern = r'^https?://[^\s/$.?#].[^\s]*$'
            issue = FieldValidator.regex_pattern(signal["url"], "url", url_pattern, "valid URL")
            if issue:
                issues.append(issue)
        
        valid = not any(i.severity in (ValidationSeverity.ERROR, ValidationSeverity.CRITICAL) for i in issues)
        
        return ValidationResult(valid=valid, issues=issues, data=signal)


class DataValidator:
    """
    Central data validation manager.
    
    Provides validation for all data types in the Mental Models System.
    """
    
    def __init__(self):
        self.validators = {
            "mental_model": MentalModelValidator(),
            "failure_mode": FailureModeValidator(),
            "decision": DecisionValidator(),
            "signal": SignalValidator()
        }
        
        self.custom_validators: Dict[str, List[Callable[[Dict], Optional[ValidationIssue]]]] = {}
    
    def register_custom_validator(
        self,
        data_type: str,
        validator: Callable[[Dict], Optional[ValidationIssue]]
    ):
        """Register a custom validator for a data type."""
        if data_type not in self.custom_validators:
            self.custom_validators[data_type] = []
        self.custom_validators[data_type].append(validator)
    
    def validate(self, data: Dict[str, Any], data_type: str) -> ValidationResult:
        """
        Validate data of a specific type.
        
        Args:
            data: Data to validate
            data_type: Type of data (mental_model, failure_mode, decision, signal)
        
        Returns:
            ValidationResult
        """
        if data_type not in self.validators:
            return ValidationResult(
                valid=False,
                issues=[ValidationIssue(
                    field="_type",
                    message=f"Unknown data type: {data_type}",
                    severity=ValidationSeverity.CRITICAL
                )]
            )
        
        # Run standard validator
        result = self.validators[data_type].validate(data)
        
        # Run custom validators
        if data_type in self.custom_validators:
            for custom_validator in self.custom_validators[data_type]:
                try:
                    issue = custom_validator(data)
                    if issue:
                        result.issues.append(issue)
                except Exception as e:
                    result.issues.append(ValidationIssue(
                        field="_custom",
                        message=f"Custom validator error: {str(e)}",
                        severity=ValidationSeverity.WARNING
                    ))
        
        # Re-check validity after custom validators
        result.valid = not any(
            i.severity in (ValidationSeverity.ERROR, ValidationSeverity.CRITICAL)
            for i in result.issues
        )
        
        return result
    
    def validate_batch(
        self,
        items: List[Dict[str, Any]],
        data_type: str
    ) -> Tuple[List[ValidationResult], Dict[str, Any]]:
        """
        Validate a batch of items.
        
        Returns:
            Tuple of (list of results, summary stats)
        """
        results = []
        valid_count = 0
        error_count = 0
        warning_count = 0
        
        for item in items:
            result = self.validate(item, data_type)
            results.append(result)
            
            if result.valid:
                valid_count += 1
            error_count += len(result.errors)
            warning_count += len(result.warnings)
        
        summary = {
            "total": len(items),
            "valid": valid_count,
            "invalid": len(items) - valid_count,
            "total_errors": error_count,
            "total_warnings": warning_count,
            "validation_rate": valid_count / len(items) if items else 0
        }
        
        return results, summary
    
    def validate_json_file(self, file_path: str, data_type: str) -> Tuple[List[ValidationResult], Dict[str, Any]]:
        """Validate all items in a JSON file."""
        with open(file_path) as f:
            data = json.load(f)
        
        if isinstance(data, list):
            return self.validate_batch(data, data_type)
        elif isinstance(data, dict):
            # Single item
            result = self.validate(data, data_type)
            return [result], {
                "total": 1,
                "valid": 1 if result.valid else 0,
                "invalid": 0 if result.valid else 1,
                "total_errors": len(result.errors),
                "total_warnings": len(result.warnings),
                "validation_rate": 1.0 if result.valid else 0.0
            }
        else:
            return [], {"error": "Invalid JSON structure"}


if __name__ == "__main__":
    # Test the validators
    validator = DataValidator()
    
    # Test mental model validation
    model = {
        "name": "Network Effects",
        "category": "Economics",
        "description": "A phenomenon where a product or service gains additional value as more people use it. The value of the network grows exponentially with the number of users.",
        "examples": ["Facebook", "Uber", "Airbnb"],
        "applicability": 0.9
    }
    
    result = validator.validate(model, "mental_model")
    print(f"Mental Model Valid: {result.valid}")
    print(f"Issues: {len(result.issues)}")
    for issue in result.issues:
        print(f"  - [{issue.severity.value}] {issue.field}: {issue.message}")
    
    # Test failure mode validation
    failure_mode = {
        "model_name": "Confirmation Bias",
        "failure_mode": "Echo Chamber Effect",
        "description": "Only seeking information that confirms existing beliefs",
        "warning_signs": ["Dismissing contradictory evidence", "Only reading agreeable sources"],
        "safeguards": ["Actively seek opposing viewpoints", "Use devil's advocate"]
    }
    
    result = validator.validate(failure_mode, "failure_mode")
    print(f"\nFailure Mode Valid: {result.valid}")
    print(f"Issues: {len(result.issues)}")
    for issue in result.issues:
        print(f"  - [{issue.severity.value}] {issue.field}: {issue.message}")
