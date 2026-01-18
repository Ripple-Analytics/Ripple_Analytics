"""
Custom Exceptions for Mental Models System

Provides clear, actionable error messages and proper error hierarchy.

"I think it is undeniably true that the human brain must work in models.
The trick is to have your brain work better than the other person's brain
because it understands the most fundamental models."
- Charlie Munger
"""


class MentalModelsException(Exception):
    """Base exception for all Mental Models System errors."""
    
    def __init__(self, message: str, details: dict = None):
        self.message = message
        self.details = details or {}
        super().__init__(self.message)
    
    def to_dict(self):
        """Convert exception to dictionary for API responses."""
        return {
            "error": self.__class__.__name__,
            "message": self.message,
            "details": self.details
        }


# Data and Input Errors

class DataError(MentalModelsException):
    """Base class for data-related errors."""
    pass


class InvalidInputError(DataError):
    """Raised when input data is invalid or malformed."""
    pass


class MissingDataError(DataError):
    """Raised when required data is missing."""
    pass


class DataValidationError(DataError):
    """Raised when data fails validation checks."""
    pass


# Model Errors

class ModelError(MentalModelsException):
    """Base class for mental model-related errors."""
    pass


class ModelNotFoundError(ModelError):
    """Raised when a mental model cannot be found."""
    
    def __init__(self, model_identifier: str):
        super().__init__(
            f"Mental model not found: {model_identifier}",
            {"model_identifier": model_identifier}
        )


class ModelLoadError(ModelError):
    """Raised when a mental model fails to load."""
    pass


class InvalidModelError(ModelError):
    """Raised when a mental model is invalid or corrupted."""
    pass


# Analysis Errors

class AnalysisError(MentalModelsException):
    """Base class for analysis-related errors."""
    pass


class AnalysisFailedError(AnalysisError):
    """Raised when analysis fails to complete."""
    pass


class InsufficientDataError(AnalysisError):
    """Raised when there is insufficient data for analysis."""
    
    def __init__(self, required: int, actual: int):
        super().__init__(
            f"Insufficient data for analysis. Required: {required}, Actual: {actual}",
            {"required": required, "actual": actual}
        )


class ConfidenceTooLowError(AnalysisError):
    """Raised when analysis confidence is below threshold."""
    
    def __init__(self, confidence: float, threshold: float):
        super().__init__(
            f"Analysis confidence ({confidence:.2f}) below threshold ({threshold:.2f})",
            {"confidence": confidence, "threshold": threshold}
        )


# Safeguard Errors

class SafeguardError(MentalModelsException):
    """Base class for safeguard-related errors."""
    pass


class FailureModeDetectedError(SafeguardError):
    """Raised when a failure mode is detected."""
    
    def __init__(self, model_name: str, failure_mode: str, severity: str):
        super().__init__(
            f"Failure mode detected in {model_name}: {failure_mode} (Severity: {severity})",
            {
                "model_name": model_name,
                "failure_mode": failure_mode,
                "severity": severity
            }
        )


class SafeguardViolationError(SafeguardError):
    """Raised when a safeguard is violated."""
    
    def __init__(self, safeguard_name: str, violation_details: str):
        super().__init__(
            f"Safeguard violation: {safeguard_name}",
            {
                "safeguard_name": safeguard_name,
                "violation_details": violation_details
            }
        )


# Decision Tracking Errors

class DecisionError(MentalModelsException):
    """Base class for decision tracking errors."""
    pass


class DecisionNotFoundError(DecisionError):
    """Raised when a decision cannot be found."""
    
    def __init__(self, decision_id: str):
        super().__init__(
            f"Decision not found: {decision_id}",
            {"decision_id": decision_id}
        )


class InvalidDecisionStateError(DecisionError):
    """Raised when a decision is in an invalid state for the requested operation."""
    
    def __init__(self, decision_id: str, current_state: str, required_state: str):
        super().__init__(
            f"Decision {decision_id} is in state '{current_state}', but '{required_state}' is required",
            {
                "decision_id": decision_id,
                "current_state": current_state,
                "required_state": required_state
            }
        )


# API Errors

class APIError(MentalModelsException):
    """Base class for API-related errors."""
    pass


class AuthenticationError(APIError):
    """Raised when authentication fails."""
    pass


class AuthorizationError(APIError):
    """Raised when user lacks required permissions."""
    pass


class RateLimitError(APIError):
    """Raised when rate limit is exceeded."""
    
    def __init__(self, limit: int, window: str):
        super().__init__(
            f"Rate limit exceeded: {limit} requests per {window}",
            {"limit": limit, "window": window}
        )


class ResourceNotFoundError(APIError):
    """Raised when a requested resource is not found."""
    
    def __init__(self, resource_type: str, resource_id: str):
        super().__init__(
            f"{resource_type} not found: {resource_id}",
            {"resource_type": resource_type, "resource_id": resource_id}
        )


# Integration Errors

class IntegrationError(MentalModelsException):
    """Base class for external integration errors."""
    pass


class DatabaseError(IntegrationError):
    """Raised when database operations fail."""
    pass


class ExternalAPIError(IntegrationError):
    """Raised when external API calls fail."""
    
    def __init__(self, api_name: str, status_code: int, message: str):
        super().__init__(
            f"External API error ({api_name}): {message}",
            {
                "api_name": api_name,
                "status_code": status_code,
                "message": message
            }
        )


class WebhookError(IntegrationError):
    """Raised when webhook operations fail."""
    pass


# Configuration Errors

class ConfigurationError(MentalModelsException):
    """Base class for configuration errors."""
    pass


class InvalidConfigurationError(ConfigurationError):
    """Raised when configuration is invalid."""
    pass


class MissingConfigurationError(ConfigurationError):
    """Raised when required configuration is missing."""
    
    def __init__(self, config_key: str):
        super().__init__(
            f"Missing required configuration: {config_key}",
            {"config_key": config_key}
        )


# Performance Errors

class PerformanceError(MentalModelsException):
    """Base class for performance-related errors."""
    pass


class TimeoutError(PerformanceError):
    """Raised when an operation times out."""
    
    def __init__(self, operation: str, timeout_seconds: float):
        super().__init__(
            f"Operation '{operation}' timed out after {timeout_seconds}s",
            {"operation": operation, "timeout_seconds": timeout_seconds}
        )


class ResourceExhaustedError(PerformanceError):
    """Raised when system resources are exhausted."""
    
    def __init__(self, resource_type: str):
        super().__init__(
            f"Resource exhausted: {resource_type}",
            {"resource_type": resource_type}
        )


# Utility Functions

def handle_exception(func):
    """
    Decorator for consistent exception handling.
    
    Usage:
        @handle_exception
        def my_function():
            # function code
    """
    def wrapper(*args, **kwargs):
        try:
            return func(*args, **kwargs)
        except MentalModelsException:
            # Re-raise our custom exceptions
            raise
        except KeyError as e:
            raise MissingDataError(f"Missing required key: {e}")
        except ValueError as e:
            raise InvalidInputError(f"Invalid value: {e}")
        except FileNotFoundError as e:
            raise DataError(f"File not found: {e}")
        except Exception as e:
            # Wrap unexpected exceptions
            raise MentalModelsException(
                f"Unexpected error in {func.__name__}: {str(e)}",
                {"function": func.__name__, "error_type": type(e).__name__}
            )
    
    return wrapper


def validate_input(condition: bool, message: str, details: dict = None):
    """
    Validate input and raise InvalidInputError if condition is False.
    
    Usage:
        validate_input(len(text) > 0, "Text cannot be empty")
    """
    if not condition:
        raise InvalidInputError(message, details)


def require_data(value, name: str):
    """
    Require that a value is not None.
    
    Usage:
        model = require_data(loader.get_model(id), f"Model {id}")
    """
    if value is None:
        raise MissingDataError(f"Required data missing: {name}")
    return value
