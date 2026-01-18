# Test Status Summary - 2026-01-18

## Overall Results
- **Total Tests**: 342
- **Passed**: 337
- **Failed**: 5
- **Warnings**: 5
- **Success Rate**: 98.5%

## Failed Tests

### 1. test_regime_detection_workflow
**Error**: IndexError: index 3 is out of bounds for axis 0 with size 1
**Location**: src/analysis/hmm_regime.py:209
**Issue**: The HMM regime detection is trying to access index 3 of mean_features array which only has size 1
**Root Cause**: Feature array dimension mismatch - expecting 4 features but only receiving 1

### 2. test_knowledge_graph_workflow
**Error**: AssertionError: Should find related models
**Issue**: Knowledge graph is not finding related models (returns empty list)
**Root Cause**: Relationship detection or graph traversal not working properly

### 3. test_failure_mode_detection_workflow
**Error**: AssertionError: Should detect potential failure modes
**Issue**: Failure mode detection returning empty list
**Root Cause**: Detection logic not triggering or thresholds too strict

### 4. test_api_integration_workflow
**Error**: assert 404 == 200 (HTTP status code)
**Issue**: API endpoint returning 404 Not Found
**Root Cause**: API endpoint not registered or routing issue

### 5. test_large_text_analysis_performance
**Error**: AssertionError: Should detect signals in large text
**Issue**: Signal detection returning empty list for large text
**Root Cause**: Performance issue or signal detection threshold problem

## Warnings
- 5 pytest warnings about @pytest.mark.asyncio on non-async functions in test_connectors.py
