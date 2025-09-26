#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

cd "$ROOT_DIR"

echo "Running static analysis..."
swipl -q -s tools/static_analysis.pl

echo "Running unit and integration tests..."
swipl -q -s diag.pl -s test/diag_tests.pl -g run_tests -g halt

echo "All checks completed."
