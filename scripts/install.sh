#!/usr/bin/env bash
set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DEFAULT_VENV="${PROJECT_ROOT}/.venv"

INSTALL_PYTHON=1
RUN_TESTS=1
DRY_RUN=0
VENV_PATH="$DEFAULT_VENV"
APT_UPDATED=0

usage() {
  cat <<'EOF'
Usage: scripts/install.sh [options]

Prepare the Advanced Computer Problem Diagnosis Expert System on macOS or Debian/Ubuntu.
Installs SWI-Prolog, bootstraps the optional Python voice pipeline, and runs health checks.

Options:
  --skip-python        Skip creating a Python virtual environment and installing requirements.
  --skip-tests         Do not run ./scripts/run_checks.sh after installation.
  --venv-name=PATH     Override the virtual environment path (default: PROJECT/.venv).
  --dry-run            Print the commands without executing them.
  --help               Show this message and exit.

Examples:
  bash scripts/install.sh --skip-python
  bash scripts/install.sh --venv-name=.voice-venv --skip-tests
  bash scripts/install.sh --dry-run
EOF
}

log() {
  printf "[install] %s\n" "$*"
}

run_cmd() {
  local cmd="$1"
  if [[ "$DRY_RUN" -eq 1 ]]; then
    log "(dry-run) $cmd"
  else
    log "$cmd"
    eval "$cmd"
  fi
}

parse_args() {
  while [[ $# -gt 0 ]]; do
    case "$1" in
      --skip-python)
        INSTALL_PYTHON=0
        ;;
      --skip-tests)
        RUN_TESTS=0
        ;;
      --venv-name=*)
        VENV_PATH="${1#*=}"
        ;;
      --dry-run)
        DRY_RUN=1
        ;;
      --help)
        usage
        exit 0
        ;;
      *)
        printf "Unknown option: %s\n\n" "$1" >&2
        usage
        exit 1
        ;;
    esac
    shift
  done
}

require_command() {
  local cmd="$1"
  local install_hint="$2"
  if command -v "$cmd" >/dev/null 2>&1; then
    return 0
  fi
  log "Command '$cmd' not found. $install_hint"
  return 1
}

ensure_swipl() {
  if command -v swipl >/dev/null 2>&1; then
    log "SWI-Prolog already installed."
    return 0
  fi

  case "$PKG_MANAGER" in
    apt)
      if [[ "$APT_UPDATED" -eq 0 && "$DRY_RUN" -eq 0 ]]; then
        run_cmd "sudo apt-get update"
        APT_UPDATED=1
      elif [[ "$APT_UPDATED" -eq 0 ]]; then
        log "(dry-run) sudo apt-get update"
        APT_UPDATED=1
      fi
      run_cmd "sudo apt-get install -y swi-prolog"
      ;;
    brew)
      run_cmd "brew install swi-prolog"
      ;;
    *)
      log "Unsupported platform. Please install SWI-Prolog manually from https://www.swi-prolog.org/download/stable"
      return 1
      ;;
  esac
}

install_python_requirements() {
  if [[ "$INSTALL_PYTHON" -ne 1 ]]; then
    log "Skipping Python dependency setup."
    return 0
  fi

  if ! command -v python3 >/dev/null 2>&1; then
    log "Python3 is required for voice mode. Please install it manually."
    return 1
  fi

  local venv_bin
  venv_bin="$VENV_PATH/bin"

  if [[ -d "$VENV_PATH" ]]; then
    log "Reusing existing virtual environment at $VENV_PATH"
  else
    run_cmd "python3 -m venv '$VENV_PATH'"
  fi

  run_cmd "'$venv_bin/pip' install --upgrade pip"
  run_cmd "'$venv_bin/pip' install -r '$PROJECT_ROOT/requirements.txt'"
}

run_health_checks() {
  if [[ "$RUN_TESTS" -ne 1 ]]; then
    log "Skipping test suite."
    return 0
  fi

  run_cmd "cd '$PROJECT_ROOT' && ./scripts/run_checks.sh"
}

summarize() {
  cat <<EOF

Installation complete!

Next steps:
  1. Launch the expert system:
       swipl -s diag.pl -g main
  2. (Optional) Activate the virtual environment before using voice input:
       source "$VENV_PATH/bin/activate"
  3. Review data/perf_profile.json for the latest benchmark metrics.

Happy diagnosing!
EOF
}

detect_pkg_manager() {
  if command -v apt-get >/dev/null 2>&1; then
    PKG_MANAGER="apt"
  elif command -v brew >/dev/null 2>&1; then
    PKG_MANAGER="brew"
  else
    PKG_MANAGER="unknown"
  fi
}

main() {
  parse_args "$@"
  detect_pkg_manager

  log "Detected project root: $PROJECT_ROOT"
  log "Package manager: $PKG_MANAGER"
  log "Virtual environment: $VENV_PATH"
  log "Install Python deps: $INSTALL_PYTHON"
  log "Run tests after install: $RUN_TESTS"
  log "Dry run: $DRY_RUN"

  ensure_swipl || exit 1
  install_python_requirements || exit 1
  run_health_checks || exit 1

  if [[ "$DRY_RUN" -eq 0 ]]; then
    summarize
  else
    log "Dry run finished. No changes were made."
  fi
}

main "$@"
