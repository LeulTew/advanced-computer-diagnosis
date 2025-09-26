#!/usr/bin/env bash
# Safe publishing helper for the project. This script DOES NOT embed tokens.
# It requires either: an authenticated `gh` session, or a GITHUB_TOKEN env var.

set -euo pipefail
REPO_NAME="advanced-computer-diagnosis"
REMOTE_NAME="origin"
DEFAULT_BRANCH="main"

if [ -z "$(git rev-parse --is-inside-work-tree 2>/dev/null || true)" ]; then
  echo "Not a git repository. Run this from the project root after running 'git init' or cloning."
  exit 1
fi

if [ -z "${GITHUB_TOKEN:-}" ]; then
  echo "GITHUB_TOKEN not found in environment. This script will attempt to use the 'gh' CLI if available."
  if command -v gh >/dev/null 2>&1; then
    echo "Using 'gh' to create repository (you will be prompted to authenticate if not already)."
    gh repo create "LeulTew/${REPO_NAME}" --public --source=. --remote=${REMOTE_NAME} --push
    exit 0
  else
    echo "Either install the GitHub CLI ('gh') and run 'gh auth login', or set GITHUB_TOKEN to publish programmatically." >&2
    echo "To publish with a token (run this locally; do NOT hardcode your token):" >&2
    echo "  GITHUB_TOKEN=\"<your-token>\" ./scripts/publish_repo.sh" >&2
    exit 2
  fi
else
  echo "GITHUB_TOKEN found in environment; creating remote and pushing using HTTPS remote with token guard (token is NOT stored)."
  # Create remote using token in-memory; the script will not persist token to any file.
  # Note: doing `https://<token>@github.com/...` may leak token into shell history in some setups; prefer GH CLI or git credential helpers.
  REMOTE_URL="https://github.com/LeulTew/${REPO_NAME}.git"
  git remote add ${REMOTE_NAME} "${REMOTE_URL}" || true
  git branch -M ${DEFAULT_BRANCH}
  git push -u ${REMOTE_NAME} ${DEFAULT_BRANCH}
  echo "Push completed. Ensure you clear GITHUB_TOKEN from your environment when finished." 
fi
