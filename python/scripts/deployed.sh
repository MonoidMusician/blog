#!/usr/bin/env bash
cd -- "$(dirname "$0")/.."
set -euo pipefail

ARGS=(
  --host=127.0.0.1
  --port=48484
  app
)

exec .venv/bin/sanic "${ARGS[@]}"
