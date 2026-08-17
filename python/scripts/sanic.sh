#!/usr/bin/env bash
cd -- "$(dirname "$0")/.."
set -euo pipefail

# PID=/tmp/sanic.veritates.love.pid

# trap 'trap - SIGTERM && rm /tmp/sanic.veritates.love.pid' EXIT SIGTERM

.venv/bin/python3 cert.py

ARGS=(
  --reload
  --host=0.0.0.0
  --port=7357
  --cert=cert/localhost.crt
  --key=cert/localhost.key
  # --pid="$PID"
  app
)

exec .venv/bin/sanic "${ARGS[@]}"
