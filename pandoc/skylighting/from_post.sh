#!/usr/bin/env bash
set -euo pipefail
# https://docs.kde.org/stable5/en/kate/katepart/highlight.html
pandoc "$1" -w json | \
  jq --raw-output '
    (.blocks[] | select(.t == "CodeBlock" and .c[0][1] == ["xml", "skylighting"]) | .c[1])
    // "<language name=\"null\"><highlighting><contexts><context name=\"\"></context></contexts></highlighting></language>"
  '
