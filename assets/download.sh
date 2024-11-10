#!/usr/bin/env bash
cd "$(dirname "$0")"
for f in */download.sh; do
  echo $f
  $f
done
