#!/bin/bash
trap 'trap - SIGTERM && kill 0' EXIT SIGTERM
make live >/dev/null &
spago build || exit 1
sleep 5
make watch-sass &
make watch-pandoc &
which trypurescript && make trypurescript 2>/dev/null &
make watch-ps
wait
