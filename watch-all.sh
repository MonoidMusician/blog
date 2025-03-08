#!/bin/bash
trap 'trap - SIGTERM && kill 0' EXIT SIGTERM
make live >/dev/null &
spago build || exit 1
sleep 5
make watch-sass &
make watch-pandoc &
which trypurescript && make trypurescript 2>/dev/null &
if test -f .psc-ide-port && echo '{"command":"cwd"}' | ncat localhost "$(cat .psc-ide-port)" | grep -Fq '"resultType":"success"'; then
  watchexec -w output/.full-build --no-vcs-ignore --debounce 1500ms -r --shell=bash --postpone 'make assets-ps >/dev/null' &
  make watch-ps-ide
else
  make watch-ps
fi
wait
