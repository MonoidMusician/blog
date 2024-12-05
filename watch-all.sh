#!/bin/bash
trap terminate SIGINT
terminate(){
    pkill -SIGINT -P $$
    exit
}
make live >/dev/null &
spago build || exit 1
sleep 5
make watch-sass &
make watch-pandoc &
which trypurescript && make trypurescript 2>/dev/null &
make watch-ps
wait
