#!/bin/bash
trap terminate SIGINT
terminate(){
    pkill -SIGINT -P $$
    exit
}
make live >/dev/null &
spago build
sleep 5
make watch-sass &
make watch-pandoc &
which trypurescript && make trypurescript &
make watch-ps
wait
