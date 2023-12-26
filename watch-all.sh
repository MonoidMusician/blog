#!/bin/bash
trap terminate SIGINT
terminate(){
    pkill -SIGINT -P $$
    exit
}
spago build
make watch-prebuild &
sleep 5
make watch-sass &
make watch-pandoc &
make watch-ps <&0 &
which trypurescript && make trypurescript &
wait
