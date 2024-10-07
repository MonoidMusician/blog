#!/bin/bash
trap terminate SIGINT
terminate(){
    pkill -SIGINT -P $$
    exit
}
spago build --purs-args "-g corefn,js"
make watch-prebuild &
sleep 5
make watch-sass &
make watch-pandoc &
#make watch-ps <&0 &
#make watch-ps &
# which trypurescript && make trypurescript &
#make watch-ps
wait
