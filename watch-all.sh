#!/bin/bash
trap terminate SIGINT
terminate(){
    pkill -SIGINT -P $$
    exit
}
make watch-sass &
make watch-pandoc &
make watch-ps <&0 &
wait
