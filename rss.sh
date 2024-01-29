#!/usr/bin/env bash
../pandoc-rss/bin/pandoc-rss \
    -t 'MonoidMusicianʼs blog' \
    -d 'MonoidMusicianʼs blog' \
    -l https://cofree.coffee/~verity \
    -c 'GPLv3' \
    -n en-US \
    -s > /tmp/rss.xml \
    $(cat rss.txt)
node ./pandoc/lua/rss.js https://cofree.coffee/~verity < /tmp/rss.xml > rss.xml
