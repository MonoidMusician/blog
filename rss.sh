#!/usr/bin/env bash
../pandoc-rss/bin/pandoc-rss \
    -t 'MonoidMusicianʼs blog' \
    -d 'MonoidMusicianʼs blog' \
    -l https://blog.veritates.love \
    -c 'GPLv3' \
    -n en-US \
    -s > /tmp/rss.xml \
    $(cat rss.txt)
node ./pandoc/lua/rss.js https://blog.veritates.love < /tmp/rss.xml > rss.xml
sed '/<description><!\[CDATA\[/,/\]\]><\/description>/d' < rss.xml | sed 's@https://blog.veritates.love/rss.xml@https://blog.veritates.love/rss-lite.xml@' > rss-lite.xml
