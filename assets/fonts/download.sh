#!/usr/bin/env bash
cd "$(dirname "$0")"
shopt -s globstar
set -euxo pipefail

FLAGS=--clobber

curl $FLAGS -OL http://sourceforge.net/projects/freetengwar/files/TengwarFont/TengwarTelcontar.008.zip
unzip -u -d TengwarTelcontar.008{,.zip}

curl $FLAGS -OL https://github.com/CatharsisFonts/Cormorant/releases/download/v3.609/Cormorant_Webfonts_v3.609.zip
unzip -u -d . Cormorant_Webfonts_v3.609.zip "*.woff2"
mkdir -p Cormorant
cp Cormorant_Webfonts_v3.609/*/Cormorant{Upright,}-*.woff2 Cormorant
rm -rf Cormorant_Webfonts_v3.609{,.zip}

curl $FLAGS -OL https://github.com/tonsky/FiraCode/releases/download/6.2/Fira_Code_v6.2.zip
unzip -u -d Fira_Code_v6.2{,.zip} "*.woff2"
mkdir -p fira_code
cp Fira_Code_v6.2/woff2/*.woff2 fira_code
rm -rf Fira_Code_v6.2{,.zip}

curl $FLAGS -OL https://github.com/alerque/libertinus/releases/download/v7.051/Libertinus-7.051.zip
unzip -u -d . Libertinus-7.051.zip "*.woff2"
mkdir -p Libertinus
cp Libertinus-7.051/static/WOFF2/*.woff2 Libertinus
rm -rf Libertinus-7.051{,.zip}

# https://neverpanic.de/blog/2014/03/19/downloading-google-web-fonts-for-local-hosting/


mkdir -p google
pushd google

curl -OL https://github.com/neverpanic/google-font-download/raw/refs/heads/master/google-font-download

rm -rf .bin
mkdir -p .bin
export PATH=$(realpath .bin):$PATH
if [[ -e /usr/local/opt/gnu-getopt/bin/getopt ]]; then
  ln -s /usr/local/opt/gnu-getopt/bin/getopt .bin/getopt
fi
if [[ -e /usr/local/opt/grep/bin/ggrep ]]; then
  ln -s /usr/local/opt/grep/bin/ggrep .bin/grep
fi

bash google-font-download --format woff2 -o Mulish.css "Mulish":{300,400,500,700}{,i} -l "$(echo {latin,cyrillic}{,-ext} | tr ' ' ',')"
bash google-font-download --format woff2 -o Oswald.css "Oswald":{200,300,400,700} -l "$(echo latin{,-ext} | tr ' ' ',')"
bash google-font-download --format woff2 -o Sofia.css "Sofia":400 -l latin
bash google-font-download --format woff2 -o Amaranth.css "Amaranth":{400,700}{,i} -l latin
bash google-font-download --format woff2 -o EB_Garamond_GR.css "EB Garamond":{400,700}{,i} -l "$(echo greek{,-ext} | tr ' ' ',')"
bash google-font-download --format woff2 -o Fira_Sans_GR.css "Fira Sans":{400,700}{,i} -l "$(echo greek{,-ext} | tr ' ' ',')"


popd

for font in **/*.ttf; do
  woff2_compress "$font"
done
