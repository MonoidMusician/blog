#!/usr/bin/env bash
cd -- "$(dirname $0)"
case $1 in
  "--no-build")
    shift;;
  *)
    spago build >/tmp/spago_build 2>/tmp/spago_build || (cat /tmp/spago_build >&2 && exit 1);;
esac
node -e 'import("./output/Script/index.js").then(({main})=>main())' "$@"
