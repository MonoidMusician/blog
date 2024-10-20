#!/usr/bin/env bash
cd -- "$(dirname $0)"
module=Script
case $1 in
  "--no-build")
    shift;;
  *)
    spago build >/tmp/spago_build 2>/tmp/spago_build || (cat /tmp/spago_build >&2 && exit 1);;
esac
case $1 in
  "-m")
    module=$2
    shift
    shift;;
esac
node --stack-size=8000 -e 'import("./output/'$module'/index.js").then(({main})=>main())' "$@"
