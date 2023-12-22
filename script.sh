#!/usr/bin/env bash
cd $(dirname $0)
spago build >/tmp/spago_build 2>/tmp/spago_build || (cat /tmp/spago_build >&2 && exit 1)
node -e 'import("./output/Script/index.js").then(({main})=>main())' "$@"

