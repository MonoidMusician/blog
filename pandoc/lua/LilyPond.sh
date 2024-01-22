#!/bin/bash
lilypond --svg -dno-point-and-click --output="$2" "${@:3}" "$1".ly > "$2".out 2>&1 || (cat "$2".out; exit 1)
