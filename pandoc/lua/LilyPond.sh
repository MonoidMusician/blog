#!/bin/bash
lilypond --svg --output=$1 $1.ly > $1.out 2>&1 || (cat $1.out; exit 1)
