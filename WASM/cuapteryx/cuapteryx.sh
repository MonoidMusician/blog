#!/bin/bash

set -euo pipefail

{
  THIS=$(basename "$0" .sh)

  if [[ "${1:-}" == "--watch" ]]; then
    watchexec -f "$THIS.c" -f "$THIS.sh" -f "$THIS.node.js" -r -c --shell=none -- "$0"
  else
    CLANG=clang
    if [[ -f /usr/local/Cellar/llvm/17.0.6_1/bin/clang ]]; then
      CLANG=/usr/local/Cellar/llvm/17.0.6_1/bin/clang
    fi

    # Preserve old text file for diff
    if [[ -f "$THIS.wat" ]]; then
      mv "$THIS.wat" "$THIS.wat.old"
    fi
    rm -f "$THIS.wasm" "$THIS.wat"

    # https://surma.dev/things/c-to-webassembly/
    # https://aransentin.github.io/cwasm/ ?
    declare -a OPTIONS
    OPTIONS=(
      "-O3"
      "-flto"
      "-nostdlib"
      "-mmultivalue" "-Xclang" "-target-abi" "-Xclang" "experimental-mv"
      "-mbulk-memory"
      "-Wl,--no-entry"
      "-Wl,--lto-O3"
      # "-Wl,-z,stack-size=0"
      # "-Wl,--initial-heap=0"
      # "-Wl,--extra-features=mutable-globals" # does nothing

      "-Wl,--export-all"
      # "-Wl,--export-dynamic"
      # "-Wl,--export-memory"
      # "-Wl,--export=input_words"
      # "-Wl,--export=output_words"
      # "-Wl,--export=force_write"
    )

    # Command to compile C -> (LLVM) -> WASM
    $CLANG \
      --target=wasm32 \
      "${OPTIONS[@]}" \
      -o "$THIS.wasm" \
      "$THIS.c"

    # Convert binary to text representation
    # (prefer S-exprs, inline exports)
    wasm2wat \
      --fold-exprs \
      --inline-exports \
      "$THIS.wasm" -o "$THIS.wat"

    # Calculate the diff
    if [[ -f "$THIS.wat.old" ]]; then
      # Preserve old diff
      if test -f "$THIS.wat.diff"; then mv "$THIS.wat.diff" ".$THIS.wat.diff"; fi
      # Calculate diff
      if git diff --no-index \
        --diff-algorithm=histogram \
        --stat --patch --unified=5 \
        --ignore-all-space \
        "$THIS.wat.old" "$THIS.wat" > "$THIS.wat.diff";
      then
        # Restore old diff
        if test -f ".$THIS.wat.diff"; then mv ".$THIS.wat.diff" "$THIS.wat.diff"; fi
        echo "No change"
      else
        # Remove old diff
        if test -f ".$THIS.wat.diff"; then rm ".$THIS.wat.diff"; fi
        # Print stats line
        head -n 2 "$THIS.wat.diff" \
          | tail -n 1 \
          | cut -d ',' -f 2- \
          | cut -c 2-
      fi
      rm "$THIS.wat.old"
    fi

    if which wasm-decompile >/dev/null; then wasm-decompile "$THIS.wasm" > "$THIS.wasm.not.c"; fi

    # LLVM textual IR
    $CLANG \
      --target=wasm32 \
      -emit-llvm \
      -c \
      -S \
      "$THIS.c" || true

    node "$THIS.node.js"
  fi
}; exit $?
