#!/bin/bash

set -euo pipefail

{
  THIS=$(basename "$0" .sh)

  if [[ "${1:-}" == "--watch" ]]; then
    watchexec -f "$THIS.c" -f "*.h" -f "$THIS.sh" -f "$THIS.node.js" -f "*.js" -r -c --shell=none -- "$0"
  else
    EXITCODE=0

    rm -f "$THIS"*.gcda "$THIS"*.gcno "$THIS".coverage*

    CLANG=clang
    if [[ -f /usr/local/Cellar/llvm/17.0.6_1/bin/clang ]]; then
      CLANG=/usr/local/Cellar/llvm/17.0.6_1/bin/clang
    fi

    # Preserve old text files for diff
    if [[ -f "$THIS.wat" ]]; then
      mv "$THIS.wat" "$THIS.wat.old"
    fi
    if [[ -f "$THIS.trace.json" ]]; then
      cp "$THIS.trace.json" "$THIS.trace.json.old"
    fi
    rm -f "$THIS.wasm" "$THIS.wat"

    # https://surma.dev/things/c-to-webassembly/
    # https://aransentin.github.io/cwasm/ ?
    declare -a OPTIONS
    OPTIONS=(
      "-O3" # optimize!
      "-g" # debug symbols
      "-flto" # link time optimizations
      "-nostdlib" # no standard library, just WASM builtins
      "-Wl,--no-entry"
      "-Wl,--lto-O3"
      # specify some WASM things
      "-mmultivalue" "-Xclang" "-target-abi" "-Xclang" "experimental-mv"
      "-mbulk-memory"
      # "-msimd128" # does not seem to help noticeably?

      # "-mmutable-globals" # does nothing
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
      "walloc.c" \
      "$THIS.c"

    NATIVE_OPTIONS=(
      -O3 # optimize!
      -g3 # debug symbols
      # --coverage
      -DCLI
      -Wno-unknown-attributes
      -Wno-int-to-void-pointer-cast
      # optimize for current processor
      -mtune=native
      # -march=native gives an instruction that valgrind cannot handle lol
    )

    # if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    #   NATIVE_OPTIONS+=( -pg )
    # fi

    clang "${NATIVE_OPTIONS[@]}" "$THIS.c" -o "$THIS.native"

    # Convert binary to text representation
    # (prefer S-exprs, inline exports)
    wasm2wat \
      --fold-exprs \
      --inline-exports \
      "$THIS.wasm" -o "$THIS.wat"

    # Calculate the diff
    if [[ -f "$THIS.wat.old" ]]; then
      printf '%s' "WAT: "
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
        if test -f ".$THIS.wat.diff"; then rm -f ".$THIS.wat.diff"; fi
        # Print stats line
        head -n 2 "$THIS.wat.diff" \
          | tail -n 1 \
          | cut -d ',' -f 2- \
          | cut -c 2-
      fi
      rm -f "$THIS.wat.old"
    fi

    rm -f "$THIS.wasm.not.c"
    if which wasm-decompile >/dev/null 2>/dev/null; then wasm-decompile "$THIS.wasm" > "$THIS.wasm.not.c" || true; fi

    # LLVM textual IR: "$THIS.ll"
    $CLANG \
      --target=wasm32 \
      -emit-llvm \
      -c \
      -S \
      "$THIS.c" || true

    # Cranelift IR
    if which wasmtime >/dev/null 2>/dev/null; then wasmtime compile -O opt-level=2 --emit-clif clif "$THIS.wasm" || true; fi
    rm -f clif/array_to_wasm_*.clif clif/wasm_to_array_trampoline_*.clif

    node "$THIS.node.js" || EXITCODE=$?
    # while ! node --inspect-brk "$THIS.node.js"; do
    #   read -p "Restart?"
    # done

    if [[ -f "$THIS.trace.json.old" ]]; then
      printf '%s' "Trace: "
      # Preserve old diff
      if test -f "$THIS.trace.json.diff"; then mv "$THIS.trace.json.diff" ".$THIS.trace.json.diff"; fi
      # Calculate diff
      if git diff --no-index \
        --diff-algorithm=histogram \
        --stat --patch --unified=5 \
        --ignore-all-space \
        "$THIS.trace.json.old" "$THIS.trace.json" > "$THIS.trace.json.diff";
      then
        # Restore old diff
        if test -f ".$THIS.trace.json.diff"; then mv ".$THIS.trace.json.diff" "$THIS.trace.json.diff"; fi
        echo "No change"
      else
        # Remove old diff
        if test -f ".$THIS.trace.json.diff"; then rm -f ".$THIS.trace.json.diff"; fi
        # Print stats line
        head -n 2 "$THIS.trace.json.diff" \
          | tail -n 1 \
          | cut -d ',' -f 2- \
          | cut -c 2-
      fi
      rm -f "$THIS.trace.json.old"
    fi

    exit "$EXITCODE"
  fi
}; exit $?
