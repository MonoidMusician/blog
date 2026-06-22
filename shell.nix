{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/3e41b24abd260e8f71dbe2f5737d24122f972158.tar.gz") {} }:

pkgs.mkShell rec {
  # nativeBuildInputs is usually what you want -- tools you need to run

  nativeBuildInputs = with pkgs.buildPackages; [
    # TODO: minimal and maximal shells?
    nodejs_26 purescript pandoc lilypond
    (haskell.packages.ghc984.ghcWithPackages (pkgs: with pkgs; [ cabal-install haskell-language-server ]))
    wasmtime wabt
    clang # for native quapteryx binary

    woff2

    # node-canvas support
    # https://discourse.nixos.org/t/node2nix-issues/10762/2
    pixman
    cairo
    pango
    libjpeg
    libpng
    libpng
    librsvg
    giflib
    libuuid
    libGL
    pkg-config # ??

    watchexec

    (
      let fenix = import (fetchTarball "https://github.com/nix-community/fenix/archive/dd2c80d0b88463ccc0402c86e9e72dbb354ac091.tar.gz") { };
      in fenix.complete.toolchain
    )
  ];

  APPEND_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath nativeBuildInputs}";
  shellHook = ''
    export LD_LIBRARY_PATH="$APPEND_LIBRARY_PATH:$LD_LIBRARY_PATH"
    export PATH="$PWD/node_modules/.bin:$PATH"
    export CLANG_WASM="${pkgs.buildPackages.clang.cc}/bin/clang" # for quaperyx WASM
    export LLD_WASM="${pkgs.buildPackages.llvmPackages.lld}/bin/wasm-ld" # for quaperyx WASM
    test -d node_modules || npm uninstall --no-save purescript
  '';
}
