{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell rec {
  # nativeBuildInputs is usually what you want -- tools you need to run

  nativeBuildInputs = with pkgs.buildPackages; [
    nodejs purescript pandoc lilypond
    (haskellPackages.ghcWithPackages (pkgs: with pkgs; [ cabal-install ]))
    # (haskell.packages.ghc948.ghcWithPackages (pkgs: with pkgs; [ cabal-install ]))

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

    watchexec
  ];

  APPEND_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath nativeBuildInputs}";
  shellHook = ''
    export LD_LIBRARY_PATH="$APPEND_LIBRARY_PATH:$LD_LIBRARY_PATH"
    export PATH="$PWD/node_modules/.bin:$PATH"
    test -d node_modules || npm uninstall --no-save purescript
  '';
}
