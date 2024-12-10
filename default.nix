{
  pkgs ? import (import nix/sources.nix).nixpkgs { },
  optimize ? false,
  profile ? false,
}:
let
  hlib = pkgs.haskell.lib.compose;
  lib = pkgs.lib;
in
lib.pipe
  (pkgs.haskellPackages.callCabal2nix "AoC24" (lib.cleanSource (
    lib.sourceFilesBySuffices ./. [
      ".hs"
      ".cabal"
      "LICENSE"
    ]
  )) { })
  (
    [
      hlib.dontHyperlinkSource
      hlib.dontCoverage
      hlib.dontHaddock
      (hlib.appendConfigureFlags [
        "--ghc-option=+RTS"
        "--ghc-option=-A256m" # allocation area size
        "--ghc-option=-n2m" # allocation area chunksize
        "--ghc-option=-RTS"
      ])
      (hlib.overrideCabal (old: {
        enableParallelBuilding = true;
      }))
      (hlib.appendConfigureFlag [ "--ghc-options=-threaded" ])
    ]
    # optimization
    ++ (if optimize then [ (hlib.appendConfigureFlags [ "-O2" ]) ] else [ hlib.disableOptimization ])
    # profiling
    ++ (
      if profile then
        [
          hlib.enableExecutableProfiling
          hlib.enableLibraryProfiling
        ]
      else
        [
          hlib.disableExecutableProfiling
          hlib.disableLibraryProfiling
        ]
    )
    # arch64
    ++ lib.optional pkgs.stdenv.isAarch64 (
      hlib.appendConfigureFlag "--ghc-option=-fwhole-archive-hs-libs"
    )
  )
