{
  devTools ? true,
}:
let
  pkgs = import <nixpkgs> { };
  myHaskellPackages = pkgs.haskellPackages.extend (
    final: prev: { aoc24 = import ./. { inherit pkgs; }; }
  );
in
myHaskellPackages.shellFor {
  packages = p: [ p.aoc24 ];
  nativeBuildInputs =
    with pkgs;
    [
      ghc
      cabal-install
      just
    ]
    ++ lib.optional devTools [
      niv
      hlint
      ormolu
      nixfmt-rfc-style
      (ghc.withPackages (p: [
        p.haskell-language-server
        p.cabal-fmt.bin
      ]))
    ];
}
