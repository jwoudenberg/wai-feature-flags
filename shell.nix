let pkgs = import ./nixpkgs.nix;

in pkgs.haskellPackages.shellFor {
  packages = p: [ (import ./default.nix) ];
  buildInputs = [
    # Haskell
    pkgs.cabal-install
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.hpack
    pkgs.ormolu
    pkgs.zlib

    # Elm
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-format
    pkgs.elmPackages.elm-test
    pkgs.elmPackages.elm-live
  ];
}
