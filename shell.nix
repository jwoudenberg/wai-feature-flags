{ pkgs ? import ./nixpkgs.nix }:

pkgs.haskellPackages.shellFor {
  packages = p: [ (pkgs.callPackage ./default.nix { }) ];
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
