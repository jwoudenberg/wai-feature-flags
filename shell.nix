{ pkgs ? import ./nixpkgs.nix }:

pkgs.haskellPackages.shellFor {
  packages = p: [ (pkgs.callPackage ./default.nix { }) ];
  buildInputs = [
    # Haskell
    pkgs.cabal-install
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.hpack
    pkgs.ormolu

    # Elm
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-format
  ];
}
