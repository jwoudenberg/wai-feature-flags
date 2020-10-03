{ pkgs ? import ./nixpkgs.nix }:

pkgs.haskellPackages.shellFor {
  packages = p: [ (pkgs.callPackage ./default.nix { }) ];
  buildInputs = [
    pkgs.cabal-install
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.hpack
    pkgs.ormolu
  ];
}
