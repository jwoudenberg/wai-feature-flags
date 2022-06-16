{
  description = "wai-feature-flags";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        app = pkgs.haskellPackages.callCabal2nix "wai-feature-flags" ./. { };
      in {
        defaultPackage = pkgs.haskell.lib.justStaticExecutables app;
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ app ];
          buildInputs = [
            # Haskell
            pkgs.cabal-install
            pkgs.haskellPackages.ghcid
            pkgs.haskellPackages.hpack
            pkgs.ormolu

            # Elm
            pkgs.elmPackages.elm
            pkgs.elmPackages.elm-format
            pkgs.elmPackages.elm-test
            pkgs.elmPackages.elm-live
          ];
        };
      });
}
