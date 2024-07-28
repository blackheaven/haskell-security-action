{
  description = "github-action-scan";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    cabal-audit = {
      url = "github:mangoiv/cabal-audit";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
          hlib = pkgs.haskell.lib.compose;
          haskellPackages = pkgs.haskell.packages.ghc982.override {
            overrides = import "${inputs.cabal-audit}/nix/haskell-overlay.nix" {inherit hlib;};
          };
        gas = haskellPackages.callCabal2nix "github-action-scan" ./. { };
      in rec {
        packages.github-action-scan = gas;
        packages.github-action-scan-static = pkgs.haskell.lib.justStaticExecutables gas;

        defaultPackage = packages.github-action-scan;

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            pkgs.haskell.packages.ghc982.haskell-language-server
            ghcid
            cabal-install
            cabal-audit
          ];
          inputsFrom = [ self.defaultPackage.${system}.env ];
        };
      });
}
