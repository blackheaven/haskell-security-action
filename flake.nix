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
          haskellPackages = pkgs.haskellPackages.override {
            overrides = import "${inputs.cabal-audit}/nix/haskell-overlay.nix" {inherit hlib;};
          };
        gas = haskellPackages.callCabal2nix "github-action-scan" ./. { };
        staticLinking = hlib.appendConfigureFlags [
          "--ghc-option=-optl=-static"
          "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
          "--extra-lib-dirs=${pkgs.zlib.static}/lib"
          "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
        ];
      in rec {
        packages.github-action-scan = gas;
        packages.github-action-scan-static = staticLinking (pkgs.haskell.lib.justStaticExecutables gas);

        defaultPackage = packages.github-action-scan;

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            # haskell-language-server
            ghcid
            cabal-install
            cabal-audit
          ];
          inputsFrom = [ self.defaultPackage.${system}.env ];
        };
      });
}
