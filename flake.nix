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
          haskellPackages= pkgs.haskell.packages.ghc982.override {
            overrides = import "${inputs.cabal-audit}/nix/haskell-overlay.nix" {inherit hlib;};
          };
        baseImage = pkgs.dockerTools.buildImage {
          name = "blackheaven/haskell-security-action";
          tag = "latest";

          copyToRoot = pkgs.buildEnv {
            name = "image-root";
            paths = [
              pkgs.ghc.out
              pkgs.gitMinimal.out
            ];
            pathsToLink = [ "/bin" "/" ];
          };
          config = {
            Env = [
              "LOCALE_ARCHIVE=${pkgs.glibcLocalesUtf8}/lib/locale/locale-archive"
              "LC_TIME=en_US.UTF-8"
              "LANG=en_US.UTF-8"
              "LANGUAGE=en"
              "LC_ALL=en_US.UTF-8"
              # "GIT_DISCOVERY_ACROSS_FILESYSTEM=1"
            ];
            Volumes = { "/repository" = { }; };
            WorkDir = "/repository";
          };
        };
        gas = haskellPackages.callCabal2nix "github-action-scan" ./. { };
      in rec {
        packages.github-action-scan = gas;
        packages.github-action-scan-image = pkgs.dockerTools.buildImage {
          name = "blackheaven/haskell-security-action";
          tag = "latest";
          fromImage = baseImage;

          copyToRoot = pkgs.buildEnv {
            name = "image-root";
            paths = [ (pkgs.haskell.lib.justStaticExecutables gas) ];
            pathsToLink = [ "/bin" "/" ];
          };
          config = {
            Cmd = [ "/bin/github-action-scan" ];
          };
        };

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
