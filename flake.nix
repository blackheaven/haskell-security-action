# SPDX-FileCopyrightText: (C) Gautier DI FOLCO <gautier.difolco@gmail.com>
# SPDX-License-Identifier: ISC
{
  description = "A Nix Flake for haskell-security-action";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    cabal-audit.url = "github:mangoiv/cabal-audit";
  };

  outputs = { self, ... }@inputs:
    let
      supportedSystems = with inputs.flake-utils.lib.system;
        [
          x86_64-linux
          #x86_64-darwin
          #aarch64-linux
          #aarch64-darwin
        ];
    in inputs.flake-utils.lib.eachSystem supportedSystems (system:
      let pkgs = import inputs.nixpkgs { inherit system; };
      in rec {
        packages.github-action-scan-image = pkgs.dockerTools.buildImage {
          name = "blackheaven/haskell-security-action";
          tag = "latest";

          copyToRoot = pkgs.buildEnv {
            name = "image-root";
            paths =
              [ inputs.cabal-audit.packages.${system}.cabal-audit-static ];
            pathsToLink = [ "/bin" ];
          };
          config = { Cmd = [ "/bin/only-for-file-transfer" ]; };
        };

        defaultPackage = packages.github-action-scan-image;

        devShell = pkgs.mkShell { };
      });
}
