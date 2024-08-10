# SPDX-FileCopyrightText: (C) Gautier DI FOLCO <gautier.difolco@gmail.com>
# SPDX-License-Identifier: CC0-1.0
{
  description = "A Nix Flake for haskell-security-action";
  # To use this Nix flake you may need to enable Nix flake support for your user with:
  #     echo >>~/.config/nix/nix.conf "experimental-features = nix-command flakes"
  # WARNING: be sure that `nix --version` is greater or equal to 2.18,
  # otherwise nix may not support some attributes used in flake.lock.

  # For any input, one can:
  # Update to the latest commit:
  #     nix flake lock --update-input nixpkgs
  # Or to a specific commit (eg. a green one on https://status.nixos.org):
  #     nix flake lock --override-input nixpkgs github:NixOS/nixpkgs/72da83d9515b43550436891f538ff41d68eecc7f
  # Or to a commit (in /etc/nix/registry.json) of a NixOS host:
  #     nix flake lock --override-input nixpkgs flake:nixpkgs
  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";

    # For trying to hit cache.iog.io one would have
    # to follow haskell.nix's nixpkgs pinned version,
    # but it may be a few months old, so pin it here instead.
    #nixpkgs.follows = "haskell-nix/nixpkgs";
    nixpkgs.url = "flake:nixpkgs";
    haskell-nix.inputs.nixpkgs.follows = "nixpkgs";

    # Convenient Nix Flake utilities, like flake-utils.lib.eachSystem.
    flake-utils.url = "github:numtide/flake-utils";

    # Git pre-commit hooks.
    git-hooks.url = "github:cachix/git-hooks.nix";
    git-hooks.inputs.nixpkgs.follows = "nixpkgs";

    cabal-audit = {
      url = "github:mangoiv/cabal-audit";
      flake = false;
    };
    haskell-security-advisories = {
      url = "github:haskell/security-advisories?submodules=1";
      flake = false;
    };
  };

  # For printing the available outputs:
  #     $ nix -L flake show --allow-import-from-derivation
  # Note that multiple-systems are enabled hence it can only work
  # without IFD because of https://github.com/NixOS/nix/issues/4265,
  # ie. with a non-null materialized=
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
      let
        pkgs = import inputs.nixpkgs {
          inherit system;
          config = inputs.haskell-nix.config;
          overlays = [
            inputs.haskell-nix.overlay
            (import nix/nixpkgs/overlays/static.nix)
            #(f: p: { gtkpod = f.hello; })
          ];
        };

        # A standard library of Nix expressions.
        lib = inputs.nixpkgs.lib;

        # A library of Nix expressions internal to haskell.nix.
        inherit (pkgs.haskell-nix) haskellLib;

        rawCabalProject = lib.readFile ./cabal.project;

        # haskell.nix's main entry point
        project = pkgs.haskell-nix.cabalProject' [
          ({ config, pkgs, ... }: {
            name = "github-action-scan";

            # Filter-in input files to avoid unnecessary rebuilds
            # after changing any file tracked in Git that is not actually used by cabalProject.
            src = with lib.fileset;
              toSource {
                root = ./.;
                fileset = unions [
                  ./LICENSE
                  ./cabal.project
                  #./cabal.project.freeze
                  ./github-action-scan.cabal
                  (fileFilter (file: lib.any file.hasExt [ "hs" ]) ./app)
                ];
              };

            # By default plan evaluation is done on the build system.
            #evalSystem = "x86_64-linux";

            # Retrieve compiler-nix-name from cabal.project's with-compiler field.
            # Eg. `with-compiler: ghc-9.4.7` becomes "ghc947"
            compiler-nix-name = lib.replaceStrings [ "-" "." ] [ "" "" ]
              (lib.head (lib.concatLists (lib.filter (l: l != null)
                (builtins.map (l: builtins.match "^with-compiler: *(.*)" l)
                  (lib.splitString "\n" rawCabalProject)))));

            # Download GHC from Nixpkgs' binary cache instead of IOHK's
            # which would be done by using: pkgs.haskell-nix.compiler
            # Beware that if any dependency has `build-depends: ghc`
            # then` reinstallableLibGhc = false` is required
            # to avoid missing `genprimopcode`.
            # See https://github.com/input-output-hk/haskell.nix/issues/1809#issuecomment-1358469589
            compilerSelection = pkgs:
              # Avoid:
              #   error: attribute 'buildGHC' missing
              #   at /nix/store/wlvllg9m9bklpzs4fk83w1lki3jwrjhg-source/builder/ghc-for-component-wrapper.nix:27:10:
              #       26|   haddock        = if stdenv.hostPlatform.isLinux && stdenv.targetPlatform.isMusl && !haskellLib.isNativeMusl
              #       27|     then ghc.buildGHC
              #         |          ^
              #       28|     else ghc;
              #lib.mapAttrs (name: ghc: ghc // { buildGHC = ghc; })
              #  pkgs.haskell.compiler;
              pkgs.haskell-nix.compiler;

            # Pinning the index-state of Hackage,
            # instead of using the latest known by haskell.nix,
            # removes haskell.nix from interfering too much into the reproducibility.
            # It also enables to materialize the plan-nix.
            index-state = haskellLib.parseIndexState rawCabalProject;

            # Materializing a project means caching the nix files
            # generated from the *.cabal/stack.yaml/package.yaml files.
            # To update:
            #     $ nix run .#update-nix-cache-haskell-nix-materialized
            # It's only checked in ciJobs.
            materialized =
              if builtins.pathExists nix/cache/haskell.nix/materialized then
                nix/cache/haskell.nix/materialized
              else
                null;

            # Using inputMap for each source-repository-package of cabal.project
            # leverages Nix Flake's inputs to automatically get their rev and sha256 hashes
            # and to check upstreams for updates (using `nix flake update`
            # or `nix flake lock --update-input <input>`).
            inputMap = let
              # findCabalFiles (in nix-tools/nix-tools/cabal2nix/Main.hs)
              # always prefers package.yaml over *.cabal,
              # but when the resulting *.cabal file is different
              # than a previously existing one,
              # the build fails with an error like this one:
              #     crawlerIsidore.cabal was modified manually, please use --force to overwrite.
              # Hence just remove this out-of-sync package.yaml.
              removePackageYaml = src:
                pkgs.symlinkJoin {
                  name = "removePackageYaml-patched";
                  paths = [ src ];
                  postBuild = "rm $out/package.yaml";
                  # Preserve rev for the inputMap
                  passthru.rev = src.rev;
                };
              applyPatches = inputName: patches:
                pkgs.buildPackages.applyPatches {
                  name = "${inputName}-patched";
                  src = inputs.${inputName};
                  inherit patches;
                } // {
                  inherit (inputs.${inputName}) rev;
                };
            in {
              "https://github.com/MangoIV/cabal-audit.git" = inputs.cabal-audit;
              "https://github.com/haskell/security-advisories.git" =
                inputs.haskell-security-advisories;
              #"https://github.com/haskell/tar.git" = applyPatches "tar" [
              #  nix/haskell.nix/patches/tar/0001-compatibility-static-build-remove-QuasiQuotes.patch
              #];
            };

            # Default project configuration.
            modules = [
              ({ pkgs, ... }:
                {
                  # Make the closure dependency significantly larger
                  # but avoid missing genprimopcode with compilerSelection = p: pkgs.haskell.compiler
                  #reinstallableLibGhc = false;

                  # Link with OpenBLAS optimized libraries.
                  # WARNING: OpenBLAS must only be used by trusted code
                  # it is inherently unsuitable for security-conscious applications.
                  # See nixpkgs/pkgs/development/libraries/science/math/openblas/default.nix
                  #packages.hmatrix.flags.openblas = true;
                })
            ];

            # Shell configuration shared by the default shell
            # and all shells from the flake.variants.
            shell = {
              # By default haskell.nix does not force cabal-install (by setting CABAL_CONFIG=)
              # to use the packages selected by project.plan-nix and available in `ghc-pkg list`,
              # leaving cabal-install in charge of provisioning Haskell packages,
              # which gives more flexibility when developing.
              #exactDeps = false;
              #allToolDeps = true;

              # haskell.nix provisions (in `ghc-pkg list`)
              # the **dependencies** of the packages selected here,
              # which are also **not** selected here.
              #
              # By default haskell.nix selects all _local_ packages here
              # (packages from both the `packages` and the `source-repository-package` stanzas)
              # which therefore excludes `source-repository-package`s from being provisioned,
              #
              # Note that it means `cabal update` must be run to get an index-state.
              # and be able to download and build missing dependencies
              # that depend on `source-repository-package`s.
              # Eg. gargantext's dependency `hstatistics` depends on `hmatrix`,
              # but hmatrix is a `source-repository-package`
              # hence `hstatistics` is not provisioned by haskell.nix.
              #packages = ps: lib.attrValues (haskellLib.selectLocalPackages ps);

              # Add in this list any development tool needed
              # that is not expected to come from the developers' own system.
              nativeBuildInputs = [
                pkgs.haskell.packages.${config.compiler-nix-name}.cabal-install
                #pkgs.haskell.packages.${config.compiler-nix-name}.ghcid
                #pkgs.haskell.packages.${config.compiler-nix-name}.haskell-language-server
                #pkgs.haskell.packages.${config.compiler-nix-name}.hlint
              ];

              shellHook = ''
                cat >&2 ${
                  pkgs.buildPackages.writeText "shellEnterMessage.txt" ''
                    **Warning**
                      This Nix development shell is not configured to provision
                      `cabal.project`'s `source-repository-package`s and their reverse dependencies,
                      therefore when `source-repository-package`s are used
                      a `cabal update` has to be run manually to fetch an `index-state`
                      before running `cabal build`.
                  ''
                }
              '' + self.checks.${system}.git-hooks-check.shellHook;

              # When true, builds a Hoogle documentation index of all dependencies,
              # and provides a "hoogle" command to search the index.
              #withHoogle = true;
            };

            # Variants to the default project configuration above.
            # They're accessed in the flake's outputs with their name prefixed.
            #     $ nix -L build .#haskell-nix-ghc:github-action-scan:exe:github-action-scan-exe
            # Or via `legacyPackages.${system}.project.projectVariants`:
            #     $ nix -L build .#project.projectVariants.haskell-nix-ghc.components.executables.github-action-scan-exe
            flake.variants = {
              # For using profiling versions of Haskell packages:
              #     $ nix develop .#profiling
              profiling = {
                modules = [{
                  # Applies to all packages of the Haskell closure. For instance:
                  #     $ nix eval .#project.hsPkgs.containers.components.library.config.enableProfiling
                  #     false
                  #     $ nix eval .#project.projectVariants.profiling.hsPkgs.containers.components.library.config.enableProfiling
                  #     true
                  enableProfiling = true;
                  enableLibraryProfiling = true;
                }];
              };

              # For using haskell.nix's GHC:
              #     $ nix -L develop .#haskell-nix-ghc
              #     $ nix -L build .#haskell-nix-ghc:github-action-scan:exe:github-action-scan-exe
              haskell-nix-ghc = {
                compilerSelection =
                  lib.mkForce (pkgs: pkgs.haskell-nix.compiler);
                materialized = lib.mkForce null;
                modules = [{
                  # Revert to the default
                  reinstallableLibGhc = lib.mkForce true;
                }];
              };
            };

            # Enable coverage report in `ciJobs` and `hydraJobs` flake outputs.
            # For building the coverages:
            #     $ nix -L build .#ciJobs.x86_64-linux.coverage.github-action-scan
            # Alas, coverage fails to find hpc when using Nixpkgs' GHC:
            # github-action-scan> no such hpc command
            # So for now the haskell-nix-ghc variant has to be used:
            #     $ nix -L build .#project.projectVariants.haskell-nix-ghc.flake"'".ciJobs.coverage.github-action-scan
            #     $ firefox result/share/hpc/vanilla/html/
            flake.doCoverage = true;
            # Defaults to haskellLib.selectProjectPackages which select cabal.project's `packages`
            # but rather make all `source-repository-package`s also available in `ciJobs.coverage.*`
            flake.packages = haskellLib.selectLocalPackages;
            # FIXME: haskell.nix uses a `doCoverage = lib.mkDefault true` which causes conflicts.
            flake.coverageProjectModule = {
              modules = [{
                packages = let
                  packageNames = project:
                    builtins.attrNames (config.flake.packages project.hsPkgs);
                in lib.genAttrs (packageNames config)
                (_: { doCoverage = true; });
              }];
            };

            # Dead-code analysis
            #     $ nix -L build .#weeder-project-analysis
            #     $ bat result
            # Note that there may be false positives
            # and that some file location may be wrong.
            weeder = {
              packages = ps:
                haskellLib.selectProjectPackages ps // lib.getAttrs [
                  #"epo-api-client"
                ] ps;
              # See https://github.com/ocharles/weeder?tab=readme-ov-file#configuration-options
              settings = {
                roots = [
                  "^Main.main$"
                  # Automatically generated by Cabal
                  "^Paths_.*"
                ];
                root-instances = [ ];
                # Consider all instances of type classes as roots.
                type-class-roots = true;
                unused-types = true;
              };
            };

            # Make some variables available to all project modules
            _module.specialArgs = {
              # Use specialArgs to avoid infinite recursion
              # when `inputs` is used in `imports`.
              inherit inputs;
            };
            _module.args = {
              inherit system;
              inherit (pkgs.haskell-nix) haskellLib;
            };
          })

          # project modules
          (import nix/haskell.nix/modules/weeder.nix)
          (import nix/haskell.nix/modules/static.nix)
        ];

        projectFlake = project.flake { };
      in {
        legacyPackages = pkgs // {
          # For exploring the project:
          #     $ nix --extra-experimental-features 'flakes repl-flake' repl .
          #     nix-repl> :lf .
          #     nix-repl> legacyPackages.x86_64-linux.project.<TAB>
          inherit project;
        };

        # For building a component of this project:
        #     $ nix -L build .#github-action-scan:exe:github-action-scan-exe
        packages = projectFlake.packages // {
          weeder-analysis = project.args.weeder.analysis;
          github-action-scan-image = pkgs.dockerTools.buildImage {
            name = "blackheaven/haskell-security-action";
            tag = "latest";

            copyToRoot = pkgs.buildEnv {
              name = "image-root";
              paths = [
                projectFlake.packages."static:github-action-scan:exe:github-action-scan"
              ];
              pathsToLink = [ "/bin" ];
            };
            config = { Cmd = [ "/bin/only-for-file-transfer" ]; };
          };
        };

        # For entering the default development shell:
        #     $ nix -L develop
        #     $ cabal build --disable-optimization
        #
        # For entering the development shell variant `profiling`:
        #     $ nix -L develop .#profiling
        #     $ cabal run --enable-profiling github-action-scan-exe
        devShells = projectFlake.devShells;

        apps = projectFlake.apps // {
          # For updating nix/cache/haskell.nix/materialized:
          #     $ nix run .#update-nix-cache-haskell-nix-materialized
          # It needs to be updated when cabal.freeze or any other input to the plan-nix changes.
          # It's only OK to use it when the plan-nix does not depend on `system`.
          # See https://github.com/input-output-hk/haskell.nix/blob/master/docs/tutorials/materialization.md#when-is-it-ok-to-materialize
          update-nix-cache-haskell-nix-materialized =
            inputs.flake-utils.lib.mkApp {
              drv = pkgs.writeShellApplication {
                name = "update-nix-cache-haskell-nix-materialized";
                text = ''
                  set -eux
                  git diff --exit-code
                  ${
                    (project.appendModule {
                      materialized = lib.mkForce null;
                    }).plan-nix.passthru.generateMaterialized
                  } nix/cache/haskell.nix/materialized
                  git add --all nix/cache/haskell.nix/materialized
                  git commit -m "nix: update nix/cache/haskell.nix/materialized"
                '';
              };
            };

          # Register the default project's toolchain,
          # to prevent nix-collect-garbage from removing them from the Nix store.
          # Note that it does not register the roots of the `projectVariants`.
          update-nix-cache-haskell-nix-gc-roots = inputs.flake-utils.lib.mkApp {
            drv = pkgs.writeShellApplication {
              name = "update-nix-cache-haskell-nix-gc-roots";
              text = ''
                set -eux
                rm -rf nix/cache/haskell.nix/gc-roots
                nix-store --add-root nix/cache/haskell.nix/gc-roots/default --indirect --realise ${project.roots}
              '';
            };
          };

        };

        # For running all checks (very slow):
        #     $ nix -L flake check
        #
        # For building a specific check of the project:
        #     $ nix -L build .#project.hsPkgs.github-action-scan.components.tests.github-action-scan-test
        #     $ result/bin/github-action-scan-test
        #
        # Alternatively, but slower:
        #     $ nix -L build .#checks.x86_64-linux.github-action-scan:test:github-action-scan-test
        #     $ bat result/test-stdout
        #
        # See names from:
        #     $ nix -L flake show --allow-import-from-derivation
        # Alas, currently coverage reports do not work (can't find hpc)
        # with nixpkgs.haskellPackages' GHC, so haskell.nix's GHC has to be used:
        #     $ # nix -L build .#project.projectCoverageReport
        #     $ nix -L build .#project.projectVariants.haskell-nix-ghc.projectCoverageReport
        #     $ firefox result/share/hpc/vanilla/html/index.html
        checks = projectFlake.checks // {
          git-hooks-check = inputs.git-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              cabal-fmt.enable = true;
              ormolu.enable = true;
              hlint.enable = true;
              # nixfmt-rfc-style.enable = true;
              # nixfmt-classic.enable = true;
              nixfmt.enable = true;
            };
          };
        };

        # Jobs for the Nix-based continuous integration system: Hydra
        # https://nixos.wiki/wiki/Hydra
        # Note that haskell.nix always set `checkMaterialization = true` in `hydraJobs`.
        #hydraJobs = projectFlake.hydraJobs;

        # `ciJobs` is like `hydraJobs` but with `${system}` first
        # so that the IFDs will not have to run for systems
        # we are not testing (placement of `${system}` is done by `flake-utils.eachSystem`
        # and it treats `hydraJobs` differently from the other flake.
        # Note that haskell.nix always set `checkMaterialization = true` in `ciJobs`.
        ciJobs = projectFlake.ciJobs;
      });

  # Ask users to set Nix config entries in ~/.local/share/nix/trusted-settings.json.
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Only useful when using the haskell-nix-ghc variant.
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];

    # haskell.nix translates to Nix expressions:
    # - the build plan usually generated in `dist-newstyle/cache/plan.json` by `cabal configure`
    # - and the `.cabal`/`stack.yaml`/`package.yaml` files of projects.
    #
    # haskell.nix can either generate those Nix expressions on-demand
    # by calling its nix-tools' make-install-plan and cabal-to-nix,
    # hence importing them from a derivation (IFD).
    # Or import pre-generated files whenever project's materialized= attribute is not null,
    # and then no longer needs to allow IFD.
    allow-import-from-derivation = "true";
  };
}
