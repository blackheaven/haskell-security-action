{ config, pkgs, lib, haskellLib, ... }:
let
  cfg = config.weeder;
  toml = pkgs.formats.toml { };
in {
  options.weeder = {
    analysis = lib.mkOption {
      type = lib.types.package;
      default = null;
      internal = true;
    };
    packages = lib.mkOption {
      type = lib.types.unspecified;
      default = haskellLib.selectProjectPackages;
    };
    settings = lib.mkOption {
      type = toml.type;
      default = { };
    };
  };

  config = {
    modules = [{
      # Enable writeHieFiles by default.
      # See https://github.com/input-output-hk/haskell.nix/issues/298#issuecomment-767936405
      # for what's going on in this trick.
      options.packages = lib.mkOption {
        type = lib.types.attrsOf (lib.types.submodule ({ config, ... }: {
          config = lib.mkIf ((cfg.packages {
            ${config.package.identifier.name} = config.package;
          }) ? ${config.package.identifier.name}) {
            writeHieFiles = lib.mkForce true;
          };
        }));
      };
    }];

    weeder.analysis = pkgs.runCommand "weeder-${config.name}" {
      buildInputs = [
        pkgs.glibcLocales
        pkgs.haskell.packages.${config.compiler-nix-name}.weeder
      ];
      allHieFiles = pkgs.linkFarm "allHieFiles" (lib.concatMap (package:
        let
          lib-hies = lib.optional (package.components ? library) {
            name = "${package.identifier.name}-library";
            path = package.components.library.hie;
          };

          exe-hies = lib.concatMap (exe:
            lib.optional (package.components.exes.${exe} ? hie) {
              name = "${package.identifier.name}-exe-${exe}";
              path = package.components.exes.${exe}.hie;
            }) (lib.attrNames package.components.exes);

          test-hies = lib.concatMap (test-name:
            let
              test = package.components.tests.${test-name};
              is-doctest =
                # doctest tests build _all_ components of a package.
                # The GHC id of these packages will be different,
                # which means that when we run weeder, all this code
                # will be uncalled.  These are false positives, so
                # we don't include hie files from anything that
                # depends on `doctest`.
                lib.any (x: x.identifier.name or "" == "doctest")
                test.config.depends;
            in lib.optional (!is-doctest) {
              name = "${package.identifier.name}-test-${test-name}";
              path = test.hie;
            }) (lib.attrNames package.components.tests);
        in lib-hies ++ exe-hies ++ test-hies)
        (builtins.attrValues (cfg.packages config.hsPkgs)));
    } ''
      export LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8 LANGUAGE=en_US.UTF-8
      cd $allHieFiles
      weeder >$out --no-default-fields --config ${
        toml.generate "weeder.toml" cfg.settings
      } ||
      test $? = 228 # One or more weeds found
    '';
  };
}
