{ config, pkgs, lib, haskellLib, ... }: {
  config = {
    flake.variants = {
      # For building static executables:
      #     $ nix -L build .#static-x86_64-unknown-linux-musl:github-action-scan:exe:github-action-scan-exe
      #
      # Note the use of the Musl cross compiling platform
      # to replace glibc which does not fully support static linking.
      #
      # For debugging static executables (Musl is not bug-to-bug compatible with glibc):
      #     $ nix -L develop .#legacyPackages.x86_64-linux.project.projectVariants.static.projectCross.musl64.hsPkgs.github-action-scan.components.executables.github-action-scan-exe
      #     devShell> $ rm -rf source outputs
      #     devShell> $ unpackPhase
      #     devShell> $ cd source
      #     devShell> $ patchPhase
      #     devShell> $ eval "$configurePhase"
      #     devShell> $ eval "$buildPhase"
      #     devShell> $ (unset LD_LIBRARY_PATH; $EDITOR **/*.hs) # hack hack hack
      #     devShell> $ eval "$buildPhase"
      #     devShell> $ eval "$installPhase"
      #
      # FIXME: this static compiling currently fails.
      # A problem to look for is pkgsCross.musl64
      # still having some libraries not built statically.
      static = {
        modules = [
          ({ config, pkgs, ... }: {
            enableStatic = true;
            # Fails with:
            # cmdargs-lib-cmdargs-x86_64-unknown-linux-musl>     Bad interface file: /nix/store/n0iwpy7240kw1mkmryzcgj9s9vvbfslb-transformers-lib-transformers-x86_64-unknown-linux-musl-0.5.6.2/lib/x86_64-linux-ghc-9.4.7/transformers-0.5.6.2-7uipldXxD6oGHq4y3fP618/Control/Monad/Trans/State.dyn_hi
            #
            # Besides it does not apply DYNAMIC_GHC_PROGRAMS=NO to GHC.
            #enableShared = false;

            # Musl does not provide the thread-safe random_r()/initstate_r()
            # so make randomVector fallback to a **non** thread-safe alternative, nrand48()
            # See https://github.com/haskell-numerics/hmatrix/issues/279
            #
            # As a consequence, random numbers may not be random between threads
            # as they now share a common state.
            # See http://www.evanjones.ca/random-thread-safe.html
            # > the best solution for truly portable applications
            # > is to include your own random number generator implementation,
            # > and not rely on the system's C library.
            # See https://github.com/haskell-numerics/hmatrix/issues/307
            #packages.hmatrix.flags.no-random_r = true;

            # Use pkg-config instead of pg_config to avoid:
            #     $ nix -L build .#legacyPackages.x86_64-linux.project.projectVariants.static.projectCross.musl64.hsPkgs.postgresql-libpq.components.library
            #     > Error: Setup: The program 'pg_config' is required but it could not be found
            #packages.postgresql-libpq.flags.use-pkg-config = true;

            # > <no location info>: error:
            # >     Couldn't find a target code interpreter. Try with -fexternal-interpreter
            packages.tar.ghcOptions = [ "-fexternal-interpreter" ];
          })
        ];

        flake.crossPlatforms = platforms:
          pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 (
            # For building static executables.
            pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
              platforms.musl64
            ]
            # For cross compiling to Windows.
            # ++ [ platforms.mingwW64 ]
          );
      };
    };
  };
}
