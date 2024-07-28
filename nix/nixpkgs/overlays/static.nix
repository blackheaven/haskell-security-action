finalPkgs: previousPkgs: {
  pkgsCross = previousPkgs.pkgsCross // {
    musl64 = previousPkgs.pkgsCross.musl64.extend (finalMusl64: previousMusl64:
      {

        #openblas = previousMusl64.openblas.override {
        #  # See https://github.com/input-output-hk/haskell.nix/issues/914#issuecomment-2021507590
        #  enableStatic = true;
        #  # Necessary to overcome lapack and blas packaging.
        #  # And avoid ghc-iserv to fail finding the shared libs.
        #  enableShared = true;
        #};

        # Not enough anyway to avoid:
        # unknown symbol `ZSTD_trace_compress_begin' in zstd Haskell package.
        #zstd = previousMusl64.zstd.override {
        #  static = true;
        #};
      });
  };
}
