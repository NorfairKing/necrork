final: prev:
with final.lib;
with final.haskell.lib;
{

  necrorkRelease =
    final.symlinkJoin {
      name = "necrork-release";
      paths = builtins.attrValues final.necrorkReleasePackages;
      passthru = final.necrorkReleasePackages;
    };

  necrorkReleasePackages =
    let
      enableStatic = pkg:
        if final.stdenv.hostPlatform.isMusl
        then
          overrideCabal pkg
            (old: {
              configureFlags = (old.configureFlags or [ ]) ++ [
                "--ghc-option=-optl=-static"
                # Static
                "--extra-lib-dirs=${final.gmp6.override { withStatic = true; }}/lib"
                "--extra-lib-dirs=${final.zlib.static}/lib"
                "--extra-lib-dirs=${final.libffi.overrideAttrs (_: { dontDisableStatic = true; })}/lib"
                # for -ltinfo
                "--extra-lib-dirs=${(final.ncurses.override { enableStatic = true; })}/lib"
              ];
              enableSharedExecutables = false;
              enableSharedLibraries = false;

              postInstall = (old.postInstall or "") + ''
                for b in $out/bin/*
                do
                  if ldd "$b"
                  then
                    echo "ldd succeeded on $b, which may mean that it is not statically linked"
                    exit 1
                  fi
                done
              '';
            })
        else pkg;

    in
    mapAttrs
      (_: pkg: justStaticExecutables (enableStatic pkg))
      final.haskellPackages.necrorkPackages;

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (self: _:
      let
        necrorkPkg = name:
          buildFromSdist (overrideCabal
            (self.callPackage (../${name}) { })
            (old: {
              configureFlags = (old.configureFlags or [ ]) ++ [
                # Optimisations
                "--ghc-options=-O2"
                # Extra warnings
                "--ghc-options=-Wall"
                "--ghc-options=-Wincomplete-uni-patterns"
                "--ghc-options=-Wincomplete-record-updates"
                "--ghc-options=-Wpartial-fields"
                "--ghc-options=-Widentities"
                "--ghc-options=-Wredundant-constraints"
                "--ghc-options=-Wcpp-undef"
                "--ghc-options=-Wunused-packages"
                "--ghc-options=-Werror"
              ];
              doBenchmark = true;
              doHaddock = false;
              doCoverage = false;
              doHoogle = false;
              doCheck = true; # TODO turn this to false once we set up coverage reports
              hyperlinkSource = false;
              enableLibraryProfiling = false;
              enableExecutableProfiling = false;
              # Ugly hack because we can't just add flags to the 'test' invocation.
              # Show test output as we go, instead of all at once afterwards.
              testTarget = (old.testTarget or "") + " --show-details=direct";
            }));

        necrorkPackages = {
          necrork-gen = necrorkPkg "necrork-gen";
          necrork = necrorkPkg "necrork";
        };
      in
      {
        inherit necrorkPackages;
      } // necrorkPackages
    );
  });
}
