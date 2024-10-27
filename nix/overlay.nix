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
    mapAttrs
      (_: pkg: justStaticExecutables pkg)
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
