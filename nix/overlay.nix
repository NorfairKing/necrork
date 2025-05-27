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
