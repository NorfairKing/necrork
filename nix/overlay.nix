final: prev:
with final.lib;
with final.haskell.lib;
{
  necrorkNotification = final.callPackage ./notification.nix { };

  necrorkRelease =
    final.symlinkJoin {
      name = "necrork-release";
      paths = builtins.attrValues final.necrorkReleasePackages;
      passthru = {
        notification = final.necrorkNotification;
      } // final.necrorkReleasePackages;
    };

  necrorkReleasePackages =
    mapAttrs
      (_: pkg: justStaticExecutables pkg)
      final.haskellPackages.necrorkPackages;

  necrork-cli = final.necrorkReleasePackages.necrork-cli;

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (self: _:
      let
        necrorkPkg = name:
          buildStrictly (overrideCabal
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
              hyperlinkSource = false;
              doCheck = false;
            }));

        necrorkPackages = {
          necrork = necrorkPkg "necrork";
          necrork-cli = necrorkPkg "necrork-cli";
          necrork-gen = necrorkPkg "necrork-gen";
        };
      in
      {
        inherit necrorkPackages;
      } // necrorkPackages
    );
  });
}
