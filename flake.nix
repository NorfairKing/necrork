{
  description = "necrork";

  nixConfig = {
    extra-substituters = "https://necrork.cachix.org";
    extra-trusted-public-keys = "necrork.cachix.org-1:5BI2rwQ+nwO61CbDcPhy3W70ivFoSPRdnXhskEJwysM=";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-24.11";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    opt-env-conf.url = "github:NorfairKing/opt-env-conf";
    opt-env-conf.flake = false;
    looper.url = "github:NorfairKing/looper";
    looper.flake = false;
    token-limiter-concurrent.url = "github:NorfairKing/token-limiter-concurrent";
    token-limiter-concurrent.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    , safe-coloured-text
    , autodocodec
    , opt-env-conf
    , looper
    , token-limiter-concurrent
    }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          (import ./nix/overlay.nix)
          (import (safe-coloured-text + "/nix/overlay.nix"))
          (import (autodocodec + "/nix/overlay.nix"))
          (import (opt-env-conf + "/nix/overlay.nix"))
          (import (looper + "/nix/overlay.nix"))
          (import (token-limiter-concurrent + "/nix/overlay.nix"))
        ];
      };
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system} = {
        default = pkgs.necrorkRelease;
      };
      checks.${system} = {
        release = self.packages.${system}.default;
        shell = self.devShells.${system}.default;
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            hlint.enable = true;
            hpack.enable = true;
            ormolu.enable = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [ ".*/default.nix" ];
            deadnix.enable = true;
            deadnix.excludes = [ ".*/default.nix" ];
            cabal2nix.enable = true;
            tagref.enable = true;
          };
        };
      };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "necrork-shell";
        packages = p: builtins.attrValues p.necrorkPackages;
        withHoogle = true;
        doBenchmark = true;
        buildInputs = with pkgs; [
          cabal-install
          zlib
        ] ++ self.checks.${system}.pre-commit.enabledPackages;
        shellHook = self.checks.${system}.pre-commit.shellHook;
        DEVELOPMENT = "True";
      };
      nix-ci.cachix = {
        name = "necrork";
        public-key = "necrork.cachix.org-1:5BI2rwQ+nwO61CbDcPhy3W70ivFoSPRdnXhskEJwysM=";
      };
    };
}
