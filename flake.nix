{
  description = "A Stress-Testing Tool for Cardano Dapps ";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.05";
    cardano-node = {
      url = "github:input-output-hk/cardano-node/1.31.0";
    };
    flake-compat-ci.url = "github:hercules-ci/flake-compat-ci";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-compat, flake-compat-ci, cardano-node }:
    let

      # Generate a user-friendly version number.
      version = builtins.substring 0 8 self.lastModifiedDate;

      # System types to support.
      supportedSystems = [ "x86_64-linux" ];

      # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);

      # Nixpkgs instantiated for supported system types.
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; overlays = [ self.overlay ]; });

      # The GHC compiler version to use, from haskell.packages.<compiler>
      compiler = "ghc8104";

    in

    {
      overlay = final: prev: {};

      ciNix = flake-compat-ci.lib.recurseIntoFlakeWith {
        flake = self;
        systems = [ "x86_64-linux" ];
      };

      checks = forAllSystems (system:
        with nixpkgsFor.${system};
        lib.optionalAttrs stdenv.isLinux {
          # A VM test of the NixOS module.
          cardano-testnet = with import (nixpkgs + "/nixos/lib/testing-python.nix") {
            inherit system;
          };

            let
              test = makeTest {
                name = "cardano-testnet";
                nodes = {
                  client = { config, pkgs, ... }: {
                    imports = [ cardano-node.nixosModules.cardano-node ];
                    services.cardano-node = {
                      enable = true;
                      forceHardForks = {
                        shelley = 0;
                        allegra = 0;
                        mary    = 0;
                        alonzo  = 0;
                      };
                    extraNodeConfig = {};
                    };
                  };
                };

                testScript = ''
                  start_all()
                  client.succeed("echo hello")
                  client.wait_for_unit("cardano-node.service")
                  client.wait_for_open_port("3001")
                '';
              };
            in test;
        });

        defaultPackage = forAllSystems (system: self.devShells.${system}.default.overrideAttrs (old: { returnShellEnv = false; }));

        devShell = forAllSystems (system: self.devShells.${system}.default);

        devShells = forAllSystems (system:
          let
            pkgs = nixpkgsFor."${system}";
          in rec
          {
            default = pkgs.haskell.packages."${compiler}".developPackage {
              returnShellEnv = true;
              name = "maesarat";
              root = ./haskell;
              modifier = drv:
                pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
                  cabal-fmt
                  cabal-install
                  cabal-plan
                  haskell-language-server
                  hlint
                  ghcid
                  ormolu
                  pkgs.zlib
                ]);
            };
          }
        );
    };
}
