{
  description = "A Stress-Testing Tool for Cardano Dapps ";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.05";
    cardano-node = {
      url = "github:input-output-hk/cardano-node/1.33.0";
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
      compiler = "ghc8107";

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
                  producer = { config, pkgs, ... }: {
                    networking.firewall.enable = false;
                    environment.systemPackages =
                      [ 
                        cardano-node.packages."${system}".cardano-cli
                        cardano-node.packages."${system}".cardano-node
                        pkgs.tree
                      ];
                    imports = [ cardano-node.nixosModules.cardano-node ];
                    services.cardano-node = {
                      hostAddr = "0.0.0.0";
                      enable = true;
                      topology = "${./data/topology-producer.json}";
                      #producers = lib.mkOverride [
                      #  {
                      #    addrs = [{ addr = "127.0.0.1"; port = 12799; }];
                      #    advertise = false;
                      #    valency = 1;
                      #  }
                      #];
                      nodeConfigFile = "${./config.json}";
                      vrfKey = "/tmp/genesis/delegate-keys/delegate1.vrf.skey";
                      kesKey = "/tmp/genesis/delegate-keys/delegate1.kes.skey";
                      operationalCertificate =
                        "/tmp/genesis/delegate-keys/opcert1.cert";
                      forceHardForks = {
                        shelley = 0;
                        allegra = 0;
                        mary    = 0;
                        alonzo  = 0;
                      };
                    };
                  };
                  relay = { config, pkgs, ... }: {
                    networking.firewall.enable = false;
                    environment.systemPackages =
                      [ 
                        cardano-node.packages."${system}".cardano-cli
                        cardano-node.packages."${system}".cardano-node
                        pkgs.tree
                      ];
                    imports = [ cardano-node.nixosModules.cardano-node ];
                    services.cardano-node = {
                      hostAddr = "0.0.0.0";
                      enable = true;
                      topology = "${./data/topology-relay.json}";
                      #producers = lib.mkOverride [
                      #  {
                      #    addrs = [{ addr = "127.0.0.1"; port = 12799; }];
                      #    adhostAddrvertise = false;
                      #    valency = 1;
                      #  }
                      #];
                      nodeConfigFile = "${./config.json}";
                      forceHardForks = {
                        shelley = 0;
                        allegra = 0;
                        mary    = 0;
                        alonzo  = 0;
                      };
                    };
                  };
                };

                testScript =
                  let
                    generateEverything = pkgs.writeScript "generateEverything.sh" ''

                      cp -R "${./genesis}" ./genesis
                      cp -R "${./genesis-byron}" ./genesis-byron
                      chmod -R 700 genesis
                      chmod -R 700 genesis-byron

                      cardano-cli genesis create \
                        --genesis-dir genesis/ \
                        --supply 1000000000 \
                        --gen-genesis-keys 1 \
                        --gen-utxo-keys 1 \
                        --testnet-magic 42

                      cardano-cli byron genesis genesis \
                        --protocol-magic 42 \
                        --start-time $(date +%s --date="10 seconds") \
                        --k 10 \
                        --n-poor-addresses 0 \
                        --n-delegate-addresses 1 \
                        --total-balance 1000000000 \
                        --delegate-share 1 \
                        --avvm-entry-count 0 \
                        --avvm-entry-balance 0 \
                        --protocol-parameters-file "${./data/byron.genesis.spec.json}" \
                        --genesis-output-dir genesis-byron/

                      cp "${./genesis/genesis.json}" ./genesis
                      cp "${./genesis/genesis.alonzo.json}" ./genesis
                      cp "${./genesis-byron/genesis.json}" ./genesis-byron

                      chmod -R 700 genesis
                      chmod -R 700 genesis-byron
                      chown -R cardano-node:cardano-node genesis
                      chown -R cardano-node:cardano-node genesis-byron
                      cat ./genesis/genesis.json
                      cat ./genesis-byron/genesis.json
                      tree genesis
                      tree genesis-byron
                      find ./genesis -type f -exec sha256sum {} \;
                      find ./genesis-byron -type f -exec sha256sum {} \;
                    '';
                  in
                ''
                  start_all()
                  producer.succeed("${generateEverything} >&2")
                  relay.succeed("${generateEverything} >&2")
                  producer.wait_for_unit("cardano-node.service")
                  producer.wait_for_open_port("3001")
                  relay.wait_for_unit("cardano-node.service")
                  relay.wait_for_open_port("3001")
                  relay.succeed("ifconfig >&2")
                  relay.sleep("600")
                  producer.succeed("ifconfig >&2")
                  producer.sleep("600")
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
              # See https://github.com/NixOS/nixpkgs/issues/82245
              withHoogle = false;
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
          });
    };
}
