{
  description = "A Stress-Testing Tool for Cardano Dapps ";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-21.05";
  inputs.cardano-node.url = "github:input-output-hk/cardano-node/1.30.1";

  outputs = { self, nixpkgs, cardano-node }:
    let

      # Generate a user-friendly version number.
      version = builtins.substring 0 8 self.lastModifiedDate;

      # System types to support.
      supportedSystems = [ "x86_64-linux" ];

      # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);

      # Nixpkgs instantiated for supported system types.
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; overlays = [ self.overlay ]; });

    in

    {
      overlay = final: prev: {};

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
                    environment.systemPackages = [ pkgs.curl ];
                    services.cardano-node.enable = true;
                    services.cardano-node.instances = 1;
                  };
                };

                testScript = ''
                  start_all()
                  client.wait_for_unit("cardano-node.service")
                  client.wait_for_open_port("3001")
                '';
              };
            in test;
        });
    };
}
