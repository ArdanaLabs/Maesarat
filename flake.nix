{
  description = "A Testing Tool for Cardano Dapps";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.05";
    cardano-node = {
      url = "github:input-output-hk/cardano-node/1.34.1";
    };
  };

  outputs = { self, nixpkgs, cardano-node }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forSystems = systems: f:
        nixpkgs.lib.genAttrs systems
        (system: f system nixpkgs.legacyPackages.${system});
      forAllSystems = forSystems supportedSystems;
      nixpkgsFor = forAllSystems (system: pkgs: import nixpkgs { inherit system; overlays = [ self.overlay ]; });
    in
    {
      lib = forAllSystems (system: pkgs: import ./lib { inherit pkgs; });
    };
}
