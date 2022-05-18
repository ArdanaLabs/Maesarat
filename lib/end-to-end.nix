{ pkgs, cardano-node, cardano-cli, e2eBinary, makeTest, plutus-apps }:
makeTest {
  name = "end-to-end";
  nodes = {
    client = { ... }: {
      virtualisation = {
        cores = 2;
        memorySize = 1024;
      };
      environment.systemPackages = [
        e2eBinary
        cardano-cli
        cardano-node
      ];
    };
  };
  testScript =
    let
      runEndToEnd = pkgs.writeScript "e2eTest" ''
        export SHELLEY_TEST_DATA="${plutus-apps}/plutus-pab/local-cluster/cluster-data/cardano-node-shelley"
        ${e2eBinary.exePath}
      '';
    in
    ''
      start_all()
      client.wait_for_unit("multi-user.target")
      client.succeed("${runEndToEnd} >&2")
    '';
}
