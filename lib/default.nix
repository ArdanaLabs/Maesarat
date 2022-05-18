{ pkgs }:
let
  makeTest = (import (pkgs.path + "/nixos/lib/testing-python.nix") { system = pkgs.hostPlatform.system; }).makeTest;
  handleLib = x: args: import x (args // { inherit pkgs makeTest; });
in
{
  end-to-end = handleLib ./end-to-end.nix;
}
