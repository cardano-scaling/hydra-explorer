# Docs for this file: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#mkhaskellprojectinshellargs
# See `shellArgs` in `mkHaskellProject` in ./project.nix for more details.

{ repoRoot, inputs, pkgs, lib, system }:

cabalProject:

{
  name = "nix-shell";
  packages = [
    pkgs.cardano-cli
    pkgs.cardano-node
    pkgs.hydra-chain-observer
    pkgs.hydra-node
    pkgs.check-jsonschema # For hydra-explorer:test:tests
    pkgs.yarn # For hacking on the UI
  ];
}
