{ inputs, ... }:
{
  hydra-explorer = import ./hydra-explorer-aws.nix { inherit inputs; };
}
