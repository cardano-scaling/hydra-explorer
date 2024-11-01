{ inputs, ... }:
inputs.nixpkgs.lib.nixosSystem
{
  system = "x86_64-linux";
  specialArgs = inputs;
  modules = [
    {
      _module.args.nixinate = {
        host = "13.38.81.225";
        sshUser = "root";
        buildOn = "local";
        substituteOnTarget = true;
        hermetic = false;
      };
    }
    {
      imports = [
        "${inputs.nixpkgs}/nixos/modules/virtualisation/amazon-image.nix"
        (import ./hydra-explorer-configuration.nix {
          cardano-node-module = inputs.cardano-node.nixosModules.cardano-node;
          hydra-explorer = inputs.self.packages.x86_64-linux.hydra-explorer;
        })
      ];
    }
  ];
}
