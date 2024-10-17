{ repoRoot, inputs }:
{
    hydra-explorer = inputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = inputs;
      modules = [
        {
          _module.args.nixinate = {
            host = "explorer.hydra.family";
            sshUser = "root";
            buildOn = "local";
            substituteOnTarget = true;
            hermetic = false;
          };
        }
        {
          imports = [
            "${inputs.nixpkgs}/nixos/modules/virtualisation/amazon-image.nix"
            inputs.cardano-node.nixosModules.cardano-node
            (import (inputs.self + "/nix/nixosModule.nix") { inherit inputs; })
          ];

          networking = {
            hostName = "hydra-explorer";
            firewall = {
              allowedTCPPorts = [ 25 80 443 ];
              enable = false;
            };
          };

          nix.settings.trusted-users = [ "root" ];

          services.cardano-node = {
            enable = true;
            environment = "preprod";
            socketPath = "/root/node.socket";
            port = 3002;
          };

          services.hydra-explorer.enable = true;

          services.openssh.enable = true;

          system.stateVersion = "24.05";
        }
      ];
    };
}
