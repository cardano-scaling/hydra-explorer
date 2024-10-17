{ repoRoot, inputs }:
{
  nixosConfigurations = {
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
            port = 3002;
          };

          services.openssh.enable = true;

          system.stateVersion = "24.05";
        }
      ];
    };
  };
}