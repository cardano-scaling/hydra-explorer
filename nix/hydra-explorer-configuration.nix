{ cardano-node-module, hydra-explorer }:
{ pkgs
, lib
, inputs
, ...
}:
{
  imports = [
    cardano-node-module
    (import ./nixosModule.nix { inherit hydra-explorer; })
  ];

  networking = {
    hostName = "hydra-explorer";
    firewall = {
      allowedTCPPorts = [ 25 80 443 ];
      enable = false;
    };
  };

  users.users.root = {
    initialPassword = ""; # No password
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIATz/Jv+AnBft+9Q01UF07OydvgTTaTdCa+nMqabkUNl" # noonio
    ];
  };

  services.getty.autologinUser = "root";

  nix.settings.trusted-users = [ "root" ];

  services.cardano-node = {
    enable = true;
    environment = "preview";
    hostAddr = "0.0.0.0";
    port = 3002;
    socketPath = "/run/cardano-node/node.socket";
  };

  services.hydra-explorer = {
    enable = true;
    socketPath = "/run/cardano-node/node.socket";
    networkArg = "--testnet-magic 2";
  };

  services.openssh = {
    settings.PasswordAuthentication = false;
    enable = true;
  };

  system.stateVersion = "24.05";
}
