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
    environment = "preprod";
    socketPath = "/run/cardano-node/node.socket";
    port = 3002;
    extraServiceConfig = _:
    {
      serviceConfig.Environment = ["CARDANO_NODE_NETWORK_ID=2"];
    };
  };

  services.hydra-explorer = {
    enable = true;
    socketPath = "/run/cardano-node/node.socket";
  };

  services.openssh = {
    settings.PasswordAuthentication = false;
    enable = true;
  };

  system.stateVersion = "24.05";
}
