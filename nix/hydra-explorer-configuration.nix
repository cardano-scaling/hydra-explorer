{ cardano-node-module, hydra-explorer, hydra-explorer-web }:
{ pkgs
, lib
, inputs
, ...
}:
{
  imports = [
    cardano-node-module
    (import ./nixosModule.nix { inherit hydra-explorer hydra-explorer-web; })
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
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEMuBv9vXsKsOsjS7B6zMOpuLw+gGGHR6hADuTeiNfKO" # locallycompact
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBoR/OGSvjN2PCi2+OGeFTSTWR0aacCwK41j1gUu5UGr" # v0d1ch
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBtamRlrHLKLzr8Pcm3qEgdbJh7vCjMO4tm0wbW3REYL" # ffakenz
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
    port = 80;
    startChainFrom = "49533501.e364500a42220ea47314215679b7e42e9bbb81fa69d1366fe738d8aef900f7ee";
  };

  services.openssh = {
    settings.PasswordAuthentication = false;
    enable = true;
  };

  system.stateVersion = "24.05";
}
