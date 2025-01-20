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
      allowedTCPPorts = [ 22 80 443 ];
      enable = true;
    };
  };

  nix = {
    settings.trusted-users = [ "root" "runner" ];
    extraOptions = ''
      experimental-features = nix-command flakes recursive-nix ca-derivations
      log-lines = 300
      warn-dirty = false
      allow-import-from-derivation = true
      accept-flake-config = true
    '';
  };

  users.users.root = {
    initialPassword = ""; # No password
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIATz/Jv+AnBft+9Q01UF07OydvgTTaTdCa+nMqabkUNl" # noonio
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEMuBv9vXsKsOsjS7B6zMOpuLw+gGGHR6hADuTeiNfKO" # locallycompact
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBoR/OGSvjN2PCi2+OGeFTSTWR0aacCwK41j1gUu5UGr" # v0d1ch
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBtamRlrHLKLzr8Pcm3qEgdbJh7vCjMO4tm0wbW3REYL" # ffakenz
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDHRjFKHOS4lOw907VWvDMrx/XawRMV2wyc+VSbA4YHnG2ecv6y/JT3gBjmdNw0bgltgQqeBBG/iTciio+Zax8I36rPWMEomDvpgq8B7i1L23eWoK9cKMqYNAUpIAfManhJKvZfBjJ9dRLz4hfUGo2Gah5reuweFrkzWGb2zqILNXoM2KowlkqMOFrd09SgP52sUuwNmaCJaPba7IdqzLqxotWaY420Msd5c8B2l/0E/hNgRu6m5qbZpidmQQJsTk2tq4CWP5xB2SbgEwAuZZ6AUOn2IqGfF8bkLfwHb5qdtss0jxZm47s5Fag9T9MzzbXCAHEdyO01+q83FKIxkiW/" # ch1bo
    ];
  };

  services.getty.autologinUser = "root";


  # Github runner registered with cardano-scaling organization
  users.users.runner = {
    isNormalUser = true;
    uid = 1001;
    group = "users";
    extraGroups = [ ];
  };
  age.secrets.github-runner-token.file = ../secrets/github-runner-token.age;
  services.github-runners.explorer = {
    enable = true;
    url = "https://github.com/cardano-scaling";
    tokenFile = "/run/agenix/github-runner-token";
    replace = true;
    extraLabels = [ "nixos" "self-hosted" "explorer" "cardano" ];
    user = "runner";
  };

  services.cardano-node = {
    enable = true;
    environment = "preview";
    hostAddr = "0.0.0.0";
    port = 3002;
    socketPath = "/run/cardano-node/node.socket";
  };

  # services.hydra-explorer = {
  #   enable = true;
  #   socketPath = "/run/cardano-node/node.socket";
  #   networkArg = "--testnet-magic 2";
  #   port = 80;
  #   startChainFrom = "49533501.e364500a42220ea47314215679b7e42e9bbb81fa69d1366fe738d8aef900f7ee";
  # };

  services.openssh = {
    settings.PasswordAuthentication = false;
    enable = true;
  };

  system.stateVersion = "24.05";
}
