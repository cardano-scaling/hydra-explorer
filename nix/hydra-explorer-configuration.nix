{ pkgs, lib, inputs, ... }:
{
  networking = {
    hostName = "hydra-explorer";
    firewall = {
      allowedTCPPorts = [ 22 80 443 ];
      enable = true;
    };
  };

  nix = {
    settings.trusted-users = [ "root" ];
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
  age.secrets.github-runner-token.file = ../secrets/github-runner-token.age;

  # TODO: Run this with 'runner' user? If yes, then we need to fix permissions
  # on the /data paths and/or make the containers rootless (run by 'runner')?
  services.github-runners.explorer = {
    enable = true;
    url = "https://github.com/cardano-scaling";
    tokenFile = "/run/agenix/github-runner-token";
    replace = true;
    extraLabels = [ "nixos" "self-hosted" "explorer" "cardano" ];
    serviceOverrides = {
      # See: https://discourse.nixos.org/t/github-runners-cp-read-only-filesystem/36513/2
      ReadWritePaths = [
        "/data/cardano"
      ];
    };
  };

  # Cardano node used by Hydra smoke tests and explorer instance
  # TODO: initialize /data/cardano/preview correctly on a fresh machine
  virtualisation.oci-containers.containers.cardano-node-preview = {
    image = "ghcr.io/intersectmbo/cardano-node:10.1.3";
    volumes = [
      "/data/cardano/preview:/data"
    ];
    cmd = [ "run" ];
    environment = {
      CARDANO_CONFIG = "/data/config.json";
      CARDANO_TOPOLOGY = "/data/topology.json";
      CARDANO_DATABASE_PATH = "/data/db";
      CARDANO_SOCKET_PATH = "/data/node.socket"; # used by cardano-node
      CARDANO_NODE_SOCKET_PATH = "/data/node.socket"; # used by cardano-cli
      CARDANO_LOG_DIR = "/data/logs";
    };
  };

  virtualisation.oci-containers.containers.hydra-explorer = {
    image = "ghcr.io/cardano-scaling/hydra-explorer:0.19.0";
    volumes = [
      "/data/cardano/preview:/data"
    ];
    ports = [
      "80:8080"
    ];
    cmd = builtins.concatLists [
      [ "--node-socket" "/data/node.socket" ]
      [ "--testnet-magic" "2" ]
      [ "--api-port" "8080" ]
      # NOTE: Block in which current master scripts were published
      [ "--start-chain-from" "49533501.e364500a42220ea47314215679b7e42e9bbb81fa69d1366fe738d8aef900f7ee" ]
    ];
  };

  services.openssh = {
    settings.PasswordAuthentication = false;
    enable = true;
  };

  system.stateVersion = "24.05";
}
