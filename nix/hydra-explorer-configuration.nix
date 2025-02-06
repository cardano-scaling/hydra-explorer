{ pkgs, lib, inputs, config, ... }:
{
  networking = {
    # NOTE: This is not hydra-explorer as a container running on this host will
    # use that dns name.
    hostName = "explorer";
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
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFkv5iajnUL4PiRREbtN4/vM+mX+9IvgKcgnwnmSoNik" # v0d1ch
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
    # Surprisingly required for this to be able to delete files owned by root.
    user = "root";
    extraLabels = [ "nixos" "self-hosted" "explorer" "cardano" ];
    serviceOverrides = {
      # See: https://discourse.nixos.org/t/github-runners-cp-read-only-filesystem/36513/2
      ReadWritePaths = [
        "/data/cardano"
      ];
    };
  };

  # Use docker to manage containers
  virtualisation.docker.enable = true;
  virtualisation.oci-containers.backend = "docker";

  # Network between hydra-explorer and hydra-chain-observer
  systemd.services."init-docker-network-explorer" =
    let networkName = "hydra-explorer";
    in {
      description = "Network bridge for ${networkName}.";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = "yes";
      };
      script =
        let cli = "${config.virtualisation.docker.package}/bin/docker";
        in ''
          if [[ $(${cli} network inspect ${networkName}) == "[]" ]]; then
            ${cli} network create ${networkName}
          else
            echo "Docker network ${networkName} already exists"
          fi
        '';
    };

  # Hydra explorer instance
  virtualisation.oci-containers.containers.hydra-explorer = {
    image = "ghcr.io/cardano-scaling/hydra-explorer:unstable";
    volumes = [
      "/data/cardano/preview:/data"
    ];
    ports = [
      "80:8000"
    ];
    cmd = builtins.concatLists [
      [ "--client-port" "8000" ]
      [ "--observer-port" "8001" ] # not bound to host
    ];
    extraOptions = [ "--network=hydra-explorer" ];
  };

  virtualisation.oci-containers.containers."hydra-chain-observer-preview-unstable" = {
    image = "ghcr.io/cardano-scaling/hydra-chain-observer:unstable";
    volumes = [
      "/data/cardano/preview:/data"
    ];
    cmd = builtins.concatLists [
      [ "--node-socket" "/data/node.socket" ]
      [ "--testnet-magic" "2" ]
      # NOTE: Block in which 0.20.0 scripts were published
      [ "--start-chain-from" "71938562.2c5fc734343ad1bf8ce2df999421cca15dffdd2b8e1909dad7127b9eaacf8b9c" ]
      # NOTE: Reachable via bridge network
      [ "--explorer" "http://hydra-explorer:8001" ]
    ];
    extraOptions = [ "--network=hydra-explorer" ];
  };

  virtualisation.oci-containers.containers."hydra-chain-observer-preprod-unstable" = {
    image = "ghcr.io/cardano-scaling/hydra-chain-observer:unstable";
    volumes = [
      "/data/cardano/preprod:/data"
    ];
    cmd = builtins.concatLists [
      [ "--node-socket" "/data/node.socket" ]
      [ "--testnet-magic" "1" ]
      # NOTE: Block in which 0.20.0 scripts were published
      [ "--start-chain-from" "82913877.aa62d900ecbd6a073e1b5bb8c014413365c26a3675bd81dc567013340bf94ec3" ]
      # NOTE: Reachable via bridge network
      [ "--explorer" "http://hydra-explorer:8001" ]
    ];
    extraOptions = [ "--network=hydra-explorer" ];
  };

  # Cardano node used by Hydra smoke tests and hydra-chain-observers
  # TODO: initialize /data/cardano/preview correctly on a fresh machine
  virtualisation.oci-containers.containers.cardano-node-preview = {
    image = "ghcr.io/intersectmbo/cardano-node:10.1.4";
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

  # Cardano node used by Hydra smoke tests and hydra-chain-observers
  # TODO: initialize /data/cardano/preprod correctly on a fresh machine
  virtualisation.oci-containers.containers.cardano-node-preprod = {
    image = "ghcr.io/intersectmbo/cardano-node:10.1.4";
    volumes = [
      "/data/cardano/preprod:/data"
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

  services.openssh = {
    settings.PasswordAuthentication = false;
    enable = true;
  };

  system.stateVersion = "24.05";
}
