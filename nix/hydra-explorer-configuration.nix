{ pkgs, lib, inputs, system, ... }:

let
  unstablePkgs = import inputs.unstableNixpkgs { inherit system; };
in

{
  networking.hostName = "explorer";
  networking.firewall.allowedTCPPorts = [ 80 443 ];

  services.nginx.enable = true;
  services.nginx.recommendedProxySettings = true;
  security.acme.acceptTerms = true;
  security.acme.defaults.email = "sebastian.nagel@iohk.io";

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
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBJd9BiDoUNl0pCVDeIKnlwJu6oOmLIz7l3Ct7xoYjBS" # noonio
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIATz/Jv+AnBft+9Q01UF07OydvgTTaTdCa+nMqabkUNl" # noonio
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEMuBv9vXsKsOsjS7B6zMOpuLw+gGGHR6hADuTeiNfKO" # locallycompact
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFkv5iajnUL4PiRREbtN4/vM+mX+9IvgKcgnwnmSoNik" # v0d1ch
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBtamRlrHLKLzr8Pcm3qEgdbJh7vCjMO4tm0wbW3REYL" # ffakenz
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDHRjFKHOS4lOw907VWvDMrx/XawRMV2wyc+VSbA4YHnG2ecv6y/JT3gBjmdNw0bgltgQqeBBG/iTciio+Zax8I36rPWMEomDvpgq8B7i1L23eWoK9cKMqYNAUpIAfManhJKvZfBjJ9dRLz4hfUGo2Gah5reuweFrkzWGb2zqILNXoM2KowlkqMOFrd09SgP52sUuwNmaCJaPba7IdqzLqxotWaY420Msd5c8B2l/0E/hNgRu6m5qbZpidmQQJsTk2tq4CWP5xB2SbgEwAuZZ6AUOn2IqGfF8bkLfwHb5qdtss0jxZm47s5Fag9T9MzzbXCAHEdyO01+q83FKIxkiW/" # ch1bo
    ];
  };

  services.getty.autologinUser = "root";

  environment.systemPackages = with pkgs; [
    git
    jq
    isd
  ];

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
    package = unstablePkgs.github-runner;
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

  # NOTE: Reverse proxy to the hydra-explorer container deployed through
  services.nginx.virtualHosts."explorer.hydra.family" = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:8080";
      proxyWebsockets = true;
      extraConfig = ''
        proxy_buffering off;
        client_max_body_size 500M;
      '';
    };
  };

  services.openssh = {
    settings.PasswordAuthentication = false;
    enable = true;
  };

  services.timesyncd.enable = true;

  system.stateVersion = "24.05";
}
