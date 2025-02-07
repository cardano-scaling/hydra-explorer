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

  services.openssh = {
    settings.PasswordAuthentication = false;
    enable = true;
  };

  system.stateVersion = "24.05";
}
