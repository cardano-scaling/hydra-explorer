# Runs hydra-doom frontend, hydra-control-plane backend and an offline
# hydra-node to serve a single player experience.
{ pkgs, lib, inputs, config, system, ... }:

{
  services.nginx.virtualHosts."doom-api.hydra.family" = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      # NOTE: hydra-control-plane running on 8000 via process compose
      proxyPass = "http://127.0.0.1:8000";
      proxyWebsockets = true;
      extraConfig = ''
        proxy_buffering off;
        client_max_body_size 500M;
      '';
    };
  };

  # Open port for hydra-node
  networking.firewall.allowedTCPPorts = [ 4001 ];

  # NOTE: Process compose including the hydra-control-plane and a hydra-node
  environment.systemPackages = with pkgs; [
    # alias to hydra-doom-process-compose
    (pkgs.writeShellScriptBin "hydra-doom-process-compose" ''
      exec ${inputs.hydra-doom.packages.${system}.default}/bin/default "$@"
    '')
  ];
  # TODO: start hydra-doom's process-compose in systemd service
  # TODO: switch to docker or dedicated systemd services?
}
