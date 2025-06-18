# Runs hydra-doom frontend, hydra-control-plane backend and an offline
# hydra-node to serve a single player experience.
{ pkgs, lib, inputs, config, system, ... }:

{
  # TODO: reverse proxy doom.hydra.family
  networking.firewall.allowedTCPPorts = [ 8000 4001 ];
  environment.systemPackages = with pkgs; [
    # alias to hydra-doom-process-compose
    (pkgs.writeShellScriptBin "hydra-doom-process-compose" ''
      exec ${inputs.hydra-doom.packages.${system}.default}/bin/default "$@"
    '')
  ];
  # TODO: start hydra-doom's process-compose in systemd service
}
