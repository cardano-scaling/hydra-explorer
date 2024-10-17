{ inputs, ... }:
{ config, pkgs, lib, ... }:

with builtins;
with lib;

let cfg = config.services.hydra-explorer;
in


{
  options = {
    services.hydra-explorer = {
      enable = mkOption {
        default = false;
        type = types.bool;
        description = ''
          Enable hydra-explorer
        '';
      };
      package = mkOption {
        type = types.package;
        default = inputs.self.packages.x86_64-linux.hydra-explorer;
        description = ''
          The hydra-explorer package to use.
        '';
      };
      socketPath = mkOption {
        type = types.path;
        default = "/root/node.socket";
        description = ''
          The node socket
        '';
      };
      port = mkOption {
        type = types.port;
        default = 9081;
        description = ''
          The port to run on
        '';
      };
      user = mkOption {
        type = types.str;
        default = "root";
        description = ''
          The user to run the systemd service
        '';
      };
      group = mkOption {
        type = types.str;
        default = "root";
        description = ''
          The group to run the systemd service.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.services.hydra-explorer = {
      after = [ "cardano-node.service" ];
      enable = true;
      description = "Hydra Explorer";
      unitConfig = {
        Type = "simple";
      };
      serviceConfig = {
        ExecStart = "${cfg.package}/bin/hydra-explorer --node-socket ${cfg.socketPath} --port ${toString cfg.port}";
        Restart = "on-failure";
        User = cfg.user;
        Group = cfg.group;
      };
      wantedBy = [ "multi-user.target" ];
    };
  };
}
