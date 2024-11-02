{ hydra-explorer }:
{ config
, pkgs
, lib
, ...
}:

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
        default = hydra-explorer;
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
        default = 8080;
        description = ''
          The api port to run on
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
      networkArg = mkOption {
        type = types.str;
        default = "--testnet-magic 2";
        description = ''
          A network argument,like '--testnet-magic 2'
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.services.hydra-explorer = {
      after = [ "cardano-node.service" ];
      requires = [ "cardano-node.service" ];
      wantedBy = [ "multi-user.target" ];
      enable = true;
      description = "Hydra Explorer";
      unitConfig = {
        Type = "simple";
      };
      serviceConfig = {
        ExecStart = "${cfg.package}/bin/hydra-explorer direct --node-socket ${cfg.socketPath} --api-port ${toString cfg.port} ${cfg.networkArg}";
        Restart = "on-failure";
        User = cfg.user;
        Group = cfg.group;
      };
    };
  };
}
