{ hydra-explorer, hydra-explorer-web }:
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
      static-assets-path = mkOption {
        type = types.package;
        default = hydra-explorer-web;
        description = ''
          The hydra-explorer-web package to use.
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
      startChainFrom = mkOption {
        type = types.str;
        description = ''
          Block in which current master scripts were published
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
      serviceConfig = {
        WorkingDirectory = "/";
        ExecStart = ''${cfg.package}/bin/hydra-explorer direct \
          --static-path ${hydra-explorer-web.out} \
          --node-socket ${cfg.socketPath} \
          --api-port ${toString cfg.port} \
          ${cfg.networkArg} \
          --start-chain-from ${cfg.startChainFrom}
        '';
        Restart = "on-failure";
        User = cfg.user;
        Group = cfg.group;
      };
    };
  };
}
