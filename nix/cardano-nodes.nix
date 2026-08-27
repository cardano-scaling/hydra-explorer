# One cardano-node systemd service per network, replacing the containers that
# used to live in docker-compose.yaml. The chain observers still run under
# docker and reach these through the sockets under dataRoot.
{
  lib,
  pkgs,
  inputs,
  ...
}:

let
  inherit (lib)
    mapAttrs'
    mapAttrsToList
    nameValuePair
    optionalString
    ;

  system = pkgs.stdenv.hostPlatform.system;

  cardano-node = inputs.cardano-node.packages.${system}.cardano-node;
  cardanoLib = inputs.cardano-node.legacyPackages.${system}.cardanoLib;
  environments = inputs.cardano-node.environments.${system};
  mithril-client = inputs.mithril.packages.${system}.mithril-client-cli;

  # One directory per network, each holding db/ and node.socket. Same layout the
  # containers used, so an existing box keeps its databases.
  dataRoot = "/data/cardano";

  user = "cardano";

  # Distinct ports so the three nodes can share the host.
  networks = {
    preview = 3001;
    preprod = 3002;
    mainnet = 3003;
  };

  dirOf = name: "${dataRoot}/${name}";

  # config.json and topology.json come out of the pinned cardano-node flake, so
  # there is nothing to fetch at runtime and nothing to keep in sync by hand.
  configFile =
    name:
    pkgs.writeText "cardano-node-${name}-config.json" (builtins.toJSON environments.${name}.nodeConfig);

  # mkTopology refers to the peer snapshot by a path relative to the topology
  # file, so the two have to end up in one directory.
  topologyDir =
    name:
    let
      env = environments.${name};
    in
    pkgs.runCommand "cardano-node-${name}-topology" { } (
      ''
        mkdir -p $out
        cp ${cardanoLib.mkTopology env} $out/topology.json
      ''
      + optionalString (env ? peerSnapshot) ''
        cp ${pkgs.writeText "${env.name}-peer-snapshot.json" (builtins.toJSON env.peerSnapshot)} $out/${env.name}-peer-snapshot.json
      ''
    );

  # Restore from a Mithril snapshot, but only when there is no chain database
  # worth keeping. A node that already has one just catches up from where it
  # left off, which is the whole point of not doing this on every start.
  bootstrap =
    name:
    let
      env = environments.${name};
      dir = dirOf name;
    in
    pkgs.writeShellApplication {
      name = "cardano-node-${name}-bootstrap";
      runtimeInputs = [
        mithril-client
        pkgs.findutils
      ];
      text = ''
        db=${dir}/db

        # find, not compgen: nixpkgs' bash is built without programmable
        # completion, so compgen exits 127 here, which would read as "no
        # database" and throw away a synced one.
        if [ -n "$(find "$db/immutable" -maxdepth 1 -name '*.chunk' -print -quit 2>/dev/null)" ]; then
          echo "${name}: chain database present at $db, not touching Mithril."
          exit 0
        fi

        echo "${name}: no chain database at $db, restoring from Mithril."
        # An earlier attempt may have died part way through and left files that
        # the v2 backend refuses to overwrite. Only ever a partial restore:
        # the check above proved there is no usable database here.
        rm -rf "$db"

        # download verifies the certificate chain against the genesis key and
        # the ancillary files against the ancillary key. --include-ancillary
        # pulls the latest ledger state too, which saves hours of replay.
        AGGREGATOR_ENDPOINT=${lib.escapeShellArg env.mithrilAggregatorEndpointUrl} \
        GENESIS_VERIFICATION_KEY=${lib.escapeShellArg env.mithrilGenesisVerificationKey} \
        ANCILLARY_VERIFICATION_KEY=${lib.escapeShellArg env.mithrilAncillaryVerificationKey} \
          mithril-client cardano-db download latest \
            --download-dir ${dir} \
            --include-ancillary

        echo "${name}: restored $db from Mithril."
      '';
    };

  nodeService =
    name: port:
    nameValuePair "cardano-node-${name}" {
      description = "cardano-node (${name})";
      documentation = [ "https://developers.cardano.org/docs/get-started/cardano-node/" ];
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];

      # A node that cannot start is one that should keep trying, not one
      # systemd gives up on after five attempts.
      unitConfig.StartLimitIntervalSec = 0;

      serviceConfig = {
        User = user;
        Group = user;
        WorkingDirectory = dirOf name;

        # The Mithril restore is an ExecStartPre rather than a unit of its own
        # so that it is re-checked on every start, including systemd's own
        # restarts, which do not reliably re-run a Requires= dependency.
        ExecStartPre = lib.getExe (bootstrap name);

        ExecStart = lib.concatStringsSep " " [
          (lib.getExe cardano-node)
          "run"
          "--config ${configFile name}"
          "--topology ${topologyDir name}/topology.json"
          "--database-path ${dirOf name}/db"
          "--socket-path ${dirOf name}/node.socket"
          "--host-addr 0.0.0.0"
          "--port ${toString port}"
        ];

        Restart = "on-failure";
        RestartSec = 30;
        # Mainnet from nothing is hours of download and verification.
        TimeoutStartSec = "12h";
        # Let it flush the ledger rather than killing it mid-write.
        TimeoutStopSec = "5min";
        LimitNOFILE = 65535;

        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectHome = true;
      };
    };
in
{
  users.users.${user} = {
    isSystemUser = true;
    group = user;
    description = "cardano-node";
  };
  users.groups.${user} = { };

  # So the admin account can point cardano-cli at the sockets.
  users.users.hydra.extraGroups = [ user ];

  systemd.tmpfiles.rules = [
    "d ${dataRoot} 0755 ${user} ${user} -"
  ]
  ++ mapAttrsToList (name: _: "d ${dirOf name} 0755 ${user} ${user} -") networks;

  systemd.services = mapAttrs' nodeService networks;

  environment.shellAliases = {
    cardano-logs = "journalctl -f -u 'cardano-node-*'";
  };
}
