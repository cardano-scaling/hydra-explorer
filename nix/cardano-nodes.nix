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

  # Distinct ports so the three nodes can share the host. prometheusPort matters
  # too: the shared default trace config binds 127.0.0.1:12798, and on one host
  # only the first node to start would get it.
  networks = {
    preview = {
      port = 3001;
      prometheusPort = 12798;
    };
    preprod = {
      port = 3002;
      prometheusPort = 12799;
    };
    mainnet = {
      port = 3003;
      prometheusPort = 12800;
    };
  };

  dirOf = name: "${dataRoot}/${name}";

  # config.json and topology.json come out of the pinned cardano-node flake, so
  # there is nothing to fetch at runtime and nothing to keep in sync by hand.
  # The one thing we do rewrite is the Prometheus backend's port, which is the
  # same in every network's defaults and cannot be with three nodes on one host.
  configFile =
    name: prometheusPort:
    let
      base = environments.${name}.nodeConfig;
      backends = base.TraceOptions."".backends or [ ];
      retarget =
        b:
        if lib.hasPrefix "PrometheusSimple " b then
          "PrometheusSimple 127.0.0.1 ${toString prometheusPort}"
        else
          b;
      nodeConfig =
        if backends == [ ] then
          base
        else
          lib.recursiveUpdate base { TraceOptions."".backends = map retarget backends; };
    in
    pkgs.writeText "cardano-node-${name}-config.json" (builtins.toJSON nodeConfig);

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

  # Restore from a Mithril snapshot when there is no usable chain database, then
  # become the node. A node that already has a database just catches up from
  # where it left off, which is the whole point of not restoring every start.
  #
  # This is one ExecStart rather than an ExecStartPre plus an ExecStart because
  # a Type=simple start job is not complete until every ExecStartPre has
  # finished, and switch-to-configuration blocks on that job: a cold mainnet
  # restore would hold `nixos-rebuild switch` open for hours. Here the unit is
  # active the moment this script forks, and the restore runs inside it.
  nodeScript =
    name:
    { port, prometheusPort }:
    let
      env = environments.${name};
      dir = dirOf name;
    in
    pkgs.writeShellApplication {
      name = "cardano-node-${name}";
      runtimeInputs = [
        cardano-node
        mithril-client
      ];
      text = ''
        db=${dir}/db

        restore() {
          if [ -e "$db" ]; then
            echo "${name}: $db has no protocolMagicId, so an earlier restore"
            echo "${name}: did not finish. Clearing it and starting over."
          else
            echo "${name}: no chain database at $db, restoring from Mithril."
          fi
          # Also required by the v2 backend, which will not write into a
          # non-empty directory.
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
        }

        # protocolMagicId is the right thing to test for. Mithril writes it as
        # the very last step of a restore, cardano-node writes it when it
        # initialises an empty database, and cardano-node refuses to start on a
        # non-empty directory that lacks it ("NoDbMarkerAndNotEmpty"). Testing
        # for immutable chunks instead would call an interrupted restore a
        # database and hand the node a directory it will not touch.
        if [ -e "$db/protocolMagicId" ]; then
          echo "${name}: chain database present at $db, not touching Mithril."
        else
          restore
        fi

        exec cardano-node run \
          --config ${configFile name prometheusPort} \
          --topology ${topologyDir name}/topology.json \
          --database-path "$db" \
          --socket-path ${dir}/node.socket \
          --host-addr 0.0.0.0 \
          --port ${toString port}
      '';
    };

  nodeService =
    name:
    { port, prometheusPort }:
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

        ExecStart = lib.getExe (nodeScript name { inherit port prometheusPort; });

        Restart = "on-failure";
        RestartSec = 30;
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
