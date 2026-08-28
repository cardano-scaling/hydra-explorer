# hydra-explorer

Contains the backend and frontend serving https://explorer.hydra.family, as well as a NixOS-based system image to deploy the service.

The backend `hydra-explorer` service aggregates data from multiple `hydra-chain-observer` instances - of different hydra versions and from different cardano networks - into a single REST API.

## Getting started

```shell
hydra-explorer
```

By default, hydra-explorer will bind onto all interfaces using hostname `0.0.0.0` and uses port `8080` for the **observer API** and port `9090` for the **client API**. To configure:

```shell
hydra-explorer \
  --observer-port 8000
  --client-port 9000
```


## Architecture

Multiple instances of `hydra-chain-observer`, each built against a specific version of the `hydra-plutus` scripts and `hydra-tx` off-chain (transaction structure), is reporting its observations to a single `hydra-explorer` instance via an [HTTP REST api](./api/observer-api.yaml).

``` mermaid
flowchart LR

  subgraph hydra 0.19.0
    plutus-19[hydra-plutus]
    tx-19[hydra-tx]
    node-19[hydra-node]
    observer-19-preview[hydra-chain-observer 0.19.0]
    plutus-19 --> tx-19
    tx-19 --> observer-19-preview
    tx-19 --> node-19
    node-19 --> observer-19-preview
  end
  preview -. chain sync .-> observer-19-preview

  subgraph hydra 0.20.0
    plutus-20[hydra-plutus]
    tx-20[hydra-tx]
    node-20[hydra-node]
    observer-20[hydra-chain-observer 0.20.0]
    plutus-20 --> tx-20
    tx-20 --> observer-20
    tx-20 --> node-20
    node-20 --> observer-20
  end
  mainnet -. chain sync .-> observer-20

  observer-19[hydra-chain-observer 0.19.0]
  mainnet -. chain sync .-> observer-19

  subgraph hydra-explorer 1.0.0
    explorer[hydra-explorer]
    explorer -. GET /heads .-> hydra-explorer-web
  end

  observer-19-preview -. POST /v1/observations/preview/19 .-> explorer
  observer-19 -. POST /v1/observations/mainnet/19 .-> explorer
  observer-20 -. POST /v1/observations/mainnet/20 .-> explorer
```

Clients to the explorer can then use the [Client REST API](../api/client-api.yaml) to query `/heads` for example.

## Build & test

In the `nix develop` shell or with `cabal` and `ghc` installed:

``` sh
cabal update
cabal build
cabal test
```

## Formatting

Format all Nix, Haskell and JavaScript sources with:

```sh
just fmt
```

This runs `nix fmt`, which uses [treefmt](https://github.com/numtide/treefmt-nix) to drive `nixfmt` (Nix), `fourmolu` (Haskell) and `prettier` (JavaScript/TypeScript).

## Deployment

The NixOS system for `explorer.hydra.family` contains:

- One `cardano-node` systemd service per network, see [nix/cardano-nodes.nix](./nix/cardano-nodes.nix)
- Github runner registered to the `cardano-scaling` organization
- Contiuously deployed `docker-compose` project, see [docker-compose.yaml](./docker-compose.yaml) and [github workflow](.github/workflows/cd.yaml)

### Cardano nodes

`preview`, `preprod` and `mainnet` each run as `cardano-node-<network>.service` under a
dedicated `cardano` user, with state in `/data/cardano/<network>/` (`db/` and `node.socket`).
The chain observers still run under docker and bind-mount those directories; there are no
cardano nodes in `docker-compose.yaml` any more.

```sh
systemctl status cardano-node-mainnet
cardano-logs                      # journalctl -f -u 'cardano-node-*'
logs-mainnet -n 10 -f             # per network, arguments pass through
stop-mainnet                      # sudo systemctl stop cardano-node-mainnet
start-mainnet                     # sudo systemctl start cardano-node-mainnet
```

`logs-<network>`, `stop-<network>` and `start-<network>` exist for all three. They are plain shell aliases rather
than wrapper scripts precisely so that arguments compose: `logs-preprod -n 10 -f` expands to
`journalctl -u cardano-node-preprod -n 10 -f`.

`config.json` and `topology.json` are generated from the pinned `cardano-node` flake input
rather than downloaded at runtime, so they move in lockstep with the node binary and a rebuild
is the only way they change. The peer snapshot ships alongside the topology in the same store
directory, which is how the node resolves the relative path in it.

Each node runs a Mithril restore as `ExecStartPre`, and **only when it needs to**. The test is
`db/protocolMagicId`: Mithril writes it as the very last step of a restore, `cardano-node`
writes it when it initialises an empty database, and `cardano-node` refuses to start on a
non-empty directory that lacks it (`NoDbMarkerAndNotEmpty`). If it is there the step logs and
exits, and the node catches up from wherever it is. If it is not, the directory is cleared and
`mithril-client cardano-db download latest` fetches the database again, verifying the
certificate chain against the network's genesis key and the ancillary files against its
ancillary key. `--include-ancillary` also pulls the latest ledger state, which saves hours of
replay on first boot. Aggregator URL and both keys come from the `cardano-node` flake, so there
is nothing to curl and nothing to hardcode.

`--json` is not cosmetic here. Without it Mithril reports progress through indicatif, which
draws to stdout and suppresses itself completely when stdout is not a terminal, so a multi-hour
restore under systemd says nothing at all. With it, steps and throttled progress go to stderr
via `eprintln` and land in the journal, where `logs-mainnet -f` can see them.

Do not test for immutable chunks instead: a restore interrupted after the first chunks land has
plenty of them and no marker, and calling that a database hands `cardano-node` a directory it
will refuse. **A `nixos-rebuild switch` during a cold restore is exactly that interruption** —
it restarts the unit and kills the download. That state now heals itself on the next start, but
it restarts the download from the beginning, so leave a fresh mainnet alone while it works.

The restore and the node are one `ExecStart` — a script that restores if it has to and then
`exec`s `cardano-node` — rather than an `ExecStartPre` plus an `ExecStart`. Two reasons. A
`Type=simple` start job is not complete until every `ExecStartPre` has finished, and
`switch-to-configuration` blocks on that job, so a cold mainnet restore under `ExecStartPre`
holds `nixos-rebuild switch` open for hours. And a separate restore *unit* would not be
re-checked on systemd's own restarts, which do not reliably re-run a `Requires=` dependency.
As `ExecStart` the unit is active the moment the script forks, the deploy returns, and the
check still runs on every single start.

The units restart on failure with no start-rate limit (`StartLimitIntervalSec=0`), so a reboot
or a transient crash recovers on its own.

Each node also gets its own Prometheus port (12798 preview, 12799 preprod, 12800 mainnet).
Every network's defaults specify `PrometheusSimple 127.0.0.1 12798`, so on one host only the
first node to start would bind it and the others log
`Network.Socket.bind: resource busy` and run without metrics.

Sizing: three nodes on one host want roughly 24 GiB of RAM once mainnet's ledger state is
loaded, and mainnet's database alone is a couple of hundred GiB. The provisioning defaults
(`m6i.2xlarge`, 500 GiB) are sized for that, not generous.

> **Migrating an existing box.** The layout is the one the containers already used, so the
> databases carry over and nothing has to be re-downloaded. Two things still need doing by
> hand. The old containers keep running until a CD run reaches the new `docker-compose.yaml`
> (which already passes `--remove-orphans`), and two nodes writing one database is worse than
> either of them being down; and the containers ran as root whereas the services do not.
>
> ```sh
> docker compose down                       # before the switch, not after
> just deploy-ec2 <host>
> chown -R cardano:cardano /data/cardano    # the switch creates the user
> systemctl restart 'cardano-node-*'        # they will have failed until the chown
> docker compose up -d --remove-orphans
> ```

There are two system images, one per cloud. They share
[nix/hydra-explorer-configuration.nix](./nix/hydra-explorer-configuration.nix) and differ only
in the bootloader, drivers and guest agent, so **an image must only ever be deployed to the
cloud it was built for**. Deploying `.#explorer-gce` to an EC2 host, or the reverse, leaves the
box unbootable.

| Cloud | Image | System |
|---|---|---|
| GCE (currently live) | `nix build .#gce` -> `result/*.raw.tar.gz` | `.#explorer-gce` |
| EC2 | `nix build .#ami` -> `result-ami/*.img` | `.#explorer-ec2` |

Configuration changes go to a running EC2 instance with:

```sh
just deploy-ec2 <host>
```

which runs `nixos-rebuild switch --flake .#explorer-ec2` against it. When prompted for the
password, just press enter. The host is a required argument until the address settles, at
which point give the parameter a default. The GCE equivalent is commented out in the
[justfile](./justfile) as `deploy-gce`, with its address baked in.

Each recipe is hardwired to one flake target, so the only way to cross the two clouds is to run
`nixos-rebuild` by hand.

Every recipe runs inside the `.#deploy` shell, which carries `aws`, `coldsnap`, `jq`,
`nixos-rebuild` and `openssh` and is separate from the Haskell `nix develop`. Nothing has to be
entered first; `nix develop .#deploy` is there for running the steps by hand.

### Github runner

The runner is registered to the `cardano-scaling` organization with the labels `nixos`,
`self-hosted`, `explorer` and `cardano`, so hydra's [smoke
test](https://github.com/cardano-scaling/hydra/blob/master/.github/workflows/smoke-test.yaml)
(`runs-on: [self-hosted, cardano]`) lands here and drives the nodes above through
`/data/cardano/<network>/`.

It runs as `root`, which is what lets it clean up the root-owned files docker leaves in its
work directory, but that root is not the usual one: the nixpkgs module empties
`CapabilityBoundingSet` and sets `PrivateUsers=true`, so it has no `CAP_DAC_OVERRIDE` and the
`cardano` uid is not mapped into its user namespace. Against the node's files it gets only the
"other" permission bits. That is why three things have to line up:

- the runner's own group is `cardano` (a supplementary group would not survive `PrivateUsers`,
  the unit's own group does),
- the nodes run with `UMask=0002`, since `connect(2)` on `node.socket` needs the write bit and
  the socket is created with the unit's umask, and
- `/data/cardano/<network>` is `2775`, since the smoke test writes its hydra-node state next to
  `db/` and setgid keeps that in the group.

`ReadWritePaths` stays too: under `ProtectSystem=strict` the whole of `/data` would be mounted
read-only for the job.

The socket only picks up a new mode when it is recreated, so after changing any of this:

```sh
just deploy-ec2 <host>
systemctl restart 'cardano-node-*'
ls -l /data/cardano/preview/node.socket   # srwxrwxr-x cardano cardano
```

### Provisioning an EC2 instance

```sh
aws configure sso --profile hydra    # once; or 'aws configure' for static keys
aws sso login --profile hydra

just profile=hydra region=eu-central-1 provision
```

`just provision` builds the image, uploads it into an EBS snapshot with
[coldsnap](https://github.com/awslabs/coldsnap) (no import job or S3 bucket involved),
registers a UEFI AMI, creates the security group and launches the instance, printing the
address at the end. The image is a raw UEFI disk: GPT, an `ESP` partition and an ext4 root
labelled `nixos`.

Settings are just variables, so they can be overridden per invocation or exported as the
matching `EC2_*` environment variable:

| Variable | Default | Notes |
|---|---|---|
| `region` | the profile's region | |
| `profile` | default credential chain | |
| `instance_type` | `m6i.2xlarge` | must be a Nitro type that supports UEFI boot; `m6i`/`c6i`/`m7i` do |
| `volume_size` | `500` | GiB. The image is ~7 GiB; the root partition grows to fill the volume on first boot |
| `name` | `hydra-explorer` | name tag, security group name and AMI name prefix |
| `key_name` | none | an EC2 key pair, as a break-glass root key (see below) |
| `snapshot`, `ami` | none | resume a run that failed late without redoing the earlier steps |

The security group and the host key are reused if they already exist, so re-running is cheap.
`EC2_SUBNET_ID`, `EC2_SSH_CIDR` and `EC2_WEB_CIDR` are honoured too, but only as environment
variables.

The AWS credentials need `ebs:StartSnapshot`, `ebs:PutSnapshotBlock`, `ebs:CompleteSnapshot`,
`ec2:DescribeSnapshots`, `ec2:RegisterImage`, `ec2:RunInstances` and the describe/security
group calls.

One wrinkle worth knowing about, since it looks like a credentials problem on your side and is
not: coldsnap builds `aws-config` with `default-features = false` and no `sso` feature, so it
cannot use an IAM Identity Center profile and fails with `the credentials provider was not
properly configured` even though the CLI is happily authenticated. `provision` works around it
by resolving credentials with `aws configure export-credentials` and passing them to coldsnap
in the environment, which covers SSO, `credential_process` and assumed roles alike.

### SSH keys and the `hydra` user

The login account is **`hydra`**, not root. Its keys, in
`users.users.hydra.openssh.authorizedKeys.keys`, are baked into the image and land in
`/etc/ssh/authorized_keys.d/hydra`; nothing on the instance side has to fetch them, which is
why the GCE variant force-disables OS Login.

`hydra` is in `wheel` with `security.sudo.wheelNeedsPassword = false`, so the deploy recipes'
`--sudo --ask-sudo-password` prompt still just takes an enter. It is also in `docker` (this box
runs its workload as a docker-compose project) and in `nix.settings.trusted-users`, which
`nixos-rebuild --target-host` needs: the closure is copied over ssh as `hydra` before sudo
takes over, and an untrusted user cannot add unsigned paths to the store.

Root keeps no keys of its own. If you pass `key_name=<ec2 key pair>`, `apply-ec2-data` appends
that key to `/root/.ssh/authorized_keys` on first boot, which is worth having purely as
break-glass for when the baked-in keys turn out to be wrong. It costs nothing and is off by
default.

> **One-time transition.** The deploy that first introduces the `hydra` user is the last one
> that works over `root@`, since it is the same switch that removes root's keys. Run it against
> the old target, then move to `hydra@` for everything after.

### Host keys and agenix

`age.secrets.github-runner-token` is encrypted to the `hostKey` in
[secrets/secrets.nix](./secrets/secrets.nix), and agenix decrypts it during system activation
using `/etc/ssh/ssh_host_ed25519_key`. On a freshly provisioned cloud instance that key is
generated on first boot and is random, so it can never match `secrets.nix`, and the runner and
ACME units fail with `no identity matched any of the recipients`.

`just provision` works around this: it generates a persistent host key under
`secrets/host-keys/<name>/` and hands it to the instance through user-data, which
`ec2-data.nix` installs on first boot. It reuses an existing key, so reprovisioning the same
box keeps its identity. At the end of the run it prints the public key; put that in
`secrets/secrets.nix` as `hostKey`, then rekey:

```sh
cd secrets && nix run github:ryantm/agenix -- -r   # needs a private key from 'users'
```

Two things to know:

- **The first boot still fails.** agenix runs during activation, `apply-ec2-data` runs later in
  `multi-user.target`, so the key only exists from the second boot onwards. Reboot once after
  provisioning and the secret decrypts.
- **The private host key sits in user-data in the clear**, readable on the instance through
  IMDS and to anyone holding `ec2:DescribeInstanceAttribute`. `secrets/host-keys/` is
  gitignored and must stay that way.

Set `EC2_USER_DATA=<path>` to supply your own user-data instead and skip all of this.

### Testing locally

```sh
nix build .#qemu
cp result/nixos.qcow2 .
chmod 755 nixos.qcow2
qemu-system-x86_64 -enable-kvm -m 8000 -drive file=nixos.qcow2,media=disk,if=virtio -nic user,model=virtio
```

#### Todo

- [x] Run cardano-nodes as systemd services, not docker; it's really annoying.
- [x] Have the mithril bootstrap automatic; without it it takes way too long
- [x] Obtain the right version of the cardano configs automatically; it's crazy to do it by hand
- [ ] Remove all the autodeployment
