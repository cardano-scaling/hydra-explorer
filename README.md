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

- Github runner registered to the `cardano-scaling` organization
- Contiuously deployed `docker-compose` project, see [docker-compose.yaml](./docker-compose.yaml) and [github workflow](.github/workflows/cd.yaml)

The service runs on a Google Compute Engine instance. Build the GCE system image (a `.raw.tar.gz` under `result/`, suitable for creating a GCP custom image) with:

```sh
nix build .#gce
```

Configuration changes are deployed to the running instance with:

```sh
just deploy
```

which runs `nixos-rebuild switch --flake .#explorer-gce` against the GCE host. When prompted for the password, just press enter.

### Testing locally

```sh
nix build .#qemu
cp result/nixos.qcow2 .
chmod 755 nixos.qcow2
qemu-system-x86_64 -enable-kvm -m 8000 -drive file=nixos.qcow2,media=disk,if=virtio -nic user,model=virtio
```

#### Todo

- [ ] Run cardano-nodes as systemd services, not docker; it's really annoying.
- [ ] Have the mithril bootstrap automatic; without it it takes way too long
- [ ] Obtain the right version of the cardano configs automatically; it's crazy to do it by hand
- [ ] Remove all the autodeployment
