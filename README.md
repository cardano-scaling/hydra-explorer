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

Multiple instances of `hydra-chain-observer`, each built against a specific version of the `hydra-plutus` scripts and `hydra-tx` off-chain (transaction structure), is reporting its observations to a single `hydra-explorer` instance via an [HTTP REST api](./api/observer.yaml).

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
  end

  observer-19-preview -. POST /v1/observations/preview/19 .-> explorer
  observer-19 -. POST /v1/observations/mainnet/19 .-> explorer
  observer-20 -. POST /v1/observations/mainnet/20 .-> explorer
```

## Build & test

In the `nix develop` shell or with `cabal` and `ghc` installed:

``` sh
cabal update
cabal build
cabal test
```

## Deployment

```sh
nixos-rebuild switch --target-host root@explorer.hydra.family --flake .#hydra-explorer --use-remote-sudo
```

## Testing locally

```sh
nix build .#qemu
cp result/nixos.qcow2 .
chmod 755 nixos.qcow2
qemu-system-x86_64 -enable-kvm -m 8000 -drive file=nixos.qcow2,media=disk,if=virtio -nic user,model=virtio
```
