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
