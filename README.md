# hydra-explorer

This is the system image repository for the hydra-explorer service.

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
