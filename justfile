[private]
default:
  @just --list

# deploy to the server
deploy:
  nixos-rebuild switch \
    --target-host root@34.51.251.235 \
    --flake .#explorer-gce \
    --sudo --ask-sudo-password
