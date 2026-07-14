[private]
default:
  @just --list

# format Nix, Haskell and JavaScript
fmt:
  nix fmt

# deploy to the server
deploy:
  nixos-rebuild switch \
    --target-host root@34.51.167.25 \
    --flake .#explorer-gce \
    --sudo --ask-sudo-password
