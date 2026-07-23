{ inputs, system }:
import inputs.nixpkgs {
  inherit system;
  config = inputs.haskell-nix.config;
  overlays = [
    inputs.iohk-nix.overlays.crypto
    inputs.iohk-nix.overlays.cardano-lib
    inputs.haskell-nix.overlay
    inputs.iohk-nix.overlays.haskell-nix-crypto
    inputs.iohk-nix.overlays.haskell-nix-extra
    (final: prev: {
      cardano-node = inputs.cardano-node.packages.${final.system}.cardano-node;
      cardano-cli = inputs.cardano-node.packages.${final.system}.cardano-cli;
      hydra-chain-observer = inputs.hydra.packages.${final.system}.hydra-chain-observer;
      hydra-node = inputs.hydra.packages.${final.system}.hydra-node;
      # Rust accumulator library required (via pkg-config) by hydra-tx >= 2.x.
      librust_accumulator = inputs.rust-accumulator.packages.${final.system}.default;
      haskell-nix = prev.haskell-nix // {
        extraPkgconfigMappings = prev.haskell-nix.extraPkgconfigMappings or { } // {
          "librust_accumulator" = [ "librust_accumulator" ];
        };
      };
    })
  ];
}
