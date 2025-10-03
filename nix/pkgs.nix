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
    inputs.nix-npm-buildpackage.overlays.default
    (final: prev: {
      cardano-node = inputs.cardano-node.packages.${final.system}.cardano-node;
      cardano-cli = inputs.cardano-node.packages.${final.system}.cardano-cli;
      hydra-chain-observer = inputs.hydra.packages.${final.system}.hydra-chain-observer;
      hydra-node = inputs.hydra.packages.${final.system}.hydra-node;
    })
  ];
}
