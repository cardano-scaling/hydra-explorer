{
  inputs,
  pkgs,
  lib,
}:

let
  cabalProject = pkgs.haskell-nix.cabalProject' (
    { config, pkgs, ... }:
    {
      name = "hydra-explorer";
      compiler-nix-name = lib.mkDefault "ghc967";
      src = lib.cleanSource ../.;
      # shell.withHoogle = true;
      inputMap = {
        "https://chap.intersectmbo.org/" = inputs.CHaP;
      };
      modules = [
        # XXX: This should not be needed here as proto-lens-protobuf,
        # proto-lens-etcd and hydra-node should not be needed
        {
          packages.proto-lens-protobuf-types.components.library.build-tools = [ pkgs.protobuf ];
          packages.proto-lens-etcd.components.library.build-tools = [ pkgs.protobuf ];
          packages.hydra-node.components.library.build-tools = [ pkgs.etcd_3_5 ];
        }
        # GHC 9.6.7 has a haddock bug (tyConStupidTheta) that panics on data
        # types declared with the deprecated DatatypeContexts extension.
        # Skip haddocks for the affected upstream packages so `withHoogle`
        # can still index everything else (same workaround as in hydra).
        {
          packages.cardano-diffusion.doHaddock = false;
          packages.cardano-ledger-shelley.doHaddock = false;
          packages.ouroboros-network.doHaddock = false;
          packages.hydra-cardano-api.doHaddock = false;
        }
      ];
      flake.variants = {
        ghc967 = { };
      };
    }
  );
in
cabalProject
