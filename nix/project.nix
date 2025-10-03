{ inputs, pkgs, lib }:

let
  cabalProject = pkgs.haskell-nix.cabalProject' (
    { config, pkgs, ... }:
    {
      name = "hydra-explorer";
      compiler-nix-name = lib.mkDefault "ghc966";
      src = lib.cleanSource ../.;
      # shell.withHoogle = true;
      inputMap = { "https://chap.intersectmbo.org/" = inputs.CHaP; };
      modules = [
        # XXX: This should not be needed here as proto-lens-protobuf,
        # proto-lens-etcd and hydra-node should not be needed
        {
          packages.proto-lens-protobuf-types.components.library.build-tools = [ pkgs.protobuf ];
          packages.proto-lens-etcd.components.library.build-tools = [ pkgs.protobuf ];
          packages.hydra-node.components.library.build-tools = [ pkgs.etcd ];
        }
      ];
      flake.variants = {
        ghc966 = {};
      };
    }
  );
in
  cabalProject
