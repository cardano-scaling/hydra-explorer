{ repoRoot, inputs, pkgs, system, lib }:

let

  cabalProject' = pkgs.haskell-nix.cabalProject' ({ pkgs, config, ... }:
    let
      # When `isCross` is `true`, it means that we are cross-compiling the project.
      # WARNING You must use the `pkgs` coming from cabalProject' for `isCross` to work.
      isCross = pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform;
    in
    {
      src = ../.;

      shell.withHoogle = true;

      inputMap = {
        "https://chap.intersectmbo.org/" = inputs.iogx.inputs.CHaP;
      };

      name = "hydra-explorer";
      compiler-nix-name = lib.mkDefault "ghc966";
      modules = [
        # XXX: This should not be needed here as proto-lens-protobuf,
        # proto-lens-etcd and hydra-node should not be needed
        {
          packages.proto-lens-protobuf-types.components.library.build-tools = [ pkgs.protobuf ];
          packages.proto-lens-etcd.components.library.build-tools = [ pkgs.protobuf ];
          packages.hydra-node.components.library.build-tools = [ pkgs.etcd ];
        }
      ];
    });


  cabalProject = cabalProject'.appendOverlays [ ];


  # Docs for mkHaskellProject: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#mkhaskellproject
  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;

    shellArgs = repoRoot.nix.shell;
  };

in

project
