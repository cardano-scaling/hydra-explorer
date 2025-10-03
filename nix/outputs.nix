{ inputs, system }:

let
  inherit (pkgs) lib;

  pkgs = import ./pkgs.nix { inherit inputs system; };

  project = import ./project.nix { inherit inputs pkgs lib; };

  mkShell = ghc: import ./shell.nix {
    inherit inputs pkgs lib project ghc;
  };

  packages = {};

  devShells = rec {
    default = mkShell "ghc966";
  };

in
  {
    # xx = builtins.throw "xx";
    explorer = builtins.throw "bleh";

    # nixosConfigurations.explorer =
    #   inputs.nixpkgs.lib.nixosSystem
    #     {
    #       system = "x86_64-linux";
    #       specialArgs = inputs;
    #       modules = [
    #         {
    #           imports = [
    #             "${inputs.nixpkgs}/nixos/modules/virtualisation/amazon-image.nix"
    #             (import ./nix/hydra-explorer-configuration.nix)
    #           ];
    #         }
    #         inputs.agenix.nixosModules.default
    #       ];
    #     };

    inherit packages;
    inherit devShells;
  }

# { repoRoot, inputs, pkgs, lib, system }:

# let
#   project = repoRoot.nix.project;
# in
# [
#   project.flake
#   rec {
#     packages.hydra-explorer-web = import ../hydra-explorer/web/hydra-explorer.nix { inherit pkgs; };

#     packages.hydra-explorer-static =
#       project.cross.musl64.cabalProject.hsPkgs.hydra-explorer.components.exes.hydra-explorer;

#     packages.docker = pkgs.dockerTools.streamLayeredImage {
#       name = "hydra-explorer";
#       tag = "latest";
#       created = "now";
#       config = {
#         Entrypoint = [ "${packages.hydra-explorer-static}/bin/hydra-explorer" ];
#         WorkingDir = "/";
#       };
#       # Copy the static files to /static in the docker image
#       contents = [
#         (pkgs.runCommand "hydra-explorer-static-files" { } ''
#           mkdir $out
#           ln -s ${packages.hydra-explorer-web} $out/static
#         '')
#       ];
#     };

#     # A place to hack on the image to see how it works.
#     packages.qemu = inputs.nixos-generators.nixosGenerate {
#       inherit system;
#       specialArgs = {
#         inherit
#           inputs;
#         diskSize = 10 * 1024;
#       };

#       format = "qcow";

#       modules = [
#         (import ./hydra-explorer-configuration.nix {
#           cardano-node-module = inputs.cardano-node.nixosModules.cardano-node;
#           hydra-explorer = inputs.self.packages.x86_64-linux.hydra-explorer;
#           hydra-explorer-web = inputs.self.packages.x86_64-linux.hydra-explorer-web;
#         })
#       ];
#     };
#   }
# ]
