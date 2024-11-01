{ repoRoot, inputs, pkgs, lib, system }:

let
  project = repoRoot.nix.project;
in
[
  {
    apps = inputs.nixinate.nixinate."x86_64-linux" inputs.self;

    # A place to hack on the image to see how it works.
    packages.qemu = inputs.nixos-generators.nixosGenerate {
      inherit system;
      specialArgs = {
        inherit
          inputs;
        diskSize = 10 * 1024;
      };

      format = "qcow";

      modules = [
        (import ./hydra-explorer-configuration.nix {
          cardano-node-module = inputs.cardano-node.nixosModules.cardano-node;
          hydra-explorer = inputs.self.packages.x86_64-linux.hydra-explorer;
        })
      ];
    };
  }
  (
    project.flake
  )
]
