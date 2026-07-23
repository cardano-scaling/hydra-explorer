{ inputs, system }:

let
  inherit (pkgs) lib;

  pkgs = import ./pkgs.nix { inherit inputs system; };

  project = import ./project.nix { inherit inputs pkgs lib; };

  # Plain nixpkgs (no haskell.nix overlay) for the formatters.
  treefmtEval = inputs.treefmt-nix.lib.evalModule (import inputs.nixpkgs {
    inherit system;
  }) ./treefmt.nix;

  mkShell =
    ghc:
    import ./shell.nix {
      inherit
        inputs
        pkgs
        lib
        project
        ghc
        ;
    };

  projectFlake = project.flake { };

  apps = projectFlake.apps;

  packages = rec {
    # Uses unstable nixpkgs as the primary (haskell.nix) pin predates pnpm 11.
    hydra-explorer-web = import ../hydra-explorer/web/hydra-explorer.nix {
      pkgs = import inputs.unstableNixpkgs { inherit system; };
    };

    hydra-explorer-static =
      project.projectCross.musl64.hsPkgs.hydra-explorer.components.exes.hydra-explorer;

    # Broken due to above being broken.
    docker = pkgs.dockerTools.streamLayeredImage {
      name = "hydra-explorer";
      tag = "latest";
      created = "now";
      config = {
        Entrypoint = [ "${hydra-explorer-static}/bin/hydra-explorer" ];
        WorkingDir = "/";
      };
      # Copy the static files to /static in the docker image
      contents = [
        (pkgs.runCommand "hydra-explorer-static-files" { } ''
          mkdir $out
          ln -s ${hydra-explorer-web} $out/static
        '')
      ];
    };

    # Broken; I don't know why.
    # A place to hack on the image to see how it works.
    qemu = inputs.nixos-generators.nixosGenerate {
      specialArgs = {
        # inherit inputs;
        diskSize = 10 * 1024;
      };

      format = "qcow";

      modules = [
        (import ./hydra-explorer-configuration.nix {
          inherit inputs pkgs lib;
          cardano-node-module = inputs.cardano-node.nixosModules.cardano-node;
          hydra-explorer = inputs.self.packages.x86_64-linux.hydra-explorer;
          hydra-explorer-web = inputs.self.packages.x86_64-linux.hydra-explorer-web;
        })
      ];
    };
  }
  // project.hsPkgs.hydra-explorer.components.exes
  // lib.optionalAttrs (system == "x86_64-linux") {
    # GCP image (.raw.tar.gz). Build with: nix build .#gce
    gce = inputs.self.nixosConfigurations.explorer-gce.config.system.build.googleComputeImage;
  };

  devShells = rec {
    default = mkShell "ghc967";
  };

in
{
  inherit apps;
  inherit packages;
  inherit devShells;
  formatter = treefmtEval.config.build.wrapper;
}
