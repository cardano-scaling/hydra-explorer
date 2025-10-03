{ inputs, system }:

let
  inherit (pkgs) lib;

  pkgs = import ./pkgs.nix { inherit inputs system; };

  project = import ./project.nix { inherit inputs pkgs lib; };

  mkShell = ghc: import ./shell.nix {
    inherit inputs pkgs lib project ghc;
  };

  packages = rec {
    hydra-explorer-web = import ../hydra-explorer/web/hydra-explorer.nix
    {
      inherit pkgs;
    };

    # Broken; we've lost this 'cross' attribute; there's a different way
    # to do it now:
    #
    # <https://input-output-hk.github.io/haskell.nix/tutorials/cross-compilation.html?highlight=cross#cross-compilation>
    #
    hydra-explorer-static = builtins.throw "Broken";
      # project.cross.musl64.cabalProject.hsPkgs.hydra-explorer.components.exes.hydra-explorer;


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
      inherit system;
      specialArgs = {
        inherit inputs;
        diskSize = 10 * 1024;
      };

      format = "qcow";

      modules = [
        (import ./hydra-explorer-configuration.nix {
          cardano-node-module = inputs.cardano-node.nixosModules.cardano-node;
          hydra-explorer = inputs.self.packages.x86_64-linux.hydra-explorer;
          hydra-explorer-web = inputs.self.packages.x86_64-linux.hydra-explorer-web;
        })
      ];
    };
  };

  devShells = rec {
    default = mkShell "ghc966";
  };

in
  {
    inherit packages;
    inherit devShells;
  }
