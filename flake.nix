{
  description = "hydra-explorer application and system image";

  inputs = {
    unstableNixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };

    nixpkgs.follows = "haskell-nix/nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cardano-node.url = "github:IntersectMBO/cardano-node/11.0.1";
    hydra.url = "github:cardano-scaling/hydra/2.3.0";
    # Rust accumulator library required (via pkg-config) by hydra-tx >= 2.x;
    # pinned to the same revision as hydra 2.3.0.
    rust-accumulator.url = "github:cardano-scaling/rust-accumulator/e5f6cfc13b075282fc0580700a66ce693c5d2d53";
    mithril.url = "github:input-output-hk/mithril/2543.1-hotfix";

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    agenix.url = "github:ryantm/agenix";
  };

  outputs =
    inputs:
    let
      output =
        inputs.flake-utils.lib.eachDefaultSystem (
          system: import ./nix/outputs.nix { inherit inputs system; }
        )
        // (
          let
            unstablePkgs = import inputs.unstableNixpkgs {
              system = "x86_64-linux";
              # github-runner bundles nodejs_20 (EOL) to run JS actions.
              config.permittedInsecurePackages = [
                "nodejs-20.20.2"
                "nodejs-slim-20.20.2"
              ];
            };
            # Newer version as 0.236.0 is incompatible; and that's the
            # latest version from haskell.nix's nixpkgs.
            github-runner-new = unstablePkgs.github-runner;
          in
          {
            # GCE system image. Build with `nix build .#gce`; deploy with
            # `nixos-rebuild switch --flake .#explorer-gce`.
            nixosConfigurations.explorer-gce = inputs.nixpkgs.lib.nixosSystem {
              system = "x86_64-linux";
              specialArgs = { inherit inputs github-runner-new; };
              modules = [
                {
                  imports = [
                    # Adds system.build.googleComputeImage.
                    "${inputs.nixpkgs}/nixos/modules/virtualisation/google-compute-image.nix"
                    # Access is granted via declared root keys, not GCP OS Login,
                    # whose PAM account module otherwise closes the connection.
                    (
                      { lib, ... }:
                      {
                        security.googleOsLogin.enable = lib.mkForce false;
                      }
                    )
                    (import ./nix/hydra-explorer-configuration.nix)
                  ];
                  # Size the image to the closure; the default is too small and
                  # the root partition grows to the disk on first boot.
                  virtualisation.diskSize = "auto";
                }
                inputs.agenix.nixosModules.default
              ];
            };
          }
        );
    in
    output;

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };
}
