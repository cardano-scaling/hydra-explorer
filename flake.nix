{
  description = "hydra-explorer application and system image";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-24.11";

    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    cardano-node.url = "github:IntersectMBO/cardano-node/9.2.0";
    # 0.20.0 + http-chain-observer work
    hydra.url = "github:cardano-scaling/hydra/3a95fb30d4a1e88d3bfde1fc868044a158061ba8";

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };

    iogx = {
      url = "github:input-output-hk/iogx";
      inputs.hackage.follows = "hackage";
      inputs.CHaP.follows = "CHaP";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.nixpkgs.follows = "haskell-nix/nixpkgs";
    };

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-npm-buildpackage.url = "github:serokell/nix-npm-buildpackage";

    agenix.url = "github:ryantm/agenix";
  };

  outputs = inputs:
    let
      systems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
        "aarch64-linux"
      ];
    in
    inputs.iogx.lib.mkFlake {

      nixpkgsArgs.overlays = [
        inputs.nix-npm-buildpackage.overlays.default
        (final: prev: {
          cardano-node = inputs.cardano-node.packages.${final.system}.cardano-node;
          cardano-cli = inputs.cardano-node.packages.${final.system}.cardano-cli;
          hydra-chain-observer = inputs.hydra.packages.${final.system}.hydra-chain-observer;
          hydra-node = inputs.hydra.packages.${final.system}.hydra-node;
        })
      ];

      inherit inputs;
      inherit systems;

      repoRoot = ./.;

      flake = _: {
        nixosConfigurations.hydra-explorer =
          inputs.nixpkgs.lib.nixosSystem
            {
              system = "x86_64-linux";
              specialArgs = inputs;
              modules = [
                {
                  imports = [
                    "${inputs.nixpkgs}/nixos/modules/virtualisation/amazon-image.nix"
                    (import ./nix/hydra-explorer-configuration.nix)
                  ];
                }
                inputs.agenix.nixosModules.default
              ];
            };
      };

      outputs = import ./nix/outputs.nix;
    };

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
