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

    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cardano-node.url = "github:IntersectMBO/cardano-node/10.1.4";
    # 0.21.0 + new HeadObservation api
    hydra.url = "github:cardano-scaling/hydra/cbbc35457a5b6252a9e690e6e9a0922799d317e3";

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
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
      output = inputs.flake-utils.lib.eachDefaultSystem (system:
        import ./nix/outputs.nix { inherit inputs system; }
        ) // {
          nixosConfigurations.explorer =
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
    in output;

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
