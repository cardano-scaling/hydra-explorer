{
  description = "hydra-explorer application and system image";

  inputs = {

    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    cardano-node.url = "github:IntersectMBO/cardano-node/9.2.0";

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
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixinate.url = "github:MatthewCroughan/nixinate";

    nix-npm-buildpackage.url = "github:serokell/nix-npm-buildpackage";

    nixpkgs.follows = "haskell-nix/nixpkgs";
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
      ];

      inherit inputs;
      inherit systems;

      repoRoot = ./.;

      flake = _: {
        nixosConfigurations.hydra-explorer =
          import ./nix/hydra-explorer-aws.nix { inherit inputs; };
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
