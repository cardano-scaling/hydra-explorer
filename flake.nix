{
  description = "hydra-explorer";

  nixConfig = {
    extra-substituters = "https://horizon.cachix.org";
    extra-trusted-public-keys = "horizon.cachix.org-1:MeEEDRhRZTgv/FFGCv3479/dmJDfJ82G6kfUDxMSAw0=";
  };

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    horizon-devtools.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-devtools?ref=lts/ghc-9.6.x";
    horizon-hydra.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-hydra";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "x86_64-linux"
      ];
      perSystem = { pkgs, system, ... }:
        let

          myOverlay = final: prev: { hydra-explorer = final.callCabal2nix "hydra-explorer" ./hydra-explorer { }; };

          legacyPackages = inputs.horizon-hydra.legacyPackages.${system}.extend myOverlay;

        in
        {

          devShells.default = legacyPackages.hydra-explorer.env.overrideAttrs (attrs: {
            buildInputs = attrs.buildInputs ++ [
              legacyPackages.cabal-install
              inputs.horizon-devtools.legacyPackages.${system}.haskell-language-server
              inputs.horizon-hydra.legacyPackages.${system}.cardano-cli
              inputs.horizon-hydra.legacyPackages.${system}.cardano-node
              inputs.horizon-hydra.legacyPackages.${system}.hydra-node
            ];
          });

          packages.default = legacyPackages.hydra-explorer;

        };
    };
}
