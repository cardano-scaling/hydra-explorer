{
  description = "hydra-explorer application and system image";

  inputs = {

    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    flake-parts.url = "github:hercules-ci/flake-parts";

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

    nixinate.url = "github:MatthewCroughan/nixinate";

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
    inputs.iogx.lib.mkFlake
      {

        inherit inputs;
        inherit systems;

        repoRoot = ./.;

        outputs = import ./nix/outputs.nix;

      } //
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      inherit systems;
      flake = {

        apps = inputs.nixinate.nixinate."x86_64-linux" inputs.self;

        nixosConfigurations = {
          hydra-explorer = inputs.nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = inputs;
            modules = [
              {
                _module.args.nixinate = {
                  host = "explorer.hydra.family";
                  sshUser = "root";
                  buildOn = "local";
                  substituteOnTarget = true;
                  hermetic = false;
                };
              }
              {
                imports = [ "${inputs.nixpkgs}/nixos/modules/virtualisation/amazon-image.nix" ];

                networking.hostName = "hydra-explorer";

                nix.settings.trusted-users = [ "root" ];

                services.openssh.enable = true;

                system.stateVersion = "24.05";

                networking.firewall = {
                  allowedTCPPorts = [ 25 80 443 ];
                  enable = false;
                };
              }
            ];
          };
        };
      };
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
