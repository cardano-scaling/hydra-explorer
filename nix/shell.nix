{
  inputs,
  pkgs,
  lib,
  project,
  ghc,
}:
let
  # Unstable nixpkgs for frontend tooling; the primary (haskell.nix) pin
  # predates pnpm 11 as used by hydra-explorer/web.
  unstablePkgs = import inputs.unstableNixpkgs {
    inherit (pkgs.stdenv.hostPlatform) system;
  };

  allTools = {
    "ghc967".cabal = project.projectVariants.ghc967.tool "cabal" "latest";
    "ghc967".haskell-language-server =
      project.projectVariants.ghc967.tool "haskell-language-server" "latest";
  };

  tools = allTools.${ghc};

  commonPackages = [
    tools.haskell-language-server
    tools.cabal

    pkgs.cardano-cli
    pkgs.cardano-node
    pkgs.hydra-chain-observer
    pkgs.hydra-node

    pkgs.check-jsonschema # For hydra-explorer:test:tests
    unstablePkgs.pnpm_11 # For hacking on the UI
    unstablePkgs.nodejs # For hacking on the UI
    pkgs.just
  ];

  shell = project.shellFor {
    name = "hydra-explorer-shell-${project.args.compiler-nix-name}";
    buildInputs = lib.concatLists [
      commonPackages
    ];

    withHoogle = true;
  };
in
shell
