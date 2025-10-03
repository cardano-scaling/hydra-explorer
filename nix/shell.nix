{ inputs, pkgs, lib, project, ghc }:
let
  allTools = {
    "ghc966".cabal                    = project.projectVariants.ghc966.tool "cabal" "latest";
    "ghc966".haskell-language-server  = project.projectVariants.ghc966.tool "haskell-language-server" "latest";
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
    pkgs.yarn # For hacking on the UI
    pkgs.isd # interactive systemd viewer
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
