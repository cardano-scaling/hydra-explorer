{ inputs, pkgs, lib, project, ghc }:
let
  allTools = {
    "ghc966".cabal                    = project.projectVariants.ghc966.tool "cabal" "latest";
    "ghc966".haskell-language-server  = project.projectVariants.ghc966.tool "haskell-language-server" "latest";
    # "ghc966".cabal-fmt                = project.projectVariants.ghc966.tool "cabal-fmt" "latest";
    # "ghc966".stylish-haskell          = project.projectVariants.ghc966.tool "stylish-haskell" "latest";
    # "ghc966".hlint                    = project.projectVariants.ghc966.tool "hlint" "latest";
  };

  tools = allTools.${ghc};

  commonPackages = [
    tools.haskell-language-server
    tools.cabal

    pkgs.cardano-cli
    pkgs.cardano-node
    pkgs.hydra-chain-observer
    pkgs.hydra-node

    pkgs.yarn
    pkgs.check-jsonschema
    pkgs.isd
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


# Docs for this file: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#mkhaskellprojectinshellargs
# See `shellArgs` in `mkHaskellProject` in ./project.nix for more details.

# { repoRoot, inputs, pkgs, lib, system }:

# cabalProject:

# {
#   name = "nix-shell";
#   packages = [
#     pkgs.cardano-cli
#     pkgs.cardano-node
#     pkgs.hydra-chain-observer
#     pkgs.hydra-node
#     pkgs.check-jsonschema # For hydra-explorer:test:tests
#     pkgs.yarn # For hacking on the UI
#   ];
# }
