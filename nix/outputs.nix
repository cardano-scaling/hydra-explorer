{ repoRoot, inputs, pkgs, lib, system }:

let

  project = repoRoot.nix.project;

in

[
  {
    apps = inputs.nixinate.nixinate."x86_64-linux" inputs.self;
  }
  (
    project.flake
  )
]
