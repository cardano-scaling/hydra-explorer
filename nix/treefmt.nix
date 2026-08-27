# treefmt config driving `nix fmt`. See nix/outputs.nix for wiring.
{ ... }:
{
  projectRootFile = "flake.nix";

  # Nix
  programs.nixfmt.enable = true;

  # Haskell (picks up ./fourmolu.yaml automatically)
  programs.fourmolu.enable = true;

  # Shell, for scripts/ec2 (defaults: 2-space indent, -s)
  programs.shfmt.enable = true;

  # JavaScript / TypeScript only; scoped so prettier does not reformat the
  # repo's markdown, docker-compose, or GitHub workflow yaml.
  programs.prettier = {
    enable = true;
    includes = [
      "*.js"
      "*.jsx"
      "*.ts"
      "*.tsx"
      "*.mjs"
      "*.cjs"
    ];
  };
}
