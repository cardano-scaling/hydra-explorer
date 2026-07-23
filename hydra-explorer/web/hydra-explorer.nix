{ pkgs }:

pkgs.stdenv.mkDerivation (finalAttrs: {
  pname = "hydra-explorer-web";
  version = "1.0.0";
  src = ./.;

  nativeBuildInputs = [
    pkgs.nodejs
    pkgs.pnpm_11
    pkgs.pnpmConfigHook
  ];

  pnpmDeps = pkgs.fetchPnpmDeps {
    inherit (finalAttrs) pname version src;
    pnpm = pkgs.pnpm_11;
    fetcherVersion = 3;
    hash = "sha256-jyN0+sJAgJ0hVx2Q+uq3xPARUL2weQDMzNJaZE/czEA=";
  };

  env.NEXT_TELEMETRY_DISABLED = "1";

  buildPhase = ''
    runHook preBuild
    pnpm build
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp -r out/* $out
    runHook postInstall
  '';
})
