{ pkgs, ... }:

{
  languages.haskell.enable = true;

  packages = [
    pkgs.ghcid
    pkgs.opentelemetry-collector-contrib
  ];

  pre-commit.hooks = {
    ormolu.enable = true;
    nixpkgs-fmt.enable = true;
  };

  processes = {
    opentelemetry-collector.exec =
      "otelcontribcol --config ./exe/otelconfig.yaml";

    server.exec =
      "ghcid -c 'cabal repl exe:server' --test main";
  };
}
