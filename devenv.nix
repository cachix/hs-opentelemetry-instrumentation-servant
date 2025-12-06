{ pkgs, ... }:

{
  languages.haskell.enable = true;

  packages = [
    pkgs.ghcid
    pkgs.opentelemetry-collector-contrib
  ];

  git-hooks.hooks = {
    ormolu.enable = true;
    nixfmt.enable = true;
  };

  processes = {
    opentelemetry-collector.exec = "otelcol-contrib --config ./exe/otelconfig.yaml";

    server.exec = "ghcid -c 'cabal repl exe:server' --test main";
  };
}
