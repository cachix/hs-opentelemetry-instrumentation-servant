{ pkgs, lib, ... }:

{
  languages.haskell.enable = true;

  processes = {
    opentelemetry-collector.exec =
      "${lib.getExe pkgs.opentelemetry-collector-contrib} --config ./exe/otelconfig.yaml";
    # Run the executable in exe
    server.exec = "${lib.getExe pkgs.haskellPackages.ghcid} -c 'cabal repl exe:server' --test 'main'";
  };

  packages = [
    pkgs.ormolu
    pkgs.opentelemetry-collector-contrib
  ];
}
