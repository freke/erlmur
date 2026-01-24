{ pkgs, inputs, ... }:
let
  pkgs-unstable = import inputs.nixpkgs-unstable { system = pkgs.stdenv.system; };
in
{
  env.LOCALE_ARCHIVE="/usr/lib/locale/locale-archive";
  dotenv.enable = true;
  
  packages = [
    pkgs.just
    pkgs.openssl
    pkgs.plantuml
    pkgs.erlfmt
  ];

  languages = {
    erlang.enable = true;
  };

  scripts.build.exec = ''
    just build
  '';

  enterShell = ''
    build
  '';

  enterTest = ''
    echo "Running tests"
    just test
  '';
}
