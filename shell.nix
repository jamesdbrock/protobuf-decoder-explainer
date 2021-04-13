{ pkgs ? import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz";
  sha256 = "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq";
  }) {}
}:

let
  easy-ps = import (builtins.fetchGit {
    url = "https://github.com/justinwoo/easy-purescript-nix.git";
    rev = "1ec689df0adf8e8ada7fcfcb513876307ea34226";
  }) { inherit pkgs; };
in
pkgs.mkShell {
  buildInputs = [
    easy-ps.purs-0_13_8
    easy-ps.spago
    # easy-ps.pulp
    pkgs.nodejs-13_x
    # pkgs.nodePackages.bower
    pkgs.nodePackages.webpack
    pkgs.nodePackages.webpack-cli
    # pkgs.nodePackages.webpack-dev-server
  ];
  LC_ALL = "C.UTF-8"; # https://github.com/purescript/spago/issues/507
  shellHook = ''
  echo ""
  echo "spago build"
  echo ""
  echo "https://github.com/purescript/spago#2-spago-bundle-module"
  echo "spago bundle-module --main Main --to index.js"
  echo ""
  echo "https://github.com/purescript/spago#get-started-from-scratch-with-webpack-frontend-projects"
  echo "webpack-dev-server --progress --inline --hot"
  '';
}

