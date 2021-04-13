# { pkgs ? import (builtins.fetchTarball {
#   url = "https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz";
#   sha256 = "1wg61h4gndm3vcprdcg7rc4s1v3jkm5xd7lw8r2f67w502y94gcy";
#   }) {}
# }:
#
# let
#   easy-ps = import (builtins.fetchGit {
#     url = "https://github.com/justinwoo/easy-purescript-nix.git";
#     rev = "1ec689df0adf8e8ada7fcfcb513876307ea34226";
#   }) { inherit pkgs; };
# in
{ pkgs ? import ./pkgs.nix {}}:
pkgs.mkShell {
  buildInputs = [
    pkgs.easy-ps.purs-0_13_8
    pkgs.easy-ps.spago
    # easy-ps.pulp
    pkgs.nodejs-14_x
    # pkgs.nodePackages.bower
    pkgs.nodePackages.webpack
    pkgs.nodePackages.webpack-cli
    pkgs.nodePackages.webpack-dev-server
    pkgs.nodePackages.purescript-psa
    # pkgs.nodePackages.long
    # pkgs.nodePackages.purs-loader
    # pkgs.nodePackages.html-webpack-plugin
  ];
  LC_ALL = "C.UTF-8"; # https://github.com/purescript/spago/issues/507
  shellHook = ''
  echo ""
  echo "spago build"
  echo ""
  echo "https://github.com/purescript/spago#2-spago-bundle-module"
  echo "spago bundle-module --main Main --to bundle.js"
  echo ""
  echo "https://github.com/purescript/spago#get-started-from-scratch-with-webpack-frontend-projects"
  echo "webpack-dev-server --progress --inline --hot"
  '';
}

