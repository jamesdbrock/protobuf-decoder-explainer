# { pkgs ? import <nixpkgs> { }
{ pkgs ? import (builtins.fetchGit {
  # https://github.com/NixOS/nixpkgs/releases/tag/21.11
  url = "https://github.com/nixos/nixpkgs/";
  ref = "refs/tags/21.11";
  rev = "a7ecde854aee5c4c7cd6177f54a99d2c1ff28a31";
  }) {}
}:
let
  easy-ps-src = builtins.fetchGit {
    url = "https://github.com/justinwoo/easy-purescript-nix.git";
    ref = "master";
    rev = "04d0799bc81af02201096000b4dc634c4c69b2d6";
  };
  easy-ps = import easy-ps-src { inherit pkgs; };
in
pkgs.mkShell {
  nativeBuildInputs = [
    easy-ps.purs-0_15_2
    easy-ps.spago
    # easy-ps.pulp-16_0_0-0
    # easy-ps.psc-package
    # easy-ps.purs-tidy
    # easy-ps.zephyr
    pkgs.nodejs-17_x
    # pkgs.nodePackages.bower
  ];
  LC_ALL = "C.UTF-8"; # https://github.com/purescript/spago/issues/507
  # https://github.com/purescript/spago#install-autocompletions-for-bash
  shellHook = ''
  source <(spago --bash-completion-script `which spago`)
  echo "First install these dependencies from"
  echo "https://github.com/purescript/spago#get-started-from-scratch-with-webpack-frontend-projects"
  echo ""
  echo "    npm install"
  echo ""
  echo "(If you want to upgrade the package-lock.json:)"
  echo""
  echo "    npm install --save-dev webpack webpack-cli webpack-dev-server purescript-psa purs-loader html-webpack-plugin"
  echo ""
  echo "Then build the ./dist/index.html file."
  echo ""
  echo "    npm run webpack"
  echo ""
  echo "Run the webpack server."
  echo ""
  echo "    npm run webpack:server"
  echo ""
  '';
}
