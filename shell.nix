{ pkgs ? import ./pkgs.nix {}}:
pkgs.mkShell {
  buildInputs = [
    pkgs.easy-ps.purs-0_13_8
    pkgs.easy-ps.spago
    # easy-ps.pulp
    pkgs.nodejs-14_x
    # pkgs.nodePackages.bower

    # pkgs.nodePackages.webpack
    # pkgs.nodePackages.webpack-cli
    # pkgs.nodePackages.webpack-dev-server
    # pkgs.nodePackages.purescript-psa

    # pkgs.nodePackages.long
    # pkgs.nodePackages.purs-loader
    # pkgs.nodePackages.html-webpack-plugin
  ];
  LC_ALL = "C.UTF-8"; # https://github.com/purescript/spago/issues/507
  # echo ""
  # echo "spago build"
  # echo ""
  # echo "https://github.com/purescript/spago#2-spago-bundle-module"
  # echo "spago bundle-module --main Main --to bundle.js"
  # echo ""
  shellHook = ''
  echo "First install these dependencies from"
  echo "https://github.com/purescript/spago#get-started-from-scratch-with-webpack-frontend-projects"
  echo ""
  echo "    npm install --save-dev webpack webpack-cli webpack-dev-server purescript-psa purs-loader html-webpack-plugin long"
  echo "    spago install"
  echo ""
  echo "Then build the ./dist/index.html file."
  echo ""
  echo "    npx webpack"
  echo ""
  '';
}

