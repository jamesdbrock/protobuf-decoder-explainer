{ pkgs ? import ./pkgs.nix {}}:
pkgs.mkShell {
  buildInputs = [
    pkgs.easy-ps.purs-0_14_3
    pkgs.easy-ps.spago
    pkgs.easy-ps.zephyr
    pkgs.nodejs-14_x
  ];
  LC_ALL = "C.UTF-8"; # https://github.com/purescript/spago/issues/507
  shellHook = ''
  echo "First install these dependencies from"
  echo "https://github.com/purescript/spago#get-started-from-scratch-with-webpack-frontend-projects"
  echo ""
  echo "    npm install"
  echo ""
  echo "(If you want to upgrade the package-lock.json:)"
  echo""
  echo "    npm install --save-dev webpack webpack-cli webpack-dev-server purescript-psa purs-loader html-webpack-plugin long"
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

