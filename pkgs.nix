{ pkgs ? import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz";
  sha256 = "1wg61h4gndm3vcprdcg7rc4s1v3jkm5xd7lw8r2f67w502y94gcy";
  }) {}
}:

let
  easy-ps = import (builtins.fetchGit {
    url = "https://github.com/justinwoo/easy-purescript-nix.git";
    rev = "598aea7973c25b0a3fa976dcac629c39f2e56680";
  }) { inherit pkgs; };
in
pkgs // { inherit easy-ps; }
