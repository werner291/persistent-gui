{ pkgs ? import <nixpkgs> {} }:

let
  easy-ps = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "aa3e608608232f4a009b5c132ae763fdabfb4aba";
    sha256 = "0y6jikncxs9l2zgngbd1775f1zy5s1hdc5rhkyzsyaalcl5cajk8";
  }) {
    inherit pkgs;
  };

in pkgs.mkShell {
  buildInputs = [ easy-ps.purs easy-ps.spago pkgs.nodejs pkgs.nodePackages.parcel-bundler easy-ps.purty ];
}
