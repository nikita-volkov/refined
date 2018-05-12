{ compiler ? "ghc822" }:

with rec {
  fetchFromGitHub = (
    (import <nixpkgs> { config = {}; overlays = []; }).fetchFromGitHub);
  _nixpkgs = fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "7f79a4fe118943b0d8dcc3aa475079ad3cfd7986";
    sha256 = "0xn88aadls399maks9w6b3jih8yws23i5ja7260bxv5w3s0c2p26"; 
  };
};

import _nixpkgs {
  config = {
    packageOverrides = super: let self = super.pkgs; in {
      haskellPackages = super.haskell.packages.${compiler}.override {
        overrides = import ./overrides.nix { pkgs = self; };
      };

    };
  };
  overlays = [];
}
