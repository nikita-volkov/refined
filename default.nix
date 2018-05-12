{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:

let
  inherit (nixpkgs) pkgs;

  f = import ./nix/refined.nix;

  haskellPackages = pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in
  drv
