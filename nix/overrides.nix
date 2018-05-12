{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  refined = (
    with rec {
      refinedSource = pkgs.lib.cleanSource ../.;
      refinedBasic = self.callCabal2nix "refined" refinedSource {};
    };
    overrideCabal refinedBasic (old: {
    })
  );
}
