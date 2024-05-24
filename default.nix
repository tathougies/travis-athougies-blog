{ nixpkgs ? import <nixpkgs> {}, ghc ? nixpkgs.haskell.packages.ghc881 }:
with nixpkgs;

let myghc =
    (ghc.ghcWithPackages (ps: with ps; [
      base hakyll pandoc blaze-html containers blaze-markup filepath
      pandoc-types edit-distance mtl zip-archive random aeson text
    ]));
in stdenv.mkDerivation {
  name = "travis-athougies-blog";

  buildInputs = [
    myghc
    haskellPackages.stack
    haskellPackages.cabal-install
  ];
}
