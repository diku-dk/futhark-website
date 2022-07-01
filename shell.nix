{pkgs ? import <nixpkgs> {}}:
   pkgs.stdenv.mkDerivation {
     name = "futhark-website";
     buildInputs = [
       pkgs.cacert
       pkgs.curl
       pkgs.file
       pkgs.git
       pkgs.cabal-install
       pkgs.ghc
       pkgs.pkgconfig
       pkgs.zlib
       pkgs.zlib.out
     ];
   }

