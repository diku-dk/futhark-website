{pkgs ? import <nixpkgs> {}}:
   pkgs.stdenv.mkDerivation {
     name = "sigkill";
     buildInputs = [
       pkgs.cacert
       pkgs.curl
       pkgs.file
       pkgs.git
       pkgs.haskell.compiler.ghc8104
       pkgs.pkgconfig
       pkgs.zlib
       pkgs.zlib.out
     ];
   }

