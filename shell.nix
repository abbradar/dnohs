{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, alex, array, base, bytestring, containers
      , happy, lens, mtl, stdenv, text, transformers, data-default-generics
      , wl-pprint-text
      }:
      mkDerivation {
        pname = "dnohs";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [
          array base bytestring containers lens mtl text transformers
          data-default-generics wl-pprint-text
        ];
        buildTools = [ alex happy ];
        license = stdenv.lib.licenses.bsd3;
      };

  hspkgs = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      alex = self.callPackage ./alex.nix { alex = super.alex; };
    };
  };

  drv = hspkgs.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
