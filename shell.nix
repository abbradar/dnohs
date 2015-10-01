{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages' = if compiler == "default"
                        then pkgs.haskellPackages
                        else pkgs.haskell.packages.${compiler};

  lib = pkgs.haskell.lib;

  haskellPackages = haskellPackages'.override {
    overrides = self: super: {
      alex = lib.overrideCabal (self.callPackage ./alex.nix { }) (drv: {
        buildTools = drv.buildTools or [] ++ [ self.happy super.alex ];
      });
      llvm-general = super.llvm-general.override {
        llvm-config = pkgs.llvm_34.override { debugVersion = true; };
      };
    };
  };

  drv = haskellPackages.callPackage ./default.nix {};

in

  if pkgs.lib.inNixShell then drv.env else drv
