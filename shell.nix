{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages_ = if compiler == "default"
                        then pkgs.haskellPackages
                        else pkgs.haskell.packages.${compiler};

  lib = pkgs.haskell.lib;

  haskellPackages = haskellPackages_.override {
    overrides = self: super: {
      caramia = lib.dontCheck (self.callPackage ./3rdparty/caramia { });
      wires = self.callPackage ./3rdparty/wires { };
    };
  };

  drv = haskellPackages.callPackage ./default.nix {};

  shell = drv.env.overrideAttrs (self: {
    nativeBuildInputs = self.nativeBuildInputs ++ [ haskellPackages.cabal-install ];
    # For Non-NixOS
    # LIBGL_DRIVERS_PATH = "${pkgs.mesa_drivers}/lib/dri";
    # LD_LIBRARY_PATH = "";
  });

in

  if pkgs.lib.inNixShell then shell else drv
