{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages_ = if compiler == "default"
                        then pkgs.haskellPackages
                        else pkgs.haskell.packages.${compiler};

  haskellPackages = haskellPackages_.override {
    overrides = self: super: {
      #mkDerivation = args: super.mkDerivation (args // { enableLibraryProfiling = true; });
    };
  };

  drv = haskellPackages.callPackage ./default.nix {};

  shell = pkgs.lib.overrideDerivation drv.env (self: {
    # For Non-NixOS
    # LIBGL_DRIVERS_PATH = "${pkgs.mesa_drivers}/lib/dri";
    # LD_LIBRARY_PATH = "";
  });

in

  if pkgs.lib.inNixShell then shell else drv
