{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs lib;

  haskellPackages_ = if compiler == "default"
                        then pkgs.haskellPackages
                        else pkgs.haskell.packages.${compiler};

  hlib = pkgs.haskell.lib;

  haskellPackages = haskellPackages_.override {
    overrides = self: super: {
      caramia = hlib.dontCheck (self.callPackage ./3rdparty/caramia { });
      sdl2 =
        if lib.versionOlder "2.5.3.2" super.sdl2.version then
          super.sdl2
        else
          hlib.dontCheck (self.callPackage ./3rdparty/sdl2.nix { });
    };
  };

  drv = haskellPackages.callPackage ./default.nix {};

  shell = drv.env.overrideAttrs (self: {
    nativeBuildInputs = self.nativeBuildInputs ++ [ haskellPackages.cabal-install ];
    # For Non-NixOS
    # LIBGL_DRIVERS_PATH = "${pkgs.mesa_drivers}/lib/dri";
    # LD_LIBRARY_PATH = "";
  });

  drv_ = drv.overrideAttrs (self: {
    passthru = self.passthru // { shell = shell; };
  });

in

  if lib.inNixShell then drv_.shell else drv_
