{ mkDerivation, attoparsec, base, bytestring, c-storable-deriving
, caramia, containers, criterion, data-default, data-default-class
, interpolatedstring-perl6, JuicyPixels, lens, linear, scientific
, sdl2, stdenv, storable-tuple, text, transformers, uuid-types
, vector
}:
mkDerivation {
  pname = "cube";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base bytestring c-storable-deriving caramia containers
    criterion data-default data-default-class interpolatedstring-perl6
    JuicyPixels lens linear scientific sdl2 storable-tuple text
    transformers uuid-types vector
  ];
  license = stdenv.lib.licenses.bsd3;
}
