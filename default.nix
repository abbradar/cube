{ mkDerivation, attoparsec, base, bytestring, c-storable-deriving
, caramia, containers, criterion, data-default, data-default-class
, hsnoise, interpolatedstring-perl6, JuicyPixels, lens, linear
, scientific, sdl2, stdenv, storable-tuple, text, transformers
, uuid-types, vector
}:
mkDerivation {
  pname = "cube";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring c-storable-deriving caramia containers
    data-default data-default-class hsnoise interpolatedstring-perl6
    JuicyPixels lens linear scientific sdl2 storable-tuple transformers
    uuid-types vector
  ];
  executableHaskellDepends = [
    base caramia containers data-default data-default-class lens linear
    sdl2 text
  ];
  benchmarkHaskellDepends = [ base bytestring criterion ];
  license = stdenv.lib.licenses.bsd3;
}
