{ mkDerivation, aeson, async, attoparsec, attoparsec-binary, base
, base64-bytestring, bytestring
, bytestring-to-vector, caramia, containers, dependent-map
, dependent-sum, dependent-sum-template, directory, exceptions
, filepath, hashable, hpack, hsnoise, JuicyPixels, lens, lib
, linear, monad-logger, mtl, ref-tf, reflex, scientific, sdl2
, storable-tuple, string-interpolate, text, transformers
, unordered-containers, vector, witherable
}:
mkDerivation {
  pname = "cube";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async attoparsec attoparsec-binary base base64-bytestring
    bytestring bytestring-to-vector caramia containers
    dependent-map dependent-sum dependent-sum-template directory
    exceptions filepath hashable hsnoise JuicyPixels lens linear
    monad-logger mtl ref-tf reflex scientific sdl2 storable-tuple
    string-interpolate text transformers unordered-containers vector
    witherable
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson async attoparsec attoparsec-binary base base64-bytestring
    bytestring bytestring-to-vector caramia containers
    dependent-map dependent-sum dependent-sum-template directory
    exceptions filepath hashable hsnoise JuicyPixels lens linear
    monad-logger mtl ref-tf reflex scientific sdl2 storable-tuple
    string-interpolate text transformers unordered-containers vector
    witherable
  ];
  prePatch = "hpack";
  license = lib.licenses.bsd3;
}
