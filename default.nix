{ mkDerivation, aeson, async, attoparsec, attoparsec-binary, base
, base64-bytestring, bytestring, bytestring-mmap
, bytestring-to-vector, caramia, containers, directory, exceptions
, filepath, hashable, hpack, hsnoise, JuicyPixels, lens, linear
, monad-logger, mtl, reflex, scientific, sdl2, stdenv
, storable-tuple, string-interpolate, text, unordered-containers
, vector
}:
mkDerivation {
  pname = "cube";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async attoparsec attoparsec-binary base base64-bytestring
    bytestring bytestring-mmap bytestring-to-vector caramia containers
    directory exceptions filepath hashable hsnoise JuicyPixels lens
    linear monad-logger mtl reflex scientific sdl2 storable-tuple
    string-interpolate text unordered-containers vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson async attoparsec attoparsec-binary base base64-bytestring
    bytestring bytestring-mmap bytestring-to-vector caramia containers
    directory exceptions filepath hashable hsnoise JuicyPixels lens
    linear monad-logger mtl reflex scientific sdl2 storable-tuple
    string-interpolate text unordered-containers vector
  ];
  prePatch = "hpack";
  license = stdenv.lib.licenses.bsd3;
}
