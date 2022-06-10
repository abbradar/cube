{ mkDerivation, base, bytestring, deepseq, exceptions, fetchgit
, lib, linear, SDL2, StateVar, text, transformers, vector, weigh
}:
mkDerivation {
  pname = "sdl2";
  version = "2.5.3.2";
  src = fetchgit {
    url = "https://github.com/haskell-game/sdl2";
    sha256 = "1nbxwfpmdzwc1p5hhjdmnaz9yr71xab3kys5m5i0prg357sj775s";
    rev = "b4fe21c1025e4be9a1eb03c19f811e1776a2b292";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base bytestring exceptions linear StateVar text transformers vector
  ];
  librarySystemDepends = [ SDL2 ];
  libraryPkgconfigDepends = [ SDL2 ];
  testHaskellDepends = [ base deepseq linear vector weigh ];
  description = "Both high- and low-level bindings to the SDL library (version 2.0.6+).";
  license = lib.licenses.bsd3;
}
