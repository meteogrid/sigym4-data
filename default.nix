{ mkDerivation, accelerate, base, bytestring, cryptonite
, data-default, deepseq, dimensional, dimensional-parsers, hspec
, ieee754, newtype, pretty, QuickCheck, should-not-typecheck
, sigym4-dimension, sigym4-geometry, spatial-reference, stdenv
, template-haskell, text, time, vector
}:
mkDerivation {
  pname = "sigym4-data";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    accelerate base bytestring cryptonite data-default deepseq
    dimensional dimensional-parsers ieee754 newtype pretty
    sigym4-dimension sigym4-geometry spatial-reference template-haskell
    text vector
  ];
  testHaskellDepends = [
    base hspec newtype QuickCheck should-not-typecheck time
  ];
  license = stdenv.lib.licenses.bsd3;
}
