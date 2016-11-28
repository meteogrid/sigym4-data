{ mkDerivation, accelerate, base, bytestring, cryptonite
, data-default, deepseq, dimensional, dimensional-parsers, hspec
, ieee754, newtype, pretty, QuickCheck, sigym4-dimension
, sigym4-geometry, spatial-reference, stdenv, template-haskell
, text, vector
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
  testHaskellDepends = [ base hspec QuickCheck ];
  license = stdenv.lib.licenses.bsd3;
}
