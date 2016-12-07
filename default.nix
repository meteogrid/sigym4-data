{ mkDerivation, base, bytestring, conduit, cryptonite
, cryptonite-conduit, data-default, deepseq, dimensional
, dimensional-parsers, either, hspec, ieee754, mtl, newtype, pretty
, QuickCheck, should-not-typecheck, sigym4-dimension
, sigym4-geometry, spatial-reference, stdenv, template-haskell
, text, time, vector, vector-th-unbox
}:
mkDerivation {
  pname = "sigym4-data";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring conduit cryptonite cryptonite-conduit data-default
    deepseq dimensional dimensional-parsers ieee754 mtl newtype pretty
    sigym4-dimension sigym4-geometry spatial-reference template-haskell
    text vector
  ];
  testHaskellDepends = [
    base either hspec newtype QuickCheck should-not-typecheck time
    vector vector-th-unbox
  ];
  license = stdenv.lib.licenses.bsd3;
}
