{ mkDerivation, base, bytestring, conduit, cryptonite, accelerate
, cryptonite-conduit, data-default, deepseq, geohs-fingerprint
, ghc-prim, hspec, ieee754, lens, linear, mtl, newtype, pretty, QuickCheck
, should-not-typecheck, sigym4-dimension, sigym4-geometry
, sigym4-units, spatial-reference, stdenv, template-haskell, text
, time, vector, vector-th-unbox
}:
mkDerivation {
  pname = "sigym4-data";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring conduit cryptonite cryptonite-conduit data-default
    accelerate
    deepseq geohs-fingerprint ghc-prim ieee754 mtl lens linear newtype pretty
    sigym4-dimension sigym4-geometry sigym4-units spatial-reference
    template-haskell text vector
  ];
  testHaskellDepends = [
    base hspec QuickCheck should-not-typecheck time vector
    vector-th-unbox
  ];
  license = stdenv.lib.licenses.bsd3;
}
