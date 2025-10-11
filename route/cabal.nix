{ mkDerivation, base, bytestring, containers, contravariant, free
, hedgehog, lib, profunctors, tasty, tasty-hedgehog, tasty-hunit
, text, time, transformers, uuid-types
}:
mkDerivation {
  pname = "route";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers contravariant free profunctors text time
    transformers uuid-types
  ];
  testHaskellDepends = [
    base containers hedgehog tasty tasty-hedgehog tasty-hunit text time
    uuid-types
  ];
  homepage = "https://github.com/k0001/hs-route";
  description = "URL route encoding and decoding";
  license = lib.licenses.asl20;
}
