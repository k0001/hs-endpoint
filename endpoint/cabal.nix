{ mkDerivation, base, bytestring, case-insensitive, containers
, free, hedgehog, http-types, lib, profunctors, tasty
, tasty-hedgehog, tasty-hunit, text, time, transformers, uuid-types
, witherable
}:
mkDerivation {
  pname = "endpoint";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers free http-types profunctors text time
    transformers uuid-types witherable
  ];
  testHaskellDepends = [
    base bytestring case-insensitive containers hedgehog tasty
    tasty-hedgehog tasty-hunit text time uuid-types
  ];
  homepage = "https://github.com/k0001/hs-endpoint";
  description = "URL endpoint encoding and decoding";
  license = lib.licenses.asl20;
}
