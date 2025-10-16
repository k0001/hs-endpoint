{ mkDerivation, base, bytestring, case-insensitive, containers
, free, hedgehog, http-types, lib, profunctors, tasty
, tasty-hedgehog, tasty-hunit, text, time, transformers, uuid-types
}:
mkDerivation {
  pname = "endpoint";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring case-insensitive containers free http-types
    profunctors text time transformers uuid-types
  ];
  testHaskellDepends = [
    base containers hedgehog tasty tasty-hedgehog tasty-hunit text time
    uuid-types
  ];
  homepage = "https://github.com/k0001/hs-endpoint";
  description = "URL endpoint encoding and decoding";
  license = lib.licenses.asl20;
}
