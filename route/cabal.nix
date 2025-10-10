{ mkDerivation, base, bytestring, containers, contravariant, free
, lib, profunctors, text, time, transformers
}:
mkDerivation {
  pname = "route";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers contravariant free profunctors text time
    transformers
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/k0001/hs-route";
  description = "URL route encoding and decoding";
  license = lib.licenses.asl20;
}
