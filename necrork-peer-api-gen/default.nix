{ mkDerivation, base, genvalidity, genvalidity-containers
, genvalidity-sydtest, genvalidity-sydtest-persistent
, genvalidity-text, intray-api-gen, lib, necrork-peer-api
, QuickCheck, servant-client, sydtest, sydtest-discover
}:
mkDerivation {
  pname = "necrork-peer-api-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-containers genvalidity-text
    intray-api-gen necrork-peer-api QuickCheck servant-client
  ];
  testHaskellDepends = [
    base genvalidity-sydtest genvalidity-sydtest-persistent
    necrork-peer-api servant-client sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/necrork#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
