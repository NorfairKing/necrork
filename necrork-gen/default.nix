{ mkDerivation, base, genvalidity, genvalidity-containers
, genvalidity-sydtest, genvalidity-sydtest-persistent
, genvalidity-text, lib, necrork, QuickCheck, servant-client
, sydtest, sydtest-discover
}:
mkDerivation {
  pname = "necrork-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-containers genvalidity-text necrork
    QuickCheck servant-client
  ];
  testHaskellDepends = [
    base genvalidity-sydtest genvalidity-sydtest-persistent necrork
    sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/necrork#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
