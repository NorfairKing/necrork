{ mkDerivation, base, lib, necrork-peer-api, servant
, servant-client, servant-client-core
}:
mkDerivation {
  pname = "necrork-peer-client";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base necrork-peer-api servant servant-client servant-client-core
  ];
  homepage = "https://github.com/NorfairKing/necrork#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
