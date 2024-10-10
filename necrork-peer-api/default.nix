{ mkDerivation, aeson, autodocodec, base, bytestring, containers
, http-api-data, lib, opt-env-conf, persistent, servant
, servant-client, text, time, validity, validity-text
}:
mkDerivation {
  pname = "necrork-peer-api";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base bytestring containers http-api-data
    opt-env-conf persistent servant servant-client text time validity
    validity-text
  ];
  homepage = "https://github.com/NorfairKing/necrork#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
