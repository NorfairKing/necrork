{ mkDerivation, aeson, autodocodec, base, bytestring, containers
, http-api-data, http-client, http-client-tls, lib, looper
, monad-logger, opt-env-conf, persistent, servant, servant-client
, servant-client-core, text, time, token-limiter-concurrent
, unliftio, validity, validity-text
}:
mkDerivation {
  pname = "necrork";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base bytestring containers http-api-data
    http-client http-client-tls looper monad-logger opt-env-conf
    persistent servant servant-client servant-client-core text time
    token-limiter-concurrent unliftio validity validity-text
  ];
  homepage = "https://github.com/NorfairKing/necrork#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
