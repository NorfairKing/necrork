{ mkDerivation, autodocodec, base, containers, http-client
, http-client-tls, lib, looper, monad-logger, necrork-peer-api
, necrork-peer-client, opt-env-conf, text, token-limiter-concurrent
, unliftio
}:
mkDerivation {
  pname = "necrork-sdk";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    autodocodec base containers http-client http-client-tls looper
    monad-logger necrork-peer-api necrork-peer-client opt-env-conf text
    token-limiter-concurrent unliftio
  ];
  homepage = "https://github.com/NorfairKing/necrork#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
