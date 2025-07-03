{ mkDerivation, autodocodec, base, bytestring, http-client
, http-client-tls, lib, monad-logger, mtl, necrork, opt-env-conf
, path-io, safe-coloured-text, text, unliftio
}:
mkDerivation {
  pname = "necrork-cli";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    autodocodec base bytestring http-client http-client-tls
    monad-logger mtl necrork opt-env-conf path-io safe-coloured-text
    text unliftio
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/necrork#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "necrork";
}
