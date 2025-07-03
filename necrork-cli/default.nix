{ mkDerivation, base, http-client, http-client-tls, lib, mtl
, necrork, opt-env-conf, path-io, servant-client, text, unliftio
}:
mkDerivation {
  pname = "necrork-cli";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base http-client http-client-tls mtl necrork opt-env-conf path-io
    servant-client text unliftio
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/necrork#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "necrork";
}
