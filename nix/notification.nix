{ lib
, formats
, writeShellScript
, necrork-cli
}:
{ peers
, switch
, timeout
, intray
}:
with lib;
let
  config = { inherit peers switch timeout intray; };
  configFile = (formats.yaml { }).generate "necrork-notify-config.yaml" config;
in
writeShellScript "necrork-notification" ''
  set -eou pipefail
  exec ${necrork-cli}/bin/necrork --config-file "${configFile}" notify
''
