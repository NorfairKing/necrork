#!/usr/bin/env bash

stack install necrork-peer \
  --file-watch --watch-all \
  --fast \
  --no-nix-pure \
  --ghc-options="-freverse-errors -j4 +RTS -A128M -n2m -RTS" \
  --exec="./scripts/restart.sh $@"

