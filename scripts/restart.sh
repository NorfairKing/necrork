#!/usr/bin/env bash


export NECRORK_PEER_LOG_LEVEL=Debug
export NECRORK_PEER_PORT=8000
export NECRORK_PEER_BASE_URL=http://localhost:8000
export NECRORK_PEER_NOTIFIER_ENABLE=True
export NECRORK_PEER_NOTIFIER_PHASE=1
export NECRORK_PEER_NOTIFIER_PERIOD=1

cd necrork-peer

killall necrork-peer || true
sleep 0.1

necrork-peer &
