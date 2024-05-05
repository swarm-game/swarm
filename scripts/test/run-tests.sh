#!/bin/bash -ex

cd $(git rev-parse --show-toplevel)

# See https://github.com/swarm-game/swarm/issues/936
cabal test -O0 -j "$@"
