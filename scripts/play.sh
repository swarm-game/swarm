#!/bin/bash -ex

cd $(git rev-parse --show-toplevel)

# This compiles without optimizations and then runs the resulting executable.
# It's been observed in certain versions of GHC that compiling with optimizations
# results in the swarm UI freezing for a potentially long time upon starting a scenario.
# See https://github.com/swarm-game/swarm/issues/1000#issuecomment-1378632269
scripts/build-game.sh && cabal run -j -O0 swarm:exe:swarm -- "$@"
