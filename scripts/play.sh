#!/bin/bash -ex

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd $SCRIPT_DIR/..

# This compiles without optimizations and then runs the resulting executable.
# It's been observed in certain versions of GHC that compiling with optimizations
# results in the swarm UI freezing for a potentially long time upon starting a scenario.
# See https://github.com/swarm-game/swarm/issues/1000#issuecomment-1378632269
stack build --fast && stack exec swarm -- "$@"
