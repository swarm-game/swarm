#!/bin/bash -ex

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd $SCRIPT_DIR/..

# See https://github.com/swarm-game/swarm/issues/1000#issuecomment-1378632269
stack build --fast && stack exec swarm -- $@
