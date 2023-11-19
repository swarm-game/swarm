#!/bin/bash -ex

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd $SCRIPT_DIR/..

# See https://github.com/swarm-game/swarm/issues/936
STACK_WORK=.stack-work-test stack test --fast "$@"
