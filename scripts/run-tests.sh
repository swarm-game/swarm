#!/bin/bash -ex

# Note: to run a single test, invoke as:
# scripts/run-tests.sh --test-arguments '--pattern my-test-name'

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd $SCRIPT_DIR/..


# See https://github.com/swarm-game/swarm/issues/936
STACK_WORK=.stack-work-test stack test "$@"
