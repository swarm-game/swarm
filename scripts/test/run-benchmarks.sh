#!/bin/bash -xe


SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd $SCRIPT_DIR/..

STACK_WORK=.stack-work-bench stack bench swarm:benchmark --benchmark-arguments "--color always $@"
