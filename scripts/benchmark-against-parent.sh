#!/bin/bash -xe

# Requires that the working tree be clean.

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd $SCRIPT_DIR/..

if git diff --quiet --exit-code
then
   echo "Working tree is clean. Starting benchmarks..."
else
   echo "Working tree is dirty! Quitting."
   exit 1
fi

BASELINE_OUTPUT=baseline.csv

git checkout HEAD~
STACK_WORK=.stack-work-bench stack bench swarm:benchmark --benchmark-arguments "--csv $BASELINE_OUTPUT --color always"

git switch -
STACK_WORK=.stack-work-bench stack bench swarm:benchmark --benchmark-arguments "--baseline $BASELINE_OUTPUT --fail-if-slower 3 --color always"