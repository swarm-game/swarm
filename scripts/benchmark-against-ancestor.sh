#!/bin/bash -xe

# Requires that the working tree be clean.

REFERENCE_COMMIT=${1:-HEAD~}

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

git checkout $REFERENCE_COMMIT

scripts/run-benchmarks.sh "--csv $BASELINE_OUTPUT"

git switch -
scripts/run-benchmarks.sh "--baseline $BASELINE_OUTPUT --fail-if-slower 3"