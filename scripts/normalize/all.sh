#!/bin/bash -xe

# Run this locally before pushing a branch to save some CI cycles

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd $SCRIPT_DIR/../..

scripts/normalize/cabal.sh
scripts/normalize/code-format.sh
scripts/normalize/yaml.sh
hlint .
