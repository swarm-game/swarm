#!/bin/bash -xe

# Run this locally before pushing a branch to save some CI cycles

cd $(git rev-parse --show-toplevel)

scripts/normalize/cabal.sh
scripts/normalize/code-format.sh
scripts/normalize/yaml.sh
hlint .
