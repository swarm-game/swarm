#!/bin/bash -ex

cd $(git rev-parse --show-toplevel)

cabal test --test-show-details=direct -O0 -j "$@"
