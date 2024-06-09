#!/bin/bash -xe

cd $(git rev-parse --show-toplevel)

cabal bench --benchmark-options "--color always $@"
