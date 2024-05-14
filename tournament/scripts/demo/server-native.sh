#!/bin/bash -ex

# Runs the tournament server natively on the host.

cd $(git rev-parse --show-toplevel)

GIT_HASH=$(git rev-parse HEAD)

cabal run -j -O0 swarm:swarm-host-tournament -- \
    --native-dev \
    --port 8080 \
    --version $GIT_HASH \
    "$@"
