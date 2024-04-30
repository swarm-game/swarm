#!/bin/bash -ex

# Runs the tournament server natively on the host.

cd $(git rev-parse --show-toplevel)

GIT_HASH=$(git rev-parse HEAD)

stack build --fast swarm:swarm-host-tournament && \
  stack exec swarm-host-tournament -- \
    --native-dev \
    --port 8080 \
    --version $GIT_HASH \
    "$@"
