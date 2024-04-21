#!/bin/bash -ex

# Usage:
# Intended to be invoked in Dockerfile.
#
#    build-server-executable.sh <output binary location>
#
# Note that we use 'cabal' instead of 'stack' becuase
# 'stack' fails to compile the 'vty' package within the Amazon Linux docker image.

# For faster development iteration, disable optimizations:
CABAL_ARGS="--disable-optimization swarm:swarm-host-tournament"
#CABAL_ARGS="swarm:swarm-host-tournament"

cabal build -j $CABAL_ARGS
cp $(cabal list-bin $CABAL_ARGS) $1