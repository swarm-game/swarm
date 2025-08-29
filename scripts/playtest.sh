#!/usr/bin/env bash
set -xeuo pipefail

cd "$(git rev-parse --show-toplevel)"

# This compiles the dependencies with optimizations (as they are not built frequently)
# and then builds the swarm executable without optimizations.
# Expect the game to run slow with many robots, but most scenarios are fine.
# This is the development workflow when making changes to the game.

cabal build -j --semaphore swarm:exe:swarm --dependencies-only
cabal run swarm:exe:swarm -O0 -- "$@"

# PS: Don't forget to also run tests and at least build benchmarks or the CI will catch you ;)
