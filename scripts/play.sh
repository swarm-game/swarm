#!/usr/bin/env bash
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

# This compiles with optimizations and then runs the resulting executable.
# Building with optimizations takes longer, but is noticeable when running
# scenarios with swarms of robots.
cabal run -j --semaphore swarm:exe:swarm -- "$@"
