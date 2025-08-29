#!/usr/bin/env bash
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

# NOTE: There are several executables within the swarm.cabal project.
# If you only want to play the swarm game, you should specify an explicit
# target 'swarm:exe:swarm' to the 'cabal' command, to avoid building
# extra dependencies.
cabal build -j --semaphore swarm:exe:swarm 
