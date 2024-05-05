#!/bin/bash -ex

cd $(git rev-parse --show-toplevel)

# NOTE: There are several executables within the swarm.cabal project.
# If you only want to play the swarm game, you should specify an explicit
# target 'swarm:exe:swarm' to the 'stack' command, to avoid building
# extra dependencies.

cabal build -j -O0 swarm:exe:swarm
