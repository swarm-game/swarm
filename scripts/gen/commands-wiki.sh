#!/bin/bash -e

cd $(git rev-parse --show-toplevel)

cabal run -j -O0 -- swarm:swarm-docs cheatsheet --commands
