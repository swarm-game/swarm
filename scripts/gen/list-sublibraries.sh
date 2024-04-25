#!/bin/bash

cd $(git rev-parse --show-toplevel)

grep '^library \w' swarm.cabal | cut -d' ' -f2
