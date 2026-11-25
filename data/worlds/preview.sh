#!/usr/bin/env bash

## Test with:
# fswatch -0 ./data/worlds/island.world | ./data/worlds/preview.sh

while IFS= read -r -d ''; do
  clear;
  cabal run swarm-scene -- Island -w $(tput cols) -h $(tput lines) -s 0
done