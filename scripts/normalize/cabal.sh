#!/bin/bash -ex

cd $(git rev-parse --show-toplevel)

CABAL_FILE=swarm.cabal
cabal-gild --input $CABAL_FILE --output $CABAL_FILE --mode format
