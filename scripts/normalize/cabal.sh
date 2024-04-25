#!/bin/bash -ex

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd $SCRIPT_DIR/../..

CABAL_FILE=swarm.cabal
cabal-gild --input $CABAL_FILE --output $CABAL_FILE --mode format
