#!/bin/bash -ex

SCRIPT_DIR=$(cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd)
cd $SCRIPT_DIR/../..

cabal haddock

OUTPUT_BASE_DIR=$(cabal list-bin swarm | grep -o '.*/x/' | head -c-3)
SWARM_HADDOCK_INDEX=$OUTPUT_BASE_DIR/doc/html/swarm/index.html

google-chrome $SWARM_HADDOCK_INDEX
