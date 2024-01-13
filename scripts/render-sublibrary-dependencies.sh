#!/bin/bash -ex

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd $SCRIPT_DIR/..

# First, install cabal-plan:
#
#   cabal install cabal-plan


# If the swarm.cabal file has changed, you may need to remove
# the 'plan.json' file from the cache and regenerate.
#
# Note that "rm -f dist-newstyle/cache/plan.json" is insufficient;
# we need remove the whole cache:
rm -r dist-newstyle/cache
cabal build

cabal-plan --hide-global --hide-builtin dot --tred --root swarm | twopi -Tsvg -o docs/image/sublibrary-graph.svg