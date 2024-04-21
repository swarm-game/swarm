#!/bin/bash -ex

cd $(git rev-parse --show-toplevel)

tournament/scripts/demo/client/submit.sh \
    localhost:8008 \
    data/scenarios/Challenges/arbitrage.yaml \
    data/scenarios/Challenges/_arbitrage/solution.sw
