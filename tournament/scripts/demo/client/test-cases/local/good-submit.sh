#!/bin/bash -ex

cd $(git rev-parse --show-toplevel)

HOST=${1:-localhost:8080}

tournament/scripts/demo/client/submit.sh \
    $HOST \
    data/scenarios/Challenges/arbitrage.yaml \
    data/scenarios/Challenges/_arbitrage/solution.sw
