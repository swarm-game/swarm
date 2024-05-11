#!/bin/bash -ex

# This test cases demonstrates the failure whether or
# not the solutions to both scenarios have already been submitted.

cd $(git rev-parse --show-toplevel)

HOST=${1:-localhost:8080}

tournament/scripts/demo/client/test-cases/local/good-submit.sh $HOST

tournament/scripts/demo/client/submit.sh \
    $HOST \
    data/scenarios/Challenges/dimsum.yaml \
    data/scenarios/Challenges/_arbitrage/solution.sw
