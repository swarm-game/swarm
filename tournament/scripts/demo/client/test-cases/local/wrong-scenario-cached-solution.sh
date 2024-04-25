#!/bin/bash -ex

# This test cases demonstrates the failure whether or
# not the solutions to both scenarios have already been submitted.

cd $(git rev-parse --show-toplevel)

tournament/scripts/demo/client/test-cases/local/good-submit.sh

tournament/scripts/demo/client/submit.sh \
    localhost:8008 \
    data/scenarios/Challenges/dimsum.yaml \
    data/scenarios/Challenges/_arbitrage/solution.sw
