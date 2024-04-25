#!/bin/bash -ex

# Parameters:
# $1 = hostname (and optional port)
# $2 = path to scenario file*
# $3 = path to solution file*
#
# *Paths are relative to the git repository root.

HOST=$1
SCENARIO_FILEPATH=$2
SOLUTION_FILEPATH=$3

# Example:
#   tournament/scripts/demo/client/submit.sh localhost:8008 data/scenarios/Challenges/dimsum.yaml data/scenarios/Challenges/_dimsum/solution.sw
# or
#   tournament/scripts/demo/client/submit.sh swarmgame.net data/scenarios/Challenges/arbitrage.yaml data/scenarios/Challenges/_arbitrage/solution.sw
#
# This exercises the tournament API by:
#
# 1. Uploading a scenario file
# 2. Uploading a solution file for that scenario
#
# This script is idempotent. Repeated invocations
# fetch from the database cache rather than re-evaluating.

cd $(git rev-parse --show-toplevel)

BASE_UPLOAD_URL=http://$HOST/upload

SCENARIO_UPLOAD_URL=$BASE_UPLOAD_URL/scenario
SCENARIO_HASH=$(curl --silent -F "my_file=@$SCENARIO_FILEPATH" $SCENARIO_UPLOAD_URL | jq -r .scenarioFileMetadata.fileHash)

SOLUTION_UPLOAD_URL=$BASE_UPLOAD_URL/solution
curl --silent -F "scenario=$SCENARIO_HASH" -F "my_file=@$SOLUTION_FILEPATH" $SOLUTION_UPLOAD_URL
