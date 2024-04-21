#!/bin/bash -ex

# This exercises the tournament API by:
#
# 1. Uploading a scenario file
# 2. Uploading a solution file for that scenario
#
# This script is idempotent. Repeated invocations
# fetch from the database cache rather than re-evaluating.

cd $(git rev-parse --show-toplevel)

SCENARIO_DATA_DIR=data/scenarios
PORT=8080
BASE_API_URL=http://localhost:$PORT

SCENARIO_UPLOAD_URL=$BASE_API_URL/upload/scenario
SCENARIO_FILEPATH=$SCENARIO_DATA_DIR/Challenges/arbitrage.yaml
SCENARIO_HASH=$(curl --silent -F "my_file=@$SCENARIO_FILEPATH" $SCENARIO_UPLOAD_URL | jq -r .scenarioFileMetadata.fileHash)

echo "Scenario hash: $SCENARIO_HASH"

SCENARIO_DOWNLOAD_URL=$BASE_API_URL/scenario/$SCENARIO_HASH/metadata
curl --silent $SCENARIO_DOWNLOAD_URL | jq .

# SCENARIO_DOWNLOAD_URL=$BASE_API_URL/scenario/$SCENARIO_HASH/fetch
#curl --silent $SCENARIO_DOWNLOAD_URL
