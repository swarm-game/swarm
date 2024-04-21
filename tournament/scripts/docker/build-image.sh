#!/bin/bash -ex

cd $(git rev-parse --show-toplevel)

CURRENT_GIT_HASH_FILEPATH=git-hash.txt

git rev-parse HEAD > $CURRENT_GIT_HASH_FILEPATH

docker build --tag swarm --file tournament/scripts/docker/Dockerfile .

rm $CURRENT_GIT_HASH_FILEPATH