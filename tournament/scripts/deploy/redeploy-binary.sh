#!/bin/bash -ex

cd $(git rev-parse --show-toplevel)

tournament/scripts/docker/build-static-binary.sh

INTERNAL_BINARY_NAME=tournament-bin
scp -r $INTERNAL_BINARY_NAME lightsail:

rm $INTERNAL_BINARY_NAME

CURRENT_GIT_HASH_FILEPATH=git-hash.txt
git rev-parse HEAD > $CURRENT_GIT_HASH_FILEPATH
scp $CURRENT_GIT_HASH_FILEPATH lightsail:
rm $CURRENT_GIT_HASH_FILEPATH

ssh lightsail -C 'sudo systemctl restart swarm-tournament'
