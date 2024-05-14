#!/bin/bash -ex

cd $(git rev-parse --show-toplevel)

TAG_NAME=swarm
docker build --tag $TAG_NAME --file tournament/scripts/docker/alpine/Dockerfile .

INTERNAL_BINARY_NAME=tournament-bin

ID=$(docker create $TAG_NAME)
docker cp $ID:/opt/swarm/$INTERNAL_BINARY_NAME - | tar xv
docker rm -v $ID

strip $INTERNAL_BINARY_NAME