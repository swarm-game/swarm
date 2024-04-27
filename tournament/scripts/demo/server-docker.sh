#!/bin/bash -ex

GIT_ROOT_DIR=$(git rev-parse --show-toplevel)
cd $GIT_ROOT_DIR

# NOTE: First, you may need to build the Docker image
tournament/scripts/docker/build-image.sh

docker run \
    --add-host=host.docker.internal:host-gateway \
    --env-file $GIT_ROOT_DIR/tournament/scripts/docker/local-pg-credentials.env \
    -it \
    -p 8080:8080 \
    --rm swarm