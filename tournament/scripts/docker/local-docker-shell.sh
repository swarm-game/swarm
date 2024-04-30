#!/bin/bash -ex

cd $(git rev-parse --show-toplevel)

#$NAME_ARG="--name swarm-container"
docker run -it $NAME_ARG -p 8080:8080 --entrypoint /bin/bash swarm