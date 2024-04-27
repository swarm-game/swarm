#!/bin/bash -ex

cd $(git rev-parse --show-toplevel)

tournament/scripts/docker/build-image.sh

AWS_DOCKER_IMAGE=254464607561.dkr.ecr.us-east-1.amazonaws.com/swarm-game:latest

docker tag swarm:latest $AWS_DOCKER_IMAGE

# Optionally log in again
tournament/scripts/docker/aws-login.sh
docker push $AWS_DOCKER_IMAGE

# Next, run:
#   eb deploy swarm-tournament-server-env