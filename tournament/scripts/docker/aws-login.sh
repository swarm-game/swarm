#!/bin/bash -ex

cd $(git rev-parse --show-toplevel)

aws ecr get-login-password --region us-east-1 | docker login --username AWS --password-stdin 254464607561.dkr.ecr.us-east-1.amazonaws.com/swarm-game