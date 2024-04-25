#!/bin/bash -ex

GIT_ROOT_DIR=$(git rev-parse --show-toplevel)

sudo service postgresql restart
dropdb swarm

sudo -u postgres psql < $GIT_ROOT_DIR/tournament/schema/schema-local.sql
