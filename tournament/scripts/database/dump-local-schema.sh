#!/bin/bash -ex

GIT_ROOT_DIR=$(git rev-parse --show-toplevel)

pg_dump --create -s -d swarm > $GIT_ROOT_DIR/tournament/schema/schema-local.sql
