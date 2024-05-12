#!/bin/bash -ex

GIT_ROOT_DIR=$(git rev-parse --show-toplevel)

sqlite3 swarm-games.db < $GIT_ROOT_DIR/tournament/schema/swarm-sqlite-schema.sql
