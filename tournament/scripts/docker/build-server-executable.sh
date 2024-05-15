#!/bin/sh -ex

# Usage:
# Intended to be invoked in Dockerfile.
#
#    build-server-executable.sh <output binary location>
#
# Note that we use 'cabal' instead of 'stack' becuase
# 'stack' fails to compile the 'vty' package within the Amazon Linux docker image.

BUILD_TARGET=swarm:swarm-host-tournament
CABAL_ARGS="-j -O0 --enable-executable-static $BUILD_TARGET"

cabal build $CABAL_ARGS
#cp $(cabal list-bin $CABAL_ARGS) $1
BIN_PATH=$(cabal list-bin $CABAL_ARGS)
strip $BIN_PATH

BIN_NAME=swarm-host-tournament
TEMP_UPLOAD_BIN_NAME=$BIN_NAME.new

rsync -P $BIN_PATH lightsail:$TEMP_UPLOAD_BIN_NAME
ssh lightsail -C "mv $TEMP_UPLOAD_BIN_NAME $BIN_NAME && sudo systemctl restart swarm-tournament"
