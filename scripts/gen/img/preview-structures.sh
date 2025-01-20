#!/bin/bash -xe

cd $(git rev-parse --show-toplevel)

SCENARIO_PATH=${1?"Usage: $0 SCENARIO_PATH"}

IMG_WIDTH=200
IMG_HEIGHT=150

FINAL_IMG_PATH=final25.png

EXECUTABLE_NAME=swarm-scene

cabal build -j -O0 $EXECUTABLE_NAME

OUTPUT_DIR=blarg

mkdir -p $OUTPUT_DIR
cabal run $EXECUTABLE_NAME -- \
    $SCENARIO_PATH structures \
    --fail-blank \
    --dest $FINAL_IMG_PATH \
    --png \
    --width $IMG_WIDTH \
    --height $IMG_HEIGHT
