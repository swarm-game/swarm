#!/bin/bash -ex

cd $(git rev-parse --show-toplevel)

# First, install Weeder:
#   cabal install weeder

cabal clean
cabal build -O0 -j all

weeder -N
