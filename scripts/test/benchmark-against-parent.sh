#!/bin/bash -xe

cd $(git rev-parse --show-toplevel)

scripts/benchmark-against-ancestor.sh HEAD~