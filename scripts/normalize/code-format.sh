#!/bin/bash -e

cd $(git rev-parse --show-toplevel)

fourmolu --mode=inplace src app test scripts