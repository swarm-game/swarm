#!/bin/bash -e

cd $(git rev-parse --show-toplevel)

find data -type f -name '*.yaml' -print0 | xargs -0 --max-args 1 sed -i -e 's/[[:blank:]]\+$//'

find data -type f -name '*.yaml' -print0 | xargs -0 --max-args 1 yq --inplace
