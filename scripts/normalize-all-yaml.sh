#!/bin/bash -e

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd $SCRIPT_DIR/..

find data -type f -name '*.yaml' -print0 | xargs -0 --max-args 1 sed -i -e 's/[[:blank:]]\+$//'

find data -type f -name '*.yaml' -print0 | xargs -0 --max-args 1 yq --inplace
