#!/bin/bash -e

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd $SCRIPT_DIR/..

find data/scenarios -name "*.yaml" -type f -print0 | xargs -0 check-jsonschema --base-uri $(git rev-parse --show-toplevel)/data/schema/scenario.json --schemafile data/schema/scenario.json

check-jsonschema --base-uri $(git rev-parse --show-toplevel)/data/schema/entities.json --schemafile data/schema/entities.json data/entities.yaml
check-jsonschema --base-uri $(git rev-parse --show-toplevel)/data/schema/recipes.json --schemafile data/schema/recipes.json data/recipes.yaml