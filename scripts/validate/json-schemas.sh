#!/bin/bash -e

cd $(git rev-parse --show-toplevel)/..

find data/scenarios -name "*.yaml" -type f -print0 | xargs -0 check-jsonschema --base-uri $(git rev-parse --show-toplevel)/data/schema/scenario.json --schemafile data/schema/scenario.json

for STEM in terrains entities recipes
do
  check-jsonschema --base-uri $(git rev-parse --show-toplevel)/data/schema/$STEM.json --schemafile data/schema/$STEM.json data/$STEM.yaml
done
