#!/bin/bash -ex

cd $(git rev-parse --show-toplevel)

curl -s http://localhost:5357/goals/render | dot -Tsvg -o goals.svg
