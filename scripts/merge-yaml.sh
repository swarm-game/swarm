#!/bin/bash -xe

# Obtained from: https://stackoverflow.com/a/67036496/105137
yq eval-all '. as $item ireduce ({}; . *+ $item)' $@