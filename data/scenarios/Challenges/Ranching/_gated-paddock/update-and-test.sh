#!/bin/bash -e

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
PARENT_DIR=$SCRIPT_DIR/..

SCENARIO_FILE=$PARENT_DIR/gated-paddock.yaml

PROGRAM=$(cat $SCRIPT_DIR/enclosure-checking.sw | sed -e 's/[[:blank:]]\+$//') yq -i '.objectives[0].condition = strenv(PROGRAM) | .objectives[].condition style="literal"' $SCENARIO_FILE

stack run -- --scenario $SCENARIO_FILE --run $SCRIPT_DIR/fence-construction.sw --cheat