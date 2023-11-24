#!/bin/bash -ex

LOCAL_ARTIFACTS_DIR=.stack-work

SCRIPT_DIR=$(cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd)
cd $SCRIPT_DIR/..

STACK_WORK=$LOCAL_ARTIFACTS_DIR stack haddock --fast

DOCPATH_ROOT=$(STACK_WORK=$LOCAL_ARTIFACTS_DIR stack path --local-doc-root)
SWARM_PACKAGE=$(stack ls dependencies --depth 0 swarm --separator -)
SWARM_HADDOCK_INDEX=$DOCPATH_ROOT/$SWARM_PACKAGE/index.html

google-chrome $SWARM_HADDOCK_INDEX