#!/bin/bash -e

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd $SCRIPT_DIR/..


if grep --line-number --include \*.hs -riP '(TODO|FIXME|XXX):?\s' src 2>&1 | grep -vP '#\d+'; then
  echo "Please add a link to Issue, for example: TODO: #123"
  exit 1
else
  echo "No TODOs without links found, all good!"
fi
