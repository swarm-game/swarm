#!/bin/bash -e

cd $(git rev-parse --show-toplevel)

DEFAULT_DBNAME=hie.sqlite
DBNAME=${1:-$DEFAULT_DBNAME}

# First, you may need to install hiedb:
#
#    cabal install hiedb

rm -r .hie
cabal clean

# This generates *.hie files:
cabal build -j -O0

hiedb --reindex --database $DBNAME --src-base-dir . index .hie
