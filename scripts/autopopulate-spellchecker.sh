#!/bin/bash -e

# This script lives 1 level deep in the directory structure.
# Ensure its commands get run at the toplevel.
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd $SCRIPT_DIR/..


# First, install hiedb:
#
#    cabal install hiedb
#
# Then, generate *.hie files:
#
#    cp hie.yaml.stack hie.yaml
#    stack build --fast

DBNAME=hie.sqlite
hiedb --database $DBNAME index .hie

WORDS_TMPFILE_UNSORTED=$(mktemp --suffix .words)

# First, get all variable names that originate from external packages.
# Exclude names that contain symbols other than underscore or apostrophe or do not contain any letters.
sqlite3 $DBNAME "SELECT substr(sym, 3) FROM (SELECT DISTINCT occ AS sym FROM refs WHERE unit NOT IN (SELECT DISTINCT unit FROM mods)) ORDER BY sym" | grep -v "[^[:alnum:]_']" | grep "[[:alpha:]]" > $WORDS_TMPFILE_UNSORTED

# Next, append the individual "conids" extracted from all "modids" that originate from external packages.
# See definition of "conid" and "monid" here:
# https://www.haskell.org/onlinereport/haskell2010/haskellch5.html
#
# E.g., takes lines of the following form:
#
#    Data.Text
#    Control.Arrow
#
# and produces a flattened list of words:
#
#   Arrow
#   Control
#   Data
#   Text
sqlite3 $DBNAME "SELECT DISTINCT mod FROM refs WHERE unit NOT IN (SELECT DISTINCT unit FROM mods)" | tr '.' '\n' >> $WORDS_TMPFILE_UNSORTED

WORDS_TMPFILE=$(mktemp --suffix .words)
sort -u $WORDS_TMPFILE_UNSORTED > $WORDS_TMPFILE
rm $WORDS_TMPFILE_UNSORTED

CSPELL_TMPFILE=$(mktemp --suffix .cspell)
CSPELL_FILEPATH=cspell.json

# Now, stuff the sorted list of names into the cspell JSON file word list.
jq '.words = $newWords' --slurpfile newWords <(jq --raw-input --null-input 'inputs' $WORDS_TMPFILE) $CSPELL_FILEPATH > $CSPELL_TMPFILE

# Overwrite the original with the modified version.
mv $CSPELL_TMPFILE $CSPELL_FILEPATH

rm $WORDS_TMPFILE
