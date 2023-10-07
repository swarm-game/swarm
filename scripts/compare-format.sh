#!/bin/bash

MIN=10
MAX=200
STEP=5

function help() {
    echo "$0 [--min $MIN --max $MAX --step $STEP] EXAMPLE_FILE.sw"
    echo
    echo "This script helps to compare the format layout for a range"
    echo "of output widths. Afterwards it prints those that differ"
    echo "as markdown with triple backticks."
}

# Simple argumet parsing from https://stackoverflow.com/a/14203146
POSITIONAL_ARGS=()
while [[ $# -gt 0 ]]; do
  case $1 in
    --min)
      MIN="$2"
      shift # past argument
      shift # past value
      ;;
    --max)
      MAX="$2"
      shift # past argument
      shift # past value
      ;;
    -s|--step)
      STEP="$2"
      shift # past argument
      shift # past value
      ;;
    -h|--help)
      help
      exit 0
      ;;
    -*)
      echo "Unknown option $1"
      help
      exit 1
      ;;
    *)
      POSITIONAL_ARGS+=("$1") # save positional arg
      shift # past argument
      ;;
  esac
done

# Build first, otherwise the ouput would go to temporary files
cabal build -O0

function compare_format() {
    echo "# $1"
    if ! test -f "$1"; then
      echo "Could not find file '$1'!"
      return
    fi
    # save each version in a temporary file
    t=$(mktemp -d)

    for i in $(seq "$MIN" "$STEP" "$MAX"); do
        echo -en "${i}\r"  # show progress
        cabal run swarm -O0 -- format --width "$i" "$1" > "$t/$i.sw";
    done
    echo -en "   \r"

    for i in $(seq "$MAX" "-$STEP" "$MIN"); do
    if ! cmp -s "$t/$i.sw" "$t"/$((i - STEP)).sw; then
        echo "$i";
        echo '```';
        cat "$t/$i.sw";
        echo '```';
    fi;
    done
}

for file in "${POSITIONAL_ARGS[@]}"; do
  compare_format "$file";
done