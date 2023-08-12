#!/usr/bin/env bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

cd $SCRIPT_DIR/..

if command -v stack &> /dev/null; then
    SWARM="stack exec swarm --"
else
    SWARM="cabal run swarm -O0 --"
fi

for s in scenarios/Tutorials/*.yaml; do
    $SWARM -i $s --autoplay;
    echo -en "$s\tCONTINUE [Y/n]: "
    read answer;
    case "${answer:0:1}" in
        n|N )
            exit 1
        ;;
        * )
        ;;
    esac
done
