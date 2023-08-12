#!/usr/bin/env bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

cd $SCRIPT_DIR/..

if command -v stack &> /dev/null; then
    SWARM="stack exec swarm --"
else
    SWARM="cabal run swarm -O0 --"
fi

for tutorial in $(cat scenarios/Tutorials/00-ORDER.txt | xargs); do
    echo -n "$tutorial"
    $SWARM -i "scenarios/Tutorials/$tutorial" --autoplay --cheat;
    echo -en "\tCONTINUE [Y/n]: "
    read answer;
    case "${answer:0:1}" in
        n|N )
            exit 1
        ;;
        * )
        ;;
    esac
done
