#!/usr/bin/env bash
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

for tutorial in $(cat scenarios/Tutorials/00-ORDER.txt | xargs); do
    echo -n "$tutorial"
    scripts/play.sh -i "scenarios/Tutorials/$tutorial" --autoplay --cheat;
    echo -en "\tCONTINUE [Y/n]: "
    read -r answer;
    case "${answer:0:1}" in
        n|N )
            exit 1
        ;;
        * )
        ;;
    esac
done
