#!/bin/bash

echo "def m1 : Cmd Unit = move; end" > moves.sw

for i in {2..100}; do cat <<EOF
def m$i : Cmd Unit = move; m$((i - 1)); end
EOF
done >> moves.sw
