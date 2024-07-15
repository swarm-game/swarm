#!/bin/bash -xe

# Opens a live-reloading preview of the world
#
# Prerequisites:
# --------------
# Install inotify-wait:
#
#    sudo apt install inotify-tools
#
# Usage:
# --------------
# Once the VS Code editor tabs are opened, one can press
# CTRL+\ (backslash) with the image selected to split the
# editor pane horizontally.
# One may then navigate to the left-pane's copy of the image
# preview with CTRL+PageUp, and then
# CTRL+w will close the redundant image preview.

cd $(git rev-parse --show-toplevel)


SCENARIO_PATH=${1?"Usage: $0 SCENARIO_PATH"}

stack build --fast swarm:swarm-scene

stack exec swarm-scene -- $SCENARIO_PATH --fail-blank --dest outpup.png --png --width 200 --height 150
code --reuse-window $SCENARIO_PATH && code --reuse-window outpup.png 

while inotifywait -e close_write $SCENARIO_PATH; do stack exec swarm-scene -- $SCENARIO_PATH --fail-blank --dest outpup.png --png --width 200 --height 150; done
