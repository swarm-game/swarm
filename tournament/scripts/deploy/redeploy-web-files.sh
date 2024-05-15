#!/bin/bash -ex

cd $(git rev-parse --show-toplevel)

scp -r tournament/web lightsail:tournament
rsync -r data lightsail:.local/share/swarm
