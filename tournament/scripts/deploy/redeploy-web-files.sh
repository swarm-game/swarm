#!/bin/bash -ex

cd $(git rev-parse --show-toplevel)

scp -r tournament/web lightsail:tournament
scp -r data lightsail:.local/share/swarm
