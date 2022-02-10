#!/usr/bin/env bash

set -e

if [[ "$1" == "" || "$2" == "" ]]; then
    echo "Expected two arguments, for the path to bots 1 and 2"
    exit 1
fi

docker run bot-runner --config-file-path ./footsoldiers/game-config.json --map-file-path ./footsoldiers/game-map --bot-dir-1 "$1" --bot-dir-2 "$2"
