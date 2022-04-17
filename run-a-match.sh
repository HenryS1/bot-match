#!/usr/bin/env bash

set -e

if [[ "$1" == "" || "$2" == "" ]]; then
    echo "Expected two arguments, for the path to bots 1 and 2"
    exit 1
fi

# Source: https://unix.stackexchange.com/questions/24293/converting-relative-path-to-absolute-path-without-symbolic-link
if [ -d "$1" ]; then DIRECTORY_OF_BOT_1=$1/.; fi
DIRECTORY_OF_BOT_1=$(cd "$(dirname -- "$DIRECTORY_OF_BOT_1")"; printf %s. "$PWD")
DIRECTORY_OF_BOT_1=${DIRECTORY_OF_BOT_1%?}

if [ -d "$2" ]; then DIRECTORY_OF_BOT_2=$2/.; fi
DIRECTORY_OF_BOT_2=$(cd "$(dirname -- "$DIRECTORY_OF_BOT_2")"; printf %s. "$PWD")
DIRECTORY_OF_BOT_2=${DIRECTORY_OF_BOT_2%?}

docker run -v /var/run/docker.sock:/var/run/docker.sock -u root -v "$DIRECTORY_OF_BOT_1":"$DIRECTORY_OF_BOT_1" -v "$DIRECTORY_OF_BOT_2":"$DIRECTORY_OF_BOT_2" henrys1/bot-match --config-file-path ./footsoldiers/game-config.json --map-file-path ./footsoldiers/game-map --bot-dir-1 "$DIRECTORY_OF_BOT_1" --bot-dir-2 "$DIRECTORY_OF_BOT_2" 
