#!/bin/bash

set -e

./gradlew clean build

mv app/build/libs/app-all.jar bot.jar