#!/bin/bash

set -e

sbt "clean" "assembly"

mv target/scala-3.1.1/scala-assembly-0.1.0-SNAPSHOT.jar bot.jar
