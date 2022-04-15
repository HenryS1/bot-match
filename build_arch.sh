#!/bin/bash

## Warning this script has not been tested and the build may fail
## Requires Yay https://github.com/Jguer/yay

set -e

echo "Beginning footsoldiers runner build"

echo "Installing Roswell with Yay"

yay -S roswell

echo "Installing lisp dependencies for footsoldiers"

ros install qlot
~/.roswell/bin/qlot install

echo "Building footsoldiers"

ros run --load footsoldiers/build.lisp
chmod +x footsoldiers-runner

echo "Done"
