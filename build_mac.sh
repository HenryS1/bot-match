#!/bin/bash

set -e

echo "Beginning footsoldiers runner build"

echo "Installing Roswell with homebrew"

brew install roswell

echo "Installing lisp dependencies for footsoldiers runner"

ros install qlot
~/.roswell/bin/qlot install

echo "Building footsoldiers"

ros run --load footsoldiers/build.lisp
chmod +x footsoldiers-runner

echo "Done"
