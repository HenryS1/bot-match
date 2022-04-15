#!/bin/bash

# Execute this script from the root of the repository to build the runner

set -e

echo "Beginning footsoldiers runner build"

# Roswell is a depedency management and build system for common lisp
# https://github.com/roswell/roswell
echo "Installing dependencies for Roswell"

sudo apt-get update 
sudo apt-get install -y wget
sudo apt-get install tar
sudo apt-get install -y libcurl3-gnutls
sudo apt-get install -y git
sudo apt-get install -y bzip2
sudo apt-get install -y make

echo "Installing Roswell"

wget https://github.com/roswell/roswell/releases/download/v20.01.14.104/roswell_20.01.14.104-1_amd64.deb
sudo dpkg -i roswell_20.01.14.104-1_amd64.deb

echo "Installing lisp dependencies for footsoldiers runner"

ros install qlot
~/.roswell/bin/qlot install

echo "Building footsoldiers"

ros run --load footsoldiers/build.lisp
chmod +x footsoldiers-runner

echo "Done"
