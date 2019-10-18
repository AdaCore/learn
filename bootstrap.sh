#!/usr/bin/env bash

# Install system deps
apt-get update
apt-get install -y \
    python2.7 \
    python-pip \
    nodejs \
    npm \
    graphviz \
    make \
    gnat

# Install learn deps
cd /vagrant
pip install -r requirements.txt
cd engine

# Install yarn
apt remove cmdtest
curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list
apt-get update && apt-get install yarn

yarn
