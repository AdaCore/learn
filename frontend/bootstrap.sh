#!/usr/bin/env bash

# Enable the NodeSource repository
curl -sL https://deb.nodesource.com/setup_10.x | sudo -E bash -

# Add yarn to apt-get
curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list

# Install system deps
apt-get update
apt-get install -y \
    python3 \
    python3-pip \
    nodejs \
    graphviz \
    make \
    gnat \
    yarn

# Install learn deps
cd /vagrant/frontend
pip3 install -r requirements.txt

yarn
