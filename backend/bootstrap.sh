#!/usr/bin/env bash

# Install system deps
apt-get update
apt-get install -y \
    python3 \
    python3-pip \
    python3-venv \
    lxc \
    lxd \
    lxd-client \
    make \
    rabbitmq-server

# Initialize lxc for code_examples_server
lxd init --auto

# Install code_examples_server deps
cd /vagrant
python3 -m venv venv
source venv/bin/activate
pip3 install -r REQUIREMENTS.txt
cd infrastructure
make

# Setup server
cd /vagrant

