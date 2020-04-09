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
    rabbitmq-server \
    apt-transport-https \
    ca-certificates \
    curl \
    gnupg-agent \
    software-properties-common

# Initialize lxc for code_examples_server
lxd init --auto

# Install docker
curl -fsSL https://get.docker.com -o get-docker.sh
sh get-docker.sh

# Install code_examples_server deps
python3 -m venv /vagrant/venv
source /vagrant/venv/bin/activate
pip3 install -r /vagrant/REQUIREMENTS.txt

cd /vagrant/infrastructure
make
