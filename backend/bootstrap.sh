#!/usr/bin/env bash

# Install system deps
DEBIAN_FRONTEND=noninteractive apt-get update
DEBIAN_FRONTEND=noninteractive apt-get install -y \
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
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
add-apt-repository \
   "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
   $(lsb_release -cs) \
   stable"
apt-get update
apt-get install -y docker-ce docker-ce-cli containerd.io

# Add vagrant user to docker to allow interactive container
# groupadd docker
usermod -aG docker vagrant

# Install code_examples_server deps
python3 -m venv /vagrant/venv
source /vagrant/venv/bin/activate
pip3 install -r /vagrant/REQUIREMENTS.txt

cd /vagrant/infrastructure
make docker
