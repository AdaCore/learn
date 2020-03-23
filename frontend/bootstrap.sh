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
    python3-venv \
    nodejs \
    graphviz \
    make \
    gnat \
    yarn \
    texlive-latex-base \
    texlive-latex-recommended \
    texlive-latex-extra \
    texlive-fonts-recommended \
    texlive-fonts-extra \
    latexmk \
    texlive-xetex \
    fonts-lmodern \
    fonts-open-sans \
    fonts-dejavu


# Install learn deps
python3 -m venv /vagrant/venv
source /vagrant/venv/bin/activate
pip3 install -r /vagrant/frontend/requirements.txt

cd /vagrant/frontend
yarn
