$frontend = <<-SHELL
  #!/bin/sh -eux

  # Enable the NodeSource repository
  curl -sL https://deb.nodesource.com/setup_16.x | sudo -E bash -

  # Add yarn to apt-get
  curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
  echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list

  # Install system deps
  DEBIAN_FRONTEND=noninteractive apt-get update
  DEBIAN_FRONTEND=noninteractive apt-get install -y \
      python3 \
      python3-pip \
      python3-venv \
      nodejs \
      graphviz \
      make \
      gnat \
      gprbuild \
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

SHELL

$epub = <<-SHELL
  #!/bin/sh -eux

  # Enable the NodeSource repository
  curl -sL https://deb.nodesource.com/setup_16.x | sudo -E bash -

  # Add yarn to apt-get
  curl -sL https://dl.yarnpkg.com/debian/pubkey.gpg | gpg --dearmor | tee /usr/share/keyrings/yarnkey.gpg >/dev/null
  echo "deb [signed-by=/usr/share/keyrings/yarnkey.gpg] https://dl.yarnpkg.com/debian stable main" | tee /etc/apt/sources.list.d/yarn.list

  apt-get update && sudo apt-get install yarn

  # Install system deps
  DEBIAN_FRONTEND=noninteractive apt-get update
  DEBIAN_FRONTEND=noninteractive apt-get install -y \
      python3 \
      python3-pip \
      python3-venv \
      nodejs \
      graphviz \
      make \
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
      fonts-dejavu \
      build-essential \
      ca-certificates \
      git \
      libdbus-1-3 \
      libfontconfig \
      libx11-xcb-dev \
      wget \
      libc6-dev

  # Install GNAT Community
  git clone https://github.com/AdaCore/gnat_community_install_script.git /gnat_installer/script \
    && wget -q https://community.download.adacore.com/v1/f3a99d283f7b3d07293b2e1d07de00e31e332325?filename=gnat-2021-20210519-x86_64-linux-bin -O /gnat_installer/actual \
    && sh /gnat_installer/script/install_package.sh /gnat_installer/actual /gnat com.adacore.spark2014_discovery,com.adacore.gnat \
    && rm -rf /gnat_installer

  echo 'export PATH="/gnat/bin:${PATH}"' >> /home/vagrant/.bashrc
  source /home/vagrant/.bashrc

  # Install learn deps
  python3 -m venv /vagrant/venv
  source /vagrant/venv/bin/activate
  pip3 install -r /vagrant/frontend/requirements.txt

  cd /vagrant/frontend
  yarn

SHELL

$backend = <<-SHELL
  #!/bin/sh -eux

  # Get docker apt repos
  curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
  add-apt-repository \
    "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
    $(lsb_release -cs) \
    stable"

  # Install system deps
  DEBIAN_FRONTEND=noninteractive apt-get update
  DEBIAN_FRONTEND=noninteractive apt-get install -y \
      python3 \
      python3-pip \
      python3-venv \
      make \
      rabbitmq-server \
      apt-transport-https \
      ca-certificates \
      curl \
      gnupg-agent \
      software-properties-common \
      docker-ce \
      docker-ce-cli \
      containerd.io

  # Add vagrant user to docker to allow interactive container
  # groupadd docker
  usermod -aG docker vagrant

  # Install code_examples_server deps
  python3 -m venv /vagrant/venv
  source /vagrant/venv/bin/activate
  pip3 install -r /vagrant/REQUIREMENTS.txt

  cd /vagrant/infrastructure
  # stop previous containers
  docker stop $(docker ps -a -q)
  # remove all images/containers on system
  docker system prune -a -f
  # Build docker image
  docker build -t "safecontainer" .
SHELL

Vagrant.configure("2") do |config|

  config.vm.provider "virtualbox" do |vb|
    vb.customize ["setextradata", :id, "VBoxInternal2/SharedFoldersEnableSymlinksCreate/v-root", "1"]
  end

  config.vm.synced_folder '.', '/vagrant', disabled: true

  config.vm.define "web" do |web|
    web.vm.box = "bento/ubuntu-18.04"
    web.vm.network "forwarded_port", guest: 8080, host: 8080, host_ip: "127.0.0.1"

    web.vm.synced_folder './frontend', '/vagrant/frontend'
    web.vm.synced_folder './content', '/vagrant/content'

    web.vm.provision :shell, inline: $frontend
  end

  config.vm.define "epub" do |epub|
    epub.vm.box = "bento/ubuntu-21.04"

    epub.vm.synced_folder './frontend', '/vagrant/frontend'
    epub.vm.synced_folder './content', '/vagrant/content'

    epub.vm.provision :shell, inline: $epub
  end

  config.vm.define "server" do |server|
    server.vm.box = "bento/ubuntu-18.04"

    server.vm.network "forwarded_port", guest: 8000, host: 8000, host_ip: "127.0.0.1"
    server.vm.network "forwarded_port", guest: 6379, host: 6379, host_ip: "127.0.0.1"

    server.vm.synced_folder './backend', '/vagrant'

    server.vm.provision :shell, inline: $backend
  end
end
