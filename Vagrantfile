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
  config.vm.box = "bento/ubuntu-18.04"

  config.vm.provider "virtualbox" do |vb|
    vb.customize ["setextradata", :id, "VBoxInternal2/SharedFoldersEnableSymlinksCreate/v-root", "1"]
  end

  config.vm.synced_folder '.', '/vagrant', disabled: true

  config.vm.define "web" do |web|
    web.vm.network "forwarded_port", guest: 8080, host: 8080, host_ip: "127.0.0.1"

    web.vm.synced_folder './frontend', '/vagrant/frontend'
    web.vm.synced_folder './content', '/vagrant/content'

    web.vm.provision :shell, inline: $frontend
  end

  config.vm.define "server" do |server|

    server.vm.network "forwarded_port", guest: 8000, host: 8000, host_ip: "127.0.0.1"
    server.vm.network "forwarded_port", guest: 6379, host: 6379, host_ip: "127.0.0.1"

    server.vm.synced_folder './backend', '/vagrant'

    server.vm.provision :shell, inline: $backend
  end
end
