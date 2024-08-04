$frontend = <<-SHELL
  #!/bin/bash -eux

  # Enable the NodeSource repository
  curl -sL https://deb.nodesource.com/setup_20.x | bash -

  # Install system deps
  DEBIAN_FRONTEND=noninteractive apt-get update
  DEBIAN_FRONTEND=noninteractive apt-get install -y \
      python3 \
      python3-pip \
      python3-venv \
      nodejs \
      graphviz \
      plantuml \
      poppler-utils \
      libjpeg-dev \
      make

  # Install FSF GNAT
  # (Required tool: gnatchop)
  wget -O gnat.tar.gz https://github.com/alire-project/GNAT-FSF-builds/releases/download/gnat-12.2.0-1/gnat-x86_64-linux-12.2.0-1.tar.gz && \
  tar xzf gnat.tar.gz && \
  mv gnat-* /usr/local/gnat && \
  rm *.tar.gz

  echo 'export PATH="/usr/local/gnat/bin:${PATH}"' >> /home/vagrant/.bashrc
  source /home/vagrant/.bashrc

  # Install learn deps
  python3 -m venv /vagrant/venv
  source /vagrant/venv/bin/activate
  pip3 install -r /vagrant/frontend/requirements.txt

  # File system: increase number of user watches
  # Needed for npm
  echo fs.inotify.max_user_watches=524288 | tee -a /etc/sysctl.conf && sysctl -p

  cd /vagrant/frontend
  echo 'export COREPACK_ENABLE_DOWNLOAD_PROMPT=0' >> /home/vagrant/.bashrc
  yes | corepack enable
  yes | yarn set version berry
  yarn

SHELL

$epub = <<-SHELL
  #!/bin/bash -eux

  # Enable the NodeSource repository
  curl -sL https://deb.nodesource.com/setup_20.x | bash -

  # Install system deps
  DEBIAN_FRONTEND=noninteractive apt-get update
  DEBIAN_FRONTEND=noninteractive apt-get install -y \
      python3 \
      python3-pip \
      python3-venv \
      nodejs \
      graphviz \
      plantuml \
      make \
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
      poppler-utils \
      libjpeg-dev \
      build-essential \
      ca-certificates \
      git \
      libdbus-1-3 \
      libfontconfig \
      libx11-xcb-dev \
      wget \
      libc6-dev

  # Install FSF GNAT
  wget -O gnat.tar.gz https://github.com/alire-project/GNAT-FSF-builds/releases/download/gnat-12.2.0-1/gnat-x86_64-linux-12.2.0-1.tar.gz && \
  wget -O gnatprove.tar.gz https://github.com/alire-project/GNAT-FSF-builds/releases/download/gnatprove-12.1.0-1/gnatprove-x86_64-linux-12.1.0-1.tar.gz && \
  wget -O gprbuild.tar.gz https://github.com/alire-project/GNAT-FSF-builds/releases/download/gprbuild-22.0.0-1/gprbuild-x86_64-linux-22.0.0-1.tar.gz && \
  tar xzf gnat.tar.gz && \
  mv gnat-* /usr/local/gnat && \
  tar xzf gnatprove.tar.gz && \
  mv gnatprove-* /usr/local/gnatprove && \
  tar xzf gprbuild.tar.gz && \
  mv gprbuild-* /usr/local/gprbuild && \
  rm *.tar.gz

  echo 'export PATH="/usr/local/gnat/bin:/usr/local/gprbuild/bin:/usr/local/gnatprove/bin:${PATH}"' >> /home/vagrant/.bashrc
  source /home/vagrant/.bashrc

  # Install learn deps
  python3 -m venv /vagrant/venv
  source /vagrant/venv/bin/activate
  pip3 install -r /vagrant/frontend/requirements.txt

  # File system: increase number of user watches
  # Needed for npm
  echo fs.inotify.max_user_watches=524288 | tee -a /etc/sysctl.conf && sysctl -p

  cd /vagrant/frontend
  echo 'export COREPACK_ENABLE_DOWNLOAD_PROMPT=0' >> /home/vagrant/.bashrc
  yes | corepack enable
  yes | yarn set version berry
  yarn

SHELL

Vagrant.configure("2") do |config|

  config.vm.provider "virtualbox" do |vb|
    vb.customize ["setextradata", :id, "VBoxInternal2/SharedFoldersEnableSymlinksCreate/v-root", "1"]
  end

  config.vm.synced_folder '.', '/vagrant', disabled: true

  config.vm.define "web" do |web|
    web.vm.box = "bento/ubuntu-23.10"
    web.vm.box_version = "202402.01.0"
    web.vm.network "forwarded_port", guest: 8080, host: 8080, host_ip: "127.0.0.1"

    web.vm.synced_folder './frontend', '/vagrant/frontend'
    web.vm.synced_folder './content', '/vagrant/content'

    web.vm.provision :shell, inline: $frontend
  end

  config.vm.define "epub" do |epub|
    epub.vm.box = "bento/ubuntu-23.10"
    epub.vm.box_version = "202402.01.0"

    epub.vm.synced_folder './frontend', '/vagrant/frontend'
    epub.vm.synced_folder './content', '/vagrant/content'

    epub.vm.provision :shell, inline: $epub
  end

end
