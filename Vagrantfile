$frontend = <<-SHELL
  #!/bin/bash -eux

  # Enable the NodeSource repository
  curl -sL https://deb.nodesource.com/setup_22.x | bash -

  # Generate list of installed packages
  dpkg -l | awk '$1 == "ii" { printf "%s\\n", $2 }' > /vagrant/vm_apt_installed.txt

  apt list --installed > /vagrant/vm_apt_list.txt

  # Install system deps
  DEBIAN_FRONTEND=noninteractive apt-get update
  DEBIAN_FRONTEND=noninteractive apt-get install -y \
      crudini \
      python3 \
      python3-pip \
      python3-venv \
      nodejs \
      graphviz \
      plantuml \
      poppler-utils \
      libjpeg-dev \
      make

  # Install/check packages from list for reproducibility
  DEBIAN_FRONTEND=noninteractive apt-get install \
    --allow-downgrades -y $(cat /home/vagrant/vm_apt.txt)

  # Force packages to be set as automatically installed
  apt-mark auto $(cat /vagrant/vm_apt_list.txt | grep "\\[installed,automatic\\]" | awk -F/ -v ORS=" " 'NR>1 {print $1}')

  # Get relevant information from configuration file
  toolchain_config=/home/vagrant/toolchain.ini
  path_ada_toolchain_root=$(crudini --get $toolchain_config toolchain_path root)
  path_ada_toolchain_selected=$(crudini --get $toolchain_config toolchain_path selected)
  path_ada_toolchain_default=$(crudini --get $toolchain_config toolchain_path default)
  default_version_gnat=$(crudini --get $toolchain_config default_version gnat)
  toolchain_versions_gnat=$(crudini --get $toolchain_config toolchains gnat)

  echo path_ada_toolchain_root:      $path_ada_toolchain_root
  echo path_ada_toolchain_selected:  $path_ada_toolchain_selected
  echo path_ada_toolchain_default:   $path_ada_toolchain_default
  echo default_version_gnat:         $default_version_gnat
  echo toolchain_versions_gnat:      $toolchain_versions_gnat

  # Install FSF GNAT
  # (Required tool: gnatchop)
  mkdir -p ${path_ada_toolchain_root}
  mkdir -p ${path_ada_toolchain_default}
  mkdir -p ${path_ada_toolchain_selected}

  gnat_version=(${toolchain_versions_gnat})
  mkdir ${path_ada_toolchain_root}/gnat
  for tool_version in ${gnat_version[@]}; do
    echo Installing GNAT $tool_version
    wget -O gnat.tar.gz https://github.com/alire-project/GNAT-FSF-builds/releases/download/gnat-${tool_version}/gnat-x86_64-linux-${tool_version}.tar.gz && \
    tar xzf gnat.tar.gz && \
    mv gnat-* ${path_ada_toolchain_root}/gnat/${tool_version} && \
    rm *.tar.gz
  done

  ln -sf ${path_ada_toolchain_root}/gnat/${default_version_gnat}            ${path_ada_toolchain_default}/gnat

  chown -R vagrant:vagrant ${path_ada_toolchain_root}

  echo "export PATH=\\"${path_ada_toolchain_selected}/gnat/bin:${path_ada_toolchain_default}/gnat/bin:${PATH}\\"" >> /home/vagrant/.bashrc
  source /home/vagrant/.bashrc

  # Install learn deps
  python3 -m venv /vagrant/venv
  source /vagrant/venv/bin/activate
  pip3 install -r /vagrant/frontend/requirements_frozen.txt

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
  curl -sL https://deb.nodesource.com/setup_22.x | bash -

  # Generate list of installed packages
  dpkg -l | awk '$1 == "ii" { printf "%s\\n", $2 }' > /vagrant/vm_apt_installed.txt

  apt list --installed > /vagrant/vm_apt_list.txt

  # Install system deps
  DEBIAN_FRONTEND=noninteractive apt-get update
  DEBIAN_FRONTEND=noninteractive apt-get install -y \
      crudini \
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
      xindy \
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

  # Install/check packages from list for reproducibility
  DEBIAN_FRONTEND=noninteractive apt-get install \
    --allow-downgrades -y $(cat /home/vagrant/vm_apt.txt)

  # Force packages to be set as automatically installed
  apt-mark auto $(cat /vagrant/vm_apt_list.txt | grep "\\[installed,automatic\\]" | awk -F/ -v ORS=" " 'NR>1 {print $1}')

  # Get relevant information from configuration file
  toolchain_config=/home/vagrant/toolchain.ini
  path_ada_toolchain_root=$(crudini --get $toolchain_config toolchain_path root)
  path_ada_toolchain_selected=$(crudini --get $toolchain_config toolchain_path selected)
  path_ada_toolchain_default=$(crudini --get $toolchain_config toolchain_path default)
  default_version_gnat=$(crudini --get $toolchain_config default_version gnat)
  default_version_gnatprove=$(crudini --get $toolchain_config default_version gnatprove)
  default_version_gprbuild=$(crudini --get $toolchain_config default_version gprbuild)
  toolchain_versions_gnat=$(crudini --get $toolchain_config toolchains gnat)
  toolchain_versions_gnatprove=$(crudini --get $toolchain_config toolchains gnatprove)
  toolchain_versions_gprbuild=$(crudini --get $toolchain_config toolchains gprbuild)

  echo path_ada_toolchain_root:      $path_ada_toolchain_root
  echo path_ada_toolchain_selected:  $path_ada_toolchain_selected
  echo path_ada_toolchain_default:   $path_ada_toolchain_default
  echo default_version_gnat:         $default_version_gnat
  echo default_version_gnatprove:    $default_version_gnatprove
  echo default_version_gprbuild:     $default_version_gprbuild
  echo toolchain_versions_gnat:      $toolchain_versions_gnat
  echo toolchain_versions_gnatprove  $toolchain_versions_gnatprove
  echo toolchain_versions_gprbuild   $toolchain_versions_gprbuild

  # Install FSF GNAT
  mkdir -p ${path_ada_toolchain_root}
  mkdir -p ${path_ada_toolchain_default}
  mkdir -p ${path_ada_toolchain_selected}

  gnat_version=(${toolchain_versions_gnat})
  mkdir ${path_ada_toolchain_root}/gnat
  for tool_version in ${gnat_version[@]}; do
    echo Installing GNAT $tool_version
    wget -O gnat.tar.gz https://github.com/alire-project/GNAT-FSF-builds/releases/download/gnat-${tool_version}/gnat-x86_64-linux-${tool_version}.tar.gz && \
    tar xzf gnat.tar.gz && \
    mv gnat-* ${path_ada_toolchain_root}/gnat/${tool_version} && \
    rm *.tar.gz
  done

  gnat_prove_version=(${toolchain_versions_gnatprove})
  mkdir ${path_ada_toolchain_root}/gnatprove
  for tool_version in ${gnat_prove_version[@]}; do
    echo Installing GNATprove $tool_version
    wget -O gnatprove.tar.gz https://github.com/alire-project/GNAT-FSF-builds/releases/download/gnatprove-${tool_version}/gnatprove-x86_64-linux-${tool_version}.tar.gz && \
    tar xzf gnatprove.tar.gz && \
    mv gnatprove-* ${path_ada_toolchain_root}/gnatprove/${tool_version} && \
    rm *.tar.gz
  done

  gprbuild_version=(${toolchain_versions_gprbuild})
  mkdir ${path_ada_toolchain_root}/gprbuild
  for tool_version in ${gprbuild_version[@]}; do
    echo Installing GPRbuild $tool_version
    wget -O gprbuild.tar.gz https://github.com/alire-project/GNAT-FSF-builds/releases/download/gprbuild-${tool_version}/gprbuild-x86_64-linux-${tool_version}.tar.gz && \
    tar xzf gprbuild.tar.gz && \
    mv gprbuild-* ${path_ada_toolchain_root}/gprbuild/${tool_version} && \
    rm *.tar.gz
  done

  ln -sf ${path_ada_toolchain_root}/gnat/${default_version_gnat}            ${path_ada_toolchain_default}/gnat
  ln -sf ${path_ada_toolchain_root}/gnatprove/${default_version_gnatprove}  ${path_ada_toolchain_default}/gnatprove
  ln -sf ${path_ada_toolchain_root}/gprbuild/${default_version_gprbuild}    ${path_ada_toolchain_default}/gprbuild

  chown -R vagrant:vagrant ${path_ada_toolchain_root}

  echo "export PATH=\\"${path_ada_toolchain_selected}/gnat/bin:${path_ada_toolchain_selected}/gprbuild/bin:${path_ada_toolchain_selected}/gnatprove/bin:${path_ada_toolchain_default}/gnat/bin:${path_ada_toolchain_default}/gprbuild/bin:${path_ada_toolchain_default}/gnatprove/bin:${PATH}\\"" >> /home/vagrant/.bashrc
  source /home/vagrant/.bashrc

  # Install learn deps
  python3 -m venv /vagrant/venv
  source /vagrant/venv/bin/activate
  pip3 install -r /vagrant/frontend/requirements_frozen.txt

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

    web.vm.provision "file", source: "./frontend/py_modules/code_projects/toolchain.ini", destination: "/home/vagrant/toolchain.ini"
    web.vm.provision "file", source: "./frontend/vm_apt_web.txt", destination: "/home/vagrant/vm_apt.txt"
    web.vm.provision :shell, inline: $frontend
  end

  config.vm.define "epub" do |epub|
    epub.vm.box = "bento/ubuntu-23.10"
    epub.vm.box_version = "202402.01.0"

    epub.vm.synced_folder './frontend', '/vagrant/frontend'
    epub.vm.synced_folder './content', '/vagrant/content'

    epub.vm.provision "file", source: "./frontend/py_modules/code_projects/toolchain.ini", destination: "/home/vagrant/toolchain.ini"
    epub.vm.provision "file", source: "./frontend/vm_apt_epub.txt", destination: "/home/vagrant/vm_apt.txt"
    epub.vm.provision :shell, inline: $epub
  end

end
