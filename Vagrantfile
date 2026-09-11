$frontend = <<-SHELL
  #!/bin/bash -eux

  # Keep downloaded .deb files in a host-side cache, so a reprovision does not
  # re-fetch them. Redirected rather than bind-mounted over
  # /var/cache/apt/archives, so apt's lock and partial/ handling stays
  # explicit. This must precede anything that runs apt, including the
  # NodeSource setup script below.
  mkdir -p /vagrant_cache/apt/partial
  echo 'Dir::Cache::Archives "/vagrant_cache/apt";' > /etc/apt/apt.conf.d/99-learn-cache
  # apt drops privileges to the _apt user to download, which cannot read a
  # vboxsf share owned by vagrant; without this it warns on every invocation.
  echo 'APT::Sandbox::User "root";' >> /etc/apt/apt.conf.d/99-learn-cache

  # Enable the NodeSource repository
  curl -sL https://deb.nodesource.com/setup_24.x | bash -

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

  # Install/check packages from list for reproducibility.
  # Set VM_APT_PIN=0 to skip this step. That is needed when bootstrapping a
  # new Ubuntu base box, whose archive does not carry the pinned versions.
  if [ "${VM_APT_PIN:-1}" = "1" ]; then
    DEBIAN_FRONTEND=noninteractive apt-get install \
      --allow-downgrades -y $(cat /home/vagrant/vm_apt.txt)
  else
    echo "VM_APT_PIN=0 -- skipping installation of pinned packages"
  fi

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

  # Toolchain download cache: fetch each tarball into the host-side folder
  # mounted at /vagrant_cache/gnat, verified against its upstream .sha256.
  # The script also runs on the host -- `vm_toolchain_fetch.sh --all` warms
  # the cache before `vagrant up`.
  export LEARN_VM_CACHE_GNAT=/vagrant_cache/gnat
  toolchain_fetch=/vagrant/frontend/vm/vm_toolchain_fetch.sh

  install_toolchain () {
    local tool=$1
    local ver=$2
    local tarball
    local tmp

    # Invoked via bash rather than directly: the repo has
    # core.fileMode disabled and the script arrives over a vboxsf
    # share, so the executable bit cannot be relied on here.
    tarball=$(bash ${toolchain_fetch} "${tool}" "${ver}")
    # Extract on the VM's own disk, never onto the shared cache folder.
    tmp=$(mktemp -d)
    tar xzf "${tarball}" -C "${tmp}"
    mv "${tmp}"/${tool}-* ${path_ada_toolchain_root}/${tool}/${ver}
    rm -rf "${tmp}"
  }

  # Install FSF GNAT
  # (Required tool: gnatchop)
  mkdir -p ${path_ada_toolchain_root}
  mkdir -p ${path_ada_toolchain_default}
  mkdir -p ${path_ada_toolchain_selected}

  gnat_version=(${toolchain_versions_gnat})
  mkdir ${path_ada_toolchain_root}/gnat
  for tool_version in ${gnat_version[@]}; do
    echo Installing GNAT $tool_version
    install_toolchain gnat ${tool_version}
  done

  ln -sf ${path_ada_toolchain_root}/gnat/${default_version_gnat}            ${path_ada_toolchain_default}/gnat

  chown -R vagrant:vagrant ${path_ada_toolchain_root}

  echo "export PATH=\\"${path_ada_toolchain_selected}/gnat/bin:${path_ada_toolchain_default}/gnat/bin:${PATH}\\"" >> /home/vagrant/.profile
  source /home/vagrant/.profile

  # Install learn deps
  python3 -m venv /vagrant/venv
  source /vagrant/venv/bin/activate
  pip3 install -r /vagrant/frontend/requirements_frozen.txt
  pip3 install -e /vagrant/frontend/python/rst_code_example_pipeline

  # File system: increase number of user watches
  # Needed for npm
  echo fs.inotify.max_user_watches=524288 | tee -a /etc/sysctl.conf && sysctl -p

  echo 'export COREPACK_ENABLE_DOWNLOAD_PROMPT=0' >> /home/vagrant/.bashrc
  yes | corepack enable
  # Keep pnpm's content-addressed store on the host cache, so a destroyed VM
  # does not take it with it. Nothing is lost by the store living on a shared
  # folder: node_modules is on one too, so pnpm already copies rather than
  # hardlinks.
  sudo -u vagrant bash -c "export COREPACK_ENABLE_DOWNLOAD_PROMPT=0; cd /vagrant/frontend && pnpm config set store-dir /vagrant_cache/node && pnpm install --frozen-lockfile"

SHELL

$epub = <<-SHELL
  #!/bin/bash -eux

  # Keep downloaded .deb files in a host-side cache, so a reprovision does not
  # re-fetch them. Redirected rather than bind-mounted over
  # /var/cache/apt/archives, so apt's lock and partial/ handling stays
  # explicit. This must precede anything that runs apt, including the
  # NodeSource setup script below.
  mkdir -p /vagrant_cache/apt/partial
  echo 'Dir::Cache::Archives "/vagrant_cache/apt";' > /etc/apt/apt.conf.d/99-learn-cache
  # apt drops privileges to the _apt user to download, which cannot read a
  # vboxsf share owned by vagrant; without this it warns on every invocation.
  echo 'APT::Sandbox::User "root";' >> /etc/apt/apt.conf.d/99-learn-cache

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

  # Install/check packages from list for reproducibility.
  # Set VM_APT_PIN=0 to skip this step. That is needed when bootstrapping a
  # new Ubuntu base box, whose archive does not carry the pinned versions.
  if [ "${VM_APT_PIN:-1}" = "1" ]; then
    DEBIAN_FRONTEND=noninteractive apt-get install \
      --allow-downgrades -y $(cat /home/vagrant/vm_apt.txt)
  else
    echo "VM_APT_PIN=0 -- skipping installation of pinned packages"
  fi

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

  # Toolchain download cache: fetch each tarball into the host-side folder
  # mounted at /vagrant_cache/gnat, verified against its upstream .sha256.
  # The script also runs on the host -- `vm_toolchain_fetch.sh --all` warms
  # the cache before `vagrant up`.
  export LEARN_VM_CACHE_GNAT=/vagrant_cache/gnat
  toolchain_fetch=/vagrant/frontend/vm/vm_toolchain_fetch.sh

  install_toolchain () {
    local tool=$1
    local ver=$2
    local tarball
    local tmp

    # Invoked via bash rather than directly: the repo has
    # core.fileMode disabled and the script arrives over a vboxsf
    # share, so the executable bit cannot be relied on here.
    tarball=$(bash ${toolchain_fetch} "${tool}" "${ver}")
    # Extract on the VM's own disk, never onto the shared cache folder.
    tmp=$(mktemp -d)
    tar xzf "${tarball}" -C "${tmp}"
    mv "${tmp}"/${tool}-* ${path_ada_toolchain_root}/${tool}/${ver}
    rm -rf "${tmp}"
  }

  # Install FSF GNAT
  mkdir -p ${path_ada_toolchain_root}
  mkdir -p ${path_ada_toolchain_default}
  mkdir -p ${path_ada_toolchain_selected}

  gnat_version=(${toolchain_versions_gnat})
  mkdir ${path_ada_toolchain_root}/gnat
  for tool_version in ${gnat_version[@]}; do
    echo Installing GNAT $tool_version
    install_toolchain gnat ${tool_version}
  done

  gnat_prove_version=(${toolchain_versions_gnatprove})
  mkdir ${path_ada_toolchain_root}/gnatprove
  for tool_version in ${gnat_prove_version[@]}; do
    echo Installing GNATprove $tool_version
    install_toolchain gnatprove ${tool_version}
  done

  gprbuild_version=(${toolchain_versions_gprbuild})
  mkdir ${path_ada_toolchain_root}/gprbuild
  for tool_version in ${gprbuild_version[@]}; do
    echo Installing GPRbuild $tool_version
    install_toolchain gprbuild ${tool_version}
  done

  rm -f ${path_ada_toolchain_default}/*

  ln -sf ${path_ada_toolchain_root}/gnat/${default_version_gnat}            ${path_ada_toolchain_default}/gnat
  ln -sf ${path_ada_toolchain_root}/gnatprove/${default_version_gnatprove}  ${path_ada_toolchain_default}/gnatprove
  ln -sf ${path_ada_toolchain_root}/gprbuild/${default_version_gprbuild}    ${path_ada_toolchain_default}/gprbuild

  chown -R vagrant:vagrant ${path_ada_toolchain_root}

  echo "export PATH=\\"${path_ada_toolchain_selected}/gnat/bin:${path_ada_toolchain_selected}/gprbuild/bin:${path_ada_toolchain_selected}/gnatprove/bin:${path_ada_toolchain_default}/gnat/bin:${path_ada_toolchain_default}/gprbuild/bin:${path_ada_toolchain_default}/gnatprove/bin:${PATH}\\"" >> /home/vagrant/.profile
  source /home/vagrant/.profile

  # Install learn deps
  python3 -m venv /vagrant/venv
  source /vagrant/venv/bin/activate
  pip3 install -r /vagrant/frontend/requirements_frozen.txt
  pip3 install -e /vagrant/frontend/python/rst_code_example_pipeline

  # File system: increase number of user watches
  # Needed for npm
  echo fs.inotify.max_user_watches=524288 | tee -a /etc/sysctl.conf && sysctl -p

  echo 'export COREPACK_ENABLE_DOWNLOAD_PROMPT=0' >> /home/vagrant/.bashrc
  yes | corepack enable
  # Keep pnpm's content-addressed store on the host cache, so a destroyed VM
  # does not take it with it. Nothing is lost by the store living on a shared
  # folder: node_modules is on one too, so pnpm already copies rather than
  # hardlinks.
  sudo -u vagrant bash -c "export COREPACK_ENABLE_DOWNLOAD_PROMPT=0; cd /vagrant/frontend && pnpm config set store-dir /vagrant_cache/node && pnpm install --frozen-lockfile"

SHELL

require 'fileutils'

# Installation of the pinned package versions is enabled by default.
# Set VM_APT_PIN=0 to disable it for a base-box bootstrap.
vm_apt_pin = ENV.fetch("VM_APT_PIN", "1")

# Host-side download cache for the GNAT-FSF toolchain tarballs, so that
# destroying a VM does not throw them away. Redirect it with
# LEARN_VM_CACHE_GNAT -- it holds several GB and may belong on another disk.
vm_cache_gnat = File.expand_path(
  ENV.fetch("LEARN_VM_CACHE_GNAT", ".toolchains/gnat"), __dir__)

# Host-side pnpm store, so that `pnpm install --frozen-lockfile` does not
# re-download half a gigabyte on every reprovision. Redirect it with
# LEARN_VM_CACHE_NODE.
vm_cache_node = File.expand_path(
  ENV.fetch("LEARN_VM_CACHE_NODE", ".toolchains/node"), __dir__)

# Host-side apt archive, shared by both VMs. Redirect it with
# LEARN_VM_CACHE_APT.
vm_cache_apt = File.expand_path(
  ENV.fetch("LEARN_VM_CACHE_APT", ".toolchains/apt"), __dir__)

# Expanded against this file's directory, so that a relative override still
# names one place: the helper scripts in frontend/vm/ resolve it the same way.
#
# Vagrant refuses to start if a synced folder's source does not exist, so the
# cache directories have to be created before they are declared below.
[vm_cache_gnat, vm_cache_node, vm_cache_apt].each { |d| FileUtils.mkdir_p(d) }

Vagrant.configure("2") do |config|

  config.vm.provider "virtualbox" do |vb|
    vb.customize ["setextradata", :id, "VBoxInternal2/SharedFoldersEnableSymlinksCreate/v-root", "1"]
  end

  config.vm.synced_folder '.', '/vagrant', disabled: true

  config.vm.define "web" do |web|
    web.vm.box = "bento/ubuntu-24.04"
    web.vm.box_version = "202510.26.0"
    web.vm.network "forwarded_port", guest: 8080, host: 8080, host_ip: "127.0.0.1"

    web.vm.synced_folder './frontend', '/vagrant/frontend'
    web.vm.synced_folder './content', '/vagrant/content'
    web.vm.synced_folder vm_cache_gnat, '/vagrant_cache/gnat'
    web.vm.synced_folder vm_cache_node, '/vagrant_cache/node'
    web.vm.synced_folder vm_cache_apt,  '/vagrant_cache/apt'

    web.vm.provision "file", source: "./frontend/python/rst_code_example_pipeline/src/rst_code_example_pipeline/data/toolchain.ini", destination: "/home/vagrant/toolchain.ini"
    web.vm.provision "file", source: "./frontend/vm/vm_apt_web.txt", destination: "/home/vagrant/vm_apt.txt"
    web.vm.provision :shell, inline: $frontend,
                     env: { "VM_APT_PIN" => vm_apt_pin,
                            "LEARN_VM_NAME" => "web" }
  end

  config.vm.define "epub" do |epub|
    epub.vm.box = "bento/ubuntu-24.04"
    epub.vm.box_version = "202510.26.0"

    epub.vm.synced_folder './frontend', '/vagrant/frontend'
    epub.vm.synced_folder './content', '/vagrant/content'
    epub.vm.synced_folder vm_cache_gnat, '/vagrant_cache/gnat'
    epub.vm.synced_folder vm_cache_node, '/vagrant_cache/node'
    epub.vm.synced_folder vm_cache_apt,  '/vagrant_cache/apt'

    epub.vm.provision "file", source: "./frontend/python/rst_code_example_pipeline/src/rst_code_example_pipeline/data/toolchain.ini", destination: "/home/vagrant/toolchain.ini"
    epub.vm.provision "file", source: "./frontend/vm/vm_apt_epub.txt", destination: "/home/vagrant/vm_apt.txt"
    epub.vm.provision :shell, inline: $epub,
                      env: { "VM_APT_PIN" => vm_apt_pin,
                             "LEARN_VM_NAME" => "epub" }
  end

end
