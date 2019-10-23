
Vagrant.configure("2") do |config|

  config.vm.define "web" do |web|
    web.vm.box = "bento/ubuntu-18.04"

    web.vm.network "forwarded_port", guest: 8080, host: 8080, host_ip: "127.0.0.1"

    web.vm.provider "virtualbox" do |vb|
        vb.customize ["setextradata", :id, "VBoxInternal2/SharedFoldersEnableSymlinksCreate/v-root", "1"]
    end

    web.vm.provision :shell, path: "frontend/bootstrap.sh"
    web.vm.synced_folder '.', '/vagrant', disabled: true
    web.vm.synced_folder './frontend', '/vagrant/frontend'
    web.vm.synced_folder './content', '/vagrant/content'
  end

  config.vm.define "server" do |server|
    server.vm.box = "bento/ubuntu-18.04"
    server.vm.network "forwarded_port", guest: 8000, host: 8000, host_ip: "127.0.0.1"
    server.vm.network "forwarded_port", guest: 6379, host: 6379, host_ip: "127.0.0.1"
    server.vm.provider "virtualbox" do |vb|
      vb.customize ["setextradata", :id, "VBoxInternal2/SharedFoldersEnableSymlinksCreate/v-root", "1"]
    end
    server.vm.provision :shell, path: "backend/bootstrap.sh"
    server.vm.synced_folder '.', '/vagrant', disabled: true
    server.vm.synced_folder './backend', '/vagrant'
  end
end
