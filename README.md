# learn.adacore.com

Sources for AdaCore's learn.adacore.com website

## Requirements

This project requires Vagrant and VirtualBox

## Getting started

To setup for development run:
```
$ vagrant up
$ vagrant ssh

# From the vagrant VM run:

$ cd /vagrant/engine
$ yarn run dev
```

This will run webpack on the typescript and scss, then sphinx for the rst using `make local` which will point the widgets at 127.0.0.1:8000

You can then point your browser on your host to 127.0.0.1:8080 to see the learn website being served from vagrant.
