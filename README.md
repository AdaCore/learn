# learn.adacore.com

Sources for AdaCore's learn.adacore.com website

---

![Typescript Test Suite](https://github.com/AdaCore/learn/workflows/Typescript%20Test%20Suite/badge.svg)
![Sphinx Plugin Tests](https://github.com/AdaCore/learn/workflows/Sphinx%20Plugin%20Tests/badge.svg)
![Sphinx Content Tests](https://github.com/AdaCore/learn/workflows/Sphinx%20Content%20Tests/badge.svg)

## Requirements

This project requires Vagrant and VirtualBox

## Getting started

To setup for development run:
```
$ vagrant up
```
This will spin up two vms:

web: Is the build system for the frontend web content. This includes the
webpack build system and sphinx build.

epub: Is the publishing server. This includes all packages needed to
generate the learn website.

To build and start the development server for the frontend, run:
```
$ vagrant ssh web

# The following commands will be run inside the vm

$ source /vagrant/venv/bin/activate
$ cd /vagrant/frontend
$ yarn run dev
```
This will run webpack on the typescript and scss, then sphinx for the rst
using `make local` which will point the widgets at 127.0.0.1:8000

You can then point your browser on your host to 127.0.0.1:8080 to see the learn
website being served from vagrant.


To build and start the publishing server, run:
```
$ vagrant ssh epub

# The following commands will be run inside the vm

$ cd /vagrant
$ source venv/bin/activate
$ make site
```
This will build the content for the learn website. You can find it in the
`/vagrant/frontend/dist` directory.
