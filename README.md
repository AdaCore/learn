# learn.adacore.com

Sources for AdaCore's learn.adacore.com website

## Requirements

This project requires Vagrant and VirtualBox

## Getting started

To setup for development run:
```
$ vagrant up
```
This will spin up two vms:

web: Is the the build system for the frontend web content. This includes the
webpack build system and sphinx build.

server: Is the backend server with the widget API.

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

To build and start the development server for the backend, run:
```
$ vagrant ssh server

# The following commands will be run inside the vm

$ source /vagrant/venv/bin/activate
$ cd /vagrant
$ flask run --host=0.0.0.0

# Open another terminal window and run

$ vagrant ssh server

# The following commands will be run inside the vm

$ source /vagrant/venv/bin/activate
$ cd /vagrant
$ celery worker -A celery_worker.celery -E --loglevel=DEBUG
```
