#!/usr/bin/env sh

all: destroy_container create_container

destroy_container:
	lxc list | grep safecontainer && lxc delete --force safecontainer || true

create_container:
	# create the container
	lxc list | grep safecontainer || \
          (lxc launch ubuntu:18.04 safecontainer -s default -c user.user-data="`cat cloud-config.yml`" && \
	# wait a bit to allow network to come up \
	sleep 5 ; lxc config set safecontainer limits.processes 300 )
