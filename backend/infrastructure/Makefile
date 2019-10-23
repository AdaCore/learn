#!/usr/bin/env sh

all: create_container setup_container push_payload

destroy_container:
	lxc list | grep safecontainer && lxc delete --force safecontainer || true

create_container:
	# create the container
	lxc list | grep safecontainer || \
          (lxc launch ubuntu:`cat /etc/issue | cut -d ' '  -f2 | cut -d '.' -f 1-2` safecontainer -s default && \
	# wait a bit to allow network to come up \
	sleep 5 ; lxc config set safecontainer limits.processes 300 )

setup_container:
	# install this first!
	lxc exec safecontainer -- apt install make

push_payload:
        # push the payload
	lxc file push --recursive container_payload/ safecontainer/root/

	# run the makefile on the container
	lxc exec safecontainer -- bash -c "cd /root/container_payload ; make -f Makefile.safecontainer"
