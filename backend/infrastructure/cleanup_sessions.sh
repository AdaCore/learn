#!/usr/bin/env sh

# Kill all old processes
lxc exec safecontainer -- killall  -u unprivileged --older-than 30s -signal KILL

# Delete all old directories, both in container and locally
lxc exec safecontainer -- find /tmp/ -mindepth 1  -type d -mmin +1 -exec rm -rf {}  \;
find /tmp/ -mindepth 1  -type d -mmin +1 -exec rm -rf {}  \;

(if [ -d /webapps/compile_server/bin/ ] ; then
    export PATH=/webapps/compile_server/bin/:$PATH
    cd /webapps/compile_server/cloudchecker
else
    cd ..
fi
./manage.py clear_sessions
)
