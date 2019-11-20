#!/usr/bin/env python
"""Safe Runner

This script is used to run applications inside the lxd container using the correct user and the preloader. This protects
the container from abusive file io and other security related attacks.

The script should be run with the following syntax:

./run.py <executable> [cli args]
"""

import subprocess
import sys


def safe_run(exe, arg_list):

    line = ['sudo', '-u', 'unprivileged', 'timeout', '10s',
                'bash', '-c',
                'LD_PRELOAD=/preloader.so {} {}'.format(
                   exe, "`echo {}`".format(" ".join(arg_list)))]

    subprocess.run(line)


if __name__ == '__main__':
    # perform some sanity checking on args - this is not meant to
    # be launched interactively

    if len(sys.argv) == 2:
        main = sys.argv[1]
        cli = []

    elif len(sys.argv) > 2:
        main = sys.argv[1]
        cli = sys.argv[2:]

    else:
        sys.stderr.write("run.py called incorrectly")
        sys.exit(1)

    safe_run(main, cli)
