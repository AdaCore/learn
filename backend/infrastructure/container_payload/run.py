#!/usr/bin/env python

import sys
import subprocess


def safe_run(main, arg_list):

    line = ['sudo', '-u', 'unprivileged', 'timeout', '10s',
                'bash', '-c',
                'LD_PRELOAD=/preloader.so {} {}'.format(
                   main, "`echo {}`".format(" ".join(arg_list)))]

    subprocess.call(line)


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
