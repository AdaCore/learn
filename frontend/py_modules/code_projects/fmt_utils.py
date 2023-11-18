#! /usr/bin/env python3

import colors as C

def header(strn):
    return C.col("{}\n{}\n".format(strn, '*' * len(strn)), C.Colors.BLUE)

def error(loc, strn):
    print("{} {}: {}".format(C.col("ERROR", C.Colors.RED), loc, strn))

def simple_error(msg):
    print(C.col(msg, C.Colors.RED))

def simple_success(msg):
    print(C.col(msg, C.Colors.GREEN))
