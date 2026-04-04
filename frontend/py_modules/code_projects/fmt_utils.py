#! /usr/bin/env python3

import colors as C

def header(strn: str) -> str:
    return C.col("{}\n{}\n".format(strn, '*' * len(strn)), C.Colors.BLUE)

def error(loc: str, strn: str) -> None:
    print("{} {}: {}".format(C.col("ERROR", C.Colors.RED), loc, strn))

def simple_error(msg: str) -> None:
    print(C.col(msg, C.Colors.RED))

def simple_success(msg: str) -> None:
    print(C.col(msg, C.Colors.GREEN))
