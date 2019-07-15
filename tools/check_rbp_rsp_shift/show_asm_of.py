#!/usr/bin/env python

import sys
import subprocess


def fetch_disasm(elfpath, addr):
    output = subprocess.check_output(['  # TODO


for line in sys.stdin:
    line_data = line.strip().split(":")[0]
    elfpath, kind, addr = line_data.split()
    elfpath = elfpath[1:-1]  # Remove '[]'
    if kind != "(E)":
        continue

    print(line, end="")
    print(fetch_disasm(elfpath, addr), end="")
    print("------")
