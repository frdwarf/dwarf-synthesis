#!/usr/bin/env python

import sys

for line in sys.stdin:
    if not line.startswith("["):
        print(line, end="")  # not our lines?
        continue
    firstpar = line.find(")")
    if firstpar < 0:
        print(line, end="")
        continue

    addr_beg = firstpar + 2
    addr_end = line.find(":", addr_beg)
    addr = line[addr_beg:addr_end]
    hexaddr = hex(int(addr))
    repl = line[:addr_beg] + hexaddr + line[addr_end:]
    print(repl, end="")
