#!/usr/bin/python

from enum import IntEnum
import sys

""" Parse a `readelf -wF` output, trying to locate CFA=f(rbp) to CFA=g(rsp) changes,
and to detect the offset applied to rsp in such cases. """


class Eof(Exception):
    pass


program_name = sys.argv[1]


def log_entry(entry):
    print("[{}] {}".format(program_name, entry))


def parse_line(line):
    spl = line.strip().split(" ")
    addr = int(spl[0], 16)
    cfa = spl[1]
    return addr, cfa


def match_fde_header(line):
    spl = line.strip().split()
    if len(spl) == 6 and spl[3] == "FDE":
        return True
    return False


class CfaType(IntEnum):
    OTHER = 0
    RSP_BASED = 1
    RBP_BASED = 2


def get_cfa_type(cfa):
    if cfa.startswith("rsp"):
        return CfaType.RSP_BASED
    if cfa.startswith("rbp"):
        return CfaType.RBP_BASED
    return CfaType.OTHER


def parse_fde(lines):
    # Read until FDE head
    for line in lines:
        if match_fde_header(line):
            break

    try:
        post_header = next(lines)  # Waste a line — FDE columns head
        if not post_header.strip():  # Empty FDE — return now
            return True
    except StopIteration:
        return False

    # Read each row until an empty line is found

    prev_rbp = False  # Was the last line rbp indexed?
    closed_rbp_block = False  # Was there already a rbp-indexed block which is over?
    for line in lines:
        line = line.strip()
        if not line:  # Empty line — FDE end
            return True

        addr, cfa = parse_line(line)
        cfa_type = get_cfa_type(cfa)

        if cfa_type == CfaType.RSP_BASED and prev_rbp:
            closed_rbp_block = True
            if cfa != "rsp+8":
                log_entry(
                    "(E) {}: CFA={} after %rbp-based index".format(hex(addr), cfa)
                )

        if cfa_type == CfaType.RBP_BASED:
            prev_rbp = True
            if closed_rbp_block:
                log_entry("(W) {}: two %rbp blocks in function".format(addr))
        else:
            prev_rbp = False

    return False


if __name__ == "__main__":
    handle = sys.stdin
    while parse_fde(handle):
        pass
