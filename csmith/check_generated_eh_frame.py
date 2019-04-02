#!/usr/bin/env python3
import sys
import argparse

verbose = False


class NotFDE(Exception):
    pass


def func_name(infos, symtb):
    for sym in symtb:
        if infos["beg"] == symtb[sym][0]:
            return sym
    return None


def parse_fde_head(line):
    spl = line.strip().split()
    assert len(spl) >= 2
    if spl[1] == "ZERO":
        raise NotFDE
    assert len(spl) >= 4
    typ = spl[3]
    if typ != "FDE":
        raise NotFDE
    assert len(spl) == 6
    pc_range = spl[5][3:]
    pc_beg, pc_end = map(lambda x: int(x, 16), pc_range.split(".."))

    return pc_beg, pc_end


def parse_fde_row(line, ra_col):
    vals = list(map(lambda x: x.strip(), line.split()))
    assert len(vals) > ra_col  # ra is the rightmost useful column
    out = {"LOC": int(vals[0], 16), "CFA": vals[1], "ra": vals[ra_col]}
    return out


def clean_rows(rows):
    # Merge equivalent contiguous rows
    if not rows:
        return rows
    assert len(rows) > 0
    out_rows = [rows[0]]
    for row in rows[1:]:
        if not row == out_rows[-1]:
            out_rows.append(row)
    return out_rows


def parse_fde(lines):
    assert len(lines) > 0
    try:
        pc_beg, pc_end = parse_fde_head(lines[0])
    except NotFDE:
        return

    rows = [{"LOC": 0, "CFA": "rsp+8", "ra": "c-8"}]  # Implicit CIE row

    if len(lines) >= 2:  # Has content
        head_row = list(map(lambda x: x.strip(), lines[1].split()))
        ra_col = head_row.index("ra")

        for line in lines[2:]:
            rows.append(parse_fde_row(line, ra_col))

    return {"beg": pc_beg, "end": pc_end, "rows": clean_rows(rows)}


def parse_eh_frame(handle, symtb):
    output = []
    cur_lines = []
    for line in handle:
        line = line.strip()
        if line == "===":
            return output
        if line.startswith("Contents of"):
            continue
        if line == "":
            if cur_lines != []:
                infos = parse_fde(cur_lines)
                if infos:
                    symname = func_name(infos, symtb)
                    if symname not in ["_start", "__libc_csu_init"]:
                        # These functions have weird instructions
                        output.append(infos)
                cur_lines = []
        else:
            cur_lines.append(line)
    return sorted(output, key=lambda x: x["beg"])


def match_segments(orig_eh, synth_eh):
    out = []
    matches = [[False] * len(orig_eh), [False] * len(synth_eh)]
    for orig_id, orig_fde in enumerate(orig_eh):
        is_plt = False
        for row in orig_fde["rows"]:
            if row["CFA"] == "exp":
                is_plt = True

        for synth_id, synth_fde in enumerate(synth_eh):
            if orig_fde["beg"] == synth_fde["beg"]:
                if is_plt:
                    matches[1][synth_id] = True  # PLT -- fake match
                    continue
                if matches[1][synth_id]:
                    if verbose:
                        print("Multiple matches (synth)")
                if matches[0][orig_id]:
                    if verbose:
                        print(
                            "Multiple matches (orig) {}--{}".format(
                                hex(orig_fde["beg"]), hex(orig_fde["end"])
                            )
                        )
                else:
                    matches[0][orig_id] = True
                    matches[1][synth_id] = True
                    out.append((orig_fde, synth_fde))
            elif (
                is_plt
                and orig_fde["beg"] <= synth_fde["beg"]
                and synth_fde["end"] <= orig_fde["end"]
            ):
                matches[1][synth_id] = True  # PLT -- fake match
        if is_plt:
            matches[0][orig_id] = True  # plt -- fake match

    unmatched_orig, unmatched_synth = [], []
    for orig_id, orig_match in enumerate(matches[0]):
        if not orig_match:
            unmatched_orig.append(orig_eh[orig_id])
    for synth_id, synth_match in enumerate(matches[1]):
        if not synth_match:
            unmatched_synth.append(synth_eh[synth_id])
    return out, unmatched_orig, unmatched_synth


def fde_pos(fde):
    return "{}--{}".format(hex(fde["beg"]), hex(fde["end"]))


def dump_light_fdes(fdes):
    for fde in fdes:
        print("FDE: {}".format(fde_pos(fde)))


def match_fde(orig, synth):
    def vals_of(row):
        return {"CFA": row["CFA"], "ra": row["ra"]}

    def loc_of(rch):
        return rch[1]["LOC"]

    rows = [orig["rows"], synth["rows"]]
    cur_val = [vals_of(rows[0][0]), vals_of(rows[1][0])]

    rowchanges = []
    for typ in [0, 1]:
        for row in rows[typ]:
            rowchanges.append((typ, row))
    rowchanges.sort(key=loc_of)

    matching = True
    for rowid, rowch in enumerate(rowchanges):
        typ, row = rowch[0], rowch[1]
        cur_val[typ] = vals_of(row)
        if len(rowchanges) > rowid + 1 and loc_of(rowch) == loc_of(
            rowchanges[rowid + 1]
        ):
            continue
        if cur_val[0] != cur_val[1]:
            if verbose:
                print(
                    "Mismatch {}: {} ; {}".format(
                        hex(row["LOC"]), cur_val[0], cur_val[1]
                    )
                )
            matching = False

    return matching


def parse_sym_table(handle):
    out_map = {}
    for line in handle:
        line = line.strip()
        if line == "===":
            break

        spl = list(map(lambda x: x.strip(), line.split()))
        loc = int(spl[1], 16)
        size = int(spl[2])
        name = spl[7]
        out_map[name] = (loc, size)
    return out_map


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "-v", "--verbose", action="store_true", help="Display verbose results"
    )
    parser.add_argument(
        "test_name",
        action="store",
        help="Base path of the test case (eg. some_test/01)",
    )
    return parser.parse_args()


def main():
    global verbose
    parser_args = parse_args()
    test_name = parser_args.test_name
    verbose = parser_args.verbose
    symtb = parse_sym_table(sys.stdin)
    orig_eh = parse_eh_frame(sys.stdin, symtb)
    synth_eh = parse_eh_frame(sys.stdin, symtb)
    matched, unmatched_orig, unmatched_synth = match_segments(orig_eh, synth_eh)
    # dump_light_fdes(unmatched_orig)
    # dump_light_fdes(unmatched_synth)

    mismatches = 0
    for (orig, synth) in matched:
        if not match_fde(orig, synth):
            mismatches += 1
    reports = []
    if mismatches > 0:
        reports.append("{} mismatches".format(mismatches))
    if unmatched_orig:
        reports.append("{} unmatched (orig)".format(len(unmatched_orig)))
    if unmatched_synth:
        reports.append("{} unmatched (synth)".format(len(unmatched_synth)))

    if reports:
        print("{}: {}".format(test_name, "; ".join(reports)))


if __name__ == "__main__":
    main()
