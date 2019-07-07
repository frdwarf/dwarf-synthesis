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


def detect_clang_flat_to_pyramid(rows):
    """ Artificially repair clang flat callee-saved saving to a gcc pyramid-like shape.

    Eg., clang will generate
       LOC           CFA      rbx   rbp   ra
    0000000000007180 rsp+8    u     u     c-8
    0000000000007181 rsp+16   u     u     c-8
    0000000000007182 rsp+24   u     u     c-8
    0000000000007189 rsp+7632 c-24  c-16  c-8


    while we would wish to have
       LOC           CFA      rbx   rbp   ra
    0000000000007180 rsp+8    u     u     c-8
    0000000000007181 rsp+16   u     c-16  c-8
    0000000000007182 rsp+24   c-24  c-16  c-8
    0000000000007189 rsp+7632 c-24  c-16  c-8

    The repair strategy is as follows:
    - ignore the implicit first row
    - find the first k lines such that only CFA changes from line to line, with a delta
      of +8, with address delta of 2. (push is 2 bytes long)
    - for every callee-saved R that concerns us and that is defined at line k+1 with
      offset c-x, while rsp+x is the CFA of line k' <= k, redefine R as c-k in lines
      [k'; k[
    """

    def is_flatness_row(row, prev_cfa, prev_loc, first_row=False):
        for reg in row:
            if reg not in ["LOC", "CFA", "ra"] and row[reg] != "u":
                return prev_cfa, prev_loc, True
        cfa = row["CFA"]
        if cfa[:4] != "rsp+":
            return prev_cfa, prev_loc, True
        cfa_offset = int(cfa[4:])
        if cfa_offset != prev_cfa + 8:
            return prev_cfa, prev_loc, True
        prev_cfa += 8
        loc = row["LOC"]
        if not first_row and loc > prev_loc + 2:
            return prev_cfa, prev_loc, True
        prev_loc = loc

        return prev_cfa, prev_loc, False

    def try_starting_at(start_row):
        if len(rows) < start_row + 1:  # Ensure we have at least the start row
            return rows, False

        flatness_row_id = start_row
        if rows[1]["CFA"][:4] != "rsp+":
            return rows, False
        first_cfa = int(rows[start_row]["CFA"][4:])
        prev_cfa = first_cfa
        prev_loc = rows[start_row]["LOC"]
        first_row = True

        for row in rows[start_row + 1 :]:
            prev_cfa, prev_loc, flatness = is_flatness_row(
                row, prev_cfa, prev_loc, first_row
            )
            first_row = False
            if flatness:
                break
            flatness_row_id += 1

        flatness_row_id += 1
        if flatness_row_id - start_row <= 1 or flatness_row_id >= len(rows):
            return rows, False  # nothing to change
        flatness_row = rows[flatness_row_id]

        reg_changes = {}
        for reg in flatness_row:
            if reg in ["LOC", "CFA", "ra"]:
                continue
            rule = flatness_row[reg]
            if rule[:2] != "c-":
                return rows, False  # Not a flat_to_pyramid after all
            rule_offset = int(rule[2:])
            rule_offset_rectified = rule_offset - first_cfa
            if rule_offset_rectified % 8 != 0:
                return rows, False
            row_change_id = rule_offset_rectified // 8 + start_row
            reg_changes[reg] = (row_change_id, rule)

        for reg in reg_changes:
            change_from, rule = reg_changes[reg]
            for row in rows[change_from:flatness_row_id]:
                row[reg] = rule

        return rows, True

    for start_row in [1, 2]:
        mod_rows, modified = try_starting_at(start_row)
        if modified:
            return mod_rows
    return rows


def parse_fde_row(line, reg_cols):
    vals = list(map(lambda x: x.strip(), line.split()))
    assert len(vals) > reg_cols["ra"]  # ra is the rightmost useful column

    out = {"LOC": int(vals[0], 16), "CFA": vals[1]}

    for reg in reg_cols:
        col_id = reg_cols[reg]
        out[reg] = vals[col_id]

    if "rbp" not in out:
        out["rbp"] = "u"

    return out


def clean_rows(rows):
    # Merge equivalent contiguous rows
    if not rows:
        return rows
    assert len(rows) > 0
    out_rows = [rows[0]]
    for row in rows[1:]:
        if not row == out_rows[-1]:
            filtered_row = row
            filter_out = []
            for reg in filtered_row:
                if reg not in ["LOC", "CFA", "rbp", "ra"]:
                    filter_out.append(reg)
            for reg in filter_out:
                filtered_row.pop(reg)
            out_rows.append(filtered_row)
    return out_rows


def parse_fde(lines):
    assert len(lines) > 0
    try:
        pc_beg, pc_end = parse_fde_head(lines[0])
    except NotFDE:
        return

    rows = [{"LOC": 0, "CFA": "rsp+8", "rbp": "u", "ra": "c-8"}]  # Implicit CIE row

    if len(lines) >= 2:  # Has content
        head_row = list(map(lambda x: x.strip(), lines[1].split()))
        reg_cols = {}
        for pos, reg in enumerate(head_row):
            if reg not in ["LOC", "CFA"]:
                reg_cols[reg] = pos

        for line in lines[2:]:
            rows.append(parse_fde_row(line, reg_cols))

    rows = detect_clang_flat_to_pyramid(rows)
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
        return {"CFA": row["CFA"], "ra": row["ra"], "rbp": row["rbp"]}

    def loc_of(rch):
        return rch[1]["LOC"]

    rows = [orig["rows"], synth["rows"]]
    cur_val = [vals_of(rows[0][0]), vals_of(rows[1][0])]

    rowchanges = []
    for typ in [0, 1]:
        for row in rows[typ]:
            rowchanges.append((typ, row))
    rowchanges.sort(key=loc_of)

    mismatch_count = 0
    match_count = 0
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
            mismatch_count += 1
        else:
            match_count += 1

    return mismatch_count, match_count


def parse_sym_table(handle):
    def readint(x):
        if x.startswith("0x"):
            return int(x[2:], 16)
        return int(x)

    out_map = {}
    for line in handle:
        line = line.strip()
        if line == "===":
            break

        spl = list(map(lambda x: x.strip(), line.split()))
        loc = int(spl[1], 16)
        size = readint(spl[2])
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
    good_match = 0
    for (orig, synth) in matched:
        cur_mismatch, cur_match = match_fde(orig, synth)
        mismatches += cur_mismatch
        good_match += cur_match
    reports = []
    if mismatches > 0:
        reports.append("{} mismatches - {} well matched".format(mismatches, good_match))
    if unmatched_orig:
        worth_reporting = False
        for unmatched in unmatched_orig:
            if len(unmatched["rows"]) > 1:
                worth_reporting = True
                break
        if worth_reporting:
            unmatched_addrs = [fde_pos(fde) for fde in unmatched_orig]
            reports.append(
                "{} unmatched (orig): {}".format(
                    len(unmatched_orig), ", ".join(unmatched_addrs)
                )
            )
    if unmatched_synth:
        unmatched_addrs = [fde_pos(fde) for fde in unmatched_synth]
        reports.append(
            "{} unmatched (synth): {}".format(
                len(unmatched_synth), ", ".join(unmatched_addrs)
            )
        )

    if reports:
        # If we had some errors to report, let's report positive data too
        reports.append("{} matched".format(len(matched)))
        print("{}: {}".format(test_name, "; ".join(reports)))
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
