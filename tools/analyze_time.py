#!/usr/bin/env python3

import sys
import re
from collections import namedtuple
import numpy as np

Timestamp = namedtuple("Timestamp", ["name", "timestamp"])
AvgNumber = namedtuple("AvgNumber", ["name", "avg", "sd"])


def read_ts_file(handle):
    time_re = re.compile(
        r"^\~\~TIME\~\~ (?P<name>[^[]*) \[(?P<timestamp>[0-9]+\.[0-9]+)\]$"
    )
    timestamps = []
    for line in handle:
        match = time_re.match(line.strip())
        if match is None:  # Not a timer line
            continue
        timestamps.append(
            Timestamp(
                name=match.group("name"), timestamp=float(match.group("timestamp"))
            )
        )

    return timestamps


def analyze(timestamps):
    assert timestamps
    for ts in range(len(timestamps)):
        if not timestamps[ts]:
            print(ts)
        assert timestamps[ts]
    analyzed = []
    for pos in range(len(timestamps[0]) - 1):
        serie = [ts[pos + 1].timestamp - ts[pos].timestamp for ts in timestamps]
        analyzed.append(
            AvgNumber(
                name=timestamps[0][pos].name,
                avg=sum(serie) / len(serie),
                sd=np.sqrt(np.var(serie)),
            )
        )

    total_serie = [ts[-1].timestamp - ts[0].timestamp for ts in timestamps]
    analyzed.append(
        AvgNumber(
            name="Total",
            avg=sum(total_serie) / len(serie),
            sd=np.sqrt(np.var(total_serie)),
        )
    )
    return analyzed


analyze_paths = sys.argv[1:]
if not analyze_paths:
    analyze_paths = ["-"]

timestamp_series = [
    read_ts_file(sys.stdin if path == "-" else open(path, "r"))
    for path in analyze_paths
]

analyzed_timestamps = analyze(timestamp_series)

for analyzed_ts in analyzed_timestamps:
    print(
        "{}: {:.3f}±{:.3f} sec".format(
            analyzed_ts.name, analyzed_ts.avg, 2 * analyzed_ts.sd
        )
    )
