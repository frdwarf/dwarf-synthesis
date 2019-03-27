#!/bin/bash

USAGE="$0 nb_tests kept_directory"

if [ "$#" -lt 1 ] ; then
    >&2 echo -e "Missing argument(s). Usage:\n$USAGE"
    exit 1
fi

NB_TESTS=$1
KEPT_DIRECTORY=$2
RUN_TIMEOUT=2

tmpdir="$(mktemp -d)"
error_count=0

function statusline {
    echo -ne "\r\e[1m[\e[32m$(printf "%03d/%03d" "$1" "$NB_TESTS")\e[39m] >>>\e[0m $2                              "
}

function reporterror {
    echo -e "\n\e[1;31m[$(printf "%03d" $1)] Error: \e[0m $2"
}

function mkpath {
    echo "$tmpdir/$1"
}

function failed_test {
    path=$(mkpath $1)
    cp "$path."* "$KEPT_DIRECTORY"
    reporterror "$1" "$2"
    error_count=$((error_count + 1))
}

function test_empty_dwarf {
    path="$2"
    statusline $1 "test DWARF emptyness"
    if [ -z "$(readelf -wF "$path.eh.bin")" ]; then
        return 1
    else
        return 0
    fi
}

function test_run {
    path="$2"
    statusline $1 "running binaries"
    orig="$(timeout $RUN_TIMEOUT "$path.orig.bin")"
    orig_status=$?
    synth="$(timeout $RUN_TIMEOUT "$path.eh.bin")"
    synth_status=$?

    if [ "$orig_status" -ne "$synth_status" ] || [ "$orig" != "$synth" ] ; then
        return 1
    else
        return 0
    fi
}

mkdir -p "$KEPT_DIRECTORY"

statusline "" ""
for num in $(seq 1 $NB_TESTS); do
    ## Generation, compilation, synthesis
    statusline $num "csmith"
    path=$(mkpath $num)
    csmith > "$path.c"
    statusline $num "compiling"
    gcc -O2 -I/usr/include/csmith-2.3.0/ -w "$path.c" -o "$path.orig.bin"
    objcopy --remove-section '.eh_frame' --remove-section '.eh_frame_hdr' \
        "$path.orig.bin" "$path.bin"
    statusline $num "generating dwarf"
    ../synthesize_dwarf.sh "$path.bin" "$path.eh.bin"

    ## Testing
    if ! test_empty_dwarf "$num" "$path"; then
        failed_test "$num" "empty generated DWARF"
    elif ! test_run "$num" "$path"; then
        failed_test "$num" "different execution behaviour"
    fi
done

echo ""
if [ "$error_count" -eq "0" ] ; then
    echo -e "\n== ALL TESTS PASSED =="
else
    echo -e "\n== FAILED TESTS: $error_count/$NB_TESTS =="
fi

rm -rf "$tmpdir"
