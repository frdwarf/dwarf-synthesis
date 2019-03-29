#!/bin/bash

USAGE="$0 output_dir nb_tests"

if [ "$#" -lt 2 ] ; then
    >&2 echo -e "Missing argument(s). Usage:\n$USAGE"
    exit 1
fi

DIR=$1
NB_TESTS=$2

mkdir -p "$DIR"
echo -n ">>>          "
for _num in $(seq 1 $NB_TESTS); do
    num=$(printf "%02d" $_num)
    echo -ne "\r>>> $num.c          "
    path="$DIR/$num"
    csmith > "$path.c"
    sed -i 's/^static \(.* func_\)/\1/g' "$path.c"
    echo -ne "\r>>> $num.bin          "
    gcc -O2 -I/usr/include/csmith-2.3.0/ -w "$path.c" -o "$path.orig.bin"
    objcopy --remove-section '.eh_frame' --remove-section '.eh_frame_hdr' \
        "$path.orig.bin" "$path.bin"
    echo -ne "\r>>> $num.eh.bin          "
    ../synthesize_dwarf.sh "$path.bin" "$path.eh.bin"
done

echo ""
