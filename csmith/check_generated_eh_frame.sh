#!/bin/bash

USAGE="$0 base_path\nThe base path of eg. 01/01.eh.bin is 01/01"

if [ "$#" -lt 1 ] ; then
    >&2 echo -e "Missing argument(s). Usage:\n$USAGE"
    exit 1
fi

base_path="$1"
orig_path="$1.orig.bin"
eh_path="$1.eh.bin"

py_checker="$(dirname "$0")/$(basename "$0" ".sh").py"

if ! [ -x "$orig_path" ] || ! [ -x "$eh_path" ]; then
    >&2 echo -e "$orig_path or $eh_path does not exist or is not executable"
    exit 1
fi

( ( readelf --syms "$orig_path" | grep "FUNC" ) ; \
    echo "===" ; \
    readelf -wF "$orig_path" ; \
    echo "===" ; \
    readelf -wF "$eh_path") | python $py_checker "$base_path"
